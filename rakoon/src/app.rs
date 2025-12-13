use crate::constants::{
    TransactionType, AUTH_PRIVATE_KEY, CLEAR_SCREEN, DEFAULT_CHANNEL_CAPACITY, DEFAULT_RPC_TIMEOUT,
    DEFAULT_TXS_PER_CORE, GREEN, RED, RESET,
};
use alloy::{
    eips::eip7702::SignedAuthorization,
    hex,
    providers::{Provider, ProviderBuilder, RootProvider},
    rpc::types::Header,
    signers::local::PrivateKeySigner,
    transports::TransportError,
};
use anyhow::Result;
use crossbeam::channel::{bounded, Receiver, Sender};
use futures::Stream;
use fuzztools::{
    builders::{contracts::AccessListTarget, TransactionBuilder},
    mutations::Mutable,
    transactions::{SignedTransaction, Transaction},
    utils::Signer,
};
use rand::Rng;
use rayon::iter::{IntoParallelIterator, IntoParallelRefMutIterator, ParallelIterator};
use std::{
    io::{self, Write},
    pin::Pin,
    sync::{
        atomic::{AtomicBool, AtomicU64, Ordering},
        Arc,
    },
    time::{Duration, Instant},
};
use tokio::runtime::Handle;

pub struct App {
    /// Rakoon prelude to display in the terminal
    prelude: String,

    /// Connection to the ethereum client
    node: RootProvider,

    /// Transaction type to fuzz
    tx_type: TransactionType,

    /// Whether fuzzing is enabled or not
    fuzzing: bool,

    /// Transaction builder
    builder: TransactionBuilder,

    /// Transaction signer
    signer: Signer,

    /// Auth signer for EIP-7702 transactions
    auth_signer: Signer,

    /// Stream of incoming block headers
    stream: Pin<Box<dyn Stream<Item = Header> + Send>>,

    /// Channel sender for dispatching signed transactions to the worker pool
    tx_sender: Sender<Vec<String>>,

    /// Channel receiver for incoming transaction batches (taken by dispatcher)
    tx_receiver: Option<Receiver<Vec<String>>>,

    /// Channel sender for completion notifications (taken by dispatcher)
    completion_sender: Option<Sender<u64>>,

    /// Channel receiver for completion notifications from workers
    completion_receiver: Receiver<u64>,

    /// Flag to signal connection loss (set by worker, read by main)
    connection_lost: Arc<AtomicBool>,

    // Stats
    total_txs_sent: u64,
    txs_since_last_update: u64,
    start_time: Instant,
    last_update: Instant,
}

impl App {
    pub async fn new(
        tx_type: TransactionType,
        key: String,
        url: String,
        fuzzing: bool,
        prelude: String,
    ) -> Result<Self> {
        // First, deploy our custom contracts
        let key_bytes = hex::decode(key)?;
        let deployer_signer = PrivateKeySigner::from_slice(&key_bytes)?;
        let deployer = ProviderBuilder::new().wallet(deployer_signer).connect(&url).await?;
        let access_list_target = AccessListTarget::deploy(&deployer).await?;
        // @todo deploy the other one: PrecompileTarget

        // Connect to the node and create some other stuff
        let node = ProviderBuilder::default().connect(&url).await?;
        let builder = TransactionBuilder::new(*access_list_target.address(), &node).await?;

        let auth_key_bytes = hex::decode(AUTH_PRIVATE_KEY)?;
        let auth_signer = Signer::new(&auth_key_bytes)?;
        let signer = Signer::new(&key_bytes)?;

        let sub = node.subscribe_blocks().await?;
        let stream = Box::pin(sub.into_stream());

        // Create channels and atomic values
        let (tx_sender, tx_receiver) = bounded::<Vec<String>>(DEFAULT_CHANNEL_CAPACITY);
        let (completion_sender, completion_receiver) = bounded::<u64>(DEFAULT_CHANNEL_CAPACITY);
        let connection_lost = Arc::new(AtomicBool::new(false));

        Ok(Self {
            prelude,
            node,
            tx_type,
            fuzzing,
            builder,
            signer,
            auth_signer,
            stream,
            tx_sender,
            tx_receiver: Some(tx_receiver),
            completion_sender: Some(completion_sender),
            completion_receiver,
            connection_lost,
            total_txs_sent: 0,
            txs_since_last_update: 0,
            start_time: Instant::now(),
            last_update: Instant::now(),
        })
    }

    pub async fn run(&mut self, random: &mut impl Rng) -> Result<()> {
        self.spawn_dispatcher()?;

        let num_cores = std::thread::available_parallelism()?.get();
        let batch_size = DEFAULT_TXS_PER_CORE * num_cores;
        let mut txs: Vec<Transaction> = (0..batch_size).map(|_| self.build_tx(random)).collect();

        loop {
            // Check for connection loss (set by worker thread)
            if self.connection_lost.load(Ordering::SeqCst) {
                eprintln!("\n\n\x1b[1;31m[!] Connection lost (detected by worker), shutting down...\x1b[0m");
                return Ok(());
            }

            // Drain all completion notifications (non-blocking)
            while let Ok(sent_count) = self.completion_receiver.try_recv() {
                self.total_txs_sent += sent_count;
                self.txs_since_last_update += sent_count;
            }

            tokio::select! {
                // If there is a new block, refresh cache
                Some(_) = futures::StreamExt::next(&mut self.stream) => {
                    self.builder.refresh(&self.node).await?;
                }

                _ = tokio::task::yield_now() => {
                    // Optionally mutate transactions
                    if self.fuzzing {
                        txs.iter_mut().for_each(|tx| { tx.mutate(random); });
                    }

                    // Sign and encode in parallel, then rebuild txs for next iteration
                    let (signer, auth_signer) = (&self.signer, &self.auth_signer);
                    let signed_txs: Vec<_> = txs
                        .par_iter_mut()
                        .map(|tx| Self::sign_and_encode(std::mem::take(tx), signer, auth_signer))
                        .collect();

                    // Rebuild transactions for next iteration (reuses allocation)
                    txs.iter_mut().for_each(|tx| { *tx = self.build_tx(random); });

                    // Send to the worker pool via crossbeam channel (blocks if full for backpressure)
                    if self.tx_sender.send(signed_txs).is_err() {
                        eprintln!("\n\n\x1b[1;31m[!] Worker pool shut down, exiting...\x1b[0m");
                        return Ok(());
                    }

                    // Update terminal logging
                    if self.last_update.elapsed().as_secs_f64() >= 1.0 {
                        self.screen()?;
                        self.txs_since_last_update = 0;
                        self.last_update = Instant::now();
                    }
                }
            }
        }
    }

    /// Spawns a background thread that dispatches transactions to the node
    fn spawn_dispatcher(&mut self) -> Result<()> {
        // Init some stuff that will be used in the thread pool
        let tx_receiver = self.tx_receiver.take().unwrap();
        let completion_sender = self.completion_sender.take().unwrap();
        let connection_lost = self.connection_lost.clone();
        let node = self.node.clone();

        // Create a thread pool
        let pool = rayon::ThreadPoolBuilder::new()
            .num_threads(std::thread::available_parallelism()?.get())
            .build()?;
        let timeout = Duration::from_secs(DEFAULT_RPC_TIMEOUT);
        let handle = Handle::current();

        std::thread::spawn(move || {
            for batch in tx_receiver {
                let count = AtomicU64::new(0);

                let result = pool.install(|| {
                    batch.into_par_iter().try_for_each(|tx| {
                        // If another thread has set the `connection_lost` flag, exit
                        if connection_lost.load(Ordering::Relaxed) {
                            return Err(());
                        }

                        // Send the transaction to the node with a timeout to avoid waiting
                        // indefinitely in case of a crash
                        let rpc = handle.block_on(async {
                            tokio::time::timeout(
                                timeout,
                                node.client().request::<_, ()>("eth_sendRawTransaction", (tx,)),
                            )
                            .await
                        });

                        // If the node is unresponsive or the connection is lost, set the
                        // `connection_lost` flag and exit
                        match rpc {
                            Err(_) | Ok(Err(TransportError::Transport(_))) => {
                                connection_lost.store(true, Ordering::SeqCst);
                                Err(())
                            }
                            Ok(_) => {
                                count.fetch_add(1, Ordering::Relaxed);
                                Ok(())
                            }
                        }
                    })
                });

                // Ping back the main thread with the number of transactions sent
                let _ = completion_sender.send(count.load(Ordering::Relaxed));
                if result.is_err() || connection_lost.load(Ordering::SeqCst) {
                    break;
                }
            }
        });

        Ok(())
    }

    /// Builds a transaction based on the configured transaction type
    #[inline]
    fn build_tx(&mut self, random: &mut impl Rng) -> Transaction {
        match self.tx_type {
            TransactionType::Legacy => self.builder.legacy(random),
            TransactionType::Eip2930 => self.builder.eip2930(random),
            TransactionType::Eip1559 => self.builder.eip1559(random),
            TransactionType::Eip7702 => self.builder.eip7702(random),
        }
    }

    /// Signs a transaction and returns it as a hex-encoded string
    fn sign_and_encode(mut tx: Transaction, signer: &Signer, auth_signer: &Signer) -> String {
        // Sign authorization list if present (EIP-7702)
        tx.signed_authorization_list = tx.authorization_list.as_ref().map(|auths| {
            auths
                .iter()
                .map(|auth| {
                    let sig = auth_signer.sign_hash(&auth.signature_hash()).unwrap();
                    SignedAuthorization::new_unchecked(
                        auth.clone(),
                        sig.v() as u8,
                        sig.r(),
                        sig.s(),
                    )
                })
                .collect()
        });

        let signature = signer.sign_hash(&tx.signing_hash()).unwrap();
        format!("0x{}", hex::encode(SignedTransaction { transaction: tx, signature }.encode()))
    }

    /// Prints the current stats to the screen
    fn screen(&self) -> Result<()> {
        let secs = self.start_time.elapsed().as_secs();
        print!(
            "{CLEAR_SCREEN}{}[{GREEN}+{RESET}] Total Txs: {RED}{}{RESET} | Tick: {RED}{}{RESET} txs | Time: {RED}{:02}h{RESET} {RED}{:02}m{RESET} {RED}{:02}s{RESET}",
            self.prelude,
            self.total_txs_sent,
            self.txs_since_last_update,
            secs / 3600,
            (secs % 3600) / 60,
            secs % 60,
        );
        io::stdout().flush()?;
        Ok(())
    }
}
