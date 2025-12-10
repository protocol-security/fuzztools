use crate::constants::{
    TransactionType, AUTH_PRIVATE_KEY, DEFAULT_CHANNEL_CAPACITY, DEFAULT_RPC_TIMEOUT,
    DEFAULT_TXS_PER_CORE, GREEN, RED, RESET,
};
use alloy::{
    eips::eip7702::SignedAuthorization,
    hex,
    providers::{Provider, ProviderBuilder, RootProvider},
    rpc::types::Header,
    signers::{local::PrivateKeySigner, SignerSync},
    transports::TransportError,
};
use anyhow::Result;
use crossbeam::channel::{bounded, Receiver, Sender};
use futures::Stream;
use fuzztools::{
    builders::{AccessListTarget, TransactionBuilder},
    mutations::Mutable,
    transactions::{SignedTransaction, Transaction},
    utils::FastPrivateKeySigner,
};
use rand::Rng;
use rayon::iter::{IntoParallelIterator, IntoParallelRefIterator, ParallelIterator};
use std::{
    io::{self, Write},
    pin::Pin,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
    time::{Duration, Instant},
};
use tokio::runtime::Handle;

pub struct App {
    /// Rakoon prelude to display
    prelude: String,

    /// Main node connection
    node: RootProvider,

    /// Transaction type
    tx_type: TransactionType,

    /// Whether fuzzing is enabled or not
    fuzzing: bool,

    /// Transaction builder
    builder: TransactionBuilder,

    /// Transaction signer
    signer: FastPrivateKeySigner,

    /// Auth signer for EIP-7702 transactions
    auth_signer: FastPrivateKeySigner,

    /// Stream of block headers
    stream: Pin<Box<dyn Stream<Item = Header> + Send>>,

    /// Channel sender for dispatching signed transactions to the worker pool
    tx_sender: Sender<Vec<String>>,

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
        // @todo deploy the other one

        // Then, create the main connection to the node and some other stuff
        let node = ProviderBuilder::default().connect(&url).await?;
        let builder = TransactionBuilder::new(*access_list_target.address(), &node).await?;

        let auth_key_bytes = hex::decode(AUTH_PRIVATE_KEY)?;
        let auth_signer = FastPrivateKeySigner::new(&auth_key_bytes)?;
        let signer = FastPrivateKeySigner::new(&key_bytes)?;

        let sub = node.subscribe_blocks().await?;
        let stream = Box::pin(sub.into_stream());

        // Create channels and atomic values
        let (tx_sender, tx_receiver) = bounded::<Vec<String>>(DEFAULT_CHANNEL_CAPACITY);
        let (completion_sender, completion_receiver) = bounded::<u64>(DEFAULT_CHANNEL_CAPACITY);
        let connection_lost = Arc::new(AtomicBool::new(false));
        let connection_lost_worker = connection_lost.clone();

        // Create a rayon threadpool
        let num_cores = std::thread::available_parallelism().unwrap().get();
        let pool = rayon::ThreadPoolBuilder::new().num_threads(num_cores).build()?;

        // Spawn a background thread that processes transaction
        let rpc_timeout = Duration::from_secs(DEFAULT_RPC_TIMEOUT);
        let node_for_workers = node.clone();
        let handle = Handle::current();

        std::thread::spawn(move || {
            for batch in tx_receiver {
                let dispatched_count = std::sync::atomic::AtomicU64::new(0);

                // Process each batch in parallel using rayon's threadpool
                // Use try_for_each to short-circuit on connection errors
                let batch_result = pool.install(|| {
                    batch.into_par_iter().try_for_each(|tx| {
                        // Check if connection was already lost (by another thread)
                        if connection_lost_worker.load(Ordering::Relaxed) {
                            return Err(());
                        }

                        // Wrap RPC call with timeout to detect unresponsive nodes quickly
                        let result = handle.block_on(async {
                            tokio::time::timeout(
                                rpc_timeout,
                                node_for_workers
                                    .client()
                                    .request::<_, ()>("eth_sendRawTransaction", (tx,)),
                            )
                            .await
                        });

                        match result {
                            Err(_elapsed) => {
                                // Node is unresponsive - we assume it crashed
                                connection_lost_worker.store(true, Ordering::SeqCst);
                                Err(())
                            }
                            Ok(rpc_result) => {
                                if let Err(e) = &rpc_result {
                                    // Transport error - node crashed for sure
                                    if matches!(e, TransportError::Transport(_)) {
                                        connection_lost_worker.store(true, Ordering::SeqCst);
                                        return Err(());
                                    }
                                }
                                dispatched_count.fetch_add(1, Ordering::Relaxed);
                                Ok(())
                            }
                        }
                    })
                });

                let _ = completion_sender.send(dispatched_count.load(Ordering::Relaxed));
                if batch_result.is_err() || connection_lost_worker.load(Ordering::SeqCst) {
                    break;
                }
            }
        });

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
            completion_receiver,
            connection_lost,
            total_txs_sent: 0,
            txs_since_last_update: 0,
            start_time: Instant::now(),
            last_update: Instant::now(),
        })
    }

    pub async fn run(&mut self, random: &mut impl Rng) -> Result<()> {
        let num_cores = std::thread::available_parallelism().unwrap().get();

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
                    self.builder.refresh_cache(&self.node).await?;
                }

                _ = tokio::task::yield_now() => {
                    // Build transactions
                    let mut unsigned_txs: Vec<Transaction> = (0..DEFAULT_TXS_PER_CORE * num_cores as u64).map(|_| match self.tx_type {
                        TransactionType::Legacy => self.builder.build_legacy_tx(random),
                        TransactionType::Eip2930 => self.builder.build_eip2930_tx(random),
                        TransactionType::Eip1559 => self.builder.build_eip1559_tx(random),
                        TransactionType::Eip7702 => self.builder.build_eip7702_tx(random),
                    }).collect::<Vec<_>>();

                    // Mutate transactions if fuzzing is enabled
                    if self.fuzzing {
                        unsigned_txs.iter_mut().for_each(|tx| {
                            tx.mutate(random);
                        });
                    }

                    // Sign transactions (including authorization lists) and encode them
                    let signed_txs: Vec<String> = unsigned_txs.into_par_iter().map(|mut tx| {
                        let signed_tx = if let Some(authorizations) = &tx.authorization_list {
                            // Sign the authorizations in parallel
                            let signed_authorizations = authorizations
                                .par_iter()
                                .map(|auth| {
                                    let signature = self.auth_signer.sign_hash_sync(&auth.signature_hash()).unwrap();
                                    SignedAuthorization::new_unchecked(auth.clone(), signature.v() as u8, signature.r(), signature.s())
                                })
                                .collect::<Vec<_>>();

                            tx.signed_authorization_list = Some(signed_authorizations);

                            let signature = self.signer.sign_hash_sync(&tx.signing_hash()).unwrap();
                            SignedTransaction {
                                transaction: tx,
                                signature,
                            }
                        } else {
                            let signature = self.signer.sign_hash_sync(&tx.signing_hash()).unwrap();
                            SignedTransaction {
                                transaction: tx,
                                signature,
                            }
                        };

                        let mut encoded = String::new();
                        encoded.push_str("0x");
                        encoded.push_str(&hex::encode(signed_tx.encode()));
                        encoded
                    }).collect::<Vec<_>>();

                    // Send to the worker pool via crossbeam channel
                    // This will block if the channel is full (backpressure)
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

    pub fn screen(&mut self) -> Result<()> {
        // Clear the screen
        std::process::Command::new("clear").status().unwrap();

        // Print `HEADER` + configuration
        print!("{}", self.prelude);

        // Print stats
        print!(
            "[{GREEN}+{RESET}] Total Txs: {RED}{}{RESET} | Tick: {RED}{}{RESET} txs | Time: \
             {RED}{:02}h{RESET} {RED}{:02}m{RESET} {RED}{:02}s{RESET}",
            self.total_txs_sent,
            self.txs_since_last_update,
            self.start_time.elapsed().as_secs() / 3600,
            (self.start_time.elapsed().as_secs() % 3600) / 60,
            self.start_time.elapsed().as_secs() % 60,
        );

        io::stdout().flush()?;

        Ok(())
    }
}
