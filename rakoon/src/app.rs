use crate::constants::{
    TransactionType, AUTH_PRIVATE_KEY, DEFAULT_SEMAPHORE_PERMITS, DEFAULT_TXS_PER_CORE, GREEN, RED,
    RESET,
};
use alloy::{
    eips::eip7702::SignedAuthorization,
    hex,
    providers::{IpcConnect, Provider, ProviderBuilder, RootProvider, WsConnect},
    rpc::types::Header,
    signers::{local::PrivateKeySigner, SignerSync},
};
use anyhow::Result;
use futures::{future::join_all, stream::FuturesUnordered, Stream, StreamExt};
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
    sync::Arc,
    time::{Duration, Instant},
};
use tokio::sync::Semaphore;

pub struct App {
    /// Rakoon prelude to display
    prelude: String,

    /// Nodes
    nodes: Vec<RootProvider>,

    /// Transaction type
    tx_type: TransactionType,

    /// Whether fuzzing is enabled or not
    fuzzing: bool,

    /// Batch size per cycle
    batch_size: u64,

    /// Transaction builder
    builder: TransactionBuilder,

    /// Transaction signer
    signer: FastPrivateKeySigner,

    /// Auth signer for EIP-7702 transactions
    auth_signer: FastPrivateKeySigner,

    /// Stream of block headers
    stream: Pin<Box<dyn Stream<Item = Header> + Send>>,

    /// Semaphore that controls the fire-and-forget throughput
    semaphore: Arc<Semaphore>,

    // Stats
    total_txs_sent: u64,
    txs_since_last_update: u64,
    elapsed_time: Duration,
    build_time: Duration,
    mutate_time: Duration,
    signing_time: Duration,
    wait_time: Duration,
    last_update: Instant,
}

impl App {
    pub async fn new(
        tx_type: TransactionType,
        key: String,
        ipc: Option<String>,
        ws: Option<String>,
        fuzzing: bool,
        prelude: String,
    ) -> Result<Self> {
        // First, create our signer, auth signer and contract deployer
        let key_bytes = hex::decode(key)?;
        let tmp_signer = PrivateKeySigner::from_slice(&key_bytes)?;
        let deployer = if let Some(ipc) = ipc.clone() {
            let conn = IpcConnect::new(ipc.clone());
            ProviderBuilder::new().wallet(tmp_signer.clone()).connect_ipc(conn).await?
        } else if let Some(ws) = ws.clone() {
            let conn = WsConnect::new(ws.clone());
            ProviderBuilder::new().wallet(tmp_signer.clone()).connect_ws(conn).await?
        } else {
            return Err(anyhow::anyhow!("Invalid URL"));
        };

        // Then, deploy the access list target if enabled
        let address = AccessListTarget::deploy(&deployer).await?;

        // Then, create our signer and auth signer
        let auth_key_bytes = hex::decode(AUTH_PRIVATE_KEY)?;
        let auth_signer = FastPrivateKeySigner::new(&auth_key_bytes)?;
        let signer = FastPrivateKeySigner::new(&key_bytes)?;

        // Then, connect to the node via IPC (or WS if IPC is not available)
        let num_cores = std::thread::available_parallelism().unwrap().get();
        let nodes: Vec<RootProvider> = if let Some(ipc) = ipc.clone() {
            let futures = (0..num_cores).map(|_| {
                let conn = IpcConnect::new(ipc.clone());
                ProviderBuilder::new().disable_recommended_fillers().connect_ipc(conn)
            });
            join_all(futures).await.into_iter().collect::<Result<Vec<_>, _>>()?
        } else if let Some(ws) = ws.clone() {
            let futures = (0..num_cores).map(|_| {
                let conn = WsConnect::new(ws.clone());
                ProviderBuilder::new().disable_recommended_fillers().connect_ws(conn)
            });
            join_all(futures).await.into_iter().collect::<Result<Vec<_>, _>>()?
        } else {
            return Err(anyhow::anyhow!("Invalid URL"));
        };

        // Create `Builder` and `FastPrivateSigner`s
        let builder = TransactionBuilder::new(*address.address(), &nodes[0]).await?;

        // Subscribe to block header stream
        let sub = nodes[0].subscribe_blocks().await?;
        let stream = Box::pin(sub.into_stream());

        // Set up the semaphore
        let semaphore = Arc::new(Semaphore::new(DEFAULT_SEMAPHORE_PERMITS));
        let batch_size = num_cores as u64 * DEFAULT_TXS_PER_CORE;

        Ok(Self {
            prelude,
            nodes,
            tx_type,
            fuzzing,
            batch_size,
            builder,
            signer,
            auth_signer,
            stream,
            semaphore,
            total_txs_sent: 0,
            txs_since_last_update: 0,
            elapsed_time: Duration::from_secs(0),
            build_time: Duration::from_secs(0),
            mutate_time: Duration::from_secs(0),
            signing_time: Duration::from_secs(0),
            wait_time: Duration::from_secs(0),
            last_update: Instant::now(),
        })
    }

    pub async fn run(&mut self, random: &mut impl Rng) -> Result<()> {
        let mut idx = 0;
        loop {
            tokio::select! {
                // If there is a new block, refresh cache
                Some(_) = self.stream.next() => {
                    self.builder.refresh_cache(&self.nodes[idx]).await?;
                    idx = (idx + 1) % self.nodes.len();
                }

                _ = tokio::task::yield_now() => {
                    if self.nodes[idx].get_chain_id().await.is_err() {
                        eprintln!("\n\n\x1b[1;31m[!] Crash detected, shutting down...\x1b[0m");
                        return Ok(());
                    }

                    // Build transactions
                    let build_start = Instant::now();
                    let mut unsigned_txs: Vec<Transaction> = (0..self.batch_size).map(|_| match self.tx_type {
                        TransactionType::Legacy => self.builder.build_legacy_tx(random),
                        TransactionType::Al => self.builder.build_access_list_tx(random),
                        TransactionType::Eip1559 => self.builder.build_eip1559_tx(random),
                        TransactionType::Eip7702 => self.builder.build_eip7702_tx(random),
                    }).collect::<Vec<_>>();
                    self.build_time = build_start.elapsed();

                    // Mutate transactions if fuzzing is enabled
                    let mutate_start = Instant::now();
                    if self.fuzzing {
                        unsigned_txs.iter_mut().for_each(|tx| { tx.mutate(random); });
                    }
                    self.mutate_time = mutate_start.elapsed();

                    // Sign transactions (including authorization lists)
                    let signing_start = Instant::now();
                    let signed_txs: Vec<SignedTransaction> = unsigned_txs.into_par_iter().map(|mut tx| {
                        if let Some(authorizations) = &tx.authorization_list {
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
                        }
                    }).collect::<Vec<_>>();
                    self.signing_time = signing_start.elapsed();

                    // Send the RPC requests through fire-and-forget tasks
                    let wait_start = Instant::now();
                    let futures = FuturesUnordered::new();
                    for tx in &signed_txs {
                        let mut encoded = String::new();
                        encoded.push_str("0x");
                        encoded.push_str(&hex::encode(tx.encode()));

                        futures.push(self.nodes[idx].client().request::<_, ()>("eth_sendRawTransaction", (encoded,)));
                    }
                    let permit = self.semaphore.clone().acquire_owned().await.unwrap();
                    tokio::spawn(async move { join_all(futures).await; drop(permit); });
                    self.wait_time = wait_start.elapsed();

                    self.total_txs_sent += self.batch_size as u64;
                    self.txs_since_last_update += self.batch_size as u64;

                    // Update terminal logging
                    if self.last_update.elapsed().as_secs_f64() >= 1.0 {
                        self.screen()?;
                        self.txs_since_last_update = 0;
                        self.last_update = Instant::now();
                    }
                    idx = (idx + 1) % self.nodes.len();
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
             {RED}{:02}h{RESET} {RED}{:02}m{RESET} {RED}{:02}s{RESET} | Build: \
             {RED}{:>6.2}ms{RESET} | Mutate: {RED}{:>6.2}ms{RESET} | Sign: {RED}{:>6.2}ms{RESET} \
             | Network wait time: {RED}{:>6.2}ms{RESET} | Free permits: {RED}{}{RESET}",
            self.total_txs_sent,
            self.txs_since_last_update,
            self.elapsed_time.as_secs() / 3600,
            (self.elapsed_time.as_secs() % 3600) / 60,
            self.elapsed_time.as_secs() % 60,
            self.build_time.as_secs_f64() * 1000.0,
            self.mutate_time.as_secs_f64() * 1000.0,
            self.signing_time.as_secs_f64() * 1000.0,
            self.wait_time.as_secs_f64() * 1000.0,
            self.semaphore.available_permits(),
        );

        io::stdout().flush()?;

        Ok(())
    }
}
