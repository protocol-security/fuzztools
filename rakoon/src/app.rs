use crate::constants::{TransactionType, AUTH_PRIVATE_KEY, CLEAR_SCREEN, GREEN, RED, RESET};
use alloy::{
    eips::eip7702::SignedAuthorization, hex, providers::ProviderBuilder, signers::SignerSync,
};
use alloy_signer_local::Secp256k1Signer;
use anyhow::Result;
use fuzztools::{
    builders::{contracts::AccessListTarget, TransactionBuilder},
    mutations::Mutable,
    rpc::RpcClient,
    transactions::{SignedTransaction, Transaction},
};
use rand::Rng;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::{
    io::{self, Write},
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
    time::{Duration, Instant},
};
use tokio::sync::mpsc::{channel, Receiver, Sender};

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub(crate) struct Context {
    pub rpc_timeout: u64,
    pub tx_channel_capacity: usize,
    pub completion_channel_capacity: usize,
    pub block_poll_interval: u64,
    pub txs_per_core: usize,
    pub chunk_size: usize,
    pub screen_update_interval: u128,
}

pub(crate) struct App {
    /// Header being displayed on the screen
    prelude: String,
    /// Context of the application
    ctx: Context,
    /// RPC client connection
    client: Arc<RpcClient>,
    /// Transaction type to fuzz
    tx_type: TransactionType,
    /// If true, the transactions will be mutated
    fuzzing: bool,
    /// Transaction builder
    builder: TransactionBuilder,
    /// Transaction signer
    signer: Arc<Secp256k1Signer>,
    /// Authorization signer
    auth_signer: Arc<Secp256k1Signer>,

    /// Channel to send transactions to the dispatcher
    tx_sender: Sender<Vec<String>>,
    /// Channel where the dispatcher is listening
    tx_receiver: Option<Receiver<Vec<String>>>,
    /// Channel to send completion counts to the main thread
    completion_sender: Option<Sender<u64>>,
    /// Channel where the main thread is listening
    completion_receiver: Receiver<u64>,
    /// Atomic boolean to signal the main thread and other threads that the connection to the RPC
    /// client has been lost
    connection_lost: Arc<AtomicBool>,
}

impl App {
    pub(crate) async fn new(
        tx_type: TransactionType,
        key: String,
        url: String,
        fuzzing: bool,
        prelude: String,
        ctx: Context,
    ) -> Result<Self> {
        let client = RpcClient::new(url.clone(), Duration::from_secs(ctx.rpc_timeout));

        // First, query the node for basic info about the chain
        let methods = &["eth_chainId", "eth_gasPrice", "eth_maxPriorityFeePerGas"];
        let [chain_id, gas_price, priority_fee] =
            client.batch_call_no_params(methods).await?.try_into().unwrap();

        // Then, create the needed signers
        let key_bytes = hex::decode(&key)?;
        let signer = Secp256k1Signer::from_slice(&key_bytes)?;
        let deployer_signer = Secp256k1Signer::from_slice(&key_bytes)?;
        let auth_key_bytes = hex::decode(AUTH_PRIVATE_KEY)?;
        let auth_signer = Secp256k1Signer::from_slice(&auth_key_bytes)?;

        // Then, deploy the target contracts
        let deployer = ProviderBuilder::new().wallet(deployer_signer).connect(url.as_str()).await?;
        let access_list_target = AccessListTarget::deploy(&deployer).await?;
        let access_list_target_address = *access_list_target.address();

        // Finally, create the transaction builder
        let builder = TransactionBuilder::from_values(
            access_list_target_address,
            chain_id as u64,
            gas_price,
            priority_fee,
        );

        // And the communication channels
        let (tx_sender, tx_receiver) = channel::<Vec<String>>(ctx.tx_channel_capacity);
        let (completion_sender, completion_receiver) =
            channel::<u64>(ctx.completion_channel_capacity);

        Ok(Self {
            prelude,
            ctx,
            client: Arc::new(client),
            tx_type,
            fuzzing,
            builder,
            signer: Arc::new(signer),
            auth_signer: Arc::new(auth_signer),
            tx_sender,
            tx_receiver: Some(tx_receiver),
            completion_sender: Some(completion_sender),
            completion_receiver,
            connection_lost: Arc::new(AtomicBool::new(false)),
        })
    }

    pub(crate) async fn run(&mut self, random: &mut impl Rng) -> Result<()> {
        // This will spawn a thread that will be listening for incoming transactions and will
        // broadcast them in chunks to the RPC client
        self.spawn_dispatcher();

        let num_cores = std::thread::available_parallelism()?.get();
        let batch_size = (self.ctx.txs_per_core * num_cores).max(1000);
        let mut txs: Vec<Transaction> = (0..batch_size).map(|_| self.build_tx(random)).collect();

        let mut total_txs_sent = 0;
        let mut txs_since_last_update = 0;
        let mut last_update = Instant::now();
        let start_time = Instant::now();
        let mut last_block_tick = Instant::now();
        let mut current_block = 0;

        loop {
            if self.connection_lost.load(Ordering::Relaxed) {
                eprintln!("\n\n\x1b[1;31m[!] Node crashed, exiting...\x1b[0m");
                return Ok(());
            }

            // Drain stats from the dispatcher
            while let Ok(count) = self.completion_receiver.try_recv() {
                total_txs_sent += count;
                txs_since_last_update += count;
            }

            // Poll for new blocks every second so that we create transactions with up to date gas
            // prices
            if last_block_tick.elapsed() >= Duration::from_secs(self.ctx.block_poll_interval) {
                last_block_tick = Instant::now();
                if let Ok(height) = self.client.call("eth_blockNumber", json!([])).await {
                    if height > current_block {
                        current_block = height;
                        let _ = self.refresh_gas_prices().await;
                    }
                }
            }

            // Update screen
            if last_update.elapsed().as_millis() >= self.ctx.screen_update_interval {
                self.screen(total_txs_sent, txs_since_last_update, start_time)?;
                txs_since_last_update = 0;
                last_update = Instant::now();
            }

            // Mutate if fuzzing is enabled
            if self.fuzzing {
                for tx in &mut txs {
                    tx.mutate(random);
                }
            }

            // Sign in parallel
            let signer = self.signer.clone();
            let auth_signer = self.auth_signer.clone();
            let signed: Vec<String> =
                txs.par_iter().map(|tx| Self::sign_and_encode(tx, &signer, &auth_signer)).collect();

            // Send the signed transactions to the dispatcher
            if self.tx_sender.send(signed).await.is_err() {
                return Ok(());
            }

            // Create new transactions in place to avoid allocations
            for tx in &mut txs {
                *tx = self.build_tx(random);
            }
        }
    }

    // ---------------------
    // Dispatcher
    // ---------------------

    fn spawn_dispatcher(&mut self) {
        let mut rx = self.tx_receiver.take().unwrap();
        let tx = self.completion_sender.take().unwrap();
        let signal = self.connection_lost.clone();
        let client = self.client.clone();
        let chunk_size = self.ctx.chunk_size;

        tokio::spawn(async move {
            while let Some(batch) = rx.recv().await {
                if signal.load(Ordering::Relaxed) {
                    break;
                }

                for chunk in batch.chunks(chunk_size) {
                    let client = client.clone();
                    let signal = signal.clone();
                    let tx = tx.clone();
                    let chunk: Vec<_> = chunk.to_vec();
                    let count = chunk.len() as u64;

                    tokio::spawn(async move {
                        match client.send_raw_transactions(chunk).await {
                            Err(e) if e.is_connect() => {
                                signal.store(true, Ordering::Relaxed);
                            }
                            _ => {
                                let _ = tx.send(count).await;
                            }
                        }
                    });
                }
            }
        });
    }

    // ---------------------
    // Tx building & signing
    // ---------------------

    #[inline(always)]
    fn build_tx(&mut self, random: &mut impl Rng) -> Transaction {
        match self.tx_type {
            TransactionType::Legacy => self.builder.legacy(random),
            TransactionType::Eip2930 => self.builder.eip2930(random),
            TransactionType::Eip1559 => self.builder.eip1559(random),
            TransactionType::Eip7702 => self.builder.eip7702(random),
        }
    }

    fn sign_and_encode(
        tx: &Transaction,
        signer: &Secp256k1Signer,
        auth_signer: &Secp256k1Signer,
    ) -> String {
        let mut tx = tx.clone();
        if let Some(auths) = tx.authorization_list.as_ref() {
            tx.signed_authorization_list = Some(
                auths
                    .par_iter()
                    .map(|auth| {
                        let sig = auth_signer.sign_hash_sync(&auth.signature_hash()).unwrap();
                        SignedAuthorization::new_unchecked(
                            auth.clone(),
                            sig.v() as u8,
                            sig.r(),
                            sig.s(),
                        )
                    })
                    .collect(),
            );
        }

        let sig = signer.sign_hash_sync(&tx.signing_hash()).unwrap();
        format!("0x{}", hex::encode(SignedTransaction { transaction: tx, signature: sig }.encode()))
    }

    // ---------------------
    // Helpers
    // ---------------------

    /// Refreshes the gas prices from the RPC client
    async fn refresh_gas_prices(&mut self) -> Result<()> {
        let [gas_price, priority_fee] = self
            .client
            .batch_call_no_params(&["eth_gasPrice", "eth_maxPriorityFeePerGas"])
            .await?
            .try_into()
            .unwrap();
        self.builder.set_gas_prices(gas_price, priority_fee);
        Ok(())
    }

    /// Updates the screen with the current stats
    #[inline(always)]
    fn screen(
        &self,
        total_txs_sent: u64,
        txs_since_last_update: u64,
        start_time: Instant,
    ) -> Result<()> {
        let secs = start_time.elapsed().as_secs();
        print!(
            "{CLEAR_SCREEN}{}[{GREEN}+{RESET}] Total: {RED}{}{RESET} | Tick: {RED}{}{RESET} | Time: {RED}{:02}h {:02}m {:02}s{RESET}",
            self.prelude, total_txs_sent, txs_since_last_update,
            secs / 3600, (secs % 3600) / 60, secs % 60,
        );
        io::stdout().flush()?;
        Ok(())
    }
}
