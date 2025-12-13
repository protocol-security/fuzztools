use crate::constants::{
    TransactionType, AUTH_PRIVATE_KEY, CLEAR_SCREEN, DEFAULT_CHANNEL_CAPACITY,
    DEFAULT_RPC_TIMEOUT, DEFAULT_TXS_PER_CORE, GREEN, RED, RESET,
};
use alloy::{eips::eip7702::SignedAuthorization, hex, primitives::Address};
use anyhow::Result;
use fuzztools::{
    builders::TransactionBuilder,
    mutations::Mutable,
    transactions::{SignedTransaction, Transaction},
    utils::Signer,
};
use rand::Rng;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use serde_json::{json, Value};
use std::{
    io::{self, Write},
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
    time::{Duration, Instant},
};
use tokio::sync::mpsc::{channel, Receiver, Sender};

pub struct App {
    prelude: String,
    client: reqwest::Client,
    rpc_url: String,
    tx_type: TransactionType,
    fuzzing: bool,
    builder: TransactionBuilder,
    signer: Signer,
    auth_signer: Signer,

    tx_sender: Sender<Vec<String>>,
    tx_receiver: Option<Receiver<Vec<String>>>,
    completion_sender: Option<Sender<u64>>,
    completion_receiver: Receiver<u64>,
    connection_lost: Arc<AtomicBool>,

    total_txs_sent: u64,
    txs_since_last_update: u64,
    start_time: Instant,
    last_update: Instant,
    current_block: u64,
}

impl App {
    pub async fn new(
        tx_type: TransactionType,
        key: String,
        url: String,
        fuzzing: bool,
        prelude: String,
    ) -> Result<Self> {
        let client = reqwest::Client::builder()
            .tcp_nodelay(true)
            .pool_max_idle_per_host(100)
            .timeout(Duration::from_secs(DEFAULT_RPC_TIMEOUT))
            .build()?;

        // Fetch initial chain state
        let (chain_id, gas_price, priority_fee, block_number) =
            Self::fetch_chain_state(&client, &url).await?;

        // Deploy access list target contract
        let key_bytes = hex::decode(&key)?;
        let access_list_target = Self::deploy_contract(&client, &url, &key_bytes).await?;

        // Build signers
        let auth_key_bytes = hex::decode(AUTH_PRIVATE_KEY)?;
        let auth_signer = Signer::new(&auth_key_bytes)?;
        let signer = Signer::new(&key_bytes)?;

        let builder =
            TransactionBuilder::from_values(access_list_target, chain_id, gas_price, priority_fee);

        let (tx_sender, tx_receiver) = channel::<Vec<String>>(DEFAULT_CHANNEL_CAPACITY);
        let (completion_sender, completion_receiver) = channel::<u64>(1000);

        Ok(Self {
            prelude,
            client,
            rpc_url: url,
            tx_type,
            fuzzing,
            builder,
            signer,
            auth_signer,
            tx_sender,
            tx_receiver: Some(tx_receiver),
            completion_sender: Some(completion_sender),
            completion_receiver,
            connection_lost: Arc::new(AtomicBool::new(false)),
            total_txs_sent: 0,
            txs_since_last_update: 0,
            start_time: Instant::now(),
            last_update: Instant::now(),
            current_block: block_number,
        })
    }

    pub async fn run(&mut self, random: &mut impl Rng) -> Result<()> {
        self.spawn_dispatcher();

        let num_cores = std::thread::available_parallelism()?.get();
        let batch_size = (DEFAULT_TXS_PER_CORE * num_cores).max(1000);

        let mut txs: Vec<Transaction> = (0..batch_size).map(|_| self.build_tx(random)).collect();
        let mut block_poll_interval = tokio::time::interval(Duration::from_secs(1));
        block_poll_interval.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);

        loop {
            if self.connection_lost.load(Ordering::Relaxed) {
                eprintln!("\n\n\x1b[1;31m[!] Critical connection failure. Exiting...\x1b[0m");
                return Ok(());
            }

            // Drain completion stats
            while let Ok(sent_count) = self.completion_receiver.try_recv() {
                self.total_txs_sent += sent_count;
                self.txs_since_last_update += sent_count;
            }

            tokio::select! {
                _ = block_poll_interval.tick() => {
                    if let Ok(new_height) = self.rpc_block_number().await {
                        if new_height > self.current_block {
                            self.current_block = new_height;
                            let _ = self.refresh_gas_prices().await;
                        }
                    }
                }

                _ = tokio::task::yield_now() => {
                    if self.last_update.elapsed().as_secs_f64() >= 1.0 {
                        self.screen()?;
                        self.txs_since_last_update = 0;
                        self.last_update = Instant::now();
                    }

                    if self.fuzzing {
                        txs.iter_mut().for_each(|tx| { tx.mutate(random); });
                    }

                    let signer = self.signer.clone();
                    let auth_signer = self.auth_signer.clone();
                    let txs_to_process = std::mem::take(&mut txs);

                    let (processed_txs, signed_batch) = tokio::task::spawn_blocking(move || {
                        let signed: Vec<String> = txs_to_process
                            .par_iter()
                            .map(|tx| Self::sign_and_encode(tx.clone(), &signer, &auth_signer))
                            .collect();
                        (txs_to_process, signed)
                    }).await?;

                    txs = processed_txs;
                    txs.iter_mut().for_each(|tx| { *tx = self.build_tx(random); });

                    if self.tx_sender.send(signed_batch).await.is_err() {
                        return Ok(());
                    }
                }
            }
        }
    }

    // ---------------------
    // RPC helpers
    // ---------------------

    async fn rpc_call(&self, method: &str, params: Value) -> Result<Value> {
        let payload = json!({
            "jsonrpc": "2.0",
            "method": method,
            "params": params,
            "id": 1
        });

        let resp: Value = self.client.post(&self.rpc_url).json(&payload).send().await?.json().await?;

        resp.get("result")
            .cloned()
            .ok_or_else(|| anyhow::anyhow!("RPC error: {:?}", resp.get("error")))
    }

    async fn rpc_block_number(&self) -> Result<u64> {
        let result = self.rpc_call("eth_blockNumber", json!([])).await?;
        let hex = result.as_str().ok_or_else(|| anyhow::anyhow!("Invalid block number"))?;
        Ok(u64::from_str_radix(hex.trim_start_matches("0x"), 16)?)
    }

    async fn rpc_gas_price(&self) -> Result<u128> {
        let result = self.rpc_call("eth_gasPrice", json!([])).await?;
        let hex = result.as_str().ok_or_else(|| anyhow::anyhow!("Invalid gas price"))?;
        Ok(u128::from_str_radix(hex.trim_start_matches("0x"), 16)?)
    }

    async fn rpc_priority_fee(&self) -> Result<u128> {
        let result = self.rpc_call("eth_maxPriorityFeePerGas", json!([])).await?;
        let hex = result.as_str().ok_or_else(|| anyhow::anyhow!("Invalid priority fee"))?;
        Ok(u128::from_str_radix(hex.trim_start_matches("0x"), 16)?)
    }

    async fn refresh_gas_prices(&mut self) -> Result<()> {
        let (gas_price, priority_fee) =
            tokio::try_join!(self.rpc_gas_price(), self.rpc_priority_fee())?;
        self.builder.set_gas_prices(gas_price, priority_fee);
        Ok(())
    }

    async fn fetch_chain_state(
        client: &reqwest::Client,
        url: &str,
    ) -> Result<(u64, u128, u128, u64)> {
        let calls = vec![
            ("eth_chainId", json!([])),
            ("eth_gasPrice", json!([])),
            ("eth_maxPriorityFeePerGas", json!([])),
            ("eth_blockNumber", json!([])),
        ];

        let batch: Vec<Value> = calls
            .iter()
            .enumerate()
            .map(|(id, (method, params))| {
                json!({
                    "jsonrpc": "2.0",
                    "method": method,
                    "params": params,
                    "id": id
                })
            })
            .collect();

        let resp: Vec<Value> = client.post(url).json(&batch).send().await?.json().await?;

        let parse_hex_u64 = |v: &Value| -> Result<u64> {
            let s = v
                .get("result")
                .and_then(|r| r.as_str())
                .ok_or_else(|| anyhow::anyhow!("Missing result"))?;
            Ok(u64::from_str_radix(s.trim_start_matches("0x"), 16)?)
        };

        let parse_hex_u128 = |v: &Value| -> Result<u128> {
            let s = v
                .get("result")
                .and_then(|r| r.as_str())
                .ok_or_else(|| anyhow::anyhow!("Missing result"))?;
            Ok(u128::from_str_radix(s.trim_start_matches("0x"), 16)?)
        };

        let chain_id = parse_hex_u64(&resp[0])?;
        let gas_price = parse_hex_u128(&resp[1])?;
        let priority_fee = parse_hex_u128(&resp[2])?;
        let block_number = parse_hex_u64(&resp[3])?;

        Ok((chain_id, gas_price, priority_fee, block_number))
    }

    async fn deploy_contract(
        _client: &reqwest::Client,
        url: &str,
        deployer_key: &[u8],
    ) -> Result<Address> {
        use fuzztools::builders::contracts::AccessListTarget;

        // Use alloy just for contract deployment (sol! macro requirement)
        let deployer_signer = alloy::signers::local::PrivateKeySigner::from_slice(deployer_key)?;
        let deployer = alloy::providers::ProviderBuilder::new()
            .wallet(deployer_signer)
            .connect(url)
            .await?;
        let contract = AccessListTarget::deploy(&deployer).await?;

        Ok(*contract.address())
    }

    // ---------------------
    // Dispatcher
    // ---------------------

    fn spawn_dispatcher(&mut self) {
        let mut tx_receiver = self.tx_receiver.take().unwrap();
        let completion_sender = self.completion_sender.take().unwrap();
        let connection_lost = self.connection_lost.clone();
        let rpc_url = self.rpc_url.clone();
        let client = self.client.clone();

        tokio::spawn(async move {
            while let Some(batch) = tx_receiver.recv().await {
                if connection_lost.load(Ordering::Relaxed) {
                    break;
                }

                for chunk in batch.chunks(100) {
                    let client = client.clone();
                    let url = rpc_url.clone();
                    let sender = completion_sender.clone();
                    let signal = connection_lost.clone();
                    let chunk = chunk.to_vec();

                    tokio::spawn(async move {
                        let payload: Vec<Value> = chunk
                            .iter()
                            .enumerate()
                            .map(|(id, tx_hex)| {
                                json!({
                                    "jsonrpc": "2.0",
                                    "method": "eth_sendRawTransaction",
                                    "params": [tx_hex],
                                    "id": id
                                })
                            })
                            .collect();

                        let count = payload.len() as u64;

                        match client.post(&url).json(&payload).send().await {
                            Ok(_) => {
                                let _ = sender.send(count).await;
                            }
                            Err(e) if e.is_connect() => {
                                signal.store(true, Ordering::Relaxed);
                            }
                            Err(_) => {
                                let _ = sender.send(count).await;
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

    fn sign_and_encode(mut tx: Transaction, signer: &Signer, auth_signer: &Signer) -> String {
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
        format!(
            "0x{}",
            hex::encode(SignedTransaction { transaction: tx, signature }.encode())
        )
    }

    fn screen(&self) -> Result<()> {
        let secs = self.start_time.elapsed().as_secs();
        print!(
            "{CLEAR_SCREEN}{}[{GREEN}+{RESET}] Total: {RED}{}{RESET} | Tick: {RED}{}{RESET} | Time: {RED}{:02}h {:02}m {:02}s{RESET}",
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
