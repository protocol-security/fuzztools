use crate::constants::{AUTH_PRIVATE_KEY, GREEN, RED, RESET};
use alloy::{
    consensus::TxType, eips::eip7702::SignedAuthorization, hex, providers::ProviderBuilder,
    signers::SignerSync,
};
use alloy_signer_local::Secp256k1Signer;
use anyhow::{anyhow, bail, Result};
use crossbeam::channel::{bounded, Receiver};
use fuzztools::{
    builders::{contracts::AccessListTarget, TransactionBuilder},
    mutations::Mutable,
    rpc::RpcClient,
    transactions::{SignedTransaction, Transaction},
};
use rand::Rng;
use rayon::prelude::*;
use std::{
    io::{self, Write},
    process::Command,
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Arc,
    },
    time::{Duration, Instant},
};
use tokio::sync::Semaphore;

pub(crate) struct App {
    /// Header being displayed on the screen.
    header: String,
    /// Transaction builder
    builder: TransactionBuilder,
    /// RPC client connection.
    client: Arc<RpcClient>,
    /// Transaction signer.
    signer: Secp256k1Signer,
    /// Authorization signer.
    auth_signer: Secp256k1Signer,
    /// Transaction type to fuzz.
    tx_type: TxType,
    /// If true, the transactions will be mutated.
    fuzzing: bool,
    /// Poll interval to query gas prices
    poll_interval: u64,
    /// Maximum concurrent RPC calls
    workers: usize,
    /// Batch size
    batch_size: usize,
}

impl App {
    pub(crate) async fn new(
        tx_type: String,
        header: String,
        key: String,
        url: String,
        fuzzing: bool,
        workers: usize,
        poll_interval: u64,
        batch_size: usize,
    ) -> Result<Self> {
        let signer = Secp256k1Signer::from_slice(&hex::decode(&key)?)?;
        let deployer_signer = Secp256k1Signer::from_slice(&hex::decode(key)?)?;
        let auth_signer = Secp256k1Signer::from_slice(&hex::decode(AUTH_PRIVATE_KEY)?)?;

        let deployer = ProviderBuilder::new().wallet(deployer_signer).connect(&url).await?;
        let access_list_target = *AccessListTarget::deploy(&deployer).await?.address();

        let client = RpcClient::new(url);
        let [chain_id, gas_price, priority_fee]: [u128; 3] = client
            .payload()
            .add("eth_chainId", None)
            .add("eth_gasPrice", None)
            .add("eth_maxPriorityFeePerGas", None)
            .execute()
            .await?
            .try_into()
            .map_err(|_| anyhow!("expected 3 RPC results"))?;

        let tx_type = match tx_type.as_str() {
            "legacy" => TxType::Legacy,
            "eip2930" => TxType::Eip2930,
            "eip1559" => TxType::Eip1559,
            "eip7702" => TxType::Eip7702,
            _ => bail!("unsupported transaction type: {tx_type}"),
        };

        Ok(Self {
            builder: TransactionBuilder::new(
                access_list_target,
                chain_id as u64,
                gas_price,
                priority_fee,
            ),
            header,
            client: Arc::new(client),
            signer,
            auth_signer,
            tx_type,
            fuzzing,
            workers,
            poll_interval,
            batch_size,
        })
    }

    pub(crate) async fn run(&mut self, random: &mut impl Rng) -> Result<()> {
        let crash_signal = Arc::new(AtomicBool::new(false));
        let total_sent = Arc::new(AtomicUsize::new(0));
        let (tx_sender, tx_receiver) = bounded(self.workers * 10);

        self.spawn_dispatcher(crash_signal.clone(), total_sent.clone(), tx_receiver);

        // ────────────────────────────────────────────────────────────────────────────────
        // Main fuzzy loop
        // ────────────────────────────────────────────────────────────────────────────────

        let poll_interval = Duration::from_secs(self.poll_interval);
        let update_interval = Duration::from_secs(1);
        let start_time = Instant::now();
        let mut last_poll = Instant::now();
        let mut last_update = Instant::now();
        let mut current_block = 0;
        let mut last_total = 0;

        let mut txs: Vec<Transaction> =
            (0..self.batch_size).map(|_| self.build_tx(random)).collect::<Result<_>>()?;

        loop {
            if crash_signal.load(Ordering::Relaxed) {
                eprintln!("\n\n\x1b[1;31m[!] Crash detected, exiting...\x1b[0m");
                return Ok(());
            }

            if last_poll.elapsed() >= poll_interval {
                self.poll_gas_prices(&mut current_block).await?;
                last_poll = Instant::now();
            }

            if last_update.elapsed() >= update_interval {
                let total = total_sent.load(Ordering::Relaxed);
                self.screen(total, total - last_total, start_time)?;
                last_total = total;
                last_update = Instant::now();
            }

            if self.fuzzing {
                for tx in &mut txs {
                    tx.mutate(random);
                }
            }

            let signer = &self.signer;
            let auth_signer = &self.auth_signer;
            let signed: Vec<String> = txs
                .par_iter_mut()
                .map(|tx| Self::sign_and_encode(tx, signer, auth_signer))
                .collect();

            // We handle errors through `crash_signal`, otherwise keep going
            let _ = tx_sender.send(signed);

            for tx in &mut txs {
                *tx = self.build_tx(random)?;
            }
        }
    }

    fn spawn_dispatcher(
        &self,
        signal: Arc<AtomicBool>,
        total_sent: Arc<AtomicUsize>,
        tx_receiver: Receiver<Vec<String>>,
    ) {
        let client = self.client.clone();
        let workers = self.workers;

        tokio::spawn(async move {
            let semaphore = Arc::new(Semaphore::new(workers));

            while let Ok(txs) = tx_receiver.recv() {
                if signal.load(Ordering::Relaxed) {
                    break;
                }

                let count = txs.len();
                let client = client.clone();
                let signal = signal.clone();
                let total_sent = total_sent.clone();
                let permit = semaphore.clone().acquire_owned().await.unwrap();

                tokio::spawn(async move {
                    let _permit = permit;

                    match client.send_raw_transactions(txs).await {
                        Err(e) if e.is_connection_error() => {
                            signal.store(true, Ordering::Relaxed);
                        }
                        _ => {
                            total_sent.fetch_add(count, Ordering::Relaxed);
                        }
                    };
                });
            }

            // Channel closed, signal threads to bye bye
            signal.store(true, Ordering::Relaxed);
        });
    }

    async fn poll_gas_prices(&mut self, current_block: &mut u128) -> Result<()> {
        let [height]: [u128; 1] = self
            .client
            .payload()
            .add("eth_blockNumber", None)
            .execute()
            .await?
            .try_into()
            .map_err(|_| anyhow!("eth_blockNumber: expected 1 result"))?;

        if height > *current_block {
            let [gas_price, priority_fee]: [u128; 2] = self
                .client
                .payload()
                .add("eth_gasPrice", None)
                .add("eth_maxPriorityFeePerGas", None)
                .execute()
                .await?
                .try_into()
                .map_err(|_| anyhow!("gas prices: expected 2 results"))?;

            self.builder.set_gas_prices(gas_price, priority_fee);
            *current_block = height;
        }
        Ok(())
    }

    fn sign_and_encode(
        tx: &mut Transaction,
        signer: &Secp256k1Signer,
        auth_signer: &Secp256k1Signer,
    ) -> String {
        if let Some(auths) = tx.authorization_list.take() {
            tx.signed_authorization_list = Some(
                auths
                    .into_iter()
                    .map(|auth| {
                        let sig = auth_signer.sign_hash_sync(&auth.signature_hash()).unwrap();
                        SignedAuthorization::new_unchecked(auth, sig.v() as u8, sig.r(), sig.s())
                    })
                    .collect(),
            );
        }

        let sig = signer.sign_hash_sync(&tx.signing_hash()).unwrap();
        let encoded =
            SignedTransaction { transaction: std::mem::take(tx), signature: sig }.encode();

        format!("0x{}", hex::encode(encoded))
    }

    fn screen(&self, total: usize, tick: usize, start: Instant) -> io::Result<()> {
        Command::new("clear").status()?;
        let s = start.elapsed().as_secs();
        print!(
            "{}[{GREEN}+{RESET}] Total: {RED}{total}{RESET} | \
             Tick: {RED}{tick}{RESET} | Time: {RED}{:02}h {:02}m {:02}s{RESET}",
            self.header,
            s / 3600,
            (s % 3600) / 60,
            s % 60,
        );
        io::stdout().flush()
    }

    #[inline(always)]
    fn build_tx(&mut self, random: &mut impl Rng) -> Result<Transaction> {
        match self.tx_type {
            TxType::Legacy => Ok(self.builder.legacy(random)),
            TxType::Eip2930 => Ok(self.builder.eip2930(random)),
            TxType::Eip1559 => Ok(self.builder.eip1559(random)),
            TxType::Eip7702 => Ok(self.builder.eip7702(random)),
            _ => bail!("Transaction type not supported"),
        }
    }
}
