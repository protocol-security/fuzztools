use super::{RpcCache, DEFAULT_GAS_LIMIT};
use crate::{
    mutations::{Random, STORAGE_KEYS},
    transactions::Transaction,
    utils::RandomChoice,
};
use alloy::{
    hex::FromHex,
    primitives::{Address, Bytes, FixedBytes, U256},
    providers::{Provider, RootProvider},
    rpc::types::{AccessList, AccessListItem, Authorization},
};
use anyhow::Result;
use rand::Rng;

/// Handles the logic of creating **VALID** transactions per mempool rules
pub struct TransactionBuilder {
    chain_id: u64,
    access_list_target: Address,
    signer_nonce: u64,
    auth_nonce: u64,
    cache: RpcCache,
}

impl TransactionBuilder {
    #[inline]
    pub async fn new(access_list_target: Address, node: &RootProvider) -> Result<Self> {
        let cache = RpcCache::fetch(node).await?;
        let chain_id = node.get_chain_id().await?;
        Ok(Self { chain_id, access_list_target, signer_nonce: 0, auth_nonce: 0, cache })
    }

    /// Refreshes the cache by fetching it from the given node
    #[inline]
    pub async fn refresh_cache(&mut self, node: &RootProvider) -> Result<()> {
        self.cache = RpcCache::fetch(node).await?;
        Ok(())
    }

    /// Populates common fields to all transaction types. The randomness comes from
    /// the `to` and `input` fields
    fn base_request(&self, random: &mut impl Rng) -> Transaction {
        let address = Address::random(random);
        let input = Bytes::random(random);

        Transaction {
            to: Some(address),
            value: Some(U256::ONE),
            gas_limit: Some(DEFAULT_GAS_LIMIT),
            chain_id: Some(self.chain_id),
            input: Some(input),

            ..Default::default()
        }
    }

    /// Handy method to compute `max_fee_per_gas`
    #[inline]
    fn max_fee_per_gas(&self, base_fee: u128, priority_fee: u128) -> u128 {
        base_fee.saturating_mul(2).saturating_add(priority_fee)
    }

    /// The idea is to call a pre-deployed contract that at construction time
    /// populates its storage with interesting stuff, and its fallback reads
    /// from input 32-bytes chunks and SLOADs them
    fn generate_access_list_and_input(&self, random: &mut impl Rng) -> (AccessList, Bytes) {
        // As the randomness of valid txs come from `to`, to speed things up
        // we just add a single entry
        let key_str = random.choice(&STORAGE_KEYS);
        let key = FixedBytes::from_hex(key_str).unwrap();
        let item = AccessListItem { address: self.access_list_target, storage_keys: vec![key] };

        (AccessList(vec![item]), Bytes::from(key))
    }

    // --- Public Transaction Builders ---

    /// Generate a `LegacyTx` transaction. The randomness comes from
    /// the `to` and `input` fields
    pub fn build_legacy_tx(&mut self, random: &mut impl Rng) -> Transaction {
        let mut tx = self.base_request(random);

        tx.tx_type = 0;
        tx.gas_price = Some(self.cache.gas_price);
        tx.nonce = Some(self.signer_nonce);
        self.signer_nonce += 1;

        tx
    }

    /// Generate an `Eip2930` transaction. The randomness comes from
    /// the `to`, as well as from `input` and `access_list` fields iff
    /// `self.contract_address` is provided, otherwise the only random field is `to`
    pub fn build_access_list_tx(&mut self, random: &mut impl Rng) -> Transaction {
        let mut tx = self.base_request(random);
        let (access_list, input) = self.generate_access_list_and_input(random);

        tx.tx_type = 1;
        tx.gas_price = Some(self.cache.gas_price);
        tx.input = Some(input);
        tx.nonce = Some(self.signer_nonce);
        self.signer_nonce += 1;
        tx.access_list = Some(access_list);

        tx
    }

    /// Generate an `Eip1559` transaction. The randomness comes from
    /// the `to`, as well as from `input` and `access_list` fields iff
    /// `self.contract_address` is provided, otherwise the only random field is `to`
    pub fn build_eip1559_tx(&mut self, random: &mut impl Rng) -> Transaction {
        let mut tx = self.base_request(random);
        let max_fee_per_gas =
            self.max_fee_per_gas(self.cache.gas_price, self.cache.max_priority_fee);
        let (access_list, input) = self.generate_access_list_and_input(random);

        tx.tx_type = 2;
        tx.max_fee_per_gas = Some(max_fee_per_gas);
        tx.max_priority_fee_per_gas = Some(self.cache.max_priority_fee);
        tx.input = Some(input);
        tx.nonce = Some(self.signer_nonce);
        self.signer_nonce += 1;
        tx.access_list = Some(access_list);

        tx
    }

    /// Generate an `Eip7702` transaction. The randomness comes from
    /// the `to` and `authorization_list`, as well as from `input` and `access_list` fields iff
    /// `self.contract_address` is provided, otherwise the only random field is `to` and
    /// `authorization_list`
    pub fn build_eip7702_tx(&mut self, random: &mut impl Rng) -> Transaction {
        let mut tx = self.base_request(random);
        let max_fee_per_gas =
            self.max_fee_per_gas(self.cache.gas_price, self.cache.max_priority_fee);
        let (access_list, input) = self.generate_access_list_and_input(random);

        let delegatee = Address::random(random);
        let chain_id = U256::from(self.chain_id);
        let authorization = Authorization { chain_id, address: delegatee, nonce: self.auth_nonce };

        tx.tx_type = 4;
        tx.max_fee_per_gas = Some(max_fee_per_gas);
        tx.max_priority_fee_per_gas = Some(self.cache.max_priority_fee);
        tx.input = Some(input);
        tx.nonce = Some(self.signer_nonce);
        self.signer_nonce += 1;
        tx.access_list = Some(access_list);
        tx.authorization_list = Some(vec![authorization]);
        self.auth_nonce += 1;

        tx
    }
}
