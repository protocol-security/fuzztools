//! Transaction builder for valid Ethereum transactions.

use crate::{mutations::Random, transactions::Transaction};
use alloy::{
    primitives::{Address, Bytes, FixedBytes, U256},
    rpc::types::{AccessList, AccessListItem, Authorization},
};
use rand::Rng;

/// Handles the logic of creating **VALID** transactions per mempool rules
pub struct TransactionBuilder {
    pub chain_id: u64,
    pub signer_nonce: u64,
    pub auth_nonce: u64,
    pub gas_price: u128,
    pub priority_fee: u128,
    pub access_list_target: Address,
}

impl TransactionBuilder {
    /// Creates a new TransactionBuilder from raw values (no provider needed)
    pub fn from_values(
        access_list_target: Address,
        chain_id: u64,
        gas_price: u128,
        priority_fee: u128,
    ) -> Self {
        Self {
            chain_id,
            access_list_target,
            signer_nonce: 0,
            auth_nonce: 0,
            gas_price,
            priority_fee,
        }
    }

    // ------------------------------
    // Helper methods
    // ------------------------------

    /// Updates gas prices directly
    #[inline(always)]
    pub fn set_gas_prices(&mut self, gas_price: u128, priority_fee: u128) {
        self.gas_price = gas_price;
        self.priority_fee = priority_fee;
    }

    /// Handy method to compute `max_fee_per_gas`
    #[inline(always)]
    fn max_fee_per_gas(&self) -> u128 {
        self.gas_price.saturating_mul(2).saturating_add(self.priority_fee)
    }

    /// Bumps `self.signer_nonce` and returns the previous value
    #[inline(always)]
    fn next_signer_nonce(&mut self) -> u64 {
        let nonce = self.signer_nonce;
        self.signer_nonce += 1;
        nonce
    }

    /// Bumps `self.auth_nonce` and returns the previous value
    #[inline(always)]
    fn next_auth_nonce(&mut self) -> u64 {
        let nonce = self.auth_nonce;
        self.auth_nonce += 1;
        nonce
    }

    /// Generates an access list with a single storage key, using that key as the calldata. The idea
    /// is to call a pre-deployed contract that at construction time populates its storage with
    /// interesting stuff, and its fallback reads from the calldata 32-bytes chunks and SLOADs
    /// them
    #[inline]
    fn access_list_and_input(&self, random: &mut impl Rng) -> (AccessList, Bytes) {
        let key = FixedBytes::random(random);
        let input = Bytes::from(key);
        let access_list = AccessList(vec![AccessListItem {
            address: self.access_list_target,
            storage_keys: vec![key],
        }]);

        (access_list, input)
    }

    // ------------------------------
    // Transaction builders
    // ------------------------------

    /// Common fields for all transaction types
    fn base_tx(&self, random: &mut impl Rng) -> Transaction {
        let to = Address::random(random);
        let input = Bytes::random(random);

        Transaction {
            to: Some(to),
            value: Some(U256::ONE),   // @audit make this configurable
            gas_limit: Some(100_000), // @audit make this configurable
            chain_id: Some(self.chain_id),
            input: Some(input),

            ..Default::default()
        }
    }

    /// Builds a legacy (type 0) transaction. The randomness comes from the `to` and `input` fields.
    pub fn legacy(&mut self, random: &mut impl Rng) -> Transaction {
        let mut tx = self.base_tx(random);
        let nonce = self.next_signer_nonce();

        tx.tx_type = 0;
        tx.nonce = Some(nonce);
        tx.gas_price = Some(self.gas_price);

        tx
    }

    /// Builds an EIP-2930 (type 1) transaction with access list. The randomness comes from the `to`
    /// and `input` fields.
    pub fn eip2930(&mut self, random: &mut impl Rng) -> Transaction {
        let mut tx = self.base_tx(random);
        let nonce = self.next_signer_nonce();
        let (access_list, input) = self.access_list_and_input(random);

        tx.tx_type = 1;
        tx.nonce = Some(nonce);
        tx.gas_price = Some(self.gas_price);
        tx.input = Some(input);
        tx.access_list = Some(access_list);

        tx
    }

    /// Builds an EIP-1559 (type 2) transaction with access list
    pub fn eip1559(&mut self, random: &mut impl Rng) -> Transaction {
        let mut tx = self.base_tx(random);
        let nonce = self.next_signer_nonce();
        let (access_list, input) = self.access_list_and_input(random);
        let max_fee_per_gas = self.max_fee_per_gas();

        tx.tx_type = 2;
        tx.nonce = Some(nonce);
        tx.max_fee_per_gas = Some(max_fee_per_gas);
        tx.max_priority_fee_per_gas = Some(self.priority_fee);
        tx.input = Some(input);
        tx.access_list = Some(access_list);

        tx
    }

    /// Builds an EIP-7702 (type 4) transaction with authorization
    pub fn eip7702(&mut self, random: &mut impl Rng) -> Transaction {
        let mut tx = self.base_tx(random);
        let nonce = self.next_signer_nonce();
        let (access_list, input) = self.access_list_and_input(random);
        let max_fee_per_gas = self.max_fee_per_gas();

        let authorization = Authorization {
            chain_id: U256::from(self.chain_id),
            address: Address::random(random),
            nonce: self.next_auth_nonce(),
        };

        tx.tx_type = 4;
        tx.nonce = Some(nonce);
        tx.max_fee_per_gas = Some(max_fee_per_gas);
        tx.max_priority_fee_per_gas = Some(self.priority_fee);
        tx.input = Some(input);
        tx.access_list = Some(access_list);
        tx.authorization_list = Some(vec![authorization]);

        tx
    }
}
