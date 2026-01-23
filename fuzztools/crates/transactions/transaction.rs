//! Transaction type.

use alloy::{
    consensus::TxType,
    eips::eip7702::SignedAuthorization,
    primitives::{utils::keccak256, Address, Bytes, B256, U256},
    rpc::types::{AccessList, Authorization},
};
use alloy_rlp::{BufMut, Encodable, Header};
use mutable::Mutable;
use rand::Rng;

use crate::mutations::Mutable;

#[derive(Debug, Clone, Default, Mutable)]
pub struct Transaction {
    // Transaction type.
    pub tx_type: TxType,

    // Common fields.
    pub chain_id: Option<u64>,
    pub nonce: Option<u64>,
    pub gas_limit: Option<u64>,
    pub to: Option<Address>,
    pub value: Option<U256>,
    pub input: Option<Bytes>,

    // Legacy and access list only.
    pub gas_price: Option<u128>,

    // From access list onwards.
    pub access_list: Option<AccessList>,

    // From eip1559 onwards.
    pub max_priority_fee_per_gas: Option<u128>,
    pub max_fee_per_gas: Option<u128>,

    // Blob transaction only.
    pub max_fee_per_blob_gas: Option<u128>,
    pub blob_versioned_hashes: Option<Vec<B256>>,

    // Eip7702 transactions only.
    pub authorization_list: Option<Vec<Authorization>>,

    // This is used as an optimization by signing them in parallel.
    pub signed_authorization_list: Option<Vec<SignedAuthorization>>,
}

impl Transaction {
    /// Returns `hash(rlp(transaction))`.
    #[inline(always)]
    pub fn signing_hash(&self) -> B256 {
        keccak256(self.encode())
    }

    /// Returns `rlp(transaction)`.
    pub fn encode(&self) -> Vec<u8> {
        let length = self.fields_length() + self.eip155_fields_length();
        let mut out = Vec::with_capacity(length);

        // Add transaction type prefix for typed transactions.
        if !matches!(self.tx_type, TxType::Legacy) {
            out.put_u8(self.tx_type as u8);
        }

        // Encode RLP header.
        let header = Header { list: true, payload_length: length };
        header.encode(&mut out);

        // Encode transaction fields.
        self.encode_fields(&mut out);

        // Only if tx type is legacy we add eip155
        // signing fields.
        if matches!(self.tx_type, TxType::Legacy) {
            self.encode_eip155_fields(&mut out);
        }

        out
    }

    pub(crate) fn encode_fields(&self, out: &mut dyn BufMut) {
        // For legacy transactions (type 0), chain_id is NOT encoded in regular fields
        // It's encoded in the EIP-155 signing fields.
        if !matches!(self.tx_type, TxType::Legacy) {
            encode_field!(self.chain_id, out);
        }

        encode_field!(self.nonce, out);
        encode_field!(self.max_priority_fee_per_gas, out);
        encode_field!(self.max_fee_per_gas, out);
        encode_field!(self.gas_price, out);
        encode_field!(self.gas_limit, out);

        if let Some(to) = &self.to {
            to.encode(out);
        } else {
            out.put_u8(0x80);
        }

        encode_field!(self.value, out);
        encode_field!(self.input, out);
        encode_field!(self.access_list, out);
        encode_field!(self.max_fee_per_blob_gas, out);
        encode_field!(self.blob_versioned_hashes, out);
        encode_field!(self.signed_authorization_list, out);
    }

    pub(crate) fn fields_length(&self) -> usize {
        // For legacy transactions (type 0), chain_id is NOT included in the regular
        // fields, it's included in the EIP-155 signing fields.
        let chain_id_len =
            if matches!(self.tx_type, TxType::Legacy) { 0 } else { field_len!(self.chain_id) };

        // CREATE txs have `to` set as `[]`, which is encoded as `0x80`
        // That's why the `1` in there.
        let to_len = if let Some(to) = &self.to { to.length() } else { 1 };

        chain_id_len +
            field_len!(self.nonce) +
            field_len!(self.max_priority_fee_per_gas) +
            field_len!(self.max_fee_per_gas) +
            field_len!(self.gas_price) +
            field_len!(self.gas_limit) +
            to_len +
            field_len!(self.value) +
            field_len!(self.input) +
            field_len!(self.access_list) +
            field_len!(self.max_fee_per_blob_gas) +
            field_len!(self.blob_versioned_hashes) +
            field_len!(self.signed_authorization_list)
    }

    #[inline(always)]
    fn encode_eip155_fields(&self, out: &mut dyn BufMut) {
        if let Some(chain_id) = &self.chain_id {
            chain_id.encode(out);
            0x00u8.encode(out); // r = 0
            0x00u8.encode(out); // s = 0
        }
    }

    #[inline(always)]
    fn eip155_fields_length(&self) -> usize {
        // Only legacy transactions (type 0) include EIP-155 signing fields.
        if matches!(self.tx_type, TxType::Legacy) {
            self.chain_id.as_ref().map_or(0, |chain_id| {
                chain_id.length() + 2 // chain_id + r(0) + s(0)
            })
        } else {
            0
        }
    }
}
