use super::transaction::Transaction;
use alloy::signers::Signature;
use alloy_rlp::{BufMut, Encodable, Header};

#[derive(Clone)]
/// A `Transaction` with a `Signature` attached.
pub struct SignedTransaction {
    pub transaction: Transaction,
    pub signature: Signature,
}

impl SignedTransaction {
    /// Returns `rlp(signed_transaction)`
    pub fn encode(&self) -> Vec<u8> {
        let mut length = self.rlp_encoded_length_with_signature();

        // If tx type is not legacy we add the tx type prefix
        if self.transaction.tx_type != 0 {
            length += 1;
        }

        // If tx type is legacy we add eip155 signing fields,
        // else do as normal
        let mut out = Vec::with_capacity(length);
        if self.transaction.tx_type == 0 {
            // Encode the header
            let payload_length = self.transaction.fields_length() +
                self.signature.rlp_rs_len() +
                self.to_eip155_value(self.signature.v(), self.transaction.chain_id).length();
            let header = Header { list: true, payload_length };
            header.encode(&mut out);

            // Encode the transaction fields
            self.transaction.encode_fields(&mut out);

            // Encode the signature
            self.signature.write_rlp_vrs(
                &mut out,
                self.to_eip155_value(self.signature.v(), self.transaction.chain_id),
            );
        } else {
            // Encode the tx type
            out.put_u8(self.transaction.tx_type);

            // Encode the header
            let payload_length = self.transaction.fields_length() +
                self.signature.rlp_rs_len() +
                self.signature.v().length();
            let header = Header { list: true, payload_length };
            header.encode(&mut out);

            // Encode the transaction fields
            self.transaction.encode_fields(&mut out);

            // Encode the signature
            self.signature.write_rlp_vrs(&mut out, self.signature.v());
        }

        out
    }

    // Taken from alloy https://docs.rs/alloy-primitives/1.3.1/src/alloy_primitives/signature/utils.rs.html
    fn rlp_encoded_length_with_signature(&self) -> usize {
        let payload_length = self.transaction.fields_length() +
            self.signature.rlp_rs_len() +
            if self.transaction.tx_type == 0 {
                self.to_eip155_value(self.signature.v(), self.transaction.chain_id).length()
            } else {
                self.signature.v().length()
            };
        let header = Header { list: true, payload_length };

        header.length_with_payload()
    }

    // Taken from alloy https://docs.rs/alloy-primitives/1.3.1/src/alloy_primitives/signature/utils.rs.html
    fn to_eip155_value(&self, y_parity: bool, chain_id: Option<u64>) -> u128 {
        match chain_id {
            Some(id) => 35 + id as u128 * 2 + y_parity as u128,
            None => 27 + y_parity as u128,
        }
    }
}
