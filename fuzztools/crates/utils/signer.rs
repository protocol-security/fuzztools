//! Fast secp256k1 signer implementation.

use alloy::{
    primitives::FixedBytes,
    signers::{Error, Signature},
};
use anyhow::Result;
use secp256k1::{ecdsa::RecoveryId, Message, Secp256k1, SecretKey};

/// Blazingly fast secp256k1 signer.
#[derive(Clone)]
pub struct Signer {
    /// The key used for signing transactions
    secret_key: SecretKey,

    /// Curve engine
    secp: Secp256k1<secp256k1::SignOnly>,
}

impl Signer {
    /// Creates a new `FastPrivateKeySigner` from a given private key
    pub fn new(key: &[u8]) -> Result<Self> {
        let secp = Secp256k1::signing_only();

        #[allow(deprecated)]
        let secret_key = SecretKey::from_slice(key)?;

        Ok(Self { secret_key, secp })
    }

    /// Signs a given hash with the private key
    pub fn sign_hash(&self, hash: &FixedBytes<32>) -> Result<Signature, Error> {
        let msg = Message::from_digest(hash.0);
        let signature = self.secp.sign_ecdsa_recoverable(msg, &self.secret_key);
        let (rec_id, rs) = signature.serialize_compact();

        // Convert recovery ID to boolean more efficiently
        let v = matches!(rec_id, RecoveryId::One);

        Ok(Signature::from_bytes_and_parity(&rs, v))
    }
}
