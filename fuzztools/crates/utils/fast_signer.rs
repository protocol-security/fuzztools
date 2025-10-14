use alloy::{
    primitives::FixedBytes,
    signers::{Error, Signature, SignerSync},
};
use anyhow::Result;
use secp256k1::{ecdsa::RecoveryId, Message, Secp256k1, SecretKey};

#[derive(Clone)]
/// This implements a faster signer than alloy's one. It uses the
/// secp256k1 bindings instead of pure rust ones, with an increase
/// in speed of about 20-30%
pub struct FastPrivateKeySigner {
    /// The key used for signing transactions
    secret_key: SecretKey,

    /// Curve engine
    secp: Secp256k1<secp256k1::SignOnly>,
}

impl FastPrivateKeySigner {
    /// Creates a new `FastPrivateKeySigner` from a given private key
    pub fn new(key: &[u8]) -> Result<Self> {
        let secp = Secp256k1::signing_only();

        #[allow(deprecated)]
        let secret_key = SecretKey::from_slice(key)?;

        Ok(Self { secret_key, secp })
    }
}

impl SignerSync for FastPrivateKeySigner {
    /// Signs a given hash with the private key
    fn sign_hash_sync(&self, hash: &FixedBytes<32>) -> Result<Signature, Error> {
        let msg = Message::from_digest(hash.0);
        let signature = self.secp.sign_ecdsa_recoverable(msg, &self.secret_key);
        let (rec_id, rs) = signature.serialize_compact();

        // Convert recovery ID to boolean more efficiently
        let v = matches!(rec_id, RecoveryId::One);

        Ok(Signature::from_bytes_and_parity(&rs, v))
    }

    /// Returns `None` as we do not support chain ID signing
    #[inline(always)]
    fn chain_id_sync(&self) -> Option<u64> {
        None
    }
}
