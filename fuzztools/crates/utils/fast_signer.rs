use anyhow::Result;
use alloy::{
    primitives::{Address, FixedBytes, keccak256},
    signers::{Error, Signature, SignerSync},
};
use secp256k1::{Message, Secp256k1, SecretKey, ecdsa::RecoveryId};

#[derive(Clone)]
/// This implements a faster signer than alloy's one. It uses the
/// secp256k1 bindings instead of pure rust ones, with an increase
/// in speed of about 20-30%
pub struct FastPrivateKeySigner {
    /// The key used for signing transactions
    secret_key: SecretKey,

    /// Curve engine
    secp: Secp256k1<secp256k1::SignOnly>,

    /// Cached public key address to not be casting
    /// from slices all the time
    address: Address,
}

impl FastPrivateKeySigner {
    pub fn new(key: &[u8]) -> Result<Self> {
        let secp = Secp256k1::signing_only();

        #[allow(deprecated)]
        let secret_key = SecretKey::from_slice(key)?;
        let public_key = secret_key.public_key(&secp);

        let uncompressed_address = public_key.serialize_uncompressed();
        let hashed_address = keccak256(&uncompressed_address[1..]);
        let address = Address::from_slice(&hashed_address[12..]);

        Ok(Self { secret_key, secp, address })
    }

    #[inline(always)]
    pub fn address(&self) -> Address {
        self.address
    }
}

impl SignerSync for FastPrivateKeySigner {
    fn sign_hash_sync(&self, hash: &FixedBytes<32>) -> Result<Signature, Error> {
        let msg = Message::from_digest(hash.0);
        let signature = self.secp.sign_ecdsa_recoverable(msg, &self.secret_key);
        let (rec_id, rs) = signature.serialize_compact();

        // Convert recovery ID to boolean more efficiently
        let v = matches!(rec_id, RecoveryId::One);

        Ok(Signature::from_bytes_and_parity(&rs, v))
    }

    #[inline(always)]
    fn chain_id_sync(&self) -> Option<u64> {
        None
    }
}
