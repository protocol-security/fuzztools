use super::traits::Mutable;
use alloy::{
    primitives::{Address, Bytes, FixedBytes, U256},
    rpc::types::Authorization,
};
use rand::Rng;

impl Mutable for Address {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        let mut bytes = self.0.to_vec();
        let should_set_to_none = bytes.mutate(random);
        *self = Address::from_slice(&bytes);
        should_set_to_none
    }
}

impl Mutable for Bytes {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        let mut vec = self.to_vec();
        let should_set_to_none = vec.mutate(random);
        *self = Bytes::from(vec);
        should_set_to_none
    }
}

impl Mutable for FixedBytes<32> {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        let mut bytes = self.to_vec();
        let should_set_to_none = bytes.mutate(random);
        *self = FixedBytes::from_slice(&bytes);
        should_set_to_none
    }
}

impl Mutable for U256 {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        let mut bytes = self.to_be_bytes_vec();
        let should_set_to_none = bytes.mutate(random);
        *self = U256::from_be_slice(&bytes);
        should_set_to_none
    }
}

impl Mutable for Authorization {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        self.address.mutate(random) || self.chain_id.mutate(random) || self.nonce.mutate(random)
    }
}
