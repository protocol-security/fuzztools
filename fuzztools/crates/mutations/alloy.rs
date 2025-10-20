use super::traits::Mutable;
use crate::mutations::{
    constants::{INTERESTING_ADDRESSES, INTERESTING_CHAIN_IDS, STORAGE_KEYS},
    traits::{Phantom, Random},
};
use alloy::{
    eips::eip7702::SignedAuthorization,
    hex::FromHex,
    primitives::{Address, Bytes, FixedBytes, U256},
    rpc::types::{AccessList, AccessListItem, Authorization},
};
use rand::Rng;

impl Random for Address {
    fn random(random: &mut impl Rng) -> Self {
        Address::from(random.random::<[u8; 20]>())
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

impl Random for Bytes {
    fn random(random: &mut impl Rng) -> Self {
        let length = random.random_range(0..=1024);
        let mut bytes = Vec::with_capacity(length);
        random.fill_bytes(&mut bytes);
        Bytes::from(bytes)
    }
}

impl<const N: usize> Mutable for FixedBytes<N> {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        let mut bytes = self.to_vec();
        let should_set_to_none = bytes.mutate(random);
        bytes.resize(N, 0);
        *self = FixedBytes::from_slice(&bytes);
        should_set_to_none
    }
}

impl<const N: usize> Random for FixedBytes<N> {
    fn random(random: &mut impl Rng) -> Self {
        let mut bytes = Vec::with_capacity(N);
        random.fill_bytes(&mut bytes);
        FixedBytes::from_slice(&bytes)
    }
}

impl<const N: usize> Phantom for FixedBytes<N> {}

impl Mutable for U256 {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        let mut bytes = self.to_be_bytes_vec();
        let should_set_to_none = bytes.mutate(random);
        bytes.resize(32, 0);
        *self = U256::from_be_slice(&bytes);
        should_set_to_none
    }
}

impl Random for U256 {
    fn random(random: &mut impl Rng) -> Self {
        let mut bytes = [0u8; 32];
        random.fill_bytes(&mut bytes);
        U256::from_be_slice(&bytes)
    }
}

impl Mutable for Authorization {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        self.address.mutate(random) || self.chain_id.mutate(random) || self.nonce.mutate(random)
    }
}

impl Random for Authorization {
    fn random(random: &mut impl Rng) -> Self {
        Authorization {
            address: Address::random(random),
            chain_id: U256::random(random),
            nonce: random.random::<u64>(),
        }
    }
}

impl Mutable for AccessList {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=7) {
            0 => {
                // Return true so that it is set for `None` if under an `Option`
                return true;
            },
            1 => self.0.clear(), // Reset the access list
            2 => {
                // Add a random entry
                let address = Address::from(random.random::<[u8; 20]>());
                let num_keys = random.random::<u8>() as usize;
                let storage_keys =
                    (0..num_keys).map(|_| FixedBytes::from(random.random::<[u8; 32]>())).collect();

                self.0.push(AccessListItem { address, storage_keys });
            },
            3 => {
                // Remove a random entry
                if !self.0.is_empty() {
                    let idx = random.random_range(0..self.0.len());
                    self.0.remove(idx);
                }
            },
            4 => {
                // Swap random entries
                if self.0.len() >= 2 {
                    let idx1 = random.random_range(0..self.0.len());
                    let idx2 = random.random_range(0..self.0.len());

                    if idx1 != idx2 {
                        self.0.swap(idx1, idx2);
                    }
                }
            },
            5 => {
                // Replace a random entry
                if !self.0.is_empty() {
                    let idx = random.random_range(0..self.0.len());
                    let address = Address::from(random.random::<[u8; 20]>());
                    let num_keys = random.random::<u8>() as usize;
                    let storage_keys = (0..num_keys)
                        .map(|_| FixedBytes::from(random.random::<[u8; 32]>()))
                        .collect();

                    self.0[idx] = AccessListItem { address, storage_keys };
                }
            },
            6 => {
                // Add entry with storage keys from `STORAGE_KEYS`
                let address = Address::from(random.random::<[u8; 20]>());
                let num_keys = random.random_range(0..STORAGE_KEYS.len());
                let storage_keys = (0..num_keys)
                    .map(|_| {
                        let key_idx = random.random_range(0..STORAGE_KEYS.len());
                        FixedBytes::from_hex(STORAGE_KEYS[key_idx]).unwrap()
                    })
                    .collect();

                self.0.push(AccessListItem { address, storage_keys });
            },
            7 => {
                // Mutate an element of `self`
                if !self.0.is_empty() {
                    let item_idx = random.random_range(0..self.0.len());
                    let item = &mut self.0[item_idx];
                    item.address.mutate(random);
                    if !item.storage_keys.is_empty() {
                        item.storage_keys.iter_mut().for_each(|key| {
                            key.mutate(random);
                        });
                    }
                }
            },
            _ => unreachable!(),
        }

        false
    }
}

impl Mutable for Vec<Authorization> {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=10) {
            0 => {
                // Return true so that it is set for `None` if under an `Option`
                return true;
            },
            1 => self.clear(), // Reset the authorization list
            2 => {
                // Add a random authorization entry
                let address = Address::from(random.random::<[u8; 20]>());
                let chain_id = U256::from(random.random::<u64>());
                let nonce = random.random::<u64>();

                self.push(Authorization { address, chain_id, nonce });
            },
            3 => {
                // Remove a random entry
                if !self.is_empty() {
                    let idx = random.random_range(0..self.len());
                    self.remove(idx);
                }
            },
            4 => {
                // Swap random entries
                if self.len() >= 2 {
                    let idx1 = random.random_range(0..self.len());
                    let idx2 = random.random_range(0..self.len());

                    if idx1 != idx2 {
                        self.swap(idx1, idx2);
                    }
                }
            },
            5 => {
                // Replace a random entry
                if !self.is_empty() {
                    let idx = random.random_range(0..self.len());
                    let address = Address::from(random.random::<[u8; 20]>());
                    let chain_id = U256::from(random.random::<u64>());
                    let nonce = random.random::<u64>();

                    self[idx] = Authorization { address, chain_id, nonce };
                }
            },
            6 => {
                // Add authorization with interesting address
                let address = Address::from_hex(
                    INTERESTING_ADDRESSES[random.random_range(0..INTERESTING_ADDRESSES.len())],
                )
                .unwrap();
                let chain_id = U256::from(
                    INTERESTING_CHAIN_IDS[random.random_range(0..INTERESTING_CHAIN_IDS.len())],
                );
                let nonce = random.random::<u64>();

                self.push(Authorization { address, chain_id, nonce });
            },
            7 => {
                // Mutate an authorization's address
                if !self.is_empty() {
                    let idx = random.random_range(0..self.len());
                    self[idx].address.mutate(random);
                }
            },
            8 => {
                // Mutate an authorization's chain_id
                if !self.is_empty() {
                    let idx = random.random_range(0..self.len());
                    self[idx].chain_id.mutate(random);
                }
            },
            9 => {
                // Mutate an authorization's nonce
                if !self.is_empty() {
                    let idx = random.random_range(0..self.len());
                    self[idx].nonce.mutate(random);
                }
            },
            10 => {
                // Mutate a whole authorization
                if !self.is_empty() {
                    let idx = random.random_range(0..self.len());
                    self[idx].mutate(random);
                }
            },
            _ => unreachable!(),
        }

        return false;
    }
}

impl Mutable for Vec<SignedAuthorization> {
    fn mutate(&mut self, _random: &mut impl Rng) -> bool {
        return false; // NOOP as this is an optimization field
    }
}
