//! Mutation implementations for Alloy types used in Ethereum fuzzing.

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
use rand::{seq::IndexedRandom, Rng};

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
        let mut bytes = vec![0u8; length];
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
        let bytes = random.random::<[u8; N]>();
        FixedBytes(bytes)
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
        let bytes = random.random::<[u8; 32]>();
        U256::from_be_bytes(bytes)
    }
}

impl Mutable for Authorization {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=2) {
            // mutate address
            0 => self.address.mutate(random),
            // mutate chain_id
            1 => self.chain_id.mutate(random),
            // mutate nonce
            2 => self.nonce.mutate(random),
            _ => unreachable!(),
        }
    }
}

impl Mutable for AccessList {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=7) {
            0 => {
                // Return true so that it is set for `None` if under an `Option`
                return true;
            }
            1 => self.0.clear(), // Reset the access list
            2 => {
                // Add a random entry
                let address = Address::random(random);
                let num_keys = random.random::<u8>(); // @audit max 255 keys
                let storage_keys = (0..num_keys).map(|_| FixedBytes::random(random)).collect();

                self.0.push(AccessListItem { address, storage_keys });
            }
            3 => {
                // Remove a random entry
                check_not_empty!(self.0);
                let idx = random.random_range(0..self.0.len());
                self.0.remove(idx);
            }
            4 => {
                // Swap random entries
                check_not_smaller!(self.0, 2);
                let idx1 = random.random_range(0..self.0.len());
                let idx2 = random.random_range(0..self.0.len());

                if idx1 != idx2 {
                    self.0.swap(idx1, idx2);
                }
            }
            5 => {
                // Replace a random entry
                check_not_empty!(self.0);
                let idx = random.random_range(0..self.0.len());
                let address = Address::random(random);
                let num_keys = random.random::<u8>(); // @audit max 255 keys
                let storage_keys = (0..num_keys).map(|_| FixedBytes::random(random)).collect();

                self.0[idx] = AccessListItem { address, storage_keys };
            }
            6 => {
                // Add entry with storage keys from `STORAGE_KEYS`
                let address = Address::random(random);
                let num_keys = random.random::<u8>(); // @audit max 255 keys
                let storage_keys = (0..num_keys)
                    .map(|_| {
                        *STORAGE_KEYS.choose(random).unwrap()
                    })
                    .collect();

                self.0.push(AccessListItem { address, storage_keys });
            }
            7 => {
                // Mutate an element of `self`
                check_not_empty!(self.0);
                let mut item = self.0.choose(random).unwrap().clone();
                item.address.mutate(random);
                if !item.storage_keys.is_empty() {
                    item.storage_keys.iter_mut().for_each(|key| {
                        key.mutate(random);
                    });
                }
            }
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
            }
            1 => self.clear(), // Reset the authorization list
            2 => {
                // Add a random authorization entry
                let address = Address::random(random);
                let chain_id = U256::random(random);
                let nonce = random.random();

                self.push(Authorization { address, chain_id, nonce });
            }
            3 => {
                // Remove a random entry
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                self.remove(idx);
            }
            4 => {
                // Swap random entries
                check_not_smaller!(self, 2);
                let idx1 = random.random_range(0..self.len());
                let idx2 = random.random_range(0..self.len());

                if idx1 != idx2 {
                    self.swap(idx1, idx2);
                }
            }
            5 => {
                // Replace a random entry
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                let address = Address::random(random);
                let chain_id = U256::random(random);
                let nonce = random.random();

                self[idx] = Authorization { address, chain_id, nonce };
            }
            6 => {
                // Add authorization with interesting address
                let address =
                    Address::from_hex(*INTERESTING_ADDRESSES.choose(random).unwrap()).unwrap();
                let chain_id = U256::from(*INTERESTING_CHAIN_IDS.choose(random).unwrap());
                let nonce = random.random();

                self.push(Authorization { address, chain_id, nonce });
            }
            7 => {
                // Mutate an authorization's address
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                self[idx].address.mutate(random);
            }
            8 => {
                // Mutate an authorization's chain_id
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                self[idx].chain_id.mutate(random);
            }
            9 => {
                // Mutate an authorization's nonce
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                self[idx].nonce.mutate(random);
            }
            10 => {
                // Mutate a whole authorization
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                self[idx].mutate(random);
            }
            _ => unreachable!(),
        }

        false
    }
}

impl Mutable for Vec<SignedAuthorization> {
    fn mutate(&mut self, _random: &mut impl Rng) -> bool {
        false // NOOP as this is an optimization field
    }
}
