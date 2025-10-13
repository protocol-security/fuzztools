use super::{
    traits::{Mutable, TransactionMutations},
    INTERESTING_ADDRESSES, INTERESTING_CHAIN_IDS, INVALID_UTF8_SEQUENCES, STORAGE_KEYS,
};
use crate::transactions::Transaction;
use alloy::{
    eips::eip7702::Authorization,
    hex::FromHex,
    primitives::{utils::keccak256, Address, Bytes, FixedBytes, B256, U256},
    rpc::types::AccessListItem,
};
use rand::{random_bool, Rng};

macro_rules! mutate_field {
    ($field:expr, $random:expr) => {
        if let Some(value) = $field.as_mut() {
            let should_set_to_none = value.mutate($random);
            if should_set_to_none {
                *$field = None;
            }
        } else if $random.random_bool(0.25) {
            *$field = Some(Default::default());
        }
    };
}

impl Mutable for Transaction {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=12) {
            0 => mutate_field!(&mut self.chain_id, random),
            1 => mutate_field!(&mut self.nonce, random),
            2 => mutate_field!(&mut self.gas_limit, random),
            3 => mutate_field!(&mut self.to, random),
            4 => mutate_field!(&mut self.value, random),
            5 => mutate_field!(&mut self.gas_price, random),
            6 => mutate_field!(&mut self.max_priority_fee_per_gas, random),
            7 => mutate_field!(&mut self.max_fee_per_gas, random),
            8 => mutate_field!(&mut self.max_fee_per_blob_gas, random),
            9 => self.mutate_access_list(random),
            10 => self.mutate_blob_versioned_hashes(random),
            11 => self.mutate_authorization_list(random),
            12 => self.mutate_calldata(random),
            _ => unreachable!(),
        }

        false
    }
}

impl TransactionMutations for Transaction {
    fn mutate_access_list(&mut self, random: &mut impl Rng) {
        if let Some(access_list) = self.access_list.as_mut() {
            match random.random_range(0..=7) {
                0 => {
                    // Set access list to None
                    self.access_list = None;
                },
                1 => access_list.0.clear(), // Reset the access list
                2 => {
                    // Add a random entry
                    let address = Address::from(random.random::<[u8; 20]>());
                    let num_keys = random.random::<u8>() as usize;
                    let storage_keys = (0..num_keys)
                        .map(|_| FixedBytes::from(random.random::<[u8; 32]>()))
                        .collect();

                    access_list.0.push(AccessListItem { address, storage_keys });
                },
                3 => {
                    // Remove a random entry
                    if !access_list.0.is_empty() {
                        let idx = random.random_range(0..access_list.0.len());
                        access_list.0.remove(idx);
                    }
                },
                4 => {
                    // Swap random entries
                    if access_list.0.len() >= 2 {
                        let idx1 = random.random_range(0..access_list.0.len());
                        let idx2 = random.random_range(0..access_list.0.len());

                        if idx1 != idx2 {
                            access_list.0.swap(idx1, idx2);
                        }
                    }
                },
                5 => {
                    // Replace a random entry
                    if !access_list.0.is_empty() {
                        let idx = random.random_range(0..access_list.0.len());
                        let address = Address::from(random.random::<[u8; 20]>());
                        let num_keys = random.random::<u8>() as usize;
                        let storage_keys = (0..num_keys)
                            .map(|_| FixedBytes::from(random.random::<[u8; 32]>()))
                            .collect();

                        access_list.0[idx] = AccessListItem { address, storage_keys };
                    }
                },
                6 => {
                    // Add entry with storage keys from `STORAGE_KEYS`
                    let address = self.to.unwrap_or_default();
                    let num_keys = random.random_range(0..STORAGE_KEYS.len());
                    let storage_keys = (0..num_keys)
                        .map(|_| {
                            let key_idx = random.random_range(0..STORAGE_KEYS.len());
                            FixedBytes::from_hex(STORAGE_KEYS[key_idx]).unwrap()
                        })
                        .collect();

                    access_list.0.push(AccessListItem { address, storage_keys });
                },
                7 => {
                    // Mutate an element of `access_list`
                    if !access_list.0.is_empty() {
                        let item_idx = random.random_range(0..access_list.0.len());
                        let item = &mut access_list.0[item_idx];
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
        } else if random_bool(0.2) {
            self.access_list = Some(Default::default());
        }
    }

    fn mutate_blob_versioned_hashes(&mut self, random: &mut impl Rng) {
        if let Some(hashes) = self.blob_versioned_hashes.as_mut() {
            match random.random_range(0..=13) {
                0 => {
                    // Set blob versioned hashes to None
                    self.blob_versioned_hashes = None;
                },
                1 => hashes.clear(), // Reset the blob versioned hashes
                2 => hashes.push(FixedBytes::from(random.random::<[u8; 32]>())), /* Add a random
                                                                                   * hash */
                3 => {
                    // Remove a random hash
                    if !hashes.is_empty() {
                        let idx = random.random_range(0..hashes.len());
                        hashes.remove(idx);
                    }
                },
                4 => {
                    // Replace a random hash
                    if !hashes.is_empty() {
                        let idx = random.random_range(0..hashes.len());
                        hashes[idx] = FixedBytes::from(random.random::<[u8; 32]>());
                    }
                },
                5 => {
                    // Swap two random hashes
                    if hashes.len() >= 2 {
                        let idx1 = random.random_range(0..hashes.len());
                        let idx2 = random.random_range(0..hashes.len());
                        if idx1 != idx2 {
                            hashes.swap(idx1, idx2);
                        }
                    }
                },
                6 => hashes.push(B256::ZERO), // Add a zero hash
                7 => hashes.push(B256::from([0xff; 32])), // Add a max hash
                8 => {
                    // Hash of hash - create hash of existing hash
                    if !hashes.is_empty() {
                        // Get a random existing hash
                        let idx = random.random_range(0..hashes.len());
                        let existing_hash = hashes[idx];
                        let double_hash = keccak256(existing_hash.as_slice());
                        hashes.push(double_hash);
                    } else {
                        // If no existing hashes, create hash of random data
                        let random_hash = FixedBytes::from(random.random::<[u8; 32]>());
                        let double_hash = keccak256(random_hash.as_slice());
                        hashes.push(double_hash);
                    }
                },
                9 => {
                    // Hash of concatenated hashes
                    if hashes.len() >= 2 {
                        let idx1 = random.random_range(0..hashes.len());
                        let idx2 = random.random_range(0..hashes.len());
                        let mut combined = Vec::with_capacity(64);
                        combined.extend_from_slice(hashes[idx1].as_slice());
                        combined.extend_from_slice(hashes[idx2].as_slice());
                        let combined_hash = keccak256(&combined);
                        hashes.push(combined_hash);
                    }
                },
                10 => {
                    // Duplicate a random hash
                    if !hashes.is_empty() {
                        let idx = random.random_range(0..hashes.len());
                        let duplicate = hashes[idx];
                        hashes.push(duplicate);
                    }
                },
                11 => {
                    // Create hash chain (hash of previous hash)
                    if !hashes.is_empty() {
                        let last_hash = *hashes.last().unwrap();
                        let next_hash = keccak256(last_hash.as_slice());
                        hashes.push(next_hash);
                    }
                },
                12 => {
                    // Mutate random hash bytes
                    if !hashes.is_empty() {
                        let idx = random.random_range(0..hashes.len());
                        hashes[idx].mutate(random);
                    }
                },
                _ => unreachable!(),
            }
        } else if random_bool(0.2) {
            self.blob_versioned_hashes = Some(Default::default());
        }
    }

    fn mutate_authorization_list(&mut self, random: &mut impl Rng) {
        if let Some(authorization_list) = self.authorization_list.as_mut() {
            match random.random_range(0..=10) {
                0 => {
                    // Set authorization list to None
                    self.authorization_list = None;
                },
                1 => authorization_list.clear(), // Reset the authorization list
                2 => {
                    // Add a random authorization entry
                    let address = Address::from(random.random::<[u8; 20]>());
                    let chain_id = U256::from(random.random::<u64>());
                    let nonce = random.random::<u64>();

                    authorization_list.push(Authorization { address, chain_id, nonce });
                },
                3 => {
                    // Remove a random entry
                    if !authorization_list.is_empty() {
                        let idx = random.random_range(0..authorization_list.len());
                        authorization_list.remove(idx);
                    }
                },
                4 => {
                    // Swap random entries
                    if authorization_list.len() >= 2 {
                        let idx1 = random.random_range(0..authorization_list.len());
                        let idx2 = random.random_range(0..authorization_list.len());

                        if idx1 != idx2 {
                            authorization_list.swap(idx1, idx2);
                        }
                    }
                },
                5 => {
                    // Replace a random entry
                    if !authorization_list.is_empty() {
                        let idx = random.random_range(0..authorization_list.len());
                        let address = Address::from(random.random::<[u8; 20]>());
                        let chain_id = U256::from(random.random::<u64>());
                        let nonce = random.random::<u64>();

                        authorization_list[idx] = Authorization { address, chain_id, nonce };
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

                    authorization_list.push(Authorization { address, chain_id, nonce });
                },
                7 => {
                    // Mutate an authorization's address
                    if !authorization_list.is_empty() {
                        let idx = random.random_range(0..authorization_list.len());
                        authorization_list[idx].address.mutate(random);
                    }
                },
                8 => {
                    // Mutate an authorization's chain_id
                    if !authorization_list.is_empty() {
                        let idx = random.random_range(0..authorization_list.len());
                        authorization_list[idx].chain_id.mutate(random);
                    }
                },
                9 => {
                    // Mutate an authorization's nonce
                    if !authorization_list.is_empty() {
                        let idx = random.random_range(0..authorization_list.len());
                        authorization_list[idx].nonce.mutate(random);
                    }
                },
                10 => {
                    // Mutate a whole authorization
                    if !authorization_list.is_empty() {
                        let idx = random.random_range(0..authorization_list.len());
                        authorization_list[idx].mutate(random);
                    }
                },
                _ => unreachable!(),
            }
        } else if random_bool(0.2) {
            self.authorization_list = Some(Default::default());
        }
    }

    fn mutate_calldata(&mut self, random: &mut impl Rng) {
        if let Some(calldata) = self.input.as_mut() {
            match random.random_range(0..=6) {
                0 => {
                    // Set calldata to None
                    self.input = None;
                },
                1 => calldata.clear(), // Reset the calldata
                2 => {
                    // Single byte
                    self.input = Some(Bytes::from_static(&[0x01]));
                },
                3 => {
                    // Random function selector
                    let mut selector = [0u8; 4];
                    random.fill(&mut selector);

                    self.input = Some(Bytes::copy_from_slice(&selector));
                },
                4 => {
                    // Max input selector
                    self.input = Some(Bytes::from_static(&[0xff, 0xff, 0xff, 0xff]));
                },
                5 => {
                    // Invalid UTF-8 sequences
                    let idx = random.random_range(0..INVALID_UTF8_SEQUENCES.len());
                    self.input = Some(Bytes::from(INVALID_UTF8_SEQUENCES[idx]));
                },
                6 => {
                    // Mutate calldata
                    if !calldata.is_empty() {
                        calldata.mutate(random);
                    }
                },
                _ => unreachable!(),
            }
        } else if random_bool(0.2) {
            self.input = Some(Default::default());
        }
    }
}
