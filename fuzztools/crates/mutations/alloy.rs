use super::{
    constants::{INTERESTING_ADDRESSES, INTERESTING_CHAIN_IDS, STORAGE_KEYS},
    traits::{Mutable, Phantom, Random},
};
use alloy::{
    consensus::TxType,
    eips::eip7702::SignedAuthorization,
    hex::FromHex,
    primitives::{Address, Bytes, FixedBytes, U256},
    rpc::types::{AccessList, AccessListItem, Authorization},
};
use rand::{seq::IndexedRandom, Rng};

// ────────────────────────────────────────────────────────────────────────────────
// Address
// ────────────────────────────────────────────────────────────────────────────────

impl Random for Address {
    #[inline(always)]
    fn random(random: &mut impl Rng) -> Self {
        Address::from(random.random::<[u8; 20]>())
    }
}

impl Mutable for Address {
    #[inline(always)]
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        self.0.mutate(random)
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// Bytes
// ────────────────────────────────────────────────────────────────────────────────

impl Random for Bytes {
    fn random(random: &mut impl Rng) -> Self {
        let mut bytes = vec![0u8; random.random_range(0..=1024)]; // @audit check
        random.fill_bytes(&mut bytes);
        Bytes::from(bytes)
    }
}

impl Mutable for Bytes {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        let mut vec = self.to_vec();
        let result = vec.mutate(random);
        *self = Bytes::from(vec);
        result
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// FixedBytes<N>
// ────────────────────────────────────────────────────────────────────────────────

impl<const N: usize> Random for FixedBytes<N> {
    #[inline(always)]
    fn random(random: &mut impl Rng) -> Self {
        FixedBytes(random.random())
    }
}

impl<const N: usize> Mutable for FixedBytes<N> {
    #[inline(always)]
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        self.0.mutate(random)
    }
}

impl<const N: usize> Phantom for FixedBytes<N> {}

// ────────────────────────────────────────────────────────────────────────────────
// U256
// ────────────────────────────────────────────────────────────────────────────────

impl Random for U256 {
    #[inline(always)]
    fn random(random: &mut impl Rng) -> Self {
        U256::from_be_bytes(random.random::<[u8; 32]>())
    }
}

impl Mutable for U256 {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        let mut bytes = self.to_be_bytes::<32>();
        let result = bytes.mutate(random);
        *self = U256::from_be_bytes(bytes);
        result
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// TxType
// ────────────────────────────────────────────────────────────────────────────────

impl Mutable for TxType {
    #[inline(always)]
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        const VARIANTS: [TxType; 5] =
            [TxType::Legacy, TxType::Eip2930, TxType::Eip1559, TxType::Eip4844, TxType::Eip7702];
        *self = *VARIANTS.choose(random).unwrap();
        false
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// Authorization
// ────────────────────────────────────────────────────────────────────────────────

impl Mutable for Authorization {
    #[inline(always)]
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..3u8) {
            0 => self.address.mutate(random),
            1 => self.chain_id.mutate(random),
            _ => self.nonce.mutate(random),
        }
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// AccessList
// ────────────────────────────────────────────────────────────────────────────────

impl Mutable for AccessList {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        use super::traits::AccessListMutations;
        let mutation = AccessListMutations::random(random);

        match mutation {
            AccessListMutations::SetNone => return true,
            AccessListMutations::Clear => self.0.clear(),
            AccessListMutations::AddRandomEntry => {
                let address = Address::random(random);
                let num_keys = random.random::<u8>(); // @audit max 255 keys
                let storage_keys = (0..num_keys).map(|_| FixedBytes::random(random)).collect();

                self.0.push(AccessListItem { address, storage_keys });
            }
            AccessListMutations::RemoveEntry => {
                check_not_empty!(self.0);
                let idx = random.random_range(0..self.0.len());
                self.0.remove(idx);
            }
            AccessListMutations::SwapEntries => {
                check_not_smaller!(self.0, 2);
                let idx1 = random.random_range(0..self.0.len());
                let idx2 = random.random_range(0..self.0.len());

                if idx1 != idx2 {
                    self.0.swap(idx1, idx2);
                }
            }
            AccessListMutations::ReplaceEntry => {
                check_not_empty!(self.0);
                let idx = random.random_range(0..self.0.len());
                let address = Address::random(random);
                let num_keys = random.random::<u8>(); // @audit max 255 keys
                let storage_keys = (0..num_keys).map(|_| FixedBytes::random(random)).collect();

                self.0[idx] = AccessListItem { address, storage_keys };
            }
            AccessListMutations::AddEntryWithInterestingStorageKeys => {
                let address = Address::random(random);
                let num_keys = random.random::<u8>(); // @audit max 255 keys
                let storage_keys =
                    (0..num_keys).map(|_| *STORAGE_KEYS.choose(random).unwrap()).collect();

                self.0.push(AccessListItem { address, storage_keys });
            }
            AccessListMutations::MutateEntry => {
                check_not_empty!(self.0);
                let mut item = self.0.choose(random).unwrap().clone();
                item.address.mutate(random);
                if !item.storage_keys.is_empty() {
                    item.storage_keys.iter_mut().for_each(|key| {
                        key.mutate(random);
                    });
                }
            }
        }
        false
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// Vec<Authorization>
// ────────────────────────────────────────────────────────────────────────────────

impl Mutable for Vec<Authorization> {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        use super::traits::VecAuthorizationMutations;
        let mutation = VecAuthorizationMutations::random(random);
        let len = self.len();

        match mutation {
            VecAuthorizationMutations::SetNone => return true,
            VecAuthorizationMutations::Clear => self.clear(),
            VecAuthorizationMutations::AddRandomAuthorization => {
                self.push(Authorization {
                    address: Address::random(random),
                    chain_id: U256::random(random),
                    nonce: random.random(),
                });
            }
            VecAuthorizationMutations::RemoveEntry => {
                check_not_empty!(self);
                self.remove(random.random_range(0..len));
            }
            VecAuthorizationMutations::SwapEntries => {
                check_not_smaller!(self, 2);
                let i = random.random_range(0..len);
                let j = random.random_range(0..len - 1);
                self.swap(i, if j >= i { j + 1 } else { j });
            }
            VecAuthorizationMutations::ReplaceEntry => {
                check_not_empty!(self);
                self[random.random_range(0..len)] = Authorization {
                    address: Address::random(random),
                    chain_id: U256::random(random),
                    nonce: random.random(),
                };
            }
            VecAuthorizationMutations::AddAuthorizationWithInterestingValues => {
                self.push(Authorization {
                    address: Address::from_hex(*INTERESTING_ADDRESSES.choose(random).unwrap())
                        .unwrap(),
                    chain_id: U256::from(*INTERESTING_CHAIN_IDS.choose(random).unwrap()),
                    nonce: random.random(),
                });
            }
            VecAuthorizationMutations::MutateAddress => {
                check_not_empty!(self);
                self[random.random_range(0..len)].address.mutate(random);
            }
            VecAuthorizationMutations::MutateChainId => {
                check_not_empty!(self);
                self[random.random_range(0..len)].chain_id.mutate(random);
            }
            VecAuthorizationMutations::MutateNonce => {
                check_not_empty!(self);
                self[random.random_range(0..len)].nonce.mutate(random);
            }
            VecAuthorizationMutations::MutateEntry => {
                check_not_empty!(self);
                self[random.random_range(0..len)].mutate(random);
            }
        }
        false
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// Vec<SignedAuthorization>
// ────────────────────────────────────────────────────────────────────────────────

impl Mutable for Vec<SignedAuthorization> {
    #[inline(always)]
    fn mutate(&mut self, _: &mut impl Rng) -> bool {
        false // NOOP - optimization field
    }
}
