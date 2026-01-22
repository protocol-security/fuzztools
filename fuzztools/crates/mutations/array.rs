use rand::{
    seq::{IndexedRandom, SliceRandom},
    Rng,
};

use super::{
    constants::{INTERESTING_U16, INTERESTING_U32, INTERESTING_U8, INVALID_UTF8_SEQUENCES},
    traits::Mutable,
};
use crate::mutations::traits::{
    ArrayMutations, BytesSetInterestingMutations, InterestingMutations,
};

impl<T, const N: usize> Mutable for [T; N]
where
    T: Mutable,
{
    #[inline(always)]
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        let mutation = ArrayMutations::random(random);

        match mutation {
            ArrayMutations::ValueSwap => {
                check_not_smaller!(self, 2);
                let idx1 = random.random_range(0..N);
                let idx2 = random.random_range(0..N);

                if idx1 != idx2 {
                    self.swap(idx1, idx2);
                }
            }
            ArrayMutations::ValueMutate => {
                check_not_empty!(self);
                let idx = random.random_range(0..N);

                self[idx].mutate(random);
            }
            ArrayMutations::RotateLeftByN => {
                check_not_empty!(self);
                let positions = random.random_range(0..N);

                self.rotate_left(positions);
            }
            ArrayMutations::RotateRightByN => {
                check_not_empty!(self);
                let positions = random.random_range(0..N);

                self.rotate_right(positions);
            }
            ArrayMutations::ShuffleArray => {
                self.shuffle(random);
            }
            ArrayMutations::ReverseArray => {
                check_not_empty!(self);
                self.reverse();
            }
            ArrayMutations::SliceSwap => {
                check_not_smaller!(self, 2);
                let idx1 = random.random_range(0..N);
                let idx2 = random.random_range(0..N);

                if idx1 != idx2 {
                    let max_idx = idx1.max(idx2);
                    let len = random.random_range(0..=N - max_idx);

                    for i in 0..len {
                        self.swap(idx1 + i, idx2 + i);
                    }
                }
            }
            ArrayMutations::SliceMutate => {
                check_not_empty!(self);
                let start = random.random_range(0..N);
                let end = start + random.random_range(0..N - start);

                self[start..end].iter_mut().for_each(|x| {
                    x.mutate(random);
                });
            }
            ArrayMutations::SetNone => return true,
        }
        false
    }
}

impl<const N: usize> InterestingMutations for [u8; N] {
    #[inline(always)]
    fn set_interesting(&mut self, random: &mut impl Rng) -> bool {
        let mutation = BytesSetInterestingMutations::random(random);

        match mutation {
            BytesSetInterestingMutations::SetInterestingU8 => {
                check_not_empty!(self);
                let idx = random.random_range(0..N);
                self[idx] = *INTERESTING_U8.choose(random).unwrap();
            }
            BytesSetInterestingMutations::SetInterestingU8ReverseBits => {
                check_not_empty!(self);
                let idx = random.random_range(0..N);
                self[idx] = INTERESTING_U8.choose(random).unwrap().reverse_bits();
            }
            BytesSetInterestingMutations::SetInterestingU16LE => {
                check_not_smaller!(self, 2);
                let idx = random.random_range(0..=N - 2);
                let value = INTERESTING_U16.choose(random).unwrap().to_le_bytes();
                self[idx..idx + 2].copy_from_slice(&value);
            }
            BytesSetInterestingMutations::SetInterestingU16BE => {
                check_not_smaller!(self, 2);
                let idx = random.random_range(0..=N - 2);
                let value = INTERESTING_U16.choose(random).unwrap().to_be_bytes();
                self[idx..idx + 2].copy_from_slice(&value);
            }
            BytesSetInterestingMutations::SetInterestingU32LE => {
                check_not_smaller!(self, 4);
                let idx = random.random_range(0..=N - 4);
                let value = INTERESTING_U32.choose(random).unwrap().to_le_bytes();
                self[idx..idx + 4].copy_from_slice(&value);
            }
            BytesSetInterestingMutations::SetInterestingU32BE => {
                check_not_smaller!(self, 4);
                let idx = random.random_range(0..=N - 4);
                let value = INTERESTING_U32.choose(random).unwrap().to_be_bytes();
                self[idx..idx + 4].copy_from_slice(&value);
            }
            BytesSetInterestingMutations::SetInvalidUtf8 => {
                check_not_empty!(self);
                let utf8 = INVALID_UTF8_SEQUENCES.choose(random).unwrap().to_vec();
                check_not_smaller!(self, utf8.len());
                let idx = random.random_range(0..=N - utf8.len());
                self[idx..idx + utf8.len()].copy_from_slice(&utf8);
            }
            BytesSetInterestingMutations::SetNone => return true,
        }
        false
    }
}
