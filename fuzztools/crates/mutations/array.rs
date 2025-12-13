//! Mutation implementations for fixed-size arrays.

use super::{
    constants::{INTERESTING_U16, INTERESTING_U32, INTERESTING_U8, INVALID_UTF8_SEQUENCES},
    traits::Mutable,
};
use crate::mutations::traits::InterestingMutations;
use rand::{
    seq::{IndexedRandom, SliceRandom},
    Rng,
};

impl<T, const N: usize> Mutable for [T; N]
where
    T: Mutable + Copy,
{
    #[inline(always)]
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=8) {
            // value_swap
            0 => {
                check_not_smaller!(self, 2);
                let idx1 = random.random_range(0..N);
                let idx2 = random.random_range(0..N);

                if idx1 != idx2 {
                    self.swap(idx1, idx2);
                }
            }
            // value_mutate
            1 => {
                check_not_empty!(self);
                let idx = random.random_range(0..N);

                self[idx].mutate(random);
            }
            // rotate_left_by_n
            2 => {
                check_not_empty!(self);
                let positions = random.random_range(0..N);

                self.rotate_left(positions);
            }
            // rotate_right_by_n
            3 => {
                check_not_empty!(self);
                let positions = random.random_range(0..N);

                self.rotate_right(positions);
            }
            // shuffle_array
            4 => {
                self.shuffle(random);
            }
            // reverse_array
            5 => {
                check_not_empty!(self);
                self.reverse();
            }
            // slice_swap
            6 => {
                check_not_smaller!(self, 2);
                let idx1 = random.random_range(0..N);
                let idx2 = random.random_range(0..N);

                if idx1 != idx2 {
                    let max_idx = idx1.max(idx2);
                    let len = random.random_range(0..=N - max_idx);

                    let slice1 = self[idx1..idx1 + len].to_vec();
                    let slice2 = self[idx2..idx2 + len].to_vec();

                    self[idx1..idx1 + len].copy_from_slice(&slice2);
                    self[idx2..idx2 + len].copy_from_slice(&slice1);
                }
            }
            // slice_mutate
            7 => {
                check_not_empty!(self);
                let start = random.random_range(0..N);
                let end = start + random.random_range(0..N - start);

                self[start..end].iter_mut().for_each(|x| {
                    x.mutate(random);
                });
            }
            // signal to set `None`
            8 => return true,
            _ => unreachable!(),
        }
        false
    }
}

impl<const N: usize> InterestingMutations for [u8; N] {
    #[inline(always)]
    fn set_interesting(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=7) {
            // set interesting `u8`
            0 => {
                check_not_empty!(self);
                let idx = random.random_range(0..N);

                self[idx] = *INTERESTING_U8.choose(random).unwrap();
            }
            // set interesting `u8` reversed
            1 => {
                check_not_empty!(self);
                let idx = random.random_range(0..N);

                self[idx] = INTERESTING_U8.choose(random).unwrap().reverse_bits();
            }
            // set interesting `u16` (little endian)
            2 => {
                check_not_smaller!(self, 2);
                let idx = random.random_range(0..=N - 2);
                let value = INTERESTING_U16.choose(random).unwrap().to_le_bytes();

                self[idx..idx + 2].copy_from_slice(&value);
            }
            // set interesting `u16` (big endian)
            3 => {
                check_not_smaller!(self, 2);
                let idx = random.random_range(0..=N - 2);
                let value = INTERESTING_U16.choose(random).unwrap().to_be_bytes();

                self[idx..idx + 2].copy_from_slice(&value);
            }
            // set interesting `u32` (little endian)
            4 => {
                check_not_smaller!(self, 4);
                let idx = random.random_range(0..=N - 4);
                let value = INTERESTING_U32.choose(random).unwrap().to_le_bytes();

                self[idx..idx + 4].copy_from_slice(&value);
            }
            // set interesting `u32` (big endian)
            5 => {
                check_not_smaller!(self, 4);
                let idx = random.random_range(0..=N - 4);
                let value = INTERESTING_U32.choose(random).unwrap().to_be_bytes();

                self[idx..idx + 4].copy_from_slice(&value);
            }
            // set invalid utf8 sequence
            6 => {
                check_not_empty!(self);
                let utf8 = INVALID_UTF8_SEQUENCES.choose(random).unwrap().to_vec();

                check_not_smaller!(self, utf8.len());
                let idx = random.random_range(0..=N - utf8.len());

                self[idx..idx + utf8.len()].copy_from_slice(&utf8);
            }
            // signal to set `None`
            7 => return true,
            _ => unreachable!(),
        }
        false
    }
}
