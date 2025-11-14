//! Mutation implementations for fixed-size arrays.

use super::{
    constants::{INTERESTING_U16, INTERESTING_U32, INTERESTING_U8, INVALID_UTF8_SEQUENCES},
    traits::{ArrayMutations, Mutable},
};
use crate::mutations::traits::InterestingMutations;
use crate::utils::RandomChoice;
use rand::{seq::SliceRandom, Rng};

macro_rules! check_not_empty {
    ($bytes:ident) => {
        if $bytes.is_empty() {
            return;
        }
    };
}

macro_rules! check_not_smaller {
    ($bytes:ident, $n:expr) => {
        if $bytes.len() < $n {
            return;
        }
    };
}

impl<T, const N: usize> Mutable for [T; N]
where
    T: Mutable + Copy,
{
    #[inline(always)]
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=8) {
            0 => self.value_swap(random),
            1 => self.value_mutate(random),
            2 => self.rotate_left_by_n(random),
            3 => self.rotate_right_by_n(random),
            4 => self.shuffle_array(random),
            5 => self.reverse_array(),
            6 => self.slice_swap(random),
            7 => self.slice_mutate(random),
            8 => return true,
            _ => unreachable!(),
        }
        false
    }
}

impl<T, const N: usize> ArrayMutations for [T; N]
where
    T: Mutable + Copy,
{
    #[inline(always)]
    fn value_swap(&mut self, random: &mut impl Rng) {
        if self.len() >= 2 {
            let idx1 = random.random_range(0..self.len());
            let idx2 = random.random_range(0..self.len());
            if idx1 != idx2 {
                self.swap(idx1, idx2);
            }
        }
    }

    #[inline(always)]
    fn value_mutate(&mut self, random: &mut impl Rng) {
        if !self.is_empty() {
            let idx = random.random_range(0..self.len());
            self[idx].mutate(random);
        }
    }

    #[inline(always)]
    fn rotate_left_by_n(&mut self, random: &mut impl Rng) {
        if !self.is_empty() {
            let positions = random.random_range(0..self.len());
            self.rotate_left(positions);
        }
    }

    #[inline(always)]
    fn rotate_right_by_n(&mut self, random: &mut impl Rng) {
        if !self.is_empty() {
            let positions = random.random_range(0..self.len());
            self.rotate_right(positions);
        }
    }

    #[inline(always)]
    fn shuffle_array(&mut self, random: &mut impl Rng) {
        self.shuffle(random);
    }

    #[inline(always)]
    fn reverse_array(&mut self) {
        if !self.is_empty() {
            self.reverse();
        }
    }

    #[inline(always)]
    fn slice_swap(&mut self, random: &mut impl Rng) {
        if self.len() >= 2 {
            let idx1 = random.random_range(0..self.len());
            let idx2 = random.random_range(0..self.len());

            if idx1 != idx2 {
                let max_idx = idx1.max(idx2);
                let len = random.random_range(0..=self.len() - max_idx);

                let slice1 = self[idx1..idx1 + len].to_vec();
                let slice2 = self[idx2..idx2 + len].to_vec();

                self[idx1..idx1 + len].copy_from_slice(&slice2);
                self[idx2..idx2 + len].copy_from_slice(&slice1);
            }
        }
    }

    #[inline(always)]
    fn slice_mutate(&mut self, random: &mut impl Rng) {
        if !self.is_empty() {
            let idx = random.random_range(0..self.len());
            let len = random.random_range(0..self.len() - idx);

            for i in idx..idx + len {
                self[i].mutate(random);
            }
        }
    }
}

impl<const N: usize> InterestingMutations for [u8; N] {
    #[inline(always)]
    fn set_interesting(&mut self, random: &mut impl Rng) {
        match random.random_range(0..=7) {
            0 => {
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                self[idx] = *random.choice(&INTERESTING_U8);
            },
            1 => {
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                self[idx] = random.choice(&INTERESTING_U8).reverse_bits();
            },
            2 => {
                check_not_smaller!(self, 2);
                let idx = random.random_range(0..=self.len() - 2);
                let value = random.choice(&INTERESTING_U16).to_le_bytes();
                self[idx..idx + 2].copy_from_slice(&value);
            },
            3 => {
                check_not_smaller!(self, 2);
                let idx = random.random_range(0..=self.len() - 2);
                let value = random.choice(&INTERESTING_U16).to_be_bytes();
                self[idx..idx + 2].copy_from_slice(&value);
            },
            4 => {
                check_not_smaller!(self, 4);
                let idx = random.random_range(0..=self.len() - 4);
                let value = random.choice(&INTERESTING_U32).to_le_bytes();
                self[idx..idx + 4].copy_from_slice(&value);
            },
            5 => {
                check_not_smaller!(self, 4);
                let idx = random.random_range(0..=self.len() - 4);
                let value = random.choice(&INTERESTING_U32).to_be_bytes();
                self[idx..idx + 4].copy_from_slice(&value);
            },
            6 => {
                check_not_empty!(self);
                let utf8 = random.choice(&INVALID_UTF8_SEQUENCES).to_vec();
                check_not_smaller!(self, utf8.len());
                let idx = random.random_range(0..=self.len() - utf8.len());
                self[idx..idx + utf8.len()].copy_from_slice(&utf8);
            },
            7 => return,
            _ => unreachable!(),
        }
    }
}
