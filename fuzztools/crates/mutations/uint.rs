//! Mutation implementations for unsigned integer types.

use super::{
    constants::{EVEN, INTERESTING_U16, INTERESTING_U32, INTERESTING_U8, ODD},
    traits::{InterestingMutations, Mutable},
};
use rand::{seq::IndexedRandom, Rng};
use std::mem::size_of;

macro_rules! impl_mutable {
    ($type:ty) => {
        impl Mutable for $type {
            fn mutate(&mut self, random: &mut impl Rng) -> bool {
                match random.random_range(0..=28) {
                    // flip_bit
                    0 => {
                        let bit = random.random_range(0..size_of::<$type>() * 8);
                        *self ^= 1 << bit;
                    }
                    // add
                    1 => {
                        let value = random.random::<$type>();
                        *self = self.saturating_add(value);
                    }
                    // add_one
                    2 => {
                        *self = self.saturating_add(1);
                    }
                    // add_mod
                    3 => {
                        let value = random.random::<$type>();
                        let modulus = random.random::<$type>();
                        if modulus != 0 {
                            *self = self.saturating_add(value) % modulus;
                        }
                    }
                    // sub
                    4 => {
                        let value = random.random::<$type>();
                        *self = self.saturating_sub(value);
                    }
                    // sub_one
                    5 => {
                        *self = self.saturating_sub(1);
                    }
                    // sub_mod
                    6 => {
                        let value = random.random::<$type>();
                        let modulus = random.random::<$type>();
                        if modulus != 0 {
                            *self = self.saturating_sub(value) % modulus;
                        }
                    }
                    // mul
                    7 => {
                        let value = random.random::<$type>();
                        *self = self.saturating_mul(value);
                    }
                    // mul_mod
                    8 => {
                        let value = random.random::<$type>();
                        let modulus = random.random::<$type>();
                        if modulus != 0 {
                            *self = self.saturating_mul(value) % modulus;
                        }
                    }
                    // div
                    9 => {
                        let value = random.random::<$type>();
                        if value != 0 {
                            *self = self.saturating_div(value);
                        }
                    }
                    // remainder
                    10 => {
                        let value = random.random::<$type>();
                        if value != 0 {
                            *self %= value;
                        }
                    }
                    // set_zero
                    11 => {
                        *self = 0;
                    }
                    // set_one
                    12 => {
                        *self = 1;
                    }
                    // set_max
                    13 => {
                        *self = <$type>::MAX;
                    }
                    // set_random
                    14 => {
                        *self = random.random::<$type>();
                    }
                    // xor
                    15 => {
                        let value = random.random::<$type>();
                        *self ^= value;
                    }
                    // and
                    16 => {
                        let value = random.random::<$type>();
                        *self &= value;
                    }
                    // or
                    17 => {
                        let value = random.random::<$type>();
                        *self |= value;
                    }
                    // not
                    18 => {
                        *self = !*self;
                    }
                    // rotate_left
                    19 => {
                        let positions = random.random_range(0..size_of::<$type>() * 8);
                        *self = (*self).rotate_left(positions as u32);
                    }
                    // rotate_right
                    20 => {
                        let positions = random.random_range(0..size_of::<$type>() * 8);
                        *self = (*self).rotate_right(positions as u32);
                    }
                    // shift_left
                    21 => {
                        let positions = random.random_range(0..size_of::<$type>() * 8);
                        *self = self.wrapping_shl(positions as u32);
                    }
                    // shift_right
                    22 => {
                        let positions = random.random_range(0..size_of::<$type>() * 8);
                        *self = self.wrapping_shr(positions as u32);
                    }
                    // reverse_bits
                    23 => {
                        *self = (*self).reverse_bits();
                    }
                    // set_interesting
                    24 => {
                        self.set_interesting(random);
                    }
                    // swap_adjacent_bits
                    25 => {
                        let v = *self as u128;
                        *self = (((v >> 1) & EVEN) | ((v << 1) & ODD)) as $type;
                    }
                    // fisher_yates_shuffle
                    26 => {
                        let mut bytes = self.to_le_bytes();
                        for i in (1..bytes.len()).rev() {
                            let j = random.random_range(0..=i);
                            bytes.swap(i, j);
                        }
                        *self = <$type>::from_le_bytes(bytes);
                    }
                    // gray_code_encoding
                    27 => {
                        *self ^= *self >> 1;
                    }
                    // signal to set None
                    28 => return true,
                    _ => unreachable!(),
                }
                false
            }
        }
    };
}

impl_mutable!(u8);
impl_mutable!(u16);
impl_mutable!(u32);
impl_mutable!(u64);
impl_mutable!(u128);

impl InterestingMutations for u8 {
    fn set_interesting(&mut self, random: &mut impl Rng) -> bool {
        *self = *INTERESTING_U8.choose(random).unwrap();
        false
    }
}

impl InterestingMutations for u16 {
    fn set_interesting(&mut self, random: &mut impl Rng) -> bool {
        *self = *INTERESTING_U16.choose(random).unwrap();
        false
    }
}

impl InterestingMutations for u32 {
    fn set_interesting(&mut self, random: &mut impl Rng) -> bool {
        *self = *INTERESTING_U32.choose(random).unwrap();
        false
    }
}

impl InterestingMutations for u64 {
    fn set_interesting(&mut self, random: &mut impl Rng) -> bool {
        let hi = *INTERESTING_U32.choose(random).unwrap() as u64;
        let lo = *INTERESTING_U32.choose(random).unwrap() as u64;
        *self = (hi << 32) | lo;
        false
    }
}

impl InterestingMutations for u128 {
    fn set_interesting(&mut self, random: &mut impl Rng) -> bool {
        let hi_hi = *INTERESTING_U32.choose(random).unwrap() as u64;
        let hi_lo = *INTERESTING_U32.choose(random).unwrap() as u64;
        let lo_hi = *INTERESTING_U32.choose(random).unwrap() as u64;
        let lo_lo = *INTERESTING_U32.choose(random).unwrap() as u64;

        let hi = (hi_hi << 32) | hi_lo;
        let lo = (lo_hi << 32) | lo_lo;

        *self = ((hi as u128) << 64) | (lo as u128);
        false
    }
}
