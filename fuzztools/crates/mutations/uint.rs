//! Mutation implementations for unsigned integer types.

use super::{
    constants::{EVEN, INTERESTING_U16, INTERESTING_U32, INTERESTING_U8, ODD},
    traits::{InterestingMutations, Mutable, UintMutations},
};
use crate::utils::RandomChoice;
use rand::Rng;
use std::mem::size_of;

macro_rules! impl_mutations {
    ($type:ty) => {
        impl UintMutations for $type {
            #[inline(always)]
            fn flip_bit(&mut self, random: &mut impl Rng) {
                let bit = random.random_range(0..size_of::<$type>() * 8);
                *self ^= 1 << bit;
            }

            #[inline(always)]
            fn add(&mut self, random: &mut impl Rng) {
                let value = random.random::<$type>();
                *self = self.saturating_add(value);
            }

            #[inline(always)]
            fn add_one(&mut self) {
                *self = self.saturating_add(1);
            }

            #[inline(always)]
            fn add_mod(&mut self, random: &mut impl Rng) {
                let value = random.random::<$type>();
                let modulus = random.random::<$type>();
                if modulus != 0 {
                    *self = self.saturating_add(value) % modulus;
                }
            }

            #[inline(always)]
            fn sub(&mut self, random: &mut impl Rng) {
                let value = random.random::<$type>();
                *self = self.saturating_sub(value);
            }

            #[inline(always)]
            fn sub_one(&mut self) {
                *self = self.saturating_sub(1);
            }

            #[inline(always)]
            fn sub_mod(&mut self, random: &mut impl Rng) {
                let value = random.random::<$type>();
                let modulus = random.random::<$type>();
                if modulus != 0 {
                    *self = self.saturating_sub(value) % modulus;
                }
            }

            #[inline(always)]
            fn mul(&mut self, random: &mut impl Rng) {
                let value = random.random::<$type>();
                *self = self.saturating_mul(value);
            }

            #[inline(always)]
            fn mul_mod(&mut self, random: &mut impl Rng) {
                let value = random.random::<$type>();
                let modulus = random.random::<$type>();
                if modulus != 0 {
                    *self = self.saturating_mul(value) % modulus;
                }
            }

            #[inline(always)]
            fn div(&mut self, random: &mut impl Rng) {
                let value = random.random::<$type>();
                if value != 0 {
                    *self = self.saturating_div(value);
                }
            }

            #[inline(always)]
            fn remainder(&mut self, random: &mut impl Rng) {
                let value = random.random::<$type>();
                if value != 0 {
                    *self = *self % value;
                }
            }

            #[inline(always)]
            fn set_zero(&mut self) {
                *self = 0;
            }

            #[inline(always)]
            fn set_one(&mut self) {
                *self = 1;
            }

            #[inline(always)]
            fn set_max(&mut self) {
                *self = <$type>::MAX;
            }

            #[inline(always)]
            fn set_random(&mut self, random: &mut impl Rng) {
                *self = random.random::<$type>();
            }

            #[inline(always)]
            fn xor(&mut self, random: &mut impl Rng) {
                let value = random.random::<$type>();
                *self ^= value;
            }

            #[inline(always)]
            fn and(&mut self, random: &mut impl Rng) {
                let value = random.random::<$type>();
                *self &= value;
            }

            #[inline(always)]
            fn or(&mut self, random: &mut impl Rng) {
                let value = random.random::<$type>();
                *self |= value;
            }

            #[inline(always)]
            fn not(&mut self) {
                *self = !*self;
            }

            #[inline(always)]
            fn rotate_left(&mut self, random: &mut impl Rng) {
                let positions = random.random_range(0..size_of::<$type>() * 8);
                *self = (*self).rotate_left(positions as u32);
            }

            #[inline(always)]
            fn rotate_right(&mut self, random: &mut impl Rng) {
                let positions = random.random_range(0..size_of::<$type>() * 8);
                *self = (*self).rotate_right(positions as u32);
            }

            #[inline(always)]
            fn shift_left(&mut self, random: &mut impl Rng) {
                let positions = random.random_range(0..size_of::<$type>() * 8);
                *self = self.wrapping_shl(positions as u32);
            }

            #[inline(always)]
            fn shift_right(&mut self, random: &mut impl Rng) {
                let positions = random.random_range(0..size_of::<$type>() * 8);
                *self = self.wrapping_shr(positions as u32);
            }

            #[inline(always)]
            fn reverse_bits(&mut self) {
                *self = (*self).reverse_bits();
            }

            #[inline(always)]
            fn swap_adjacent_bits(&mut self) {
                let v = *self as u128;
                *self = (((v >> 1) & EVEN) | ((v << 1) & ODD)) as $type;
            }

            #[inline(always)]
            fn fisher_yates_shuffle(&mut self, random: &mut impl Rng) {
                let mut bytes = self.to_le_bytes();
                for i in (1..bytes.len()).rev() {
                    let j = random.random_range(0..=i);
                    bytes.swap(i, j);
                }
                *self = <$type>::from_le_bytes(bytes);
            }

            #[inline(always)]
            fn gray_code_encoding(&mut self) {
                *self ^= *self >> 1;
            }
        }
    };
}

impl InterestingMutations for u8 {
    fn set_interesting(&mut self, random: &mut impl Rng) {
        *self = *random.choice(&INTERESTING_U8);
    }
}

impl InterestingMutations for u16 {
    fn set_interesting(&mut self, random: &mut impl Rng) {
        *self = *random.choice(&INTERESTING_U16);
    }
}

impl InterestingMutations for u32 {
    fn set_interesting(&mut self, random: &mut impl Rng) {
        *self = *random.choice(&INTERESTING_U32);
    }
}

impl InterestingMutations for u64 {
    fn set_interesting(&mut self, random: &mut impl Rng) {
        let hi = *random.choice(&INTERESTING_U32) as u64;
        let lo = *random.choice(&INTERESTING_U32) as u64;
        *self = (hi << 32) | lo;
    }
}

impl InterestingMutations for u128 {
    fn set_interesting(&mut self, random: &mut impl Rng) {
        let hi_hi = *random.choice(&INTERESTING_U32) as u64;
        let hi_lo = *random.choice(&INTERESTING_U32) as u64;
        let lo_hi = *random.choice(&INTERESTING_U32) as u64;
        let lo_lo = *random.choice(&INTERESTING_U32) as u64;

        let hi = (hi_hi << 32) | hi_lo;
        let lo = (lo_hi << 32) | lo_lo;

        *self = ((hi as u128) << 64) | (lo as u128);
    }
}

impl_mutations!(u8);
impl_mutations!(u16);
impl_mutations!(u32);
impl_mutations!(u64);
impl_mutations!(u128);

macro_rules! impl_mutable {
    ($type:ty) => {
        impl Mutable for $type {
            fn mutate(&mut self, random: &mut impl Rng) -> bool {
                match random.random_range(0..=28) {
                    0 => self.flip_bit(random),
                    1 => self.add(random),
                    2 => self.add_one(),
                    3 => self.add_mod(random),
                    4 => self.sub(random),
                    5 => self.sub_one(),
                    6 => self.sub_mod(random),
                    7 => self.mul(random),
                    8 => self.mul_mod(random),
                    9 => self.div(random),
                    10 => self.remainder(random),
                    11 => self.set_zero(),
                    12 => self.set_one(),
                    13 => self.set_max(),
                    14 => self.set_random(random),
                    15 => self.xor(random),
                    16 => self.and(random),
                    17 => self.or(random),
                    18 => self.not(),
                    19 => self.rotate_left(random),
                    20 => self.rotate_right(random),
                    21 => self.shift_left(random),
                    22 => self.shift_right(random),
                    23 => self.reverse_bits(),
                    24 => self.set_interesting(random),
                    25 => self.swap_adjacent_bits(),
                    26 => self.fisher_yates_shuffle(random),
                    27 => self.gray_code_encoding(),
                    28 => return true,
                    _ => unreachable!(),
                }
                return false;
            }
        }
    };
}

impl_mutable!(u8);
impl_mutable!(u16);
impl_mutable!(u32);
impl_mutable!(u64);
impl_mutable!(u128);
