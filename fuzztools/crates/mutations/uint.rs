use std::mem::size_of;

use rand::{seq::IndexedRandom, Rng};

use super::{
    constants::{EVEN, INTERESTING_U16, INTERESTING_U32, INTERESTING_U8, ODD},
    traits::{InterestingMutations, Mutable, UintMutations},
};

macro_rules! impl_mutable {
    ($type:ty) => {
        impl Mutable for $type {
            fn mutate(&mut self, random: &mut impl Rng) -> bool {
                const BITS: usize = size_of::<$type>() * 8;
                let mutation = UintMutations::random(random);

                match mutation {
                    UintMutations::FlipBit => {
                        let bit = random.random_range(0..BITS);
                        *self ^= 1 << bit;
                    }
                    UintMutations::Add => {
                        let value = random.random::<$type>();
                        *self = self.saturating_add(value);
                    }
                    UintMutations::AddOne => {
                        *self = self.saturating_add(1);
                    }
                    UintMutations::AddMod => {
                        let value = random.random::<$type>();
                        let modulus = random.random::<$type>();
                        if modulus != 0 {
                            *self = self.saturating_add(value) % modulus;
                        }
                    }
                    UintMutations::Sub => {
                        let value = random.random::<$type>();
                        *self = self.saturating_sub(value);
                    }
                    UintMutations::SubOne => {
                        *self = self.saturating_sub(1);
                    }
                    UintMutations::SubMod => {
                        let value = random.random::<$type>();
                        let modulus = random.random::<$type>();
                        if modulus != 0 {
                            *self = self.saturating_sub(value) % modulus;
                        }
                    }
                    UintMutations::Mul => {
                        let value = random.random::<$type>();
                        *self = self.saturating_mul(value);
                    }
                    UintMutations::MulMod => {
                        let value = random.random::<$type>();
                        let modulus = random.random::<$type>();
                        if modulus != 0 {
                            *self = self.saturating_mul(value) % modulus;
                        }
                    }
                    UintMutations::Div => {
                        let value = random.random::<$type>();
                        if value != 0 {
                            *self = self.saturating_div(value);
                        }
                    }
                    UintMutations::Remainder => {
                        let value = random.random::<$type>();
                        if value != 0 {
                            *self %= value;
                        }
                    }
                    UintMutations::SetZero => {
                        *self = 0;
                    }
                    UintMutations::SetOne => {
                        *self = 1;
                    }
                    UintMutations::SetMax => {
                        *self = <$type>::MAX;
                    }
                    UintMutations::SetRandom => {
                        *self = random.random::<$type>();
                    }
                    UintMutations::Xor => {
                        let value = random.random::<$type>();
                        *self ^= value;
                    }
                    UintMutations::And => {
                        let value = random.random::<$type>();
                        *self &= value;
                    }
                    UintMutations::Or => {
                        let value = random.random::<$type>();
                        *self |= value;
                    }
                    UintMutations::Not => {
                        *self = !*self;
                    }
                    UintMutations::RotateLeft => {
                        let positions = random.random_range(0..BITS);
                        *self = (*self).rotate_left(positions as u32);
                    }
                    UintMutations::RotateRight => {
                        let positions = random.random_range(0..BITS);
                        *self = (*self).rotate_right(positions as u32);
                    }
                    UintMutations::ShiftLeft => {
                        let positions = random.random_range(0..BITS);
                        *self = self.wrapping_shl(positions as u32);
                    }
                    UintMutations::ShiftRight => {
                        let positions = random.random_range(0..BITS);
                        *self = self.wrapping_shr(positions as u32);
                    }
                    UintMutations::ReverseBits => {
                        *self = (*self).reverse_bits();
                    }
                    UintMutations::SetInteresting => {
                        self.set_interesting(random);
                    }
                    UintMutations::SwapAdjacentBits => {
                        let v = *self as u128;
                        *self = (((v >> 1) & EVEN) | ((v << 1) & ODD)) as $type;
                    }
                    UintMutations::FisherYatesShuffle => {
                        let mut bytes = self.to_le_bytes();
                        for i in (1..bytes.len()).rev() {
                            let j = random.random_range(0..=i);
                            bytes.swap(i, j);
                        }
                        *self = <$type>::from_le_bytes(bytes);
                    }
                    UintMutations::GrayCodeEncoding => {
                        *self ^= *self >> 1;
                    }
                    UintMutations::SetNone => return true,
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
