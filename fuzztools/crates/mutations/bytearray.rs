use super::{
    traits::{ByteArrayMutations, Mutable, UintMutations},
    INTERESTING_U16, INTERESTING_U32, INTERESTING_U8, INVALID_UTF8_SEQUENCES,
};
use crate::mutations::traits::ByteArrayInteresting;
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

macro_rules! impl_mutate {
    ($type:ty) => {
        impl Mutable for $type {
            #[inline(always)]
            fn mutate(&mut self, random: &mut impl Rng) -> bool {
                match random.random_range(0..=24) {
                    0 => self.byte_clone(random),
                    1 => self.byte_remove(random),
                    2 => self.byte_swap(random),
                    3 => self.byte_mutate(random),
                    4 => self.set_all_zero(),
                    5 => self.set_all_one(),
                    6 => self.set_all_max(),
                    7 => self.set_all_pattern(),
                    8 => self.set_all_random(random),
                    9 => self.shuffle_array(random),
                    10 => self.rotate_left_by_n(random),
                    11 => self.rotate_right_by_n(random),
                    12 => self.reverse_array(),
                    13 => self.slice_clone(random),
                    14 => self.slice_swap(random),
                    15 => self.slice_swap_with_invalid_utf8(random),
                    16 => self.slice_mutate(random),
                    17 => self.set_interesting_u8(random),
                    18 => self.set_interesting_u8_be(random),
                    19 => self.set_interesting_u16_le(random),
                    20 => self.set_interesting_u16_be(random),
                    21 => self.set_interesting_u32_le(random),
                    22 => self.set_interesting_u32_be(random),
                    23 => self.set_slice_with_invalid_utf8(random),
                    24 => return true,
                    _ => unreachable!(),
                }

                false
            }
        }
    };
}

macro_rules! impl_mutations {
    ($type:ty) => {
        impl ByteArrayMutations for $type {
            #[inline(always)]
            fn byte_clone(&mut self, random: &mut impl Rng) {
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                let byte = self[idx];
                self.push(byte);
            }

            #[inline(always)]
            fn byte_remove(&mut self, random: &mut impl Rng) {
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                self.remove(idx);
            }

            #[inline(always)]
            fn byte_swap(&mut self, random: &mut impl Rng) {
                check_not_smaller!(self, 2);
                let idx1 = random.random_range(0..self.len());
                let idx2 = random.random_range(0..self.len());
                if idx1 != idx2 {
                    self.swap(idx1, idx2);
                }
            }

            #[inline(always)]
            fn byte_mutate(&mut self, random: &mut impl Rng) {
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                self[idx].mutate(random);
            }

            #[inline(always)]
            fn set_all_zero(&mut self) {
                // No need to check as it is a NOOP if empty
                self.fill(0x00);
            }

            #[inline(always)]
            fn set_all_one(&mut self) {
                // No need to check as it is a NOOP if empty
                self.fill(0x01);
            }

            #[inline(always)]
            fn set_all_max(&mut self) {
                // No need to check as it is a NOOP if empty
                self.fill(0xff);
            }

            #[inline(always)]
            fn set_all_pattern(&mut self) {
                check_not_empty!(self);
                let mut i = 0;
                self.iter_mut().for_each(|byte| {
                    if i % 2 == 0 {
                        byte.set_zero();
                    } else {
                        byte.set_max();
                    }
                    i += 1;
                });
            }

            #[inline(always)]
            fn set_all_random(&mut self, random: &mut impl Rng) {
                check_not_empty!(self);
                random.fill_bytes(self);
            }

            #[inline(always)]
            fn rotate_left_by_n(&mut self, random: &mut impl Rng) {
                check_not_empty!(self);
                let positions = random.random_range(0..self.len());
                self.rotate_left(positions);
            }

            #[inline(always)]
            fn rotate_right_by_n(&mut self, random: &mut impl Rng) {
                check_not_empty!(self);
                let positions = random.random_range(0..self.len());
                self.rotate_right(positions);
            }

            #[inline(always)]
            fn shuffle_array(&mut self, random: &mut impl Rng) {
                // No need to check as it is a NOOP if empty
                self.shuffle(random);
            }

            #[inline(always)]
            fn reverse_array(&mut self) {
                check_not_empty!(self);
                self.reverse();
            }

            #[inline(always)]
            fn slice_clone(&mut self, random: &mut impl Rng) {
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                let len = random.random_range(0..self.len() - idx);
                let slice_to_clone = self[idx..idx + len].to_vec();
                self.extend_from_slice(&slice_to_clone);
            }

            #[inline(always)]
            fn slice_swap(&mut self, random: &mut impl Rng) {
                check_not_smaller!(self, 2);
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

            #[inline(always)]
            fn slice_swap_with_invalid_utf8(&mut self, random: &mut impl Rng) {
                check_not_empty!(self);
                let utf8 = INVALID_UTF8_SEQUENCES
                    [random.random_range(0..INVALID_UTF8_SEQUENCES.len())]
                .to_vec();

                check_not_smaller!(self, utf8.len());

                let idx2 = random.random_range(0..=self.len() - utf8.len());
                self[idx2..idx2 + utf8.len()].copy_from_slice(&utf8);
            }

            #[inline(always)]
            fn slice_mutate(&mut self, random: &mut impl Rng) {
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                let len = random.random_range(0..self.len() - idx);
                let mut slice = self[idx..idx + len].to_vec();

                match random.random_range(0..9) {
                    0 => slice.byte_mutate(random),
                    1 => slice.set_all_zero(),
                    2 => slice.set_all_one(),
                    3 => slice.set_all_max(),
                    4 => slice.set_all_pattern(),
                    5 => slice.shuffle(random),
                    6 => slice.rotate_left_by_n(random),
                    7 => slice.rotate_right_by_n(random),
                    8 => slice.reverse_array(),
                    _ => unreachable!(),
                }

                self[idx..idx + len].copy_from_slice(&slice);
            }
        }
    };
}

impl ByteArrayInteresting for Vec<u8> {
    #[inline(always)]
    fn set_interesting_u8(&mut self, random: &mut impl Rng) {
        check_not_empty!(self);
        let idx = random.random_range(0..self.len());
        let value_idx = random.random_range(0..INTERESTING_U8.len());
        self[idx] = INTERESTING_U8[value_idx];
    }

    #[inline(always)]
    fn set_interesting_u8_be(&mut self, random: &mut impl Rng) {
        check_not_empty!(self);
        let idx = random.random_range(0..self.len());
        let value_idx = random.random_range(0..INTERESTING_U8.len());
        self[idx] = INTERESTING_U8[value_idx].reverse_bits();
    }

    #[inline(always)]
    fn set_interesting_u16_le(&mut self, random: &mut impl Rng) {
        check_not_smaller!(self, 2);
        let idx = random.random_range(0..=self.len() - 2);
        let value_idx = random.random_range(0..INTERESTING_U16.len());
        let value = INTERESTING_U16[value_idx].to_le_bytes();
        self[idx..idx + 2].copy_from_slice(&value);
    }

    #[inline(always)]
    fn set_interesting_u16_be(&mut self, random: &mut impl Rng) {
        check_not_smaller!(self, 2);
        let idx = random.random_range(0..=self.len() - 2);
        let value_idx = random.random_range(0..INTERESTING_U16.len());
        let value = INTERESTING_U16[value_idx].to_be_bytes();
        self[idx..idx + 2].copy_from_slice(&value);
    }

    #[inline(always)]
    fn set_interesting_u32_le(&mut self, random: &mut impl Rng) {
        check_not_smaller!(self, 4);
        let idx = random.random_range(0..=self.len() - 4);
        let value_idx = random.random_range(0..INTERESTING_U32.len());
        let value = INTERESTING_U32[value_idx].to_le_bytes();
        self[idx..idx + 4].copy_from_slice(&value);
    }

    #[inline(always)]
    fn set_interesting_u32_be(&mut self, random: &mut impl Rng) {
        check_not_smaller!(self, 4);
        let idx = random.random_range(0..=self.len() - 4);
        let value_idx = random.random_range(0..INTERESTING_U32.len());
        let value = INTERESTING_U32[value_idx].to_be_bytes();
        self[idx..idx + 4].copy_from_slice(&value);
    }

    #[inline(always)]
    fn set_slice_with_invalid_utf8(&mut self, random: &mut impl Rng) {
        check_not_empty!(self);
        let utf8 =
            INVALID_UTF8_SEQUENCES[random.random_range(0..INVALID_UTF8_SEQUENCES.len())].to_vec();
        check_not_smaller!(self, utf8.len());
        let idx = random.random_range(0..=self.len() - utf8.len());
        self[idx..idx + utf8.len()].copy_from_slice(&utf8);
    }
}

impl_mutations!(Vec<u8>);
impl_mutate!(Vec<u8>);
