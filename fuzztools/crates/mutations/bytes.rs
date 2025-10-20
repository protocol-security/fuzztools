use super::{
    constants::{INTERESTING_U16, INTERESTING_U32, INTERESTING_U8, INVALID_UTF8_SEQUENCES},
    traits::{BytesMutations, Mutable, UintMutations},
};
use crate::mutations::traits::InterestingMutations;
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
impl Mutable for Vec<u8> {
    #[inline(always)]
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=19) {
            0 => self.random_byte_push(random),
            1 => self.byte_clone(random),
            2 => self.byte_remove(random),
            3 => self.byte_swap(random),
            4 => self.byte_mutate(random),
            5 => self.set_all_zero(),
            6 => self.set_all_one(),
            7 => self.set_all_max(),
            8 => self.set_all_pattern(),
            9 => self.set_all_random(random),
            10 => self.shuffle_array(random),
            11 => self.rotate_left_by_n(random),
            12 => self.rotate_right_by_n(random),
            13 => self.reverse_array(),
            14 => self.slice_clone(random),
            15 => self.slice_swap(random),
            16 => self.slice_swap_with_invalid_utf8(random),
            17 => self.slice_mutate(random),
            18 => self.set_interesting(random),
            19 => return true,
            _ => unreachable!(),
        }
        return false;
    }
}

impl BytesMutations for Vec<u8> {
    #[inline(always)]
    fn random_byte_push(&mut self, random: &mut impl Rng) {
        let byte = random.random::<u8>();
        self.push(byte);
    }

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

impl InterestingMutations for Vec<u8> {
    fn set_interesting(&mut self, random: &mut impl Rng) {
        match random.random_range(0..=7) {
            0 => {
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                let value_idx = random.random_range(0..INTERESTING_U8.len());
                self[idx] = INTERESTING_U8[value_idx];
            },
            1 => {
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                let value_idx = random.random_range(0..INTERESTING_U8.len());
                self[idx] = INTERESTING_U8[value_idx].reverse_bits();
            },
            2 => {
                check_not_smaller!(self, 2);
                let idx = random.random_range(0..=self.len() - 2);
                let value_idx = random.random_range(0..INTERESTING_U16.len());
                let value = INTERESTING_U16[value_idx].to_le_bytes();
                self[idx..idx + 2].copy_from_slice(&value);
            },
            3 => {
                check_not_smaller!(self, 2);
                let idx = random.random_range(0..=self.len() - 2);
                let value_idx = random.random_range(0..INTERESTING_U16.len());
                let value = INTERESTING_U16[value_idx].to_be_bytes();
                self[idx..idx + 2].copy_from_slice(&value);
            },
            4 => {
                check_not_smaller!(self, 4);
                let idx = random.random_range(0..=self.len() - 4);
                let value_idx = random.random_range(0..INTERESTING_U32.len());
                let value = INTERESTING_U32[value_idx].to_le_bytes();
                self[idx..idx + 4].copy_from_slice(&value);
            },
            5 => {
                check_not_smaller!(self, 4);
                let idx = random.random_range(0..=self.len() - 4);
                let value_idx = random.random_range(0..INTERESTING_U32.len());
                let value = INTERESTING_U32[value_idx].to_be_bytes();
                self[idx..idx + 4].copy_from_slice(&value);
            },
            6 => {
                check_not_empty!(self);
                let utf8 = INVALID_UTF8_SEQUENCES
                    [random.random_range(0..INVALID_UTF8_SEQUENCES.len())]
                .to_vec();
                check_not_smaller!(self, utf8.len());
                let idx = random.random_range(0..=self.len() - utf8.len());
                self[idx..idx + utf8.len()].copy_from_slice(&utf8);
            },
            7 => return,
            _ => unreachable!(),
        }
    }
}
