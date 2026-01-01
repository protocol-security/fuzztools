//! Mutation implementations for byte sequences (`Vec<u8>`).

use super::{
    constants::{INTERESTING_U16, INTERESTING_U32, INTERESTING_U8, INVALID_UTF8_SEQUENCES},
    traits::Mutable,
};
use rand::{
    seq::{IndexedRandom, SliceRandom},
    Rng,
};

impl Mutable for Vec<u8> {
    #[inline(always)]
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=19) {
            0 => {
                // random_byte_push
                let byte = random.random();
                self.push(byte);
            }
            1 => {
                // byte_clone
                check_not_empty!(self);
                let byte = self.choose(random).unwrap();
                self.push(*byte);
            }
            2 => {
                // byte_remove
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                self.remove(idx);
            }
            3 => {
                // byte_swap
                check_not_smaller!(self, 2);
                let idx1 = random.random_range(0..self.len());
                let idx2 = random.random_range(0..self.len());
                if idx1 != idx2 {
                    self.swap(idx1, idx2);
                }
            }
            4 => {
                // byte_mutate
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                self[idx].mutate(random);
            }
            5 => {
                // set_all_zero
                // No need to check as it is a NOOP if empty
                self.fill(0x00);
            }
            6 => {
                // set_all_one
                // No need to check as it is a NOOP if empty
                self.fill(0x01);
            }
            7 => {
                // set_all_max
                // No need to check as it is a NOOP if empty
                self.fill(0xff);
            }
            8 => {
                // set_all_pattern
                check_not_empty!(self);
                let mut i = 0;
                self.iter_mut().for_each(|byte| {
                    if i % 2 == 0 {
                        *byte = 0x00;
                    } else {
                        *byte = 0xff;
                    }
                    i += 1;
                });
            }
            9 => {
                // set_all_random
                check_not_empty!(self);
                random.fill_bytes(self);
            }
            10 => {
                // shuffle_array
                // No need to check as it is a NOOP if empty
                self.shuffle(random);
            }
            11 => {
                // rotate_left_by_n
                check_not_empty!(self);
                let positions = random.random_range(0..self.len());
                self.rotate_left(positions);
            }
            12 => {
                // rotate_right_by_n
                check_not_empty!(self);
                let positions = random.random_range(0..self.len());
                self.rotate_right(positions);
            }
            13 => {
                // reverse_array
                check_not_empty!(self);
                self.reverse();
            }
            14 => {
                // slice_clone
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                let len = random.random_range(0..self.len() - idx);
                let slice_to_clone = self[idx..idx + len].to_vec();
                self.extend_from_slice(&slice_to_clone);
            }
            15 => {
                // slice_swap
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
            16 => {
                // slice_swap_with_invalid_utf8
                check_not_empty!(self);
                let utf8 = INVALID_UTF8_SEQUENCES.choose(random).unwrap().to_vec();

                check_not_smaller!(self, utf8.len());

                let idx2 = random.random_range(0..=self.len() - utf8.len());
                self[idx2..idx2 + utf8.len()].copy_from_slice(&utf8);
            }
            17 => {
                // slice_mutate
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                let len = random.random_range(0..self.len() - idx);

                self[idx..idx + len].iter_mut().for_each(|x| {
                    x.mutate(random);
                });
            }
            18 => {
                // set_interesting
                match random.random_range(0..=7) {
                    0 => {
                        check_not_empty!(self);
                        let idx = random.random_range(0..self.len());
                        self[idx] = *INTERESTING_U8.choose(random).unwrap();
                    }
                    1 => {
                        check_not_empty!(self);
                        let idx = random.random_range(0..self.len());
                        self[idx] = INTERESTING_U8.choose(random).unwrap().reverse_bits();
                    }
                    2 => {
                        check_not_smaller!(self, 2);
                        let idx = random.random_range(0..=self.len() - 2);
                        let value = INTERESTING_U16.choose(random).unwrap().to_le_bytes();
                        self[idx..idx + 2].copy_from_slice(&value);
                    }
                    3 => {
                        check_not_smaller!(self, 2);
                        let idx = random.random_range(0..=self.len() - 2);
                        let value = INTERESTING_U16.choose(random).unwrap().to_be_bytes();
                        self[idx..idx + 2].copy_from_slice(&value);
                    }
                    4 => {
                        check_not_smaller!(self, 4);
                        let idx = random.random_range(0..=self.len() - 4);
                        let value = INTERESTING_U32.choose(random).unwrap().to_le_bytes();
                        self[idx..idx + 4].copy_from_slice(&value);
                    }
                    5 => {
                        check_not_smaller!(self, 4);
                        let idx = random.random_range(0..=self.len() - 4);
                        let value = INTERESTING_U32.choose(random).unwrap().to_be_bytes();
                        self[idx..idx + 4].copy_from_slice(&value);
                    }
                    6 => {
                        check_not_empty!(self);
                        let utf8 = INVALID_UTF8_SEQUENCES.choose(random).unwrap().to_vec();

                        check_not_smaller!(self, utf8.len());

                        let idx = random.random_range(0..=self.len() - utf8.len());
                        self[idx..idx + utf8.len()].copy_from_slice(&utf8);
                    }
                    7 => return false,
                    _ => unreachable!(),
                }
            }
            19 => return true,
            _ => unreachable!(),
        }
        false
    }
}
