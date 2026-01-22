use rand::{
    seq::{IndexedRandom, SliceRandom},
    Rng,
};

use super::{
    constants::{INTERESTING_U16, INTERESTING_U32, INTERESTING_U8, INVALID_UTF8_SEQUENCES},
    traits::{BytesMutations, Mutable},
};

impl Mutable for Vec<u8> {
    #[inline(always)]
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        let mutation = BytesMutations::random(random);

        match mutation {
            BytesMutations::RandomBytePush => {
                let byte = random.random();
                self.push(byte);
            }
            BytesMutations::ByteClone => {
                check_not_empty!(self);
                let byte = self.choose(random).unwrap();
                self.push(*byte);
            }
            BytesMutations::ByteRemove => {
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                self.remove(idx);
            }
            BytesMutations::ByteSwap => {
                check_not_smaller!(self, 2);
                let idx1 = random.random_range(0..self.len());
                let idx2 = random.random_range(0..self.len());
                if idx1 != idx2 {
                    self.swap(idx1, idx2);
                }
            }
            BytesMutations::ByteMutate => {
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                self[idx].mutate(random);
            }
            BytesMutations::SetAllZero => {
                self.fill(0x00);
            }
            BytesMutations::SetAllOne => {
                self.fill(0x01);
            }
            BytesMutations::SetAllMax => {
                self.fill(0xff);
            }
            BytesMutations::SetAllPattern => {
                check_not_empty!(self);
                for (i, byte) in self.iter_mut().enumerate() {
                    if i % 2 == 0 {
                        *byte = 0x00;
                    } else {
                        *byte = 0xff;
                    }
                }
            }
            BytesMutations::SetAllRandom => {
                check_not_empty!(self);
                random.fill_bytes(self);
            }
            BytesMutations::ShuffleArray => {
                self.shuffle(random);
            }
            BytesMutations::RotateLeftByN => {
                check_not_empty!(self);
                let positions = random.random_range(0..self.len());
                self.rotate_left(positions);
            }
            BytesMutations::RotateRightByN => {
                check_not_empty!(self);
                let positions = random.random_range(0..self.len());
                self.rotate_right(positions);
            }
            BytesMutations::ReverseArray => {
                check_not_empty!(self);
                self.reverse();
            }
            BytesMutations::SliceClone => {
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                let len = random.random_range(0..self.len() - idx);
                let slice_to_clone = self[idx..idx + len].to_vec();
                self.extend_from_slice(&slice_to_clone);
            }
            BytesMutations::SliceSwap => {
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
            BytesMutations::SliceSwapWithInvalidUtf8 => {
                check_not_empty!(self);
                let utf8 = INVALID_UTF8_SEQUENCES.choose(random).unwrap().to_vec();

                check_not_smaller!(self, utf8.len());

                let idx2 = random.random_range(0..=self.len() - utf8.len());
                self[idx2..idx2 + utf8.len()].copy_from_slice(&utf8);
            }
            BytesMutations::SliceMutate => {
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                let len = random.random_range(0..self.len() - idx);

                self[idx..idx + len].iter_mut().for_each(|x| {
                    x.mutate(random);
                });
            }
            BytesMutations::SetInteresting => {
                use super::traits::BytesSetInterestingMutations;
                let mutation = BytesSetInterestingMutations::random(random);

                match mutation {
                    BytesSetInterestingMutations::SetInterestingU8 => {
                        check_not_empty!(self);
                        let idx = random.random_range(0..self.len());
                        self[idx] = *INTERESTING_U8.choose(random).unwrap();
                    }
                    BytesSetInterestingMutations::SetInterestingU8ReverseBits => {
                        check_not_empty!(self);
                        let idx = random.random_range(0..self.len());
                        self[idx] = INTERESTING_U8.choose(random).unwrap().reverse_bits();
                    }
                    BytesSetInterestingMutations::SetInterestingU16LE => {
                        check_not_smaller!(self, 2);
                        let idx = random.random_range(0..=self.len() - 2);
                        let value = INTERESTING_U16.choose(random).unwrap().to_le_bytes();
                        self[idx..idx + 2].copy_from_slice(&value);
                    }
                    BytesSetInterestingMutations::SetInterestingU16BE => {
                        check_not_smaller!(self, 2);
                        let idx = random.random_range(0..=self.len() - 2);
                        let value = INTERESTING_U16.choose(random).unwrap().to_be_bytes();
                        self[idx..idx + 2].copy_from_slice(&value);
                    }
                    BytesSetInterestingMutations::SetInterestingU32LE => {
                        check_not_smaller!(self, 4);
                        let idx = random.random_range(0..=self.len() - 4);
                        let value = INTERESTING_U32.choose(random).unwrap().to_le_bytes();
                        self[idx..idx + 4].copy_from_slice(&value);
                    }
                    BytesSetInterestingMutations::SetInterestingU32BE => {
                        check_not_smaller!(self, 4);
                        let idx = random.random_range(0..=self.len() - 4);
                        let value = INTERESTING_U32.choose(random).unwrap().to_be_bytes();
                        self[idx..idx + 4].copy_from_slice(&value);
                    }
                    BytesSetInterestingMutations::SetInvalidUtf8 => {
                        check_not_empty!(self);
                        let utf8 = INVALID_UTF8_SEQUENCES.choose(random).unwrap().to_vec();

                        check_not_smaller!(self, utf8.len());

                        let idx = random.random_range(0..=self.len() - utf8.len());
                        self[idx..idx + utf8.len()].copy_from_slice(&utf8);
                    }
                    BytesSetInterestingMutations::SetNone => return false,
                }
            }
            BytesMutations::SetNone => return true,
        }
        false
    }
}
