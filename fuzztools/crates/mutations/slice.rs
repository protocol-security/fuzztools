use super::traits::{Mutable, Phantom, Random, SliceMutations};
use rand::{seq::SliceRandom, Rng};

// ────────────────────────────────────────────────────────────────────────────────
// &mut [T] - Generic slice mutations
// ────────────────────────────────────────────────────────────────────────────────

impl<T> Mutable for &mut [T]
where
    T: Mutable + Copy + Default + Random + Phantom,
{
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        let mutation = SliceMutations::random(random);

        match mutation {
            SliceMutations::Replace => {
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                self[idx] = T::random(random);
            }
            SliceMutations::Swap => {
                check_not_smaller!(self, 2);
                let idx1 = random.random_range(0..self.len());
                let idx2 = random.random_range(0..self.len());
                if idx1 != idx2 {
                    self.swap(idx1, idx2);
                }
            }
            SliceMutations::Shuffle => self.shuffle(random),
            SliceMutations::Reverse => self.reverse(),
            SliceMutations::Mutate => {
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                self[idx].mutate(random);
            }
            SliceMutations::MutateAll => {
                check_not_empty!(self);
                let start = random.random_range(0..self.len());
                let end = start + random.random_range(1..=self.len() - start);
                self[start..end].iter_mut().for_each(|x| {
                    x.mutate(random);
                });
            }
            SliceMutations::SetNone => return true,
        }
        false
    }
}
