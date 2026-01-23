use super::traits::{Mutable, Phantom, Random, VecMutations};
use rand::{seq::SliceRandom, Rng};

impl<T> Mutable for Vec<T>
where
    T: Mutable + Copy + Default + Random + Phantom,
{
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        let mutation = VecMutations::random(random);

        match mutation {
            VecMutations::Push => self.push(T::random(random)),
            VecMutations::Remove => {
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                self.remove(idx);
            }
            VecMutations::Replace => {
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                self[idx] = T::random(random);
            }
            VecMutations::Swap => {
                check_not_smaller!(self, 2);
                let idx1 = random.random_range(0..self.len());
                let idx2 = random.random_range(0..self.len());
                if idx1 != idx2 {
                    self.swap(idx1, idx2);
                }
            }
            VecMutations::Default => self.push(T::default()),
            VecMutations::Shuffle => self.shuffle(random),
            VecMutations::Reverse => self.reverse(),
            VecMutations::Duplicate => {
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                let dup = self[idx];
                self.push(dup);
            }
            VecMutations::Mutate => {
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                self[idx].mutate(random);
            }
            VecMutations::MutateAll => {
                check_not_empty!(self);
                let start = random.random_range(0..self.len());
                let end = start + random.random_range(0..self.len() - start);
                self[start..end].iter_mut().for_each(|x| {
                    x.mutate(random);
                });
            }
            VecMutations::SetNone => return true,
        }
        false
    }
}
