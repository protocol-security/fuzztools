//! Mutation implementations for generic vectors.

use super::traits::Mutable;
use crate::mutations::traits::{Phantom, Random};
use rand::{seq::SliceRandom, Rng};

impl<T> Mutable for Vec<T>
where
    T: Mutable + Copy + Default + Random + Phantom,
{
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=10) {
            // Push a random value
            0 => self.push(T::random(random)),
            // Remove a random element
            1 => {
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                self.remove(idx);
            }
            // Replace a random element with a random value
            2 => {
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                self[idx] = T::random(random);
            }
            // Swap two random elements
            3 => {
                check_not_smaller!(self, 2);
                let idx1 = random.random_range(0..self.len());
                let idx2 = random.random_range(0..self.len());
                if idx1 != idx2 {
                    self.swap(idx1, idx2);
                }
            }
            // Push a default value
            4 => self.push(T::default()),
            // Shuffle the vector
            5 => self.shuffle(random),
            // Reverse the vector
            6 => self.reverse(),
            // Duplicate a random element
            7 => {
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                let dup = self[idx];
                self.push(dup);
            }
            // Mutate a random element
            8 => {
                check_not_empty!(self);
                let idx = random.random_range(0..self.len());
                self[idx].mutate(random);
            }
            // Mutate all elements in a random slice
            9 => {
                check_not_empty!(self);
                let start = random.random_range(0..self.len());
                let end = start + random.random_range(0..self.len() - start);
                self[start..end].iter_mut().for_each(|x| {
                    x.mutate(random);
                });
            }
            _ => return true,
        }
        false
    }
}
