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
            // Remove a random element from the vector
            1 => {
                if !self.is_empty() {
                    let idx = random.random_range(0..self.len());
                    self.remove(idx);
                }
            }
            // Replace a random element from the vector with a new random value
            2 => {
                if !self.is_empty() {
                    let idx = random.random_range(0..self.len());
                    let new_value = T::random(random);
                    self[idx] = new_value;
                }
            }
            // Swap two random elements from the vector
            3 => {
                if self.len() >= 2 {
                    let idx1 = random.random_range(0..self.len());
                    let idx2 = random.random_range(0..self.len());
                    if idx1 != idx2 {
                        self.swap(idx1, idx2);
                    }
                }
            }
            // Push a default value
            4 => self.push(T::default()),
            // Shuffle the vector
            5 => self.shuffle(random),
            // Reverse the vector
            6 => self.reverse(),
            // Push a duplicate of a random element from the vector
            7 => {
                if !self.is_empty() {
                    let idx = random.random_range(0..self.len());
                    let duplicate = self[idx];
                    self.push(duplicate);
                }
            }
            // Mutate a random element from the vector
            8 => {
                if !self.is_empty() {
                    let idx = random.random_range(0..self.len());
                    self[idx].mutate(random);
                }
            }
            // Mutate all elements of a random slice
            9 => {
                if !self.is_empty() {
                    let idx = random.random_range(0..self.len());
                    let len = random.random_range(0..self.len() - idx);
                    for i in idx..idx + len {
                        self[i].mutate(random);
                    }
                }
            }
            // Return true to mutate the parent instead
            10 => return true,
            _ => unreachable!(),
        }

        false
    }
}
