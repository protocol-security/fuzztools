//! Mutation implementations for boolean types.

use super::traits::Mutable;
use rand::Rng;

impl Mutable for bool {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=1) {
            // flip
            0 => {
                *self = !*self;
            }
            // signal to set `None`
            1 => return true,
            _ => unreachable!(),
        }
        false
    }
}
