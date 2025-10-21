use super::traits::Mutable;
use rand::Rng;

impl Mutable for bool {
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        match random.random_range(0..=1) {
            0 => {
                *self = !*self;
                false
            },
            1 => true,
            _ => unreachable!(),
        }
    }
}