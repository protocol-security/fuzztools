use super::traits::Mutable;
use rand::Rng;

impl Mutable for bool {
    fn mutate(&mut self, _random: &mut impl Rng) -> bool {
        *self = !*self;
        false
    }
}
