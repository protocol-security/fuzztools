use super::traits::Mutable;
use rand::Rng;

impl Mutable for bool {
    #[inline(always)]
    fn mutate(&mut self, random: &mut impl Rng) -> bool {
        if random.random() {
            return true;
        }

        *self = !*self;
        false
    }
}
