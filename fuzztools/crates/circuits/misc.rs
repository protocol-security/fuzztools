use crate::circuits::{
    context::Context,
    types::{Struct, Type},
};
use rand::{seq::IndexedRandom, Rng};

pub struct Global {
    pub name: String,
    pub ty: Type,
}

impl Global {
    pub fn random(random: &mut impl Rng, ctx: &Context, structs: &[Struct]) -> Self {
        Self {
            name: random_string(random, ctx.max_name_characters_count),
            ty: Type::random(random, ctx, structs),
        }
    }
}

pub struct Input {
    pub name: String,
    pub ty: Type,
}

impl Input {
    pub fn random(random: &mut impl Rng, ctx: &Context, structs: &[Struct]) -> Self {
        Self {
            name: random_string(random, ctx.max_name_characters_count),
            ty: Type::random(random, ctx, structs),
        }
    }
}

const LETTERS: [&str; 52] = [
    "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S",
    "T", "U", "V", "W", "X", "Y", "Z", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l",
    "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
];

#[inline(always)]
pub fn random_string(rng: &mut impl Rng, size: usize) -> String {
    (0..size).map(|_| *LETTERS.choose(rng).unwrap()).collect()
}
