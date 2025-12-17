//! Implements the Noir IR AST

use rand::{seq::IndexedRandom, Rng};

pub mod operators;
pub mod scope;
pub mod tree;
pub mod types;

const LETTERS: [&str; 52] = [
    "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S",
    "T", "U", "V", "W", "X", "Y", "Z", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l",
    "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
];

#[inline(always)]
pub fn random_string(rng: &mut impl Rng, size: usize) -> String {
    (0..size).map(|_| *LETTERS.choose(rng).unwrap()).collect()
}
