//! # Utils
//!
//! This module contains utility functions and types that are used throughout all my fuzzers.
//!
//! ## Modules
//!
//! - [`fast_signer`] - Implements a secp256k1 signer that is faster than alloy's one by 20-30%.
//! - [`random`] - Implements the `choice` function for any `Rng` type, allowing to pick a random
//!   element from a non-empty slice.

mod fast_signer;
mod random;

pub use fast_signer::FastPrivateKeySigner;
use rand::Rng;
pub use random::RandomChoice;

const CHARS: [&str; 62] = [
    "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S",
    "T", "U", "V", "W", "X", "Y", "Z", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l",
    "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "0", "1", "2", "3", "4",
    "5", "6", "7", "8", "9",
];

const LETTERS: [&str; 52] = [
    "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S",
    "T", "U", "V", "W", "X", "Y", "Z", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l",
    "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
];

#[inline(always)]
pub fn random_id(rng: &mut impl Rng, size: usize) -> String {
    (0..size).map(|_| *rng.choice(&CHARS)).collect()
}

#[inline(always)]
pub fn random_string(rng: &mut impl Rng, size: usize) -> String {
    (0..size).map(|_| *rng.choice(&LETTERS)).collect()
}
