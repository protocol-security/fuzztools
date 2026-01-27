use super::context::Context;
use crate::mutations::Random;
use alloy::primitives::U256;
use rand::{seq::IndexedRandom, Rng};
use std::str::FromStr;

const CURVES: &[(&str, &str)] = &[
    ("bn254", "21888242871839275222246405745257275088548364400416034343698204186575808495617"),
    ("bn128", "21888242871839275222246405745257275088548364400416034343698204186575808495617"),
    ("goldilocks", "18446744069414584321"),
    ("stark", "3618502788666131213697322783095070105526743751716087489154079457884512865583"),
    ("secp256k1", "115792089237316195423570985008687907852837564279074904382605163141518161494337"),
];

#[inline(always)]
fn curve_prime(curve: &str) -> U256 {
    CURVES
        .iter()
        .find(|(c, _)| *c == curve)
        .map(|(_, p)| U256::from_str(p).unwrap())
        .unwrap_or_else(|| panic!("Unsupported curve: {curve}"))
}

#[inline(always)]
pub(crate) fn bernoulli(random: &mut impl Rng, prob: f64) -> bool {
    random.random_bool(prob.clamp(0.0, 1.0))
}

pub(crate) fn random_field_element(
    random: &mut impl Rng,
    ctx: &Context,
    curve: &str,
    with_type: bool,
) -> String {
    let prime = curve_prime(curve);

    let element = if bernoulli(random, ctx.boundary_value_probability) {
        *[U256::ZERO, U256::ONE, prime - U256::ONE].choose(random).unwrap()
    } else if bernoulli(random, ctx.small_value_probability) {
        U256::from(random.random_range(0..=ctx.max_small_value))
    } else {
        U256::random(random) % prime
    };

    let mut value = element.to_string();

    if with_type {
        value.push_str("Field")
    }

    value
}

// Indices 0..52 are single-byte plain characters (A-Z, a-z).
// Indices 52..58 are escape sequences (2 bytes each).
const CHARACTERS: [&str; 58] = [
    "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S",
    "T", "U", "V", "W", "X", "Y", "Z", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l",
    "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "\\r", "\\n", "\\t",
    "\\0", "\\\"", "\\\\",
];

const PLAIN_CHAR_COUNT: usize = 52;

pub(crate) fn random_string(random: &mut impl Rng, size: usize, is_raw: bool) -> String {
    let mut out = String::with_capacity(size);
    let mut current_len = 0;

    while current_len < size {
        let remaining = size - current_len;

        let ch = if remaining == 1 {
            // Must use a single-byte character.
            CHARACTERS[random.random_range(0..PLAIN_CHAR_COUNT)]
        } else {
            *CHARACTERS.choose(random).unwrap()
        };

        out.push_str(ch);

        // If using raw strings, we count the real length, otherwise we count the logical length.
        current_len += if is_raw { ch.len() } else { 1 };
    }

    out
}
