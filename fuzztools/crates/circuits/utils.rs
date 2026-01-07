use crate::{circuits::context::Context, mutations::Random};
use alloy::primitives::U256;
use rand::{seq::IndexedRandom, Rng};
use std::str::FromStr;

pub fn curve_prime(curve: &str) -> U256 {
    let value = match curve {
        "bn254" => "21888242871839275222246405745257275088548364400416034343698204186575808495617",
        "goldilocks" => "18446744069414584321",
        "bn128" => "21888242871839275222246405745257275088548364400416034343698204186575808495617",
        "stark" => "3618502788666131213697322783095070105526743751716087489154079457884512865583",
        "secp256k1" => {
            "115792089237316195423570985008687907852837564279074904382605163141518161494337"
        }
        _ => panic!("Unsupported curve: {}", curve),
    };

    U256::from_str(value).unwrap()
}

pub fn bernoulli(random: &mut impl Rng, prob: f64) -> bool {
    if prob <= 0.0 {
        return false;
    }
    if prob >= 1.0 {
        return true;
    }

    random.random_range(0.0..1.0) < prob
}

/// Generate a random field element with configurable distribution:
/// - `boundary_value_probability`: chance of [0, 1, Fp-1]
/// - `small_value_probability`: chance of [0..max_small_value]
/// - Otherwise: uniform random in [0..Fp-1]
pub fn random_field_element(random: &mut impl Rng, ctx: &Context, curve: &str) -> U256 {
    let prime = curve_prime(curve);

    // Choose from boundary values
    if bernoulli(random, ctx.boundary_value_probability) {
        *[U256::ZERO, U256::ONE, prime - U256::ONE].choose(random).unwrap()
    } else if bernoulli(random, ctx.small_value_probability) {
        // Choose from small values
        U256::from(random.random_range(0..=ctx.max_small_value))
    } else {
        // Uniform random in field
        U256::random(random) % prime
    }
}

pub const CHARACTERS: [&str; 58] = [
    "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S",
    "T", "U", "V", "W", "X", "Y", "Z", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l",
    "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "\\r", "\\n", "\\t",
    "\\0", "\\\"", "\\\\",
];

/// Generate a random string of the given size.
/// - If `is_raw` is true, escape sequences count as their actual length (2 chars for `\r`, `\n`,
///   etc.)
/// - If `is_raw` is false, escape sequences count as 1 character (representing a single logical
///   char)
#[inline(always)]
pub fn random_string(rng: &mut impl Rng, size: usize, is_raw: bool) -> String {
    if is_raw {
        let mut result = String::new();
        let mut actual_len = 0;
        while actual_len < size {
            let remaining = size - actual_len;
            // If only 1 char remaining, only pick single-char options
            let ch = if remaining == 1 {
                CHARACTERS[rng.random_range(0..52)]
            } else {
                *CHARACTERS.choose(rng).unwrap()
            };
            result.push_str(ch);
            actual_len += ch.len();
        }
        result
    } else {
        let mut result = String::new();
        for _ in 0..size {
            let ch = *CHARACTERS.choose(rng).unwrap();
            result.push_str(ch);
        }
        result
    }
}
