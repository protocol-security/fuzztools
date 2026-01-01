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

/// The `boundary_value_probability` value indicates the probability of choosing
/// a boundary value, i.e. `[0, 1]` or `[0, 1, Fp - 1]` if
/// `exclude_prime` is `false`. The `small_upper_bound_probability` value indicates the
/// probability of choosing a small integer, i.e. from the domain `[0..=small_upper_bound]`.
pub fn random_field_element(random: &mut impl Rng, ctx: &Context, curve: &str) -> U256 {
    let prime = curve_prime(curve);
    let exclude_prime = bernoulli(random, ctx.exclude_prime_probability);

    // Choose from boundary values
    if bernoulli(random, ctx.boundary_value_probability) {
        if exclude_prime {
            // Choose from `[0, 1]`
            *[U256::ZERO, U256::ONE].choose(random).unwrap()
        } else {
            // Choose from `[0, 1, Fp - 1]`
            *[U256::ZERO, U256::ONE, prime - U256::ONE].choose(random).unwrap()
        }
    } else {
        // Choose from `[0..=small_upper_bound]`
        if bernoulli(random, ctx.small_upper_bound_probability) {
            U256::from(random.random_range(0..=ctx.max_small_upper_bound))
        } else {
            // Choose from `[0..=Fp - 1]`
            U256::random(random) % prime
        }
    }
}

pub const LETTERS: [&str; 52] = [
    "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S",
    "T", "U", "V", "W", "X", "Y", "Z", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l",
    "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
];

#[inline(always)]
pub fn random_string(rng: &mut impl Rng, size: usize) -> String {
    (0..size).map(|_| *LETTERS.choose(rng).unwrap()).collect()
}
