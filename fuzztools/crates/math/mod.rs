use anyhow::Result;
use alloy::primitives::U256;
use rand::{
    distr::{weighted::WeightedIndex, Distribution},
    Rng,
};
use std::{collections::HashMap, fmt::Display, hash::Hash};
use std::str::FromStr;

use crate::{mutations::Random, utils::RandomChoice};

/// Returns the prime value for the provided field curve.
pub fn curve_prime(curve: &str) -> U256 {
    let value = match curve {
        "bn254" => U256::from_str(
            "21888242871839275222246405745257275088548364400416034343698204186575808495617",
        )
        .unwrap(),
        "goldilocks" => U256::from_str("18446744069414584321").unwrap(),
        "bn128" => U256::from_str(
            "21888242871839275222246405745257275088548364400416034343698204186575808495617",
        )
        .unwrap(),
        "stark" => U256::from_str(
            "3618502788666131213697322783095070105526743751716087489154079457884512865583",
        )
        .unwrap(),
        "secp256k1" => U256::from_str(
            "115792089237316195423570985008687907852837564279074904382605163141518161494337",
        )
        .unwrap(),
        _ => unreachable!(),
    };

    value
}

pub fn bernoulli(prob: f64, random: &mut impl Rng) -> bool {
    if prob <= 0.0 {
        return false;
    }
    if prob >= 1.0 {
        return true;
    }

    random.random_range(0.0..1.0) < prob
}

pub fn weighted_select<T: Eq + Hash + Clone + Display>(
    options: &[T],
    weight_map: &HashMap<T, f64>,
    random: &mut impl Rng,
) -> Result<T> {
    if options.is_empty() {
        return Err(anyhow::anyhow!("Unable to select from empty options"));
    }

    let mut weights = Vec::new();
    for option in options {
        match weight_map.get(option) {
            Some(weight) => weights.push(*weight),
            None => return Err(anyhow::anyhow!("Unable to find weight for option '{}'", option)),
        }
    }

    if weights.len() != options.len() {
        return Err(anyhow::anyhow!("Number of weights does not match number of options"));
    }

    let dist = match WeightedIndex::new(weights) {
        Ok(d) => d,
        Err(e) => return Err(anyhow::anyhow!("Error creating WeightedIndex: {}", e)),
    };

    let index = dist.sample(random);

    Ok(options[index].clone())
}

/// The `boundary_prob` value indicates the probability of choosing
/// a boundary value, i.e. `Fp - 1` or `1`. The `small_upper_bound_prob` value
/// indicates the probability of choosing a small integer, i.e. from
/// the domain `[1..=small_upper_bound]`.
pub fn random_non_zero_field_element(
    curve: &str,
    random: &mut impl Rng,
    boundary_prob: f64,
    small_upper_bound_prob: f64,
    // default value is 10 @audit
    small_upper_bound: u128,
) -> U256 {
    let prime = curve_prime(curve);

    // Choose from boundary values `[1, Fp - 1]`
    if bernoulli(boundary_prob, random) {
        let value = random.choice(&[U256::ONE, prime - U256::ONE]).clone();
        return value;
    }

    // Choose from `[1..=small_upper_bound]`
    if bernoulli(small_upper_bound_prob, random) {
        let value = random.random_range(1..=small_upper_bound);
        return U256::from(value);
    } else {
        // Choose from `[1..=Fp - 1]`
        let value = U256::random(random) % prime; // @audit induces bias but negligible and this is not crypto secure so whatever

        // The odds of this is negligible but we check just in case
        if value == U256::ZERO {
            return U256::ONE;
        }

        return value;
    }
}

/// The `boundary_prob` value indicates the probability of choosing
/// a boundary value, i.e. `[0, 1, Fp - 1, Fp]` or `[0, 1, Fp - 1]` if
/// `exclude_prime` is `True`. The `small_upper_bound_prob` value indicates the
/// probability of choosing a small integer, i.e. from the domain `[0..=small_upper_bound]`.
pub fn random_field_element(
    curve: &str,
    random: &mut impl Rng,
    exclude_prime: bool,
    boundary_prob: f64,
    small_upper_bound_prob: f64,
    // default value is 10 @audit
    small_upper_bound: u128,
) -> U256 {
    let prime = curve_prime(curve);

    // Choose from boundary values
    if bernoulli(boundary_prob, random) {
        if exclude_prime {
            // Choose from `[0, 1, Fp - 1]`
            let value = random.choice(&[U256::ZERO, U256::ONE, prime - U256::ONE]).clone();
            return value;
        } else {
            // Choose from `[0, 1, Fp - 1, Fp]`
            let value =
                random.choice(&[U256::ZERO, U256::ONE, prime - U256::ONE, prime]).clone();
            return value;
        }
    } else {
        // Choose from `[0..=small_upper_bound]`
        if bernoulli(small_upper_bound_prob, random) {
            let value = random.random_range(0..=small_upper_bound);
            return U256::from(value);
        } else {
            if exclude_prime {
                // Choose from `[0..=Fp - 1]`
                let value = U256::random(random) % (prime - U256::ONE); // @audit induces bias but negligible and this is not crypto secure so whatever
                return value;
            } else {
                // Choose from `[0..=Fp]`
                let value = U256::random(random) % prime; // @audit induces bias but negligible and this is not crypto secure so whatever
                return value;
            }
        }
    }
}