//! # Utils
//!
//! This module contains utility functions and types that are used throughout all my fuzzers.
//!
//! ## Modules
//!
//! - [`fast_signer`] - Implements a secp256k1 signer that is faster than alloy's one by 20-30%.
//! - [`random`] - Implements the `choice` function for any `Rng` type, allowing to pick a random element from a non-empty slice.

mod fast_signer;
mod random;

pub use fast_signer::FastPrivateKeySigner;
pub use random::RandomChoice;