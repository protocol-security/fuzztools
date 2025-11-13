//! # Utils
//!
//! This module contains utility functions and types that are used throughout all my fuzzers.
//!
//! ## Modules
//!
//! - [`fast_signer`] - Implements a secp256k1 signer that is faster than alloy's one by 20-30%.

mod fast_signer;

pub use fast_signer::FastPrivateKeySigner;
