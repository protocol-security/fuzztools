//! Mutation primitives for fuzzing.
//!
//! Provides the [`Mutable`] trait and implementations for common types including
//! primitives, collections, and Alloy types for Ethereum fuzzing.

mod bytes;
mod constants;
mod traits;
mod uint;
mod array;
mod alloy;
mod vec;

pub use crate::mutations::traits::*;
pub use crate::mutations::constants::*;
