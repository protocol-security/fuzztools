//! Mutation primitives for fuzzing.
//!
//! Provides the [`Mutable`] trait and implementations for common types including
//! primitives, collections, and Alloy types for Ethereum fuzzing.

mod bytes;
pub mod constants;
mod traits;
mod uint;
mod array;
mod alloy;
mod vec;

pub use crate::mutations::traits::Mutable;
