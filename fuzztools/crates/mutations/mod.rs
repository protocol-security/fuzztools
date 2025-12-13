//! Mutation implementations for fuzzing.

#[macro_use]
mod macros;

mod alloy;
mod array;
mod bool;
mod bytes;
mod constants;
mod traits;
mod uint;
mod vec;

pub use traits::{Mutable, Random};
