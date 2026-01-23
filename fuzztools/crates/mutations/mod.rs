//! This crate implements the `Mutable` trait for common Rust and Alloy types.

#[macro_use]
mod macros;

mod alloy;
mod array;
mod bool;
mod bytes;
mod constants;
mod slice;
mod traits;
mod uint;
mod vec;

pub use traits::{Mutable, Random};
