//! Builders for creating valid fuzzing inputs.

pub mod contracts;

mod circuit;
mod transaction;

pub use circuit::CircuitBuilder;
pub use transaction::TransactionBuilder;
