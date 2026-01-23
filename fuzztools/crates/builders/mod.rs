//! This crates hold `CircuitBuilder` and `TransactionBuilder`, whose task is to create **VALID**
//! circuits/transactions according to protocol rules. It holds too a set of contracts to be used in
//! onchain testing.

mod circuit;
mod transaction;

pub mod contracts;

pub use circuit::CircuitBuilder;
pub use transaction::TransactionBuilder;
