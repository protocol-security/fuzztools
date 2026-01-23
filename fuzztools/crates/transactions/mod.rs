//! This crate implements `Transaction`,  which is a wrapper around all
//! possible fields an Ethereum transaction can have (without taking into
//! account EIP-4844 sidecars, which I don't think I will be using for now), and
//! `SignedTransaction`, which is a wrapper over it + signature.

#[macro_use]
mod macros;

mod signed;
mod transaction;

pub use signed::SignedTransaction;
pub use transaction::Transaction;
