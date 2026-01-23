//! # Fuzztools
//!
//! A collection of utils for building fuzzers that are:
//!
//! 1) fast
//! 2) easy to maintain
//! 3) able to find bugs
//!
//! ## Modules
//!
//! - [`blockchain`] - Consensus and execution layer types and constants from the Ethereum spec.
//! - [`builders`] - Logic for creating **valid** instances of types according to protocol rules.
//! - [`circuits`] - Noir IR implementation for generating random circuits using DAGs.
//! - [`mutations`] - [`Mutable`](mutations::Mutable) trait implementations for various types.
//! - [`rpc`] - Fast JSON-RPC client for single and batched requests.
//! - [`transactions`] - Flexible transaction types supporting malformed/cross-type transactions.

pub mod blockchain;
pub mod builders;
pub mod circuits;
pub mod mutations;
pub mod rpc;
pub mod transactions;
