//! # Builders
//!
//! This module contains some builders that **ALWAYS** generate **VALID** Ethereum types according
//! to protocol rules:
//!
//! - `TransactionBuilder` - Handles the logic of creating **VALID** transactions per mempool rules.
//!
//! Moreover, it also contains a set of pre-deployed contracts as well as some optimizations to
//! speed up stuff like an `RpcCache` that updates itself every time a new block is received.

mod contracts;
mod cache;
mod transaction;

use alloy::transports::TransportError;
pub use cache::rpc::RpcCache;
pub use contracts::access_list_target::AccessListTarget;
use thiserror::Error;
pub use transaction::TransactionBuilder;

/// Calldata size to be used in base transactions
pub(crate) const DEFAULT_INPUT_SIZE: usize = 256;

/// Gas limit to be used in base transactions
pub(crate) const DEFAULT_GAS_LIMIT: u64 = 100_000;

#[derive(Error, Debug)]
/// Errors that can occur when fetching RPC data
pub enum RpcError {
    #[error("rpc request failed: {0}")]
    /// RPC request failed, possibly due to a network issue
    TransportError(#[from] TransportError),
}
