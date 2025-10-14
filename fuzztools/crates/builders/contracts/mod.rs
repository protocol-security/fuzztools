//! # Contracts
//!
//! This module contains a set of pre-deployed contracts:
//!
//! - `AccessListTarget` - The idea of this contract is that it takes `n` chunks of 32 bytes from
//!   the calldata and `SLOAD`s them from the storage, effectively testing EIP-2930 correctness.

pub(crate) mod access_list_target;
