//! # Contracts
//!
//! This module contains a set of pre-deployed contracts:
//!
//! - `AccessListTarget` - The idea of this contract is that it takes `n` chunks of 32 bytes from
//!   the calldata and `SLOAD`s them from the storage, effectively testing EIP-2930 correctness.
//! - `PrecompileTarget` - The idea of this contract is that it tests the correctness of the
//!   precompile contracts by doing many operations on top like `selfdestruct`, `call`,
//!   `delegatecall`, `EXT*` operations, etc.
pub(crate) mod access_list_target;
pub(crate) mod precompile_target;
