//! # Fuzztools
//!
//! This crate implements many types as well as stuff I will be using in my fuzzers. The reason why
//! is because I am lazy as fuck and do not wanna copy-paste the same code over and over again :P
//!
//! ## Modules
//!
//! - [`blockchain`] - Implements the consensus and execution spec types and constants.
//! - [`builders`] - Handles the logic of creating **VALID** types according to protocol rules.
//! - [`evm`] - Handles the logic of creating **VALID** calldata for the EVM for pre-deployed contracts (like [AccessListTarget](https://github.com/nethoxa/fuzzers/blob/main/fuzztools/crates/builders/contracts/AccessListTarget.sol)).
//! - [`mutations`] - Implements the `Mutable` trait for:
//!     - Alloy's `Address`, `Bytes`, `FixedBytes<N>`, `U256`, `Authorization`, `AccessList`,
//!       `Vec<Authorization>` and `Vec<SignedAuthorization>`.
//!     - `[T; N]` and `[u8; N]` arrays.
//!     - `Vec<T>` and `Vec<u8>`.
//!     - `u8`, `u16`, `u32`, `u64`, `u128`.
//! - [`net`] - WIP.
//! - [`rpc`] - WIP.
//! - [`transactions`] - Implements the `Transaction` and `SignedTransaction` types.
//! - [`utils`] - Some stuff I do not know where to put like `FastPrivateKeySigner`.
//! - [`zk`] - Implements common stuff for fuzzing ZK compilers/VMs. Right now, it only supports
//!   Noir IR.
//!
//! ## Example
//!
//! The idea is to make it as easy as possible to create fuzzers that are 1) fast 2) easy to maintain and 3) actually useful. If you do not trust me, I already found many bugs in Ethereum clients, so check'em out [here](https://nethoxa.github.io/bug-bounties).

pub mod blockchain;
pub mod builders;
pub mod evm;
pub mod mutations;
pub mod net;
pub mod rpc;
pub mod transactions;
pub mod utils;
pub mod math;
pub mod zk;
