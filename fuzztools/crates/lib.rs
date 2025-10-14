//! # Fuzztools
//!
//! This crate implements the `Mutable` trait for common Ethereum data types
//! that I will be using in my fuzzers, as well as some other stuff. The reason why is because I am
//! lazy as fuck and do not wanna copy-paste the same code over and over again :P.
//!
//! ## Modules
//!
//! - [`blockchain`] - Implements the `Block` and `Header` types.
//! - [`builders`] - Handles the logic of creating **VALID** types according to protocol rules.
//! - [`evm`] - Handles the logic of creating **VALID** calldata for the EVM for pre-deployed contracts (like [AccessListTarget](https://github.com/nethoxa/fuzzers/blob/main/fuzztools/crates/builders/contracts/AccessListTarget.sol)).
//! - [`mutations`] - Implements the `Mutable` trait for common Rust types, as well as for the types
//!   within this crate.
//! - [`net`] - WIP.
//! - [`rpc`] - WIP.
//! - [`transactions`] - Implements the `Transaction` and `SignedTransaction` types.
//! - [`utils`] - Some stuff I do not know where to put like `FastPrivateKeySigner`, which is used
//!   among many other places.
//!
//! ## Example
//!
//! The idea is to make it as easy as possible to create fuzzers that are 1) fast 2) easy to maintain and 3) actually useful. If you do not trust me, I already found many bugs in Ethereum clients, so check'em out [here](https://nethoxa.github.io/bug-bounties). For example, if you were to fuzz a given struct like this:
//!
//! ```rust,no_run
//! struct Whatever {
//!     a: u64,
//!     b: u64,
//! }
//! ```
//!
//! you would only need to implement the `Mutable` trait for it, like this:
//!
//! ```rust,no_run
//! impl Mutable for Whatever {
//!     fn mutate(&mut self, random: &mut impl Rng) -> bool {
//!         match random.random_range(0..=1) {
//!             0 => self.a.mutate(random),
//!             1 => self.b.mutate(random),
//!             _ => unreachable!(),
//!         }
//!
//!         false
//!     }
//! }
//! ```
//!
//! or however you wanna split the probability between the two fields, which I can assure you will
//! output **ALWAYS** different non-random and interesting values. About the `utils` module, it
//! contains stuff like `FastPrivateKeySigner`, which is faster than `alloy`'s one, as well as some
//! logic I will be putting to send request blazingly fast via network (I'm testing it in `rakoon`
//! so bear with it a little bit).

pub mod blockchain;
pub mod builders;
pub mod evm;
pub mod mutations;
pub mod net;
pub mod rpc;
pub mod transactions;
pub mod utils;
