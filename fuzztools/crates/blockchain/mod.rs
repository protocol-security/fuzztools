//! # Blockchain
//!
//! Implements the consensus and execution spec types and constants, so that they can be easily
//! exported and used wherever needed (for example, in `raidan`).

mod cl;
mod el;

pub use cl::forks::{electra::BeaconBlockBody, fulu::BeaconState};
