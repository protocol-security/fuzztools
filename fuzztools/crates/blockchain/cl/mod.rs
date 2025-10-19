//! # CL blockchain
//!
//! Exports the latest:
//! - `BeaconState`
//! - `BeaconBlock`
//!
//! As well as implements the consensus spec types as a whole.

pub mod forks;

pub use forks::{electra::BeaconBlockBody, fulu::BeaconState};
