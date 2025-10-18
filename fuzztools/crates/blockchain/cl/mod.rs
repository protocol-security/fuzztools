//! # CL blockchain
//!
//! Exports the latest fork types:
//! - `BeaconState` (Fulu)
//! - `BeaconBlockBody` (Electra)

mod forks;

pub use forks::fulu::BeaconState;
pub use forks::electra::BeaconBlockBody;