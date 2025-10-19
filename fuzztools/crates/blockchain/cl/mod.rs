//! # CL blockchain
//!
//! Implements the following types:
//! - `BeaconState`
//! - `BeaconBlock`
//! 
//! As well as the consensus spec types as a whole, but as we do not need them, we only export the types above.

mod forks;

pub struct BeaconState {

}

pub struct BeaconBlock {}

pub use forks::fulu::BeaconState;
pub use forks::electra::BeaconBlockBody;