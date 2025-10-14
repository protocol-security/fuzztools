//! # Cache
//!
//! This module contains the some structs that are used to cache data:
//!
//! - `RpcCache` - Stores the `gas_price` and `max_priority_fee` for the current block as they are
//!   constant per block.

pub(crate) mod rpc;
