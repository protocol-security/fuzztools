use crate::builders::{Node, RpcError};
use alloy::providers::Provider;

#[derive(Clone, Copy)]
/// Stores `gas_price` and `max_priority_fee` of the current block.
pub struct RpcCache {
    /// Gas price is constant per block
    pub gas_price: u128,

    /// Max priority fee is constant per block
    pub max_priority_fee: u128,
}

impl RpcCache {
    /// Fetches `gas_price` and `max_priority_fee` from the `node`
    pub async fn fetch(node: &Node) -> Result<Self, RpcError> {
        let (gas_price, max_priority_fee) =
            tokio::try_join!(node.get_gas_price(), node.get_max_priority_fee_per_gas())?;

        Ok(Self { gas_price, max_priority_fee })
    }
}
