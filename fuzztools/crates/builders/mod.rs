mod transaction;
mod cache;
mod contracts;

pub use contracts::contract::AccessListTarget;

use alloy::{providers::RootProvider, transports::TransportError};
pub use cache::RpcCache;
use thiserror::Error;
pub use transaction::TransactionBuilder;

/// Calldata size to be used in base transactions
pub const DEFAULT_INPUT_SIZE: usize = 256;

/// Gas limit to be used in base transactions
pub const DEFAULT_GAS_LIMIT: u64 = 100_000;

#[derive(Error, Debug)]
pub enum RpcError {
    #[error("rpc request failed: {0}")]
    TransportError(#[from] TransportError),
}

/// This is an alias for readiness
pub type Node = alloy::providers::fillers::FillProvider<
    alloy::providers::fillers::JoinFill<
        alloy::providers::fillers::JoinFill<
            alloy::providers::Identity,
            alloy::providers::fillers::JoinFill<
                alloy::providers::fillers::GasFiller,
                alloy::providers::fillers::JoinFill<
                    alloy::providers::fillers::BlobGasFiller,
                    alloy::providers::fillers::JoinFill<
                        alloy::providers::fillers::NonceFiller,
                        alloy::providers::fillers::ChainIdFiller,
                    >,
                >,
            >,
        >,
        alloy::providers::fillers::WalletFiller<alloy::network::EthereumWallet>,
    >,
    RootProvider,
>;
