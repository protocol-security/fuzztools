use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Serialize, Deserialize)]
pub(crate) struct Config {
    pub rpc_timeout: u64,
    pub block_poll_interval: u64,
    pub screen_update_interval: u64,
    pub chunk_size: usize,
    pub txs_per_core: usize,
    pub tx_channel_capacity: usize,
    pub completion_channel_capacity: usize,
}
