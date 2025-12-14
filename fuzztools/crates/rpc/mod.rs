//! Implements a blazingly fast `RpcClient` to send JSON-RPC requests.

use reqwest::Client;
use serde_json::{json, Value};
use std::time::Duration;

mod error;
pub use error::RpcError;

/// A client for making RPC calls to a JSON-RPC endpoint.
///
/// # Examples
///
/// ```
/// let client = RpcClient::new("https://api.example.com", Duration::from_secs(10));
/// let result = client.call("eth_blockNumber", json!({})).await?;
/// println!("Result: {:?}", result);
/// ```
pub struct RpcClient {
    inner: Client,
    url: String,
}

impl RpcClient {
    pub fn new(url: String, timeout: Duration) -> Self {
        let client = Client::builder()
            .tcp_nodelay(true)
            .pool_max_idle_per_host(100)
            .timeout(timeout)
            .build()
            .unwrap();
        Self { inner: client, url }
    }

    /// Make a single RPC call to the endpoint.
    pub async fn call(&self, method: &str, params: Value) -> Result<u128, RpcError> {
        let payload = json!({
            "jsonrpc": "2.0",
            "method": method,
            "params": params,
            "id": 1
        });

        let resp: Value = self.inner.post(&self.url).json(&payload).send().await?.json().await?;
        let hex = resp
            .get("result")
            .and_then(|r| r.as_str())
            .ok_or_else(|| RpcError::Parse("missing result".to_string()))?;
        u128::from_str_radix(hex.trim_start_matches("0x"), 16)
            .map_err(|_| RpcError::Parse(format!("invalid hex: {}", hex)))
    }

    /// Make a batch of RPC calls to the endpoint without any parameters. Useful for `GET` queries
    /// like `eth_blockNumber`, `eth_chainId`, etc.
    pub async fn batch_call_no_params(&self, methods: &[&str]) -> Result<Vec<u128>, RpcError> {
        let payload: Vec<Value> = methods
            .iter()
            .enumerate()
            .map(|(id, method)| json!({ "jsonrpc": "2.0", "method": method, "params": [], "id": id }))
            .collect();

        let resp: Vec<Value> =
            self.inner.post(&self.url).json(&payload).send().await?.json().await?;

        resp.iter()
            .map(|v| {
                let hex = v
                    .get("result")
                    .and_then(|r| r.as_str())
                    .ok_or_else(|| RpcError::Parse("missing result".to_string()))?;
                u128::from_str_radix(hex.trim_start_matches("0x"), 16)
                    .map_err(|_| RpcError::Parse(format!("invalid hex: {}", hex)))
            })
            .collect()
    }

    /// Send a batch of raw transactions to the endpoint.
    pub async fn send_raw_transactions(&self, transactions: Vec<String>) -> Result<(), RpcError> {
        let payload: Vec<Value> = transactions
            .iter()
            .map(|tx| json!({ "jsonrpc": "2.0", "method": "eth_sendRawTransaction", "params": [tx], "id": 1 }))
            .collect();

        let _: Vec<Value> = self.inner.post(&self.url).json(&payload).send().await?.json().await?;
        Ok(())
    }
}
