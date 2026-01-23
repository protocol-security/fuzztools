//! A custom client for sending arbitrary JSON-RPC request. Uses `reqwest::Client` under-the-hood.

mod error;

use error::RpcError;
use reqwest::Client;
use serde_json::{json, Value};

pub struct RpcClient {
    inner: Client,
    url: String,
}

impl RpcClient {
    pub fn new(url: String) -> Self {
        let inner =
            Client::builder().tcp_nodelay(true).pool_max_idle_per_host(100).build().unwrap();

        Self { inner, url }
    }

    pub const fn payload(&self) -> Payload<'_> {
        Payload { client: self, calls: Vec::new() }
    }

    pub async fn send_raw_transactions(&self, txs: Vec<String>) -> Result<(), RpcError> {
        let payload: Vec<Value> = txs
            .into_iter()
            .enumerate()
            .map(|(i, tx)| json!({"jsonrpc": "2.0", "method": "eth_sendRawTransaction", "params": [tx], "id": i}))
            .collect();

        let _ = self.inner.post(&self.url).json(&payload).send().await?;

        Ok(())
    }

    async fn call(&self, calls: Vec<(&str, Value)>) -> Result<Vec<u128>, RpcError> {
        let requests: Vec<Value> = calls
            .into_iter()
            .enumerate()
            .map(|(i, (method, params))| json!({"jsonrpc": "2.0", "id": i, "method": method, "params": params}))
            .collect();

        let body = if requests.len() == 1 {
            requests.into_iter().next().unwrap()
        } else {
            Value::Array(requests)
        };

        let response: Value = self.inner.post(&self.url).json(&body).send().await?.json().await?;

        let results = match response {
            Value::Array(arr) => arr,
            single => vec![single],
        };

        results
            .into_iter()
            .map(|r| {
                let hex =
                    r["result"].as_str().ok_or_else(|| RpcError::MissingResult(r.to_string()))?;

                u128::from_str_radix(hex.trim_start_matches("0x"), 16)
                    .map_err(|e| RpcError::ParseError(e.to_string()))
            })
            .collect()
    }
}

pub struct Payload<'a> {
    client: &'a RpcClient,
    calls: Vec<(&'a str, Value)>,
}

impl<'a> Payload<'a> {
    #[inline(always)]
    pub fn add(mut self, method: &'a str, params: Option<Value>) -> Self {
        self.calls.push((method, params.unwrap_or(json!([]))));
        self
    }

    #[inline(always)]
    pub async fn execute(self) -> Result<Vec<u128>, RpcError> {
        self.client.call(self.calls).await
    }
}
