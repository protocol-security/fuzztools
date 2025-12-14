//! Implements some helper errors to correctly handle when the node crashed and when it did not, but only the request failed.

#[derive(Debug)]
pub enum RpcError {
    Request(reqwest::Error),
    Parse(String),
}

impl RpcError {
    pub fn is_connect(&self) -> bool {
        matches!(self, RpcError::Request(e) if e.is_connect())
    }
}

impl std::fmt::Display for RpcError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RpcError::Request(e) => write!(f, "request error: {}", e),
            RpcError::Parse(s) => write!(f, "parse error: {}", s),
        }
    }
}

impl std::error::Error for RpcError {}

impl From<reqwest::Error> for RpcError {
    fn from(e: reqwest::Error) -> Self {
        RpcError::Request(e)
    }
}
