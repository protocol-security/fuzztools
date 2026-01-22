#[derive(Debug)]
pub enum RpcError {
    Request(reqwest::Error),
    MissingResult(String),
    ParseError(String),
}

impl RpcError {
    pub fn is_connection_error(&self) -> bool {
        matches!(self, RpcError::Request(e) if e.is_connect())
    }
}

impl std::fmt::Display for RpcError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RpcError::Request(e) => write!(f, "request failed: {e}"),
            RpcError::MissingResult(r) => write!(f, "missing result field: {r}"),
            RpcError::ParseError(e) => write!(f, "parse error: {e}"),
        }
    }
}

impl std::error::Error for RpcError {}

impl From<reqwest::Error> for RpcError {
    fn from(e: reqwest::Error) -> Self {
        RpcError::Request(e)
    }
}
