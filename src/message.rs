use crate::error::ResponseError;
use serde::{Deserialize, Serialize};

#[derive(Debug, Default, Clone, Copy, Serialize, Deserialize)]
#[serde(try_from = "&str", into = "&str")]
struct JsonrpcVersion;

impl From<JsonrpcVersion> for &str {
    fn from(_: JsonrpcVersion) -> Self {
        "2.0"
    }
}

impl TryFrom<&str> for JsonrpcVersion {
    type Error = ResponseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if value == "2.0" {
            Ok(Self)
        } else {
            Err(ResponseError::parse_error()
                .message(&format!("Unsupported JSON-RPC version: {value}")))
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Message {
    jsonrpc: JsonrpcVersion,
}

impl Message {
    pub const JSONRPC_VERSION: &'static str = "2.0";

    pub fn from_bytes(buf: &[u8]) -> Result<Self, ResponseError> {
        let msg: Message = serde_json::from_slice(buf)?;
        todo!()
    }
}
