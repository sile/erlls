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
    pub fn from_bytes(buf: &[u8]) -> Result<Self, ResponseMessage> {
        #[derive(Deserialize)]
        #[allow(dead_code)]
        struct IdAndMethod {
            jsonrpc: JsonrpcVersion,
            #[serde(default)]
            id: Option<RequestId>,
            method: String,
        }
        let IdAndMethod { id, method, .. } = serde_json::from_slice(buf)
            .map_err(ResponseError::from)
            .map_err(|e| ResponseMessage::error(None, e))?;
        todo!()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum RequestId {
    Integer(i32),
    String(String),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResponseMessage {
    id: Option<RequestId>,

    // result
    #[serde(default, skip_serializing_if = "Option::is_none")]
    error: Option<ResponseError>,
}

impl ResponseMessage {
    pub fn error(id: Option<RequestId>, error: ResponseError) -> Self {
        Self {
            id,
            error: Some(error),
        }
    }
}
