use crate::error::ResponseError;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Message {
    Request(RequestMessage),
    Notification(NotificationMessage),
    Response(ResponseMessage),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RequestMessage {
    jsonrpc: JsonrpcVersion,
    pub id: RequestId,
    pub method: String,
    pub params: Option<serde_json::Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NotificationMessage {
    jsonrpc: JsonrpcVersion,
    pub method: String,
    pub params: Option<serde_json::Value>,
}

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct ResponseMessage {
    jsonrpc: JsonrpcVersion,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub id: Option<RequestId>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub result: Option<serde_json::Value>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub error: Option<ResponseError>,
}

impl ResponseMessage {
    pub fn error(id: Option<RequestId>, error: ResponseError) -> Self {
        Self {
            id,
            error: Some(error),
            ..Self::default()
        }
    }
}

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
            Err(ResponseError::invalid_request()
                .message(&format!("Unsupported JSON-RPC version: {value}")))
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(untagged)]
pub enum RequestId {
    Integer(i32),
    String(String),
}
