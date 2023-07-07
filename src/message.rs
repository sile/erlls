use std::path::PathBuf;

use crate::error::{ErrorCode, ResponseError};
use orfail::OrFail;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Message {
    Request(RequestMessage),
    Notification(NotificationMessage),
    Response(ResponseMessage),
}

impl Message {
    // pub fn take_id_and_method(&mut self) -> (Option<RequestId>, Option<String>) {
    //     match self {
    //         Message::Request(x) => (x.id.take(), x.method.take()),
    //         Message::Notification(x) => (x.id.take(), x.method.take()),
    //         Message::Response(x) => (x.
    //     }

    pub fn method(&self) -> Option<&str> {
        match self {
            Message::Request(x) => Some(&x.method),
            Message::Notification(x) => Some(&x.method),
            Message::Response(_) => None,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RequestMessage {
    jsonrpc: JsonrpcVersion,
    pub id: RequestId,
    pub method: String,
    #[serde(default)]
    pub params: serde_json::Value,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NotificationMessage {
    jsonrpc: JsonrpcVersion,
    pub method: String,
    #[serde(default)]
    pub params: serde_json::Value,
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
    pub fn result<T: Serialize>(result: T) -> orfail::Result<Self> {
        Ok(Self {
            result: Some(serde_json::to_value(result).or_fail()?),
            ..Self::default()
        })
    }

    pub fn error(error: ResponseError) -> Self {
        Self {
            error: Some(error),
            ..Self::default()
        }
    }

    pub fn id(mut self, id: RequestId) -> Self {
        self.id = Some(id);
        self
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct DocumentUri(url::Url);

impl DocumentUri {
    pub fn get(&self) -> &url::Url {
        &self.0
    }

    pub fn to_existing_path_buf(&self) -> orfail::Result<PathBuf> {
        (self.0.scheme() == "file")
            .or_fail()
            .map_err(|e| e.code(ErrorCode::INVALID_PARAMS.as_u32()))?;
        let path = PathBuf::from(self.0.path());
        path.exists()
            .or_fail()
            .map_err(|e| e.code(ErrorCode::INVALID_PARAMS.as_u32()))?;
        Ok(path)
    }
}

impl std::fmt::Display for DocumentUri {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InitializeParams {
    pub root_uri: DocumentUri,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InitializeResult {}

impl InitializeResult {
    pub fn new() -> Self {
        Self {}
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InitializedParams {}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DidOpenTextDocumentParams {
    pub text_document: TextDocumentItem,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TextDocumentItem {
    pub uri: DocumentUri,
    pub language_id: String,
    pub version: i32,
    pub text: String,
}

impl TextDocumentItem {
    pub fn is_erlang(&self) -> bool {
        self.language_id == "erlang"
    }
}
