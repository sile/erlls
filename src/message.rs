use crate::error::{ErrorCode, ResponseError};
use orfail::OrFail;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Message {
    Request(RequestMessage),
    Notification(NotificationMessage),
    Response(ResponseMessage),
}

impl Message {
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
    pub client_info: Option<ClientInfo>,
    pub capabilities: ClientCapabilities,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ClientCapabilities {
    #[serde(default)]
    pub workspace: WorkspaceCapabilitylies,
}

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct WorkspaceCapabilitylies {
    #[serde(default)]
    pub workspace_edit: WorkspaceEditClientCapabilities,
}

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct WorkspaceEditClientCapabilities {
    #[serde(default)]
    pub document_changes: bool,
}

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InitializeResult {
    pub capabilities: ServerCapabilities,
    pub server_info: ServerInfo,
}

impl InitializeResult {
    pub fn new() -> Self {
        Self::default()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClientInfo {
    pub name: String,
    pub version: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServerInfo {
    pub name: String,
    pub version: String,
}

impl Default for ServerInfo {
    fn default() -> Self {
        Self {
            name: "erlls".to_owned(),
            version: env!("CARGO_PKG_VERSION").to_owned(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ServerCapabilities {
    pub rename_provider: bool,
    pub document_formatting_provider: bool,
    pub text_document_sync: TextDocumentSyncKind,
    pub position_encoding: PositionEncodingKind,
}

impl Default for ServerCapabilities {
    fn default() -> Self {
        Self {
            rename_provider: true,
            document_formatting_provider: true,
            text_document_sync: TextDocumentSyncKind::INCREMENTAL,
            position_encoding: PositionEncodingKind::Utf32,
        }
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

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RenameParams {
    #[serde(flatten)]
    pub text_document_position: TextDocumentPositionParams,
    pub new_name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TextDocumentPositionParams {
    pub text_document: TextDocumentIdentifier,
    pub position: Position,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TextDocumentIdentifier {
    pub uri: DocumentUri,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Position {
    pub line: u32,
    pub character: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct WorkspaceEdit {
    pub document_changes: Vec<TextDocumentEdit>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TextDocumentEdit {
    pub text_document: OptionalVersionedTextDocumentIdentifier,
    pub edits: Vec<TextEdit>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct VersionedTextDocumentIdentifier {
    pub uri: DocumentUri,
    pub version: i32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct OptionalVersionedTextDocumentIdentifier {
    pub uri: DocumentUri,
    pub version: Option<i32>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TextEdit {
    pub range: Range,
    pub new_text: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DocumentFormattingParams {
    pub text_document: TextDocumentIdentifier,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct TextDocumentSyncKind(u8);

impl TextDocumentSyncKind {
    pub const NONE: Self = Self(0);
    pub const FULL: Self = Self(1);
    pub const INCREMENTAL: Self = Self(2);
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DidChangeTextDocumentParams {
    pub text_document: VersionedTextDocumentIdentifier,
    pub content_changes: Vec<TextDocumentContentChangeEvent>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TextDocumentContentChangeEvent {
    #[serde(default)]
    pub range: Option<Range>,
    pub text: String,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PositionEncodingKind {
    #[serde(rename = "utf-8")]
    Utf8,
    #[default]
    #[serde(rename = "utf-16")]
    Utf16,
    #[serde(rename = "utf-32")]
    Utf32,
}
