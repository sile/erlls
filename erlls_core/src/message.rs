use crate::error::ResponseError;
use orfail::OrFail;
use serde::{Deserialize, Serialize};
use std::path::Path;

// TODO: rename
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Message {
    Request(RequestMessage),
    Notification(NotificationMessage),
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

impl NotificationMessage {
    pub fn new(method: &str, params: impl Serialize) -> orfail::Result<Self> {
        Ok(Self {
            jsonrpc: JsonrpcVersion,
            method: method.to_owned(),
            params: serde_json::to_value(params).or_fail()?,
        })
    }
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

    pub fn null_result() -> Self {
        Self {
            result: Some(serde_json::Value::Null),
            ..Self::default()
        }
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
    pub fn from_path<P0: AsRef<Path>, P1: AsRef<Path>>(root: P0, path: P1) -> orfail::Result<Self> {
        let url = if path.as_ref().is_absolute() {
            url::Url::parse(&format!("file://{}", path.as_ref().to_str().or_fail()?)).or_fail()?
        } else {
            url::Url::parse(&format!(
                "file://{}",
                root.as_ref().join(path).to_str().or_fail()?
            ))
            .or_fail()?
        };
        Ok(Self(url))
    }

    pub fn path(&self) -> &Path {
        Path::new(self.0.path())
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
    pub general: GeneralClientCapabilities,
}

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct WorkspaceCapabilitylies {
    #[serde(default)]
    pub workspace_edit: WorkspaceEditClientCapabilities,
}

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct GeneralClientCapabilities {
    #[serde(default)]
    pub position_encodings: Vec<PositionEncodingKind>,
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
    pub document_formatting_provider: bool,
    pub definition_provider: bool,
    pub completion_provider: CompletionOptions,
    pub semantic_tokens_provider: serde_json::Value,
    pub text_document_sync: TextDocumentSyncKind,
    pub position_encoding: PositionEncodingKind,
}

impl Default for ServerCapabilities {
    fn default() -> Self {
        Self {
            document_formatting_provider: true,
            definition_provider: true,
            completion_provider: CompletionOptions::default(),

            // TODO: use struct
            semantic_tokens_provider: serde_json::json!({
                "legend": {
                    "tokenTypes": [
                        "comment",
                        "string",
                        "keyword",
                        "variable",
                        "number",
                        "operator"
                    ],
                    "tokenModifiers": []
                },
                "range": true,
                "full": true
            }),
            text_document_sync: TextDocumentSyncKind::INCREMENTAL,

            // As VSCode does not support `Utf32` yet, we use `Utf16`.
            position_encoding: PositionEncodingKind::Utf16,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SemanticTokensParams {
    pub text_document: TextDocumentIdentifier,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SemanticTokensRangeParams {
    pub text_document: TextDocumentIdentifier,
    pub range: Range,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CompletionOptions {
    pub trigger_characters: Vec<String>,
}

impl Default for CompletionOptions {
    fn default() -> Self {
        Self {
            trigger_characters: vec![":".to_owned()],
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CompletionParams {
    #[serde(flatten)]
    inherit0: TextDocumentPositionParams,
}

impl CompletionParams {
    pub fn text_document(&self) -> &TextDocumentIdentifier {
        &self.inherit0.text_document
    }

    pub fn position(&self) -> Position {
        self.inherit0.position
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CompletionItem {
    pub label: String,
    pub kind: CompletionItemKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct CompletionItemKind(u8);

impl CompletionItemKind {
    pub const TEXT: Self = Self(1);
    pub const METHOD: Self = Self(2);
    pub const FUNCTION: Self = Self(3);
    pub const CONSTRUCTOR: Self = Self(4);
    pub const FIELD: Self = Self(5);
    pub const VARIABLE: Self = Self(6);
    pub const CLASS: Self = Self(7);
    pub const INTERFACE: Self = Self(8);
    pub const MODULE: Self = Self(9);
    pub const PROPERTY: Self = Self(10);
    pub const UNIT: Self = Self(11);
    pub const VALUE: Self = Self(12);
    pub const ENUM: Self = Self(13);
    pub const KEYWORD: Self = Self(14);
    pub const SNIPPET: Self = Self(15);
    pub const COLOR: Self = Self(16);
    pub const FILE: Self = Self(17);
    pub const REFERENCE: Self = Self(18);
    pub const FOLLOW: Self = Self(19);
    pub const ENUM_MEMBER: Self = Self(20);
    pub const CONSTANT: Self = Self(21);
    pub const STRUCT: Self = Self(22);
    pub const EVENT: Self = Self(23);
    pub const OPERATOR: Self = Self(24);
    pub const TYPE_PARAMETER: Self = Self(25);
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
pub struct DidCloseTextDocumentParams {
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
pub struct TextDocumentPositionParams {
    pub text_document: TextDocumentIdentifier,
    pub position: Position,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DefinitionParams {
    #[serde(flatten)]
    text_document_position: TextDocumentPositionParams,
}

impl DefinitionParams {
    pub fn text_document(&self) -> &TextDocumentIdentifier {
        &self.text_document_position.text_document
    }

    pub fn position(&self) -> Position {
        self.text_document_position.position
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TextDocumentIdentifier {
    pub uri: DocumentUri,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Position {
    pub line: usize,
    pub character: usize,
}

impl Position {
    pub fn new(line: usize, character: usize) -> Self {
        Self { line, character }
    }

    pub fn from_efmt_position(text: &str, pos: efmt_core::span::Position) -> Self {
        let character = text[..pos.offset()]
            .chars()
            .rev()
            .take(pos.column() - 1)
            .map(|c| c.len_utf16())
            .sum();
        Self {
            line: pos.line() - 1,
            character,
        }
    }
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

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

impl Range {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn from_parse_error(e: &efmt_core::parse::Error) -> Self {
        // TODO: Consider UTF-16
        let (start, end) = match e {
            efmt_core::parse::Error::UnexpectedEof { position, .. } => (
                Position::new(position.line() - 1, 0),
                Position::new(position.line() - 1, position.column()),
            ),
            efmt_core::parse::Error::UnexpectedToken { position, .. } => (
                Position::new(position.line() - 1, 0),
                Position::new(position.line() - 1, position.column()),
            ),
            efmt_core::parse::Error::TokenizeError { source, .. } => {
                let position = source.position();
                (
                    Position::new(position.line() - 1, 0),
                    Position::new(position.line() - 1, position.column()),
                )
            }
        };
        Self { start, end }
    }

    pub fn beginning() -> Self {
        Self::new(Position::new(0, 0), Position::new(0, 0))
    }

    pub fn from_efmt_range(text: &str, range: std::ops::Range<efmt_core::span::Position>) -> Self {
        let start = Position::from_efmt_position(text, range.start);
        let end = Position::from_efmt_position(text, range.end);
        Self { start, end }
    }
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Location {
    pub uri: DocumentUri,
    pub range: Range,
}

impl Location {
    pub fn new(uri: DocumentUri, range: Range) -> Self {
        Self { uri, range }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Diagnostic {
    pub range: Range,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub severity: Option<DiagnosticSeverity>,

    pub message: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct DiagnosticSeverity(u8);

impl DiagnosticSeverity {
    pub const ERROR: Self = Self(1);
    pub const WARNING: Self = Self(2);
    pub const INFORMATION: Self = Self(3);
    pub const HINT: Self = Self(4);
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PublishDiagnosticsParams {
    pub uri: DocumentUri,
    pub diagnostics: Vec<Diagnostic>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub version: Option<i32>,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum SemanticTokenType {
    Namespace,
    Type,
    Class,
    Enum,
    Interface,
    Struct,
    TypeParameter,
    Parameter,
    Variable,
    Property,
    EnumMember,
    Event,
    Function,
    Method,
    Macro,
    Keyword,
    Modifier,
    Comment,
    String,
    Number,
    Regexp,
    Operator,
    Decorator,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum SemanticTokenModifier {
    Declaration,
    Definition,
    Readonly,
    Static,
    Deprecated,
    Abstract,
    Async,
    Modification,
    Documentation,
    DefaultLibrary,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SemanticTokensLegend {
    pub token_types: Vec<SemanticTokenType>,
    pub token_modifiers: Vec<SemanticTokenModifier>,
}

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SemanticTokens {
    pub data: Vec<u32>,
}
