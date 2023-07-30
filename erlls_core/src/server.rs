use crate::{
    config::Config,
    definition_provider::DefinitionProvider,
    document::DocumentRepository,
    error::ResponseError,
    fs::FileSystem,
    message::{
        DefinitionParams, Diagnostic, DiagnosticSeverity, DocumentFormattingParams,
        InitializeParams, InitializeResult, Message, NotificationMessage, PublishDiagnosticsParams,
        Range, RequestMessage, ResponseMessage, TextEdit,
    },
};
use orfail::OrFail;
use serde::Deserialize;

#[derive(Debug)]
pub struct LanguageServer<FS> {
    state: Option<LanguageServerState<FS>>,
    config: Config,
}

impl<FS: FileSystem> LanguageServer<FS> {
    pub fn new(config: Config) -> Self {
        Self {
            state: None,
            config,
        }
    }

    pub fn config(&self) -> &Config {
        &self.config
    }

    pub fn update_config(&mut self, config: Config) {
        self.config = config.clone();
        if let Some(state) = &mut self.state {
            state.config = config.clone();
            state.document_repository.update_config(config);
        }
    }

    pub fn handle_incoming_message(&mut self, json: &[u8]) {
        let msg = match serde_json::from_slice(json) {
            Err(e) => {
                log::warn!("Invalid message: {e}");
                return;
            }
            Ok(msg) => msg,
        };
        match msg {
            Message::Request(msg) => {
                let res = self.handle_request(msg);
                self.push_outgoing_message(res);
            }
            Message::Notification(msg) => {
                self.handle_notification(msg);
            }
        }
    }

    pub fn take_outgoing_message(&mut self) -> Option<Vec<u8>> {
        self.state.as_mut()?.outgoing_messages.pop()
    }

    fn push_outgoing_message<T: serde::Serialize>(&mut self, msg: T) {
        if let Some(state) = self.state.as_mut() {
            state
                .outgoing_messages
                .extend(serde_json::to_vec(&msg).ok());
        }
    }

    fn handle_request(&mut self, msg: RequestMessage) -> ResponseMessage {
        fn deserialize_params<T>(params: serde_json::Value) -> Result<T, ResponseError>
        where
            T: for<'a> Deserialize<'a>,
        {
            serde_json::from_value(params).map_err(ResponseError::from)
        }

        let result = if msg.method == "initialize" {
            deserialize_params(msg.params).and_then(|params| self.handle_initialize_request(params))
        } else if let Some(state) = self.state.as_mut() {
            match msg.method.as_str() {
                "textDocument/formatting" => deserialize_params(msg.params)
                    .and_then(|params| state.handle_formatting_request(params)),
                "textDocument/definition" => deserialize_params(msg.params)
                    .and_then(|params| state.handle_definition_request(params)),
                "shutdown" => state.handle_shutdown_request(),
                _ => {
                    todo!("handle_request: method={}", msg.method)
                }
            }
        } else {
            Err(ResponseError::server_not_initialized())
        };

        result
            .unwrap_or_else(|e| ResponseMessage::error(e))
            .id(msg.id)
    }

    fn handle_notification(&mut self, msg: NotificationMessage) {
        let Some(state) = self.state.as_mut() else {
            log::warn!("Dropped a notification as the server is not initialized yet: {msg:?}");
            return;
        };
        if let Err(e) = state
            .document_repository
            .handle_notification(&msg)
            .or_fail()
        {
            log::warn!("Failed to handle {:?} notification: reason={e}", msg.method);
            return;
        }
    }

    fn handle_initialize_request(
        &mut self,
        params: InitializeParams,
    ) -> Result<ResponseMessage, ResponseError> {
        let root_dir = params.root_uri.path().to_path_buf();
        self.config.root_dir = root_dir;

        let state = LanguageServerState {
            config: self.config.clone(),
            definition_provider: DefinitionProvider::new(),
            outgoing_messages: Vec::new(),
            document_repository: DocumentRepository::new(),
        };

        log::info!("Client: {:?}", params.client_info);
        log::info!("Server config: {:?}", state.config.root_dir);
        log::info!("Client capabilities: {:?}", params.capabilities);

        // Client capabilities check
        // TODO: Use appropriate error code
        params
            .capabilities
            .workspace
            .workspace_edit
            .document_changes
            .or_fail()?;

        self.state = Some(state);
        Ok(ResponseMessage::result(InitializeResult::new()).or_fail()?)
    }
}

#[derive(Debug)]
struct LanguageServerState<FS> {
    config: Config,
    definition_provider: DefinitionProvider,
    outgoing_messages: Vec<Vec<u8>>,
    document_repository: DocumentRepository<FS>,
}

impl<FS: FileSystem> LanguageServerState<FS> {
    fn handle_shutdown_request(&mut self) -> Result<ResponseMessage, ResponseError> {
        Ok(ResponseMessage::default())
    }

    fn handle_definition_request(
        &mut self,
        params: DefinitionParams,
    ) -> Result<ResponseMessage, ResponseError> {
        self.definition_provider
            .handle_request(params, &self.document_repository)
    }

    fn handle_formatting_request(
        &mut self,
        params: DocumentFormattingParams,
    ) -> Result<ResponseMessage, ResponseError> {
        let doc = self
            .document_repository
            .get_from_editings(&params.text_document.uri)
            .or_fail()?;
        let text = doc.text.to_string();
        let new_text = match efmt_core::format_text::<efmt_core::items::ModuleOrConfig>(&text) {
            Err(e) => {
                // TODO: check client capabilities (e.g., "publishDiagnostics":{"relatedInformation":true,"tagSupport":{"valueSet":[1,2]},"versionSupport":true})
                let diagnostic = Diagnostic {
                    range: Range::from_parse_error(&e),
                    message: e.to_string(),
                    severity: Some(DiagnosticSeverity::ERROR),
                };
                let params = PublishDiagnosticsParams {
                    uri: params.text_document.uri.clone(),
                    diagnostics: vec![diagnostic],
                    version: doc.version,
                };
                let notification =
                    NotificationMessage::new("textDocument/publishDiagnostics", params)
                        .or_fail()?;
                self.outgoing_messages
                    .push(serde_json::to_vec(&notification).or_fail()?);
                return Err(ResponseError::request_failed().message("Failed to format"));
            }
            Ok(new_text) => new_text,
        };

        // TODO: optimzie
        let mut edits = Vec::new();
        if text != new_text {
            edits.push(TextEdit {
                range: doc.text.range(),
                new_text,
            });
        }
        return Ok(ResponseMessage::result(edits).or_fail()?);
    }
}
