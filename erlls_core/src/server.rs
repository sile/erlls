use crate::{
    config::Config,
    definition_provider::DefinitionProvider,
    document::{Document, EditingDocuments, Text},
    error::ResponseError,
    fs::FileSystem,
    message::{
        DefinitionParams, Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams,
        DidCloseTextDocumentParams, DidOpenTextDocumentParams, DocumentFormattingParams,
        InitializeParams, InitializeResult, InitializedParams, Message, NotificationMessage,
        PublishDiagnosticsParams, Range, RequestMessage, ResponseMessage, TextEdit,
    },
};
use orfail::OrFail;
use serde::Deserialize;
use std::marker::PhantomData;

#[derive(Debug)]
pub struct LanguageServer<FS> {
    state: Option<LanguageServerState<FS>>,
    config: Config,
    _fs: PhantomData<FS>,
}

impl<FS: FileSystem> LanguageServer<FS> {
    pub fn new(config: Config) -> Self {
        Self {
            state: None,
            config,
            _fs: PhantomData,
        }
    }

    pub fn config(&self) -> &Config {
        &self.config
    }

    pub fn update_config(&mut self, config: Config) {
        self.config = config.clone();
        if let Some(state) = &mut self.state {
            state.config = config.clone();
            state.definition_provider.update_config(config);
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
        let result = match msg.method.as_str() {
            "initialized" => serde_json::from_value(msg.params)
                .or_fail()
                .and_then(|params| state.handle_initialized_notification(params).or_fail()),
            "textDocument/didOpen" => serde_json::from_value(msg.params)
                .or_fail()
                .and_then(|params| state.handle_did_open_notification(params).or_fail()),
            "textDocument/didChange" => serde_json::from_value(msg.params)
                .or_fail()
                .and_then(|params| state.handle_did_change_notification(params).or_fail()),
            "textDocument/didClose" => serde_json::from_value(msg.params)
                .or_fail()
                .and_then(|params| state.handle_did_close_notification(params).or_fail()),
            method => {
                log::warn!("Unknown notification: {method:?}");
                Ok(())
            }
        };
        if let Err(e) = result {
            log::warn!("Failed to handle {:?} notification: reason={e}", msg.method);
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
            documents: EditingDocuments::default(),
            definition_provider: DefinitionProvider::new(self.config.clone()),
            outgoing_messages: Vec::new(),
            _fs: PhantomData,
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
    documents: EditingDocuments,
    definition_provider: DefinitionProvider<FS>,
    outgoing_messages: Vec<Vec<u8>>,
    _fs: PhantomData<FS>,
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
            .handle_request(params, &self.documents)
    }

    fn handle_formatting_request(
        &mut self,
        params: DocumentFormattingParams,
    ) -> Result<ResponseMessage, ResponseError> {
        let doc = self.documents.get(&params.text_document.uri).or_fail()?;
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

    fn handle_initialized_notification(
        &mut self,
        _params: InitializedParams,
    ) -> orfail::Result<()> {
        Ok(())
    }

    fn handle_did_open_notification(
        &mut self,
        params: DidOpenTextDocumentParams,
    ) -> orfail::Result<()> {
        log::info!(
            "didOpen: uri={:?}, lang={}, version={}",
            params.text_document.uri,
            params.text_document.language_id,
            params.text_document.version
        );
        if !params.text_document.is_erlang() {
            log::warn!(
                "Unsupported language: lang={}, uri={}",
                params.text_document.language_id,
                params.text_document.uri
            );
            return Ok(());
        }

        self.documents.insert(
            params.text_document.uri,
            Document {
                version: Some(params.text_document.version),
                text: Text::new(&params.text_document.text),
            },
        );
        Ok(())
    }

    fn handle_did_close_notification(
        &mut self,
        params: DidCloseTextDocumentParams,
    ) -> orfail::Result<()> {
        self.documents.remove(&params.text_document.uri);
        Ok(())
    }

    fn handle_did_change_notification(
        &mut self,
        params: DidChangeTextDocumentParams,
    ) -> orfail::Result<()> {
        log::info!("didChange: uri={:?}", params.text_document.uri);
        let doc = self
            .documents
            .get_mut(&params.text_document.uri)
            .or_fail()?;
        doc.version = Some(params.text_document.version);

        {
            // TODO: Send the notification only if neeeded
            let params = PublishDiagnosticsParams {
                uri: params.text_document.uri.clone(),
                diagnostics: vec![],
                version: doc.version,
            };
            let notification =
                NotificationMessage::new("textDocument/publishDiagnostics", params).or_fail()?;
            self.outgoing_messages
                .push(serde_json::to_vec(&notification).or_fail()?);
        }

        log::info!("Before lines: {}", doc.text.lines.len());
        for change in params.content_changes {
            log::info!("Change: range={:?}", change.range);
            if let Some(range) = change.range {
                doc.text.apply_change(range, &change.text).or_fail()?;
            } else {
                doc.text = Text::new(&change.text);
            }
            log::info!("After lines: {}", doc.text.lines.len());
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::message::{Position, Range};

    #[test]
    fn apply_change_work() -> orfail::Result<()> {
        let mut text = Text::new("abc\ndef\nghi");
        assert_eq!(text.to_string(), "abc\ndef\nghi");

        text.apply_change(Range::new(Position::new(0, 1), Position::new(0, 2)), "xyz")
            .or_fail()?;
        assert_eq!(text.to_string(), "axyzc\ndef\nghi");

        text.apply_change(Range::new(Position::new(0, 1), Position::new(2, 2)), "123")
            .or_fail()?;
        assert_eq!(text.to_string(), "a123i");

        let mut text = Text::new("111\n222\n333\n444\n555\n666");
        text.apply_change(Range::new(Position::new(1, 1), Position::new(5, 0)), "")
            .or_fail()?;
        assert_eq!(text.to_string(), "111\n2666");

        Ok(())
    }
}
