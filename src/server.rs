use orfail::OrFail;
use serde::Deserialize;
use std::{collections::HashMap, path::PathBuf};

use crate::{
    error::ResponseError,
    message::{
        DidChangeTextDocumentParams, DidOpenTextDocumentParams, DocumentFormattingParams,
        DocumentUri, InitializeParams, InitializeResult, InitializedParams, Message,
        NotificationMessage, RenameParams, RequestMessage, ResponseMessage,
    },
};

#[derive(Debug, Default)]
pub struct LanguageServer {
    state: Option<LanguageServerState>,
}

impl LanguageServer {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn handle_message(&mut self, msg: Message) -> Option<ResponseMessage> {
        match msg {
            Message::Request(msg) => {
                let res = self.handle_request(msg);
                Some(res)
            }
            Message::Notification(msg) => {
                self.handle_notification(msg);
                None
            }
            Message::Response(msg) => {
                self.handle_response(msg);
                None
            }
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
                "textDocument/rename" => deserialize_params(msg.params)
                    .and_then(|params| state.handle_rename_request(params)),
                "textDocument/formatting" => deserialize_params(msg.params)
                    .and_then(|params| state.handle_formatting_request(params)),
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
            method => {
                log::warn!("Unknown notification: {method:?}");
                Ok(())
            }
        };
        if let Err(e) = result {
            log::warn!("Failed to handle {:?} notification: reason={e}", msg.method);
        }
    }

    fn handle_response(&mut self, _msg: ResponseMessage) {}

    fn handle_initialize_request(
        &mut self,
        params: InitializeParams,
    ) -> Result<ResponseMessage, ResponseError> {
        let state = LanguageServerState {
            root_dir: params.root_uri.to_existing_path_buf().or_fail()?,
            documents: HashMap::new(),
        };

        log::info!("Client: {:?}", params.client_info);
        log::info!("Root dir: {:?}", state.root_dir);
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
struct LanguageServerState {
    root_dir: PathBuf,
    documents: HashMap<DocumentUri, Document>,
}

impl LanguageServerState {
    fn handle_rename_request(
        &mut self,
        params: RenameParams,
    ) -> Result<ResponseMessage, ResponseError> {
        todo!()
    }

    fn handle_formatting_request(
        &mut self,
        params: DocumentFormattingParams,
    ) -> Result<ResponseMessage, ResponseError> {
        todo!()
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
                text: params.text_document.text,
            },
        );
        Ok(())
    }

    fn handle_did_change_notification(
        &mut self,
        params: DidChangeTextDocumentParams,
    ) -> orfail::Result<()> {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct Document {
    pub version: Option<i32>,
    pub text: String,
}
