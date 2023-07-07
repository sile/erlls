use std::path::PathBuf;

use orfail::OrFail;
use serde::Deserialize;

use crate::{
    error::ResponseError,
    message::{
        DidOpenTextDocumentParams, InitializeParams, InitializeResult, InitializedParams, Message,
        NotificationMessage, RequestMessage, ResponseMessage,
    },
};

#[derive(Debug)]
struct LanguageServerState {
    root_dir: PathBuf,
}

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
        if msg.method != "initialize" && self.state.is_none() {
            return ResponseMessage::error(ResponseError::server_not_initialized()).id(msg.id);
        }

        fn deserialize_params<T>(params: serde_json::Value) -> Result<T, ResponseError>
        where
            T: for<'a> Deserialize<'a>,
        {
            serde_json::from_value(params).map_err(ResponseError::from)
        }

        let result = match msg.method.as_str() {
            "initialize" => deserialize_params(msg.params)
                .and_then(|params| self.handle_initialize_request(params)),
            _ => {
                todo!()
            }
        };
        result
            .unwrap_or_else(|e| ResponseMessage::error(e))
            .id(msg.id)
    }

    fn handle_notification(&mut self, msg: NotificationMessage) {
        if self.state.is_none() {
            log::warn!("Dropped a notification as the server is not initialized yet: {msg:?}");
            return;
        }
        let result = match msg.method.as_str() {
            "initialized" => serde_json::from_value(msg.params)
                .or_fail()
                .and_then(|params| self.handle_initialized_notification(params).or_fail()),
            "textDocument/didOpen" => {
                serde_json::from_value(msg.params)
                    .or_fail()
                    .and_then(|params| {
                        self.handle_did_open_text_document_notification(params)
                            .or_fail()
                    })
            }
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
        log::debug!("{params:?}");
        let state = LanguageServerState {
            root_dir: params.root_uri.to_existing_path_buf().or_fail()?,
        };
        self.state = Some(state);

        Ok(ResponseMessage::result(InitializeResult::new()).or_fail()?)
    }

    fn handle_initialized_notification(
        &mut self,
        _params: InitializedParams,
    ) -> orfail::Result<()> {
        Ok(())
    }

    fn handle_did_open_text_document_notification(
        &mut self,
        _params: DidOpenTextDocumentParams,
    ) -> orfail::Result<()> {
        Ok(())
    }
}
