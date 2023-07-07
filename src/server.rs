use std::path::PathBuf;

use orfail::OrFail;

use crate::{
    error::ResponseError,
    message::{InitializeParams, Message, NotificationMessage, RequestMessage, ResponseMessage},
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
            return ResponseMessage::error(Some(msg.id), ResponseError::server_not_initialized());
        }

        let result = match msg.method.as_str() {
            "initialize" => serde_json::from_value(msg.params)
                .map_err(ResponseError::from)
                .and_then(|params| self.handle_initialize_request(params)),
            _ => {
                todo!()
            }
        };
        result.unwrap_or_else(|e| ResponseMessage::error(Some(msg.id), e))
    }

    fn handle_notification(&mut self, msg: NotificationMessage) {
        if self.state.is_none() {
            log::warn!("Dropped a notification as the server is not initialized yet: {msg:?}");
            return;
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
        todo!()
    }
}
