use crate::{
    error::ResponseError,
    message::{InitializeParams, Message, NotificationMessage, RequestMessage, ResponseMessage},
};

#[derive(Debug)]
pub struct LanguageServer {
    initialized: bool,
}

impl LanguageServer {
    pub fn new() -> Self {
        Self { initialized: false }
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
        if msg.method != "initialized" && !self.initialized {
            todo!();
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
        if !self.initialized {
            log::warn!("Dropped a notification as the server is not initialized yet: {msg:?}");
            return;
        }
    }

    fn handle_response(&mut self, _msg: ResponseMessage) {}

    fn handle_initialize_request(
        &mut self,
        params: InitializeParams,
    ) -> Result<ResponseMessage, ResponseError> {
        self.initialized = true;
        todo!()
    }
}
