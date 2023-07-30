use crate::{
    config::Config,
    definition_provider::DefinitionProvider,
    document::DocumentRepository,
    error::ResponseError,
    formatting_provider::FormattingProvider,
    fs::FileSystem,
    message::{
        InitializeParams, InitializeResult, Message, NotificationMessage, RequestMessage,
        ResponseMessage,
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
                "textDocument/formatting" => deserialize_params(msg.params).and_then(|params| {
                    state
                        .formatting_provider
                        .handle_request(params, &state.document_repository)
                }),
                "textDocument/definition" => deserialize_params(msg.params).and_then(|params| {
                    state
                        .definition_provider
                        .handle_request(params, &state.document_repository)
                }),
                "shutdown" => Ok(ResponseMessage::default()),
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
            outgoing_messages: Vec::new(),
            document_repository: DocumentRepository::new(),
            definition_provider: DefinitionProvider::default(),
            formatting_provider: FormattingProvider::default(),
        };

        log::info!("Client: {:?}", params.client_info);
        log::info!("Server config: {:?}", self.config.root_dir);
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
    outgoing_messages: Vec<Vec<u8>>,
    document_repository: DocumentRepository<FS>,
    definition_provider: DefinitionProvider,
    formatting_provider: FormattingProvider,
}
