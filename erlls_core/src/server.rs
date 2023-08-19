use crate::{
    completion_provider::CompletionProvider,
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
    push_diagnostics_provider::PushDiagnosticsProvider,
    semantic_tokens_provider::SemanticTokensProvider,
};
use orfail::OrFail;
use serde::Deserialize;

#[derive(Debug)]
pub struct LanguageServer<FS> {
    initialized: bool,
    config: Config,
    outgoing_messages: Vec<Vec<u8>>,
    document_repository: DocumentRepository<FS>,
    definition_provider: DefinitionProvider,
    formatting_provider: FormattingProvider,
    completion_provider: CompletionProvider,
    semantic_tokens_provider: SemanticTokensProvider,
    push_diagnostics_provider: PushDiagnosticsProvider,
}

impl<FS: FileSystem> LanguageServer<FS> {
    pub fn new(config: Config, fs: FS) -> Self {
        Self {
            initialized: false,
            config,
            outgoing_messages: Vec::new(),
            document_repository: DocumentRepository::new(fs),
            definition_provider: DefinitionProvider,
            formatting_provider: FormattingProvider,
            completion_provider: CompletionProvider,
            semantic_tokens_provider: SemanticTokensProvider,
            push_diagnostics_provider: PushDiagnosticsProvider,
        }
    }

    pub fn fs_mut(&mut self) -> &mut FS {
        self.document_repository.fs_mut()
    }

    pub fn config(&self) -> &Config {
        &self.config
    }

    pub fn update_config(&mut self, config: Config) {
        self.config = config.clone();
        self.document_repository.update_config(config);
    }

    pub async fn handle_incoming_message(&mut self, json: Vec<u8>) {
        let msg = match serde_json::from_slice(&json) {
            Err(e) => {
                log::warn!("Invalid message: {e}");
                return;
            }
            Ok(msg) => msg,
        };
        match msg {
            Message::Request(msg) => {
                let res = self.handle_request(msg).await;
                self.push_outgoing_message(res);
            }
            Message::Notification(msg) => {
                self.handle_notification(msg);
            }
        }
    }

    pub fn take_outgoing_message(&mut self) -> Option<Vec<u8>> {
        self.outgoing_messages.pop()
    }

    fn push_outgoing_message<T: serde::Serialize>(&mut self, msg: T) {
        self.outgoing_messages.extend(serde_json::to_vec(&msg).ok());
    }

    async fn handle_request(&mut self, msg: RequestMessage) -> ResponseMessage {
        fn deserialize_params<T>(params: serde_json::Value) -> Result<T, ResponseError>
        where
            T: for<'a> Deserialize<'a>,
        {
            serde_json::from_value(params).map_err(ResponseError::from)
        }

        let result = if msg.method == "initialize" {
            deserialize_params(msg.params).and_then(|params| self.handle_initialize_request(params))
        } else if self.initialized {
            match msg.method.as_str() {
                "textDocument/formatting" => deserialize_params(msg.params).and_then(|params| {
                    self.formatting_provider
                        .handle_request(params, &self.document_repository)
                }),
                "textDocument/definition" => match deserialize_params(msg.params) {
                    Err(e) => Err(e),
                    Ok(params) => {
                        self.definition_provider
                            .handle_request(params, &mut self.document_repository)
                            .await
                    }
                },
                "textDocument/completion" => match deserialize_params(msg.params) {
                    Err(e) => Err(e),
                    Ok(params) => {
                        self.completion_provider
                            .handle_request(params, &mut self.document_repository)
                            .await
                    }
                },
                "textDocument/semanticTokens/range" => {
                    deserialize_params(msg.params).and_then(|params| {
                        self.semantic_tokens_provider
                            .handle_range_request(params, &self.document_repository)
                    })
                }
                "textDocument/semanticTokens/full" => {
                    deserialize_params(msg.params).and_then(|params| {
                        self.semantic_tokens_provider
                            .handle_full_request(params, &self.document_repository)
                    })
                }
                "shutdown" => Ok(ResponseMessage::default()),
                _ => {
                    todo!("handle_request: method={}", msg.method)
                }
            }
        } else {
            Err(ResponseError::server_not_initialized())
        };

        result.unwrap_or_else(ResponseMessage::error).id(msg.id)
    }

    fn handle_notification(&mut self, msg: NotificationMessage) {
        if !self.initialized {
            log::warn!("Dropped a notification as the server is not initialized yet: {msg:?}");
            return;
        };
        if let Err(e) = self.document_repository.handle_notification(&msg).or_fail() {
            log::warn!("Failed to handle {:?} notification: reason={e}", msg.method);
        }
        match self
            .push_diagnostics_provider
            .handle_notification(&msg, &self.document_repository)
            .or_fail()
        {
            Err(e) => {
                log::warn!("Failed to handle {:?} notification: reason={e}", msg.method);
            }
            Ok(Some(msg)) => {
                self.push_outgoing_message(msg);
            }
            Ok(None) => {}
        }
    }

    fn handle_initialize_request(
        &mut self,
        params: InitializeParams,
    ) -> Result<ResponseMessage, ResponseError> {
        let root_dir = params.root_uri.path().to_path_buf();
        self.config.root_dir = root_dir;
        self.update_config(self.config.clone());

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

        self.initialized = true;
        Ok(ResponseMessage::result(InitializeResult::new()).or_fail()?)
    }
}
