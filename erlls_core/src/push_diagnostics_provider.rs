use crate::{
    document::DocumentRepository,
    fs::FileSystem,
    message::{
        Diagnostic, DiagnosticSeverity, NotificationMessage, PublishDiagnosticsParams, Range,
        TextDocumentIdentifier,
    },
};
use efmt_core::items::ModuleOrConfig;
use orfail::OrFail;
use serde::{Deserialize, Serialize};

#[derive(Debug)]
pub struct PushDiagnosticsProvider;

impl PushDiagnosticsProvider {
    pub fn handle_notification<FS: FileSystem>(
        &mut self,
        msg: &NotificationMessage,
        documents: &DocumentRepository<FS>,
    ) -> orfail::Result<Option<NotificationMessage>> {
        match msg.method.as_str() {
            "textDocument/didOpen" | "textDocument/didSave" => {
                let params = serde_json::from_value(msg.params.clone()).or_fail()?;
                self.handle_did_open_or_save(params, documents)
                    .or_fail()
                    .map(Some)
            }
            _ => Ok(None),
        }
    }

    fn handle_did_open_or_save<FS: FileSystem>(
        &mut self,
        params: TextDocumentParams,
        documents: &DocumentRepository<FS>,
    ) -> orfail::Result<NotificationMessage> {
        let doc = documents
            .get_from_editings(&params.text_document.uri)
            .or_fail()?;
        let text = doc.text.to_string();
        let mut diagnostics = PublishDiagnosticsParams {
            uri: params.text_document.uri.clone(),
            diagnostics: vec![],
            version: doc.version,
        };
        if let Err(e) = efmt_core::format_text::<ModuleOrConfig>(&text) {
            let message = match e {
                efmt_core::parse::Error::UnexpectedEof { .. } => "parse failed: unexpected eof",
                efmt_core::parse::Error::UnexpectedToken { .. } => "parse failed: unexpected token",
                efmt_core::parse::Error::TokenizeError { .. } => "parse failed: tokenize error",
            };
            let diagnostic = Diagnostic {
                range: Range::from_parse_error(&e),
                message: message.to_owned(),
                severity: Some(DiagnosticSeverity::ERROR),
                source: "efmt".to_owned(),
            };
            diagnostics.diagnostics.push(diagnostic);
        }
        let notification =
            NotificationMessage::new("textDocument/publishDiagnostics", diagnostics).or_fail()?;
        Ok(notification)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct TextDocumentParams {
    text_document: TextDocumentIdentifier,
}
