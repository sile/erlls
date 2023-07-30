use orfail::OrFail;

use crate::{
    document::DocumentRepository,
    error::ResponseError,
    fs::FileSystem,
    message::{DocumentFormattingParams, ResponseMessage, TextEdit},
};

#[derive(Debug, Default)]
pub struct FormattingProvider;

impl FormattingProvider {
    pub fn handle_request<FS: FileSystem>(
        &mut self,
        params: DocumentFormattingParams,
        documents: &DocumentRepository<FS>,
    ) -> Result<ResponseMessage, ResponseError> {
        let doc = documents
            .get_from_editings(&params.text_document.uri)
            .or_fail()?;
        let text = doc.text.to_string();
        let new_text = match efmt_core::format_text::<efmt_core::items::ModuleOrConfig>(&text) {
            Err(_e) => {
                // TODO: suppport diagnostics
                // // TODO: check client capabilities (e.g., "publishDiagnostics":{"relatedInformation":true,"tagSupport":{"valueSet":[1,2]},"versionSupport":true})
                // let diagnostic = Diagnostic {
                //     range: Range::from_parse_error(&e),
                //     message: e.to_string(),
                //     severity: Some(DiagnosticSeverity::ERROR),
                // };
                // let params = PublishDiagnosticsParams {
                //     uri: params.text_document.uri.clone(),
                //     diagnostics: vec![diagnostic],
                //     version: doc.version,
                // };
                // let notification =
                //     NotificationMessage::new("textDocument/publishDiagnostics", params)
                //         .or_fail()?;
                // self.outgoing_messages
                //     .push(serde_json::to_vec(&notification).or_fail()?);
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
