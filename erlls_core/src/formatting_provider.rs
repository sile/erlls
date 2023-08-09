use orfail::OrFail;

use crate::{
    document::DocumentRepository,
    error::ResponseError,
    fs::FileSystem,
    message::{DocumentFormattingParams, ResponseMessage, TextEdit},
};

#[derive(Debug)]
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
        Ok(ResponseMessage::result(edits).or_fail()?)
    }
}
