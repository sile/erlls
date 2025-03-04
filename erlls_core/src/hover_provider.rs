use orfail::OrFail;

use crate::{
    document::DocumentRepository,
    error::ResponseError,
    fs::FileSystem,
    message::{Hover, HoverParams, MarkupContent, MarkupKind, ResponseMessage},
};

#[derive(Debug)]
pub struct HoverProvider;

impl HoverProvider {
    pub fn handle_request<FS: FileSystem>(
        &mut self,
        _params: HoverParams,
        _documents: &DocumentRepository<FS>,
    ) -> Result<ResponseMessage, ResponseError> {
        let hover = Hover {
            contents: MarkupContent {
                kind: MarkupKind::Markdown,
                value: "Hello, world!".to_string(),
            },
            range: None,
        };
        Ok(ResponseMessage::result(hover).or_fail()?)
    }
}
