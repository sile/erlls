use orfail::OrFail;

use crate::{
    document::DocumentRepository,
    error::ResponseError,
    fs::FileSystem,
    message::{HoverParams, ResponseMessage},
};

#[derive(Debug)]
pub struct HoverProvider;

impl HoverProvider {
    pub fn handle_request<FS: FileSystem>(
        &mut self,
        params: HoverParams,
        documents: &DocumentRepository<FS>,
    ) -> Result<ResponseMessage, ResponseError> {
        todo!()
    }
}
