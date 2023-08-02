use crate::{
    document::DocumentRepository,
    error::ResponseError,
    fs::FileSystem,
    message::{ResponseMessage, SemanticTokensRangeParams},
};

#[derive(Debug)]
pub struct SemanticTokensProvider;

impl SemanticTokensProvider {
    pub fn handle_range_request<FS: FileSystem>(
        &mut self,
        params: SemanticTokensRangeParams,
        documents: &mut DocumentRepository<FS>,
    ) -> Result<ResponseMessage, ResponseError> {
        todo!("{:?}", params)
    }
}
