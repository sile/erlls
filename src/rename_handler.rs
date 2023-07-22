use crate::{
    document::EditingDocuments,
    error::ResponseError,
    message::{RenameParams, ResponseMessage},
    syntax_tree::SyntaxTree,
};
use orfail::OrFail;
use std::path::PathBuf;

#[derive(Debug)]
pub struct RenameHandler {
    root_dir: PathBuf,
}

impl RenameHandler {
    pub fn new(root_dir: PathBuf) -> Self {
        Self { root_dir }
    }

    pub fn handle(
        &mut self,
        params: RenameParams,
        editing_documents: &EditingDocuments,
    ) -> Result<ResponseMessage, ResponseError> {
        let document = editing_documents
            .get(&params.text_document_position.text_document.uri)
            .or_fail()?;
        let position = params.text_document_position.position;
        let new_name = params.new_name;
        let mut tree = SyntaxTree::parse(document.text.to_string()).or_fail()?;
        let efmt_position = document.text.to_efmt_position(position);
        let Some(target) = tree.find_target(efmt_position)  else {
            return Err(ResponseError::request_failed().message("No rename target found"));
        };
        todo!()
    }
}
