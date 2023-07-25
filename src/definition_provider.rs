use crate::{
    document::EditingDocuments,
    error::ResponseError,
    message::{DefinitionParams, DocumentUri, Location, Range, ResponseMessage},
    syntax_tree::{SyntaxTree, Target},
};
use orfail::OrFail;
use std::path::PathBuf;

#[derive(Debug)]
pub struct DefinitionProvider {
    root_dir: PathBuf,
}

impl DefinitionProvider {
    pub fn new(root_dir: PathBuf) -> Self {
        Self { root_dir }
    }

    pub fn handle_request(
        &mut self,
        params: DefinitionParams,
        editing_documents: &EditingDocuments,
    ) -> Result<ResponseMessage, ResponseError> {
        let document = editing_documents
            .get(&params.text_document().uri)
            .or_fail()?;
        let position = params.position();
        let mut tree = SyntaxTree::parse(document.text.to_string()).or_fail()?;
        let efmt_position = document.text.to_efmt_position(position);
        let Some(target) = tree.find_target(efmt_position) else {
            return Err(ResponseError::request_failed().message("No definitions found"));
        };
        log::debug!("target: {target:?}");

        let location = self
            .find_definition(
                target,
                params.text_document().uri.clone(),
                editing_documents,
            )
            .or_fail()?;
        log::debug!("location: {location:?}");

        let response = ResponseMessage::result(location).or_fail()?;
        Ok(response)
    }

    fn find_definition(
        &self,
        item: Target,
        mut target_uri: DocumentUri,
        editing_documents: &EditingDocuments,
    ) -> orfail::Result<Location> {
        match &item {
            Target::Module { module_name, .. }
            | Target::Function {
                module_name: Some(module_name),
                ..
            }
            | Target::Type {
                module_name: Some(module_name),
                ..
            } => {
                target_uri = self.resolve_module_uri(module_name).or_fail()?;
            }
            _ => {}
        }

        let text = if let Some(doc) = editing_documents.get(&target_uri) {
            doc.text.to_string()
        } else {
            target_uri.read().or_fail()?
        };
        let tree = SyntaxTree::parse(text).or_fail()?;
        if let Some(range) = tree.find_definition(&item) {
            Ok(Location::new(target_uri, Range::from_efmt_range(range)))
        } else {
            // TODO: handle include
            todo!()
        }
    }

    fn resolve_module_uri(&self, module: &str) -> orfail::Result<DocumentUri> {
        let path = self.root_dir.join(format!("src/{}.erl", module));
        path.exists()
            .or_fail()
            .map_err(|f| f.message(format!("Path not found: {path:?}")))?;
        DocumentUri::from_path(path).or_fail()
    }
}
