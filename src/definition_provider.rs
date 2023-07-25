use crate::{
    document::EditingDocuments,
    error::ResponseError,
    message::{DefinitionParams, DocumentUri, Location, Range, ResponseMessage},
    syntax_tree::{SyntaxTree, Target},
};
use orfail::OrFail;
use std::{collections::HashSet, path::PathBuf};

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

        // TODO: rename
        let mut target_uri = params.text_document().uri.clone();
        match &target {
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

        let location = self
            .find_definition(&target, target_uri, editing_documents, &mut HashSet::new())
            .or_fail()?;
        log::debug!("location: {location:?}");

        let response = ResponseMessage::result(location).or_fail()?;
        Ok(response)
    }

    fn find_definition(
        &self,
        target: &Target,
        target_uri: DocumentUri, // TODO: rename
        editing_documents: &EditingDocuments,
        visited: &mut HashSet<DocumentUri>,
    ) -> orfail::Result<Location> {
        let text = if let Some(doc) = editing_documents.get(&target_uri) {
            doc.text.to_string()
        } else {
            target_uri.read().or_fail()?
        };
        let tree = SyntaxTree::parse(text).or_fail()?;
        if let Some(range) = tree.find_definition(target) {
            Ok(Location::new(target_uri, Range::from_efmt_range(range)))
        } else {
            visited.insert(target_uri.clone());

            for include in tree.collect_includes() {
                log::debug!("include: {include:?}");
                if let Some(uri) = include.resolve_document_uri(&target_uri) {
                    if visited.contains(&uri) {
                        continue;
                    }
                    if let Ok(location) =
                        self.find_definition(target, uri, editing_documents, visited)
                    {
                        return Ok(location);
                    }
                }
            }

            Err(orfail::Failure::new().message("No definitions found"))
        }
    }

    fn resolve_module_uri(&self, module: &str) -> orfail::Result<DocumentUri> {
        // TODO: ERL_LIBS
        // TODO: Find non src/ dirs (e.g., tests/)
        let path = self.root_dir.join(format!("src/{}.erl", module));
        path.exists()
            .or_fail()
            .map_err(|f| f.message(format!("Path not found: {path:?}")))?;
        DocumentUri::from_path(path).or_fail()
    }
}
