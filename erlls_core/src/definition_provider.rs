use crate::{
    document::DocumentRepository,
    error::ResponseError,
    fs::FileSystem,
    message::{DefinitionParams, DocumentUri, Location, Range, ResponseMessage},
    syntax_tree::{SyntaxTree, Target},
};
use orfail::OrFail;
use std::collections::HashSet;

#[derive(Debug)]
pub struct DefinitionProvider;

impl DefinitionProvider {
    pub async fn handle_request<FS: FileSystem>(
        &mut self,
        params: DefinitionParams,
        documents: &mut DocumentRepository<FS>,
    ) -> Result<ResponseMessage, ResponseError> {
        let document = documents
            .get_from_editings(&params.text_document().uri)
            .or_fail()?;
        let position = params.position();
        let efmt_position = document.text.to_efmt_position(position);
        let mut tree = SyntaxTree::parse(document.text.to_string())
            .or_else(|err| {
                SyntaxTree::partial_parse(document.text.to_string(), efmt_position).map_err(|_| err)
            })
            .or_fail()?;
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
                target_uri = documents.resolve_module_uri(module_name).await.or_fail()?;
            }
            Target::Include { include, .. } => {
                return documents
                    .resolve_include_uri(&target_uri, include)
                    .await
                    .map(|uri| Location::new(uri, Range::beginning()))
                    .and_then(|location| ResponseMessage::result(location).ok())
                    .ok_or_else(|| {
                        ResponseError::request_failed().message("No definitions found")
                    });
            }
            _ => {}
        }

        let result = Self::find_definition(&target, target_uri.clone(), documents, true).await;
        let location = if let Ok(location) = result {
            location
        } else {
            Self::find_definition(&target, target_uri, documents, false)
                .await
                .or_fail()?
        };
        log::debug!("location: {location:?}");

        let response = ResponseMessage::result(location).or_fail()?;
        Ok(response)
    }

    async fn find_definition<FS: FileSystem>(
        target: &Target,
        target_uri: DocumentUri, // TODO: rename
        documents: &mut DocumentRepository<FS>,
        strict: bool,
    ) -> orfail::Result<Location> {
        let mut visited: HashSet<DocumentUri> = HashSet::new();
        visited.insert(target_uri.clone());

        let mut stack = vec![target_uri.clone()];
        while let Some(target_uri) = stack.pop() {
            let text = documents.get_or_read_text(&target_uri).await.or_fail()?;
            let tree = SyntaxTree::parse_as_much_as_possible(text).or_fail()?;
            if let Some(range) = tree.find_definition(target, strict) {
                return Ok(Location::new(
                    target_uri,
                    Range::from_efmt_range(&tree.text(), range),
                ));
            } else {
                visited.insert(target_uri.clone());

                for include in tree.collect_includes() {
                    log::debug!("include: {include:?}");
                    if let Some(uri) = documents.resolve_include_uri(&target_uri, &include).await {
                        if visited.contains(&uri) {
                            continue;
                        }
                        stack.push(uri);
                    }
                }
            }
        }

        Err(orfail::Failure::new("No definitions found"))
    }
}
