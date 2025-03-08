use std::collections::HashSet;

use orfail::OrFail;

use crate::{
    document::DocumentRepository,
    error::ResponseError,
    fs::FileSystem,
    message::{DocumentUri, Hover, HoverParams, MarkupContent, MarkupKind, ResponseMessage},
    syntax_tree::{SyntaxTree, Target},
};

#[derive(Debug)]
pub struct HoverProvider;

impl HoverProvider {
    pub async fn handle_request<FS: FileSystem>(
        &mut self,
        params: HoverParams,
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
            _ => {}
        }

        let doc = Self::find_hover_doc(&target, target_uri.clone(), documents)
            .await
            .or_fail()?;
        log::debug!("hover: {doc:?}");

        let hover = Hover {
            contents: MarkupContent {
                kind: MarkupKind::Markdown,
                value: doc,
            },
            range: None,
        };
        Ok(ResponseMessage::result(hover).or_fail()?)
    }

    async fn find_hover_doc<FS: FileSystem>(
        target: &Target,
        target_uri: DocumentUri, // TODO: rename
        documents: &mut DocumentRepository<FS>,
    ) -> orfail::Result<String> {
        let mut visited: HashSet<DocumentUri> = HashSet::new();
        visited.insert(target_uri.clone());

        let mut stack = vec![target_uri.clone()];
        while let Some(target_uri) = stack.pop() {
            let text = documents.get_or_read_text(&target_uri).await.or_fail()?;
            let tree = SyntaxTree::parse_as_much_as_possible(text).or_fail()?;
            if let Some(doc) = tree.find_hover_doc(target) {
                if doc.is_empty() {
                    break;
                }
                return Ok(doc);
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

        Err(orfail::Failure::new("No doc found"))
    }
}
