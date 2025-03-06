use orfail::OrFail;

use crate::{
    document::DocumentRepository,
    error::ResponseError,
    fs::FileSystem,
    message::{Hover, HoverParams, MarkupContent, MarkupKind, ResponseMessage},
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
