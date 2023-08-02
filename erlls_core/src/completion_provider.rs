use orfail::OrFail;

use crate::{
    document::DocumentRepository,
    error::ResponseError,
    fs::FileSystem,
    message::{CompletionItem, CompletionItemKind, CompletionParams, ResponseMessage},
    syntax_tree::SyntaxTree,
};

#[derive(Debug)]
pub struct CompletionProvider;

impl CompletionProvider {
    pub fn handle_request<FS: FileSystem>(
        &mut self,
        params: CompletionParams,
        documents: &DocumentRepository<FS>,
    ) -> Result<ResponseMessage, ResponseError> {
        let document = documents
            .get_from_editings(&params.text_document().uri)
            .or_fail()?;
        let position = params.position();
        let efmt_position = document.text.to_efmt_position(position);
        let Some((module, function)) =
            SyntaxTree::parse_partial_funcall(document.text.to_string(), efmt_position)
        else {
            return Ok(ResponseMessage::null_result());
        };
        log::debug!("target: {module:?}, {function:?}");

        let Ok(module_uri) = documents.resolve_module_uri(&module) else {
            return Ok(ResponseMessage::null_result());
        };
        log::debug!("module_uri: {module_uri:?}");

        let text = documents.get_or_read_text(&module_uri).or_fail()?;
        let Ok(tree) = SyntaxTree::parse_as_much_as_possible(text) else {
            return Ok(ResponseMessage::null_result());
        };

        let mut completions = Vec::new();
        for export in tree.iter_exports() {
            if export
                .name
                .chars()
                .next()
                .map_or(true, |c| !c.is_ascii_alphabetic())
                || function
                    .as_ref()
                    .map_or(false, |x| !export.name.starts_with(x))
            {
                continue;
            }

            let kind = if export.is_type {
                CompletionItemKind::TYPE_PARAMETER
            } else {
                CompletionItemKind::FUNCTION
            };
            completions.push(CompletionItem {
                label: export.name,
                kind,
            });
        }
        completions.sort_by(|a, b| (a.kind, &a.label).cmp(&(b.kind, &b.label)));
        completions.dedup_by(|a, b| a.label == b.label);

        log::debug!("completions: {completions:?}");

        Ok(ResponseMessage::result(completions).or_fail()?)
    }
}
