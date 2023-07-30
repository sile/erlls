use crate::{
    config::Config,
    document::EditingDocuments,
    error::ResponseError,
    fs::FileSystem,
    message::{DefinitionParams, DocumentUri, Location, Range, ResponseMessage},
    syntax_tree::{SyntaxTree, Target},
};
use orfail::OrFail;
use std::{collections::HashSet, marker::PhantomData};

#[derive(Debug)]
pub struct DefinitionProvider<FS> {
    config: Config,
    _fs: PhantomData<FS>,
}

impl<FS: FileSystem> DefinitionProvider<FS> {
    pub fn new(config: Config) -> Self {
        Self {
            config,
            _fs: PhantomData,
        }
    }

    pub fn update_config(&mut self, config: Config) {
        self.config = config;
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
                target_uri = self.resolve_module_uri(module_name).or_fail()?;
            }
            Target::Include { include, .. } => {
                return include
                    .resolve_document_uri::<FS>(&target_uri, &self.config)
                    .map(|uri| Location::new(uri, Range::beginning()))
                    .and_then(|location| ResponseMessage::result(location).ok())
                    .ok_or_else(|| {
                        ResponseError::request_failed().message("No definitions found")
                    });
            }
            _ => {}
        }

        let location = self
            .find_definition(
                &target,
                target_uri.clone(),
                editing_documents,
                &mut HashSet::new(),
                true,
            )
            .or_else(|_| {
                self.find_definition(
                    &target,
                    target_uri,
                    editing_documents,
                    &mut HashSet::new(),
                    false,
                )
            })
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
        strict: bool,
    ) -> orfail::Result<Location> {
        let text = if let Some(doc) = editing_documents.get(&target_uri) {
            doc.text.to_string()
        } else {
            FS::read_file(&target_uri.path()).or_fail()?
        };
        let tree = SyntaxTree::parse_as_much_as_possible(text).or_fail()?;
        if let Some(range) = tree.find_definition(target, strict) {
            Ok(Location::new(target_uri, Range::from_efmt_range(range)))
        } else {
            visited.insert(target_uri.clone());

            for include in tree.collect_includes() {
                log::debug!("include: {include:?}");
                if let Some(uri) = include.resolve_document_uri::<FS>(&target_uri, &self.config) {
                    if visited.contains(&uri) {
                        continue;
                    }
                    if let Ok(location) =
                        self.find_definition(target, uri, editing_documents, visited, strict)
                    {
                        return Ok(location);
                    }
                }
            }

            Err(orfail::Failure::new().message("No definitions found"))
        }
    }

    fn resolve_module_uri(&self, module: &str) -> orfail::Result<DocumentUri> {
        let path = self.config.root_dir.join(format!("src/{}.erl", module));
        if FS::exists(&path) {
            return DocumentUri::from_path(&self.config.root_dir, path).or_fail();
        }

        let path = self.config.root_dir.join(format!("test/{}.erl", module));
        if FS::exists(&path) {
            return DocumentUri::from_path(&self.config.root_dir, path).or_fail();
        }

        for lib_dir in &self.config.erl_libs {
            for app_dir in FS::read_sub_dirs(lib_dir).ok().into_iter().flatten() {
                let path = app_dir.join(format!("src/{}.erl", module));
                if FS::exists(&path) {
                    return DocumentUri::from_path(&self.config.root_dir, path).or_fail();
                }
            }
        }

        Err(orfail::Failure::new().message(format!("Module not found: {module:?}")))
    }
}
