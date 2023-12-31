use crate::{
    config::Config,
    fs::FileSystem,
    message::{
        DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
        DocumentUri, NotificationMessage, Position, Range,
    },
    syntax_tree::Include,
};
use orfail::OrFail;
use std::{collections::HashMap, path::Component};

#[derive(Debug)]
pub struct DocumentRepository<FS> {
    config: Config,
    editings: HashMap<DocumentUri, Document>,
    module_uri_cache: HashMap<String, DocumentUri>,
    fs: FS,
}

impl<FS: FileSystem> DocumentRepository<FS> {
    pub fn new(fs: FS) -> Self {
        Self {
            config: Config::default(),
            editings: HashMap::new(),
            module_uri_cache: HashMap::new(),
            fs,
        }
    }

    pub fn fs_mut(&mut self) -> &mut FS {
        &mut self.fs
    }

    pub fn update_config(&mut self, config: Config) {
        self.config = config;
    }

    pub fn get_from_editings(&self, uri: &DocumentUri) -> Option<&Document> {
        self.editings.get(uri)
    }

    pub async fn get_or_read_text(&mut self, uri: &DocumentUri) -> orfail::Result<String> {
        if let Some(doc) = self.editings.get(uri) {
            Ok(doc.text.to_string())
        } else {
            self.fs.read_file(uri).await.or_fail()
        }
    }

    pub async fn resolve_include_uri(
        &mut self,
        current_uri: &DocumentUri,
        include: &Include,
    ) -> Option<DocumentUri> {
        let parent = current_uri.parent().ok()?;
        let grand_parent = parent.parent().ok()?;

        if include.is_lib {
            let mut include_path_components = include.path.components();
            let target_app_name = include_path_components.next().and_then(|x| {
                if let Component::Normal(x) = x {
                    x.to_str()
                } else {
                    None
                }
            })?;
            for lib_dir in &self.config.erl_libs {
                let lib_uri = self.config.root_uri.join(lib_dir.to_str()?).ok()?;
                for app_dir_uri in self.fs.read_sub_dirs(&lib_uri).await {
                    let app_name = app_dir_uri
                        .path()
                        .file_name()
                        .and_then(|name| name.to_str())
                        .and_then(|namd_and_version| namd_and_version.split('-').next());
                    if app_name != Some(target_app_name) {
                        continue;
                    }

                    let uri = app_dir_uri
                        .join(include_path_components.as_path().to_str()?)
                        .ok()?;
                    if self.fs.exists(&uri).await {
                        return Some(uri);
                    }
                }
            }
        } else {
            let uri = parent.join(include.path.to_str()?).ok()?;
            if self.fs.exists(&uri).await {
                return Some(uri);
            }

            let uri = grand_parent
                .join("include")
                .ok()?
                .join(include.path.to_str()?)
                .ok()?;
            if self.fs.exists(&uri).await {
                return Some(uri);
            }
        }
        None
    }

    pub async fn resolve_module_uri(&mut self, module_name: &str) -> orfail::Result<DocumentUri> {
        if let Some(uri) = self.module_uri_cache.get(module_name) {
            log::debug!("Module URI cache hit: module={}, uri={}", module_name, uri);
            return Ok(uri.clone());
        }

        let uri = self
            .config
            .root_uri
            .join(&format!("src/{}.erl", module_name))
            .or_fail()?;
        if self.fs.exists(&uri).await {
            self.module_uri_cache
                .insert(module_name.to_string(), uri.clone());
            return Ok(uri);
        }

        let uri = self
            .config
            .root_uri
            .join(&format!("test/{}.erl", module_name))
            .or_fail()?;
        if self.fs.exists(&uri).await {
            self.module_uri_cache
                .insert(module_name.to_string(), uri.clone());
            return Ok(uri);
        }

        for lib_dir in &self.config.erl_libs {
            let lib_uri = self
                .config
                .root_uri
                .join(lib_dir.to_str().or_fail()?)
                .or_fail()?;
            for app_dir_uri in self.fs.read_sub_dirs(&lib_uri).await {
                let uri = app_dir_uri
                    .join(&format!("src/{}.erl", module_name))
                    .or_fail()?;
                if self.fs.exists(&uri).await {
                    self.module_uri_cache
                        .insert(module_name.to_string(), uri.clone());
                    return Ok(uri);
                }
            }
        }

        Err(orfail::Failure::new(format!(
            "Module not found: {module_name:?}"
        )))
    }

    pub fn handle_notification(&mut self, msg: &NotificationMessage) -> orfail::Result<()> {
        match msg.method.as_str() {
            "textDocument/didOpen" => {
                let params = serde_json::from_value(msg.params.clone()).or_fail()?;
                self.handle_did_open(params).or_fail()?;
            }
            "textDocument/didClose" => {
                let params = serde_json::from_value(msg.params.clone()).or_fail()?;
                self.handle_did_close(params).or_fail()?;
            }
            "textDocument/didChange" => {
                let params = serde_json::from_value(msg.params.clone()).or_fail()?;
                self.handle_did_change(params).or_fail()?;
            }
            _ => {}
        }
        Ok(())
    }

    fn handle_did_open(&mut self, params: DidOpenTextDocumentParams) -> orfail::Result<()> {
        if !params.text_document.is_erlang() {
            log::warn!(
                "Unsupported language: lang={}, uri={}",
                params.text_document.language_id,
                params.text_document.uri
            );
            return Ok(());
        }

        self.editings.insert(
            params.text_document.uri,
            Document {
                version: Some(params.text_document.version),
                text: Text::new(&params.text_document.text),
            },
        );

        Ok(())
    }

    fn handle_did_close(&mut self, params: DidCloseTextDocumentParams) -> orfail::Result<()> {
        self.editings.remove(&params.text_document.uri);
        Ok(())
    }

    fn handle_did_change(&mut self, params: DidChangeTextDocumentParams) -> orfail::Result<()> {
        let doc = self.editings.get_mut(&params.text_document.uri).or_fail()?;
        doc.version = Some(params.text_document.version);

        for change in params.content_changes {
            if let Some(range) = change.range {
                doc.text.apply_change(range, &change.text).or_fail()?;
            } else {
                doc.text = Text::new(&change.text);
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Document {
    pub version: Option<i32>,
    pub text: Text,
}

#[derive(Debug, Clone)]
pub struct Text {
    pub lines: Vec<String>,
}

impl Text {
    pub fn new(text: &str) -> Self {
        let lines = text.split('\n').map(|s| s.to_string()).collect();
        Self { lines }
    }

    pub fn to_efmt_position(&self, mut position: Position) -> efmt_core::span::Position {
        let mut offset = 0;
        let mut last_line = None;
        for line in self.lines.iter().take(position.line) {
            offset += line.len() + 1;
            last_line = Some(line);
        }

        for (i, c) in last_line
            .into_iter()
            .flat_map(|line| line.chars())
            .enumerate()
        {
            if c.len_utf16() == 2 {
                position.character -= 1;
            }
            if i == position.character {
                break;
            }
        }
        offset += position.character;

        efmt_core::span::Position::new(offset, position.line + 1, position.character + 1)
    }

    pub fn apply_change(&mut self, mut range: Range, text: &str) -> orfail::Result<()> {
        log::debug!(
            "Apply change: range={:?}, lines={:?}",
            range,
            self.lines.len()
        );

        // TODO: optimize (use drain)
        while range.start.line + 1 < range.end.line {
            self.lines.remove(range.start.line + 1);
            range.end.line -= 1;
        }

        let start_offset = self.utf8_column_offset(range.start).or_fail()?;
        let end_offset = self.utf8_column_offset(range.end).or_fail()?;
        let mut trailing = String::new();
        if range.start.line == range.end.line {
            let line = self.lines.get_mut(range.start.line).or_fail()?;
            trailing = line.split_off(end_offset);
            line.truncate(start_offset);
        } else {
            let line = self.lines.get_mut(range.start.line).or_fail()?;
            line.truncate(start_offset);

            if let Some(line) = self.lines.get_mut(range.end.line) {
                trailing = line.split_off(end_offset);
                self.lines.remove(range.end.line);
            }
        };

        let mut lines = text.split('\n');
        if let Some(line) = lines.next() {
            self.lines
                .get_mut(range.start.line)
                .or_fail()?
                .push_str(line);
        }

        let before_lines = self.lines.len();
        self.lines.splice(
            range.start.line + 1..range.start.line + 1,
            lines.map(|s| s.to_string()),
        );
        let end_line = range.start.line + self.lines.len() - before_lines;

        self.lines.get_mut(end_line).or_fail()?.push_str(&trailing);

        Ok(())
    }

    fn utf8_column_offset(&self, mut position: Position) -> orfail::Result<usize> {
        if position.character == 0 {
            return Ok(0);
        }

        let mut offset = 0;
        for (i, c) in self
            .lines
            .get(position.line)
            .or_fail_with(|()| {
                format!(
                    "position={position:?}, text_range_end={:?}",
                    self.range().end
                )
            })?
            .chars()
            .enumerate()
        {
            if c.len_utf16() == 2 {
                position.character -= 1;
            }
            if i == position.character {
                break;
            }
            offset += c.len_utf8();
        }
        Ok(offset)
    }

    pub fn range(&self) -> Range {
        Range::new(Position::new(0, 0), Position::new(self.lines.len(), 0))
    }

    /// Returns the string of the specified range.
    ///
    /// Note that this method only considers the line number of the range.
    pub fn to_range_string(&self, range: Range) -> String {
        let mut s = String::new();
        for line in self.lines[range.start.line..range.end.line].iter() {
            s.push_str(line);
            s.push('\n');
        }
        s
    }
}

impl std::fmt::Display for Text {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for line in self.lines.iter() {
            writeln!(f, "{}", line)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::message::{Position, Range};

    #[test]
    fn apply_change_work() -> orfail::Result<()> {
        let mut text = Text::new("abc\ndef\nghi");
        assert_eq!(text.to_string(), "abc\ndef\nghi\n");

        text.apply_change(Range::new(Position::new(0, 1), Position::new(0, 2)), "xyz")
            .or_fail()?;
        assert_eq!(text.to_string(), "axyzc\ndef\nghi\n");

        text.apply_change(Range::new(Position::new(0, 1), Position::new(2, 2)), "123")
            .or_fail()?;
        assert_eq!(text.to_string(), "a123i\n");

        let mut text = Text::new("111\n222\n333\n444\n555\n666");
        text.apply_change(Range::new(Position::new(1, 1), Position::new(5, 0)), "")
            .or_fail()?;
        assert_eq!(text.to_string(), "111\n2666\n");

        Ok(())
    }
}
