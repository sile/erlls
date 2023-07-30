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
    _fs: std::marker::PhantomData<FS>,
}

impl<FS: FileSystem> DocumentRepository<FS> {
    pub fn new() -> Self {
        Self {
            config: Config::default(),
            editings: HashMap::new(),
            _fs: std::marker::PhantomData,
        }
    }

    pub fn update_config(&mut self, config: Config) {
        self.config = config;
    }

    pub fn get_from_editings(&self, uri: &DocumentUri) -> Option<&Document> {
        self.editings.get(uri)
    }

    pub fn get_or_read_text(&self, uri: &DocumentUri) -> orfail::Result<String> {
        if let Some(doc) = self.editings.get(uri) {
            Ok(doc.text.to_string())
        } else {
            FS::read_file(&uri.path()).or_fail()
        }
    }

    pub fn resolve_include_uri(
        &self,
        current_uri: &DocumentUri,
        include: &Include,
    ) -> Option<DocumentUri> {
        let current = current_uri.path().to_path_buf();
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
                for app_dir in FS::read_sub_dirs(&lib_dir).ok().into_iter().flatten() {
                    let app_name = app_dir
                        .file_name()
                        .and_then(|name| name.to_str())
                        .and_then(|namd_and_version| namd_and_version.splitn(2, '-').next());
                    if app_name != Some(target_app_name) {
                        continue;
                    }

                    let path = app_dir.join(include_path_components.as_path());
                    if FS::exists(&path) {
                        return DocumentUri::from_path(&self.config.root_dir, path).ok();
                    }
                }
            }
        } else {
            let path = current.parent()?.join(&include.path);
            if FS::exists(&path) {
                return DocumentUri::from_path(&self.config.root_dir, path).ok();
            }

            let path = current
                .parent()?
                .parent()?
                .join("include")
                .join(&include.path);
            if FS::exists(&path) {
                return DocumentUri::from_path(&self.config.root_dir, path).ok();
            }
        }
        None
    }

    pub fn resolve_module_uri(&self, module_name: &str) -> orfail::Result<DocumentUri> {
        let path = self
            .config
            .root_dir
            .join(format!("src/{}.erl", module_name));
        if FS::exists(&path) {
            return DocumentUri::from_path(&self.config.root_dir, path).or_fail();
        }

        let path = self
            .config
            .root_dir
            .join(format!("test/{}.erl", module_name));
        if FS::exists(&path) {
            return DocumentUri::from_path(&self.config.root_dir, path).or_fail();
        }

        for lib_dir in &self.config.erl_libs {
            for app_dir in FS::read_sub_dirs(lib_dir).ok().into_iter().flatten() {
                let path = app_dir.join(format!("src/{}.erl", module_name));
                if FS::exists(&path) {
                    return DocumentUri::from_path(&self.config.root_dir, path).or_fail();
                }
            }
        }

        Err(orfail::Failure::new().message(format!("Module not found: {module_name:?}")))
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
        let lines = text.split("\n").map(|s| s.to_string()).collect();
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

        let mut lines = text.split("\n");
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
            .or_fail()
            .map_err(|f| {
                f.message(format!(
                    "position={position:?}, text_range_end={:?}",
                    self.range().end
                ))
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

    pub fn to_string(&self) -> String {
        self.lines.join("\n")
    }

    pub fn range(&self) -> Range {
        Range::new(Position::new(0, 0), Position::new(self.lines.len(), 0))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::message::{Position, Range};

    #[test]
    fn apply_change_work() -> orfail::Result<()> {
        let mut text = Text::new("abc\ndef\nghi");
        assert_eq!(text.to_string(), "abc\ndef\nghi");

        text.apply_change(Range::new(Position::new(0, 1), Position::new(0, 2)), "xyz")
            .or_fail()?;
        assert_eq!(text.to_string(), "axyzc\ndef\nghi");

        text.apply_change(Range::new(Position::new(0, 1), Position::new(2, 2)), "123")
            .or_fail()?;
        assert_eq!(text.to_string(), "a123i");

        let mut text = Text::new("111\n222\n333\n444\n555\n666");
        text.apply_change(Range::new(Position::new(1, 1), Position::new(5, 0)), "")
            .or_fail()?;
        assert_eq!(text.to_string(), "111\n2666");

        Ok(())
    }
}
