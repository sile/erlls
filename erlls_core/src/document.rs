use crate::message::{DocumentUri, Position, Range};
use orfail::OrFail;
use std::collections::HashMap;

#[derive(Debug, Default, Clone)]
pub struct EditingDocuments(HashMap<DocumentUri, Document>);

impl EditingDocuments {
    pub fn get(&self, uri: &DocumentUri) -> Option<&Document> {
        self.0.get(uri)
    }

    pub fn get_mut(&mut self, uri: &DocumentUri) -> Option<&mut Document> {
        self.0.get_mut(uri)
    }

    pub fn insert(&mut self, uri: DocumentUri, document: Document) {
        self.0.insert(uri, document);
    }

    pub fn remove(&mut self, uri: &DocumentUri) {
        self.0.remove(uri);
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
        // TODO: conssider newline character
        let lines = text.split("\n").map(|s| s.to_string()).collect();
        Self { lines }
    }

    pub fn to_efmt_position(&self, position: Position) -> efmt_core::span::Position {
        let mut offset = 0;
        for line in self.lines.iter().take(position.line) {
            offset += line.len() + 1;
        }
        offset += position.character;
        efmt_core::span::Position::new(offset, position.line + 1, position.character + 1)
    }

    pub fn apply_change(&mut self, mut range: Range, text: &str) -> orfail::Result<()> {
        log::info!(
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

        let mut lines = text.split("\n"); // TODO: consider newline character
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

    fn utf8_column_offset(&self, position: Position) -> orfail::Result<usize> {
        if position.character == 0 {
            return Ok(0);
        }

        Ok(self
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
            .take(position.character)
            .map(|c| c.len_utf8())
            .sum())
    }

    pub fn to_string(&self) -> String {
        // TODO: conssider newline character
        self.lines.join("\n")
    }

    pub fn range(&self) -> Range {
        Range::new(Position::new(0, 0), Position::new(self.lines.len(), 0))
    }
}
