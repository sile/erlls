use efmt::parse::TokenStream;
use erl_tokenize::Tokenizer;
use orfail::OrFail;

use crate::message::Position;

#[derive(Debug)]
pub struct SyntaxTree {}

impl SyntaxTree {
    pub fn parse(text: String) -> orfail::Result<Self> {
        let tokenizer = Tokenizer::new(text);
        // TODO: tokenizer.set_file(path);
        let mut ts = TokenStream::new(tokenizer);
        let module: efmt::items::Module = ts.parse().or_fail()?;
        Ok(Self {})
    }

    pub fn find_rename_target(&self, position: Position) -> Option<RenameTarget> {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct RenameTarget {
    pub name: String,
    pub kind: RenamableItemKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RenamableItemKind {
    ModuleName,
    TypeName,
    FunctionName,
    MacroName,
    RecordName,
    Variable,
}
