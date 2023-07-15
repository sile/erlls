use efmt::{parse::TokenStream, span::Position, span::Span};
use erl_tokenize::Tokenizer;
use orfail::OrFail;

#[derive(Debug)]
pub struct SyntaxTree {
    ts: TokenStream,
    module: efmt::items::Module,
}

impl SyntaxTree {
    pub fn parse(text: String) -> orfail::Result<Self> {
        let tokenizer = Tokenizer::new(text);
        // TODO: tokenizer.set_file(path);
        let mut ts = TokenStream::new(tokenizer);
        let module: efmt::items::Module = ts.parse().or_fail()?;
        Ok(Self { ts, module })
    }

    pub fn find_rename_target(&self, position: Position) -> Option<RenameTarget> {
        // TODO: find macro call
        self.module.find_rename_target(position)
    }
}

pub trait FindRenameTarget {
    fn find_rename_target(&self, position: Position) -> Option<RenameTarget>;

    fn find_rename_target_if_contains(&self, position: Position) -> Option<RenameTarget>
    where
        Self: Span,
    {
        if self.contains(position) {
            self.find_rename_target(position)
        } else {
            None
        }
    }
}

impl FindRenameTarget for efmt::items::Module {
    fn find_rename_target(&self, position: Position) -> Option<RenameTarget> {
        for form in self.children() {
            if let Some(target) = form.find_rename_target_if_contains(position) {
                return Some(target);
            }
        }
        None
    }
}

impl FindRenameTarget for efmt::items::Form {
    fn find_rename_target(&self, position: Position) -> Option<RenameTarget> {
        match self.get() {
            efmt::items::forms::Form::Module(module) => {
                if module.module_name().contains(position) {
                    let target = RenameTarget{
                        name: module.module_name().value().to_owned(),
                        kind: RenamableItemKind::ModuleName,
                        position: module.module_name().start_position(),
                    };
                    return Some(target)
                }
            }
            _ => {}
    // Define(DefineDirective),
    // Include(IncludeDirective),
    // FunSpec(FunSpec),
    // FunDecl(FunDecl),
    // TypeDecl(TypeDecl),
    // RecordDecl(RecordDecl),
    // Export(ExportAttr),
    // Module(ModuleAttr),
    // Attr(Attr),

        }
        None
    }
}

#[derive(Debug, Clone)]
pub struct RenameTarget {
    pub name: String,
    pub kind: RenamableItemKind,
    pub position: Position,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RenamableItemKind {
    ModuleName,
    TypeName,
    FunctionName,
    MacroName,
    RecordName,
    FieldName,
    Variable,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn find_rename_target_module_name_works() -> orfail::Result<()> {
        let text = "-module(foo).";
        let tree = SyntaxTree::parse(text.to_owned()).or_fail()?;
        for i in 0..text.len() {
            if (8..=10).contains(&i) {
                let target = tree.find_rename_target(offset(i)).or_fail()?;
                assert_eq!(target.name, "foo");
                assert_eq!(target.kind, RenamableItemKind::ModuleName);
                assert_eq!(target.position.offset(), offset(8).offset());
            } else {
                assert!(tree.find_rename_target(offset(i)).is_none());
            }
        }
        Ok(())
    }

    fn offset(offset: usize) -> Position {
        Position::new(offset, 0, 0)
    }
}
