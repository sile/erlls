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
            efmt::items::forms::Form::Module(form) => {
                if form.module_name().contains(position) {
                    let target = RenameTarget{
                        name: form.module_name().value().to_owned(),
                        kind: RenamableItemKind::ModuleName,
                        position: form.module_name().start_position(),
                    };
                    return Some(target)
                }
            }
            efmt::items::forms::Form::TypeDecl(form) => {
                if form.type_name().contains(position) {
                    let target = RenameTarget{
                        name: form.type_name().value().to_owned(),
                        kind: RenamableItemKind::TypeName,
                        position: form.type_name().start_position(),
                    };
                    return Some(target)
                }
                for param in form.params() {
                    if param.contains(position) {
                        let target = RenameTarget{
                            name: param.value().to_owned(),
                            kind: RenamableItemKind::Variable,
                            position: param.start_position(),
                        };
                        return Some(target)
                    }
                }
                return form.type_value().find_rename_target_if_contains(position);
            }
            _ => {}
    // Define(DefineDirective),
    // Include(IncludeDirective),
    // FunSpec(FunSpec),
    // FunDecl(FunDecl),
    // RecordDecl(RecordDecl),
    // Export(ExportAttr),
    // Module(ModuleAttr),
    // Attr(Attr),

        }
        None
    }
}

impl FindRenameTarget for efmt::items::Type {
    fn find_rename_target(&self, position: Position) -> Option<RenameTarget> {
        self.children()
            .find_map(|child| child.find_rename_target_if_contains(position))
    }
}

impl FindRenameTarget for efmt::items::types::NonUnionType {
    fn find_rename_target(&self, position: Position) -> Option<RenameTarget> {
        match self {
            Self::Base(x) => x.find_rename_target_if_contains(position),
            Self::BinaryOp(x) => x.find_rename_target_if_contains(position),
        }
    }
}

impl FindRenameTarget for efmt::items::types::BaseType {
    fn find_rename_target(&self, position: Position) -> Option<RenameTarget> {
        match self {
            Self::Mfargs(x) => x.find_rename_target_if_contains(position),
            Self::List(x) => x.find_rename_target_if_contains(position),
            Self::Tuple(x) => x.find_rename_target_if_contains(position),
            Self::Map(_) => todo!(),
            Self::Record(_) => todo!(),
            Self::Function(_) => todo!(),
            Self::Parenthesized(_) => todo!(),
            Self::Annotated(_) => todo!(),
            Self::Literal(x) => x.find_rename_target_if_contains(position),
            Self::Bitstring(_) | Self::UnaryOp(_) => None,
        }
    }
}

impl FindRenameTarget for efmt::items::types::BinaryOpType {
    fn find_rename_target(&self, _position: Position) -> Option<RenameTarget> {
        // As binary op types are only applied to integer types, they are never renamed.
        None
    }
}

impl FindRenameTarget for efmt::items::types::LiteralType {
    fn find_rename_target(&self, position: Position) -> Option<RenameTarget> {
        match self {
            Self::Variable(x) => {
                if x.contains(position) {
                    let target = RenameTarget {
                        name: x.value().to_owned(),
                        kind: RenamableItemKind::Variable,
                        position: x.start_position(),
                    };
                    return Some(target);
                }
            }
            Self::Atom(_) | Self::Char(_) | Self::Integer(_) => {}
        }
        None
    }
}

impl FindRenameTarget for efmt::items::types::MfargsType {
    fn find_rename_target(&self, position: Position) -> Option<RenameTarget> {
        if let Some(module_name) = self.module_name() {
            if module_name.contains(position) {
                let target = RenameTarget {
                    name: module_name.value().to_owned(),
                    kind: RenamableItemKind::ModuleName,
                    position: module_name.start_position(),
                };
                return Some(target);
            }
        }
        if self.type_name().contains(position) {
            let target = RenameTarget {
                name: self.type_name().value().to_owned(),
                kind: RenamableItemKind::TypeName,
                position: self.type_name().start_position(),
            };
            return Some(target);
        }
        for arg in self.args() {
            if let Some(target) = arg.find_rename_target_if_contains(position) {
                return Some(target);
            }
        }
        None
    }
}

impl FindRenameTarget for efmt::items::types::TupleType {
    fn find_rename_target(&self, position: Position) -> Option<RenameTarget> {
        self.items()
            .1
            .find_map(|item| item.find_rename_target_if_contains(position))
    }
}

impl FindRenameTarget for efmt::items::types::ListType {
    fn find_rename_target(&self, position: Position) -> Option<RenameTarget> {
        self.item_type()
            .and_then(|item| item.find_rename_target_if_contains(position))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    fn find_rename_target_works() -> orfail::Result<()> {
        let text = r#"-module(foo).

-type foo() :: any().
-type bar(A) :: {A, [b:b()], c}.

-spec foo(foo:foo()) -> ok.
foo(A) ->
    B = A + 2,
    io:format("B=~p~n", [B]),
    ok.
"#;
        let tree = SyntaxTree::parse(text.to_owned()).or_fail()?;
        for i in 0..text.len() {
            match i {
                8..=10 => {
                    let target = tree.find_rename_target(offset(i)).or_fail()?;
                    assert_eq!(target.name, "foo");
                    assert_eq!(target.kind, RenamableItemKind::ModuleName);
                    assert_eq!(target.position.offset(), offset(8).offset());
                }
                21..=23 => {
                    let target = tree.find_rename_target(offset(i)).or_fail()?;
                    assert_eq!(target.name, "foo");
                    assert_eq!(target.kind, RenamableItemKind::TypeName);
                    assert_eq!(target.position.offset(), offset(21).offset());
                }
                30..=32 => {
                    let target = tree.find_rename_target(offset(i)).or_fail()?;
                    assert_eq!(target.name, "any");
                    assert_eq!(target.kind, RenamableItemKind::TypeName);
                    assert_eq!(target.position.offset(), offset(30).offset());
                }
                43..=45 => {
                    let target = tree.find_rename_target(offset(i)).or_fail()?;
                    assert_eq!(target.name, "bar");
                    assert_eq!(target.kind, RenamableItemKind::TypeName);
                    assert_eq!(target.position.offset(), offset(43).offset());
                }
                47 => {
                    let target = tree.find_rename_target(offset(i)).or_fail()?;
                    assert_eq!(target.name, "A");
                    assert_eq!(target.kind, RenamableItemKind::Variable);
                    assert_eq!(target.position.offset(), offset(47).offset());
                }
                54 => {
                    let target = tree.find_rename_target(offset(i)).or_fail()?;
                    assert_eq!(target.name, "A");
                    assert_eq!(target.kind, RenamableItemKind::Variable);
                    assert_eq!(target.position.offset(), offset(54).offset());
                }
                58 => {
                    let target = tree.find_rename_target(offset(i)).or_fail()?;
                    assert_eq!(target.name, "b");
                    assert_eq!(target.kind, RenamableItemKind::ModuleName);
                    assert_eq!(target.position.offset(), offset(58).offset());
                }
                60 => {
                    let target = tree.find_rename_target(offset(i)).or_fail()?;
                    assert_eq!(target.name, "b");
                    assert_eq!(target.kind, RenamableItemKind::TypeName);
                    assert_eq!(target.position.offset(), offset(60).offset());
                }
                _ => {
                    assert_eq!(None, tree.find_rename_target(offset(i)));
                }
            }
        }
        Ok(())
    }

    fn offset(offset: usize) -> Position {
        Position::new(offset, 0, 0)
    }
}
