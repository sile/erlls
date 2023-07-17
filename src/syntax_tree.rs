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
            if let Some(target) = form.get().find_rename_target_if_contains(position) {
                return Some(target);
            }
        }
        None
    }
}

impl FindRenameTarget for efmt::items::forms::Form {
    fn find_rename_target(&self, position: Position) -> Option<RenameTarget> {
        match self {
            Self::Module(x) => x.find_rename_target_if_contains(position),
            Self::TypeDecl(x) => x.find_rename_target_if_contains(position),
            Self::FunSpec(x) => x.find_rename_target_if_contains(position),
            Self::FunDecl(x) => x.find_rename_target_if_contains(position),
            Self::Include(_) | Self::Attr(_) => None,
            _ => todo!(),
        }

        // Define(DefineDirective),
        // RecordDecl(RecordDecl),
        // Export(ExportAttr),
    }
}

impl FindRenameTarget for efmt::items::forms::FunDecl {
    fn find_rename_target(&self, position: Position) -> Option<RenameTarget> {
        for clause in self.clauses() {
            if clause.function_name().contains(position) {
                let target = RenameTarget {
                    name: clause.function_name().value().to_owned(),
                    kind: RenamableItemKind::FunctionName,
                    position: clause.function_name().start_position(),
                };
                return Some(target);
            }
            for expr in clause.children() {
                if let Some(target) = expr.find_rename_target_if_contains(position) {
                    return Some(target);
                }
            }
        }
        None
    }
}

impl FindRenameTarget for efmt::items::forms::FunSpec {
    fn find_rename_target(&self, position: Position) -> Option<RenameTarget> {
        if let Some(name) = self.module_name() {
            if name.contains(position) {
                let target = RenameTarget {
                    name: name.value().to_owned(),
                    kind: RenamableItemKind::ModuleName,
                    position: name.start_position(),
                };
                return Some(target);
            }
        }
        if self.function_name().contains(position) {
            let target = RenameTarget {
                name: self.function_name().value().to_owned(),
                kind: RenamableItemKind::FunctionName,
                position: self.function_name().start_position(),
            };
            return Some(target);
        }
        for ty in self.clauses().flat_map(|x| {
            x.params()
                .iter()
                .chain(std::iter::once(x.return_type()))
                .chain(x.guard_types())
        }) {
            if let Some(target) = ty.find_rename_target_if_contains(position) {
                return Some(target);
            }
        }
        None
    }
}

impl FindRenameTarget for efmt::items::forms::TypeDecl {
    fn find_rename_target(&self, position: Position) -> Option<RenameTarget> {
        if self.type_name().contains(position) {
            let target = RenameTarget {
                name: self.type_name().value().to_owned(),
                kind: RenamableItemKind::TypeName,
                position: self.type_name().start_position(),
            };
            return Some(target);
        }
        for param in self.params() {
            if param.contains(position) {
                let target = RenameTarget {
                    name: param.value().to_owned(),
                    kind: RenamableItemKind::Variable,
                    position: param.start_position(),
                };
                return Some(target);
            }
        }
        self.type_value().find_rename_target_if_contains(position)
    }
}

impl FindRenameTarget for efmt::items::forms::ModuleAttr {
    fn find_rename_target(&self, position: Position) -> Option<RenameTarget> {
        if self.module_name().contains(position) {
            let target = RenameTarget {
                name: self.module_name().value().to_owned(),
                kind: RenamableItemKind::ModuleName,
                position: self.module_name().start_position(),
            };
            Some(target)
        } else {
            None
        }
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
            Self::Map(x) => x.find_rename_target_if_contains(position),
            Self::Record(x) => x.find_rename_target_if_contains(position),
            Self::Function(x) => x.find_rename_target_if_contains(position),
            Self::Parenthesized(x) => x.get().find_rename_target_if_contains(position),
            Self::Annotated(x) => x.find_rename_target_if_contains(position),
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

impl FindRenameTarget for efmt::items::types::AnnotatedVariableType {
    fn find_rename_target(&self, position: Position) -> Option<RenameTarget> {
        if self.variable().contains(position) {
            let target = RenameTarget {
                name: self.variable().value().to_owned(),
                kind: RenamableItemKind::Variable,
                position: self.variable().start_position(),
            };
            return Some(target);
        }
        self.ty().find_rename_target_if_contains(position)
    }
}

impl FindRenameTarget for efmt::items::types::FunctionType {
    fn find_rename_target(&self, position: Position) -> Option<RenameTarget> {
        for param in self.params() {
            if let Some(target) = param.find_rename_target_if_contains(position) {
                return Some(target);
            }
        }
        self.return_type()
            .and_then(|x| x.find_rename_target_if_contains(position))
    }
}

impl FindRenameTarget for efmt::items::types::RecordType {
    fn find_rename_target(&self, position: Position) -> Option<RenameTarget> {
        if self.name().contains(position) {
            let target = RenameTarget {
                name: self.name().value().to_owned(),
                kind: RenamableItemKind::RecordName,
                position: self.name().start_position(),
            };
            return Some(target);
        }
        self.fields().find_map(|(name, field)| {
            if name.contains(position) {
                let target = RenameTarget {
                    name: name.value().to_owned(),
                    kind: RenamableItemKind::RecordFieldName,
                    position: name.start_position(),
                };
                Some(target)
            } else {
                field.find_rename_target_if_contains(position)
            }
        })
    }
}

impl FindRenameTarget for efmt::items::types::MapType {
    fn find_rename_target(&self, position: Position) -> Option<RenameTarget> {
        self.items().find_map(|(k, v)| {
            k.find_rename_target_if_contains(position)
                .or_else(|| v.find_rename_target_if_contains(position))
        })
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

impl FindRenameTarget for efmt::items::Expr {
    fn find_rename_target(&self, position: Position) -> Option<RenameTarget> {
        self.get().find_rename_target_if_contains(position)
    }
}

impl FindRenameTarget for efmt::items::expressions::FullExpr {
    fn find_rename_target(&self, position: Position) -> Option<RenameTarget> {
        match self {
            Self::Base(x) => x.find_rename_target_if_contains(position),
            Self::FunctionCall(x) => x.find_rename_target_if_contains(position),
            Self::BinaryOpCall(x) => x.find_rename_target_if_contains(position),
        }
    }
}

impl FindRenameTarget for efmt::items::expressions::BaseExpr {
    fn find_rename_target(&self, position: Position) -> Option<RenameTarget> {
        match self {
            Self::List(_) => todo!(),
            Self::Tuple(_) => todo!(),
            Self::Map(_) => todo!(),
            Self::RecordConstructOrIndex(_) => todo!(),
            Self::Bitstring(_) => todo!(),
            Self::Function(_) => todo!(),
            Self::UnaryOpCall(_) => todo!(),
            Self::Parenthesized(_) => todo!(),
            Self::Literal(x) => x.find_rename_target_if_contains(position),
            Self::Block(_) => todo!(),
            Self::MapUpdate(_) => todo!(),
            Self::RecordAccessOrUpdate(_) => todo!(),
        }
    }
}

impl FindRenameTarget for efmt::items::expressions::FunctionCallExpr {
    fn find_rename_target(&self, position: Position) -> Option<RenameTarget> {
        if let Some(x) = self.module_expr() {
            if let Some(x) = x.as_atom_token() {
                if x.contains(position) {
                    let target = RenameTarget {
                        name: x.value().to_owned(),
                        kind: RenamableItemKind::ModuleName,
                        position: x.start_position(),
                    };
                    return Some(target);
                }
            }
            if let Some(target) = x.find_rename_target_if_contains(position) {
                return Some(target);
            }
        }
        if let Some(x) = self.function_expr().as_atom_token() {
            if x.contains(position) {
                let target = RenameTarget {
                    name: x.value().to_owned(),
                    kind: RenamableItemKind::FunctionName,
                    position: x.start_position(),
                };
                return Some(target);
            }
        }
        if let Some(target) = self
            .function_expr()
            .find_rename_target_if_contains(position)
        {
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

impl FindRenameTarget for efmt::items::expressions::BinaryOpCallExpr {
    fn find_rename_target(&self, position: Position) -> Option<RenameTarget> {
        self.children()
            .find_map(|child| child.find_rename_target_if_contains(position))
    }
}

impl FindRenameTarget for efmt::items::expressions::LiteralExpr {
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
            Self::Atom(_) | Self::Char(_) | Self::Float(_) | Self::Integer(_) | Self::String(_) => {
            }
        }
        None
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
    ModuleName,   // TODO: name
    TypeName,     // TODO: module, name, arity
    FunctionName, // TODO: module, name, arity
    MacroName,    // TODO: option<arity>
    RecordName,
    RecordFieldName, // TODO: record_namee, field_name
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
                77..=79 => {
                    let target = tree.find_rename_target(offset(i)).or_fail()?;
                    assert_eq!(target.name, "foo");
                    assert_eq!(target.kind, RenamableItemKind::FunctionName);
                    assert_eq!(target.position.offset(), offset(77).offset());
                }
                81..=83 => {
                    let target = tree.find_rename_target(offset(i)).or_fail()?;
                    assert_eq!(target.name, "foo");
                    assert_eq!(target.kind, RenamableItemKind::ModuleName);
                    assert_eq!(target.position.offset(), offset(81).offset());
                }
                85..=87 => {
                    let target = tree.find_rename_target(offset(i)).or_fail()?;
                    assert_eq!(target.name, "foo");
                    assert_eq!(target.kind, RenamableItemKind::TypeName);
                    assert_eq!(target.position.offset(), offset(85).offset());
                }
                99..=101 => {
                    let target = tree.find_rename_target(offset(i)).or_fail()?;
                    assert_eq!(target.name, "foo");
                    assert_eq!(target.kind, RenamableItemKind::FunctionName);
                    assert_eq!(target.position.offset(), offset(99).offset());
                }
                103 => {
                    let target = tree.find_rename_target(offset(i)).or_fail()?;
                    assert_eq!(target.name, "A");
                    assert_eq!(target.kind, RenamableItemKind::Variable);
                    assert_eq!(target.position.offset(), offset(i).offset());
                }
                113 => {
                    let target = tree.find_rename_target(offset(i)).or_fail()?;
                    assert_eq!(target.name, "B");
                    assert_eq!(target.kind, RenamableItemKind::Variable);
                    assert_eq!(target.position.offset(), offset(i).offset());
                }
                117 => {
                    let target = tree.find_rename_target(offset(i)).or_fail()?;
                    assert_eq!(target.name, "A");
                    assert_eq!(target.kind, RenamableItemKind::Variable);
                    assert_eq!(target.position.offset(), offset(i).offset());
                }
                128..=129 => {
                    let target = tree.find_rename_target(offset(i)).or_fail()?;
                    assert_eq!(target.name, "io");
                    assert_eq!(target.kind, RenamableItemKind::ModuleName);
                    assert_eq!(target.position.offset(), offset(128).offset());
                }
                131..=136 => {
                    let target = tree.find_rename_target(offset(i)).or_fail()?;
                    assert_eq!(target.name, "format");
                    assert_eq!(target.kind, RenamableItemKind::FunctionName);
                    assert_eq!(target.position.offset(), offset(131).offset());
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
