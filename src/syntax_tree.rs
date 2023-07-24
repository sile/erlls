use efmt_core::{items::Macro, parse::TokenStream, span::Position, span::Span};
use erl_tokenize::Tokenizer;
use orfail::OrFail;

pub type ItemRange = std::ops::Range<Position>;

fn item_range<T: Span>(item: &T) -> ItemRange {
    item.start_position()..item.end_position()
}

#[derive(Debug)]
pub struct SyntaxTree {
    ts: TokenStream,
    module: efmt_core::items::Module,
}

impl SyntaxTree {
    pub fn parse(text: String) -> orfail::Result<Self> {
        let tokenizer = Tokenizer::new(text);
        // TODO: tokenizer.set_file(path);
        let mut ts = TokenStream::new(tokenizer);
        let module: efmt_core::items::Module = ts.parse().or_fail()?;
        Ok(Self { ts, module })
    }

    pub fn find_definition(&self, item: &ItemKind) -> Option<ItemRange> {
        self.module.find_definition(&self.ts.text(), item)
    }

    pub fn find_target(&mut self, position: Position) -> Option<Target> {
        let mut candidate = None;
        for macro_call in self.ts.macros().values() {
            if macro_call.contains(position) {
                candidate = Some(macro_call.clone());
                break;
            }
            if position < macro_call.start_position() {
                break;
            }
        }
        if let Some(macro_call) = candidate {
            self.find_target_macro_call(position, macro_call)
        } else {
            self.module.find_target(&self.ts.text(), position)
        }
    }

    fn find_target_macro_call(&mut self, position: Position, macro_call: Macro) -> Option<Target> {
        if !macro_call.contains(position) {
            return None;
        }
        if macro_call.macro_name().contains(position) {
            let target = Target::macro_name(
                macro_call.macro_name().start_position(),
                macro_call.macro_name().value().to_owned(),
                macro_call.arity(),
            );
            return Some(target);
        }
        if let Some(target) = macro_call
            .args()
            .find(|x| x.contains(position))
            .and_then(|x| x.parse_expr(&mut self.ts))
            .and_then(|x| x.find_target_if_contains(&self.ts.text(), position))
        {
            return Some(target);
        }
        None
    }
}

pub trait FindDefinition {
    fn find_definition(&self, text: &str, item: &ItemKind) -> Option<ItemRange>;
}

pub trait FindTarget {
    fn find_target(&self, text: &str, position: Position) -> Option<Target>;

    fn find_target_if_contains(&self, text: &str, position: Position) -> Option<Target>
    where
        Self: Span,
    {
        if self.contains(position) {
            self.find_target(text, position)
        } else {
            None
        }
    }
}

impl FindTarget for efmt_core::items::Module {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        for form in self.children() {
            if let Some(target) = form.get().find_target_if_contains(text, position) {
                return Some(target);
            }
        }
        None
    }
}

impl FindDefinition for efmt_core::items::Module {
    fn find_definition(&self, text: &str, item: &ItemKind) -> Option<ItemRange> {
        self.children()
            .find_map(|x| x.get().find_definition(text, item))
    }
}

impl FindTarget for efmt_core::items::forms::Form {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        match self {
            Self::Module(x) => x.find_target_if_contains(text, position),
            Self::TypeDecl(x) => x.find_target_if_contains(text, position),
            Self::FunSpec(x) => x.find_target_if_contains(text, position),
            Self::FunDecl(x) => x.find_target_if_contains(text, position),
            Self::Define(x) => x.find_target_if_contains(text, position),
            Self::RecordDecl(x) => x.find_target_if_contains(text, position),
            Self::Export(x) => x.find_target_if_contains(text, position),
            Self::Include(_) | Self::Attr(_) => None,
        }
    }
}

impl FindDefinition for efmt_core::items::forms::Form {
    fn find_definition(&self, text: &str, item: &ItemKind) -> Option<ItemRange> {
        match self {
            Self::Module(x) => x.find_definition(text, item),
            Self::TypeDecl(x) => x.find_definition(text, item),
            Self::FunDecl(x) => x.find_definition(text, item),
            Self::Define(x) => x.find_definition(text, item),
            Self::FunSpec(_) | Self::Export(_) | Self::Include(_) | Self::Attr(_) => None,
            _ => None,
        }
        // Self::RecordDecl(_) => todo!(),
    }
}

impl FindTarget for efmt_core::items::forms::DefineDirective {
    fn find_target(&self, _text: &str, position: Position) -> Option<Target> {
        if self.macro_name_token().contains(position) {
            let target = Target::macro_name(
                self.macro_name_token().start_position(),
                self.macro_name_token().value().to_owned(),
                self.variables().map(|x| x.len()),
            );
            return Some(target);
        }
        None
    }
}

impl FindDefinition for efmt_core::items::forms::DefineDirective {
    fn find_definition(&self, _text: &str, item: &ItemKind) -> Option<ItemRange> {
        let ItemKind::MacroName { name, arity } = item else {
            return None;
        };

        if self.macro_name() != name {
            return None;
        }
        if self.variables().map(|x| x.len()) != *arity {
            return None;
        }

        Some(item_range(self.macro_name_token()))
    }
}

impl FindTarget for efmt_core::items::forms::RecordDecl {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        if self.record_name().contains(position) {
            let target = Target {
                name: self.record_name().value().to_owned(),
                kind: ItemKind::RecordName,
                position: self.record_name().start_position(),
            };
            return Some(target);
        }
        for field in self.fields() {
            if field.field_name().contains(position) {
                let target = Target {
                    name: field.field_name().value().to_owned(),
                    kind: ItemKind::RecordFieldName,
                    position: field.field_name().start_position(),
                };
                return Some(target);
            }
            if let Some(target) = field
                .default_value()
                .and_then(|x| x.find_target_if_contains(text, position))
            {
                return Some(target);
            }
            if let Some(target) = field
                .field_type()
                .and_then(|x| x.find_target_if_contains(text, position))
            {
                return Some(target);
            }
        }
        None
    }
}

impl FindTarget for efmt_core::items::forms::ExportAttr {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        for e in self.exports() {
            if e.name().contains(position) {
                let position = e.name().start_position();
                let name = e.name().value().to_owned();
                let arity = e.arity().text(text).parse::<usize>().ok()?;
                let target = if self.is_function() {
                    Target::function_name(position, None, name, arity)
                } else {
                    Target::type_name(position, None, name, arity)
                };
                return Some(target);
            }
        }
        None
    }
}

impl FindTarget for efmt_core::items::forms::FunDecl {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        for clause in self.clauses() {
            if clause.function_name().contains(position) {
                let target = Target::function_name(
                    clause.function_name().start_position(),
                    None,
                    clause.function_name().value().to_owned(),
                    clause.params().len(),
                );
                return Some(target);
            }
            for expr in clause.children() {
                if let Some(target) = expr.find_target_if_contains(text, position) {
                    return Some(target);
                }
            }
        }
        None
    }
}

impl FindDefinition for efmt_core::items::forms::FunDecl {
    fn find_definition(&self, _text: &str, item: &ItemKind) -> Option<ItemRange> {
        let ItemKind::FunctionName(mfa) = item else {
            return None;
        };

        let clause = self.clauses().next()?;
        if clause.function_name().value() != mfa.name {
            return None;
        }
        if clause.params().len() != mfa.arity {
            return None;
        }
        Some(item_range(clause.function_name()))
    }
}

impl FindTarget for efmt_core::items::forms::FunSpec {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        if let Some(name) = self.module_name() {
            if name.contains(position) {
                let target = Target::module_name(name.start_position(), name.value().to_owned());
                return Some(target);
            }
        }
        if self.function_name().contains(position) {
            let target = Target::function_name(
                self.function_name().start_position(),
                self.module_name().map(|x| x.value().to_owned()),
                self.function_name().value().to_owned(),
                self.clauses().next()?.params().len(),
            );
            return Some(target);
        }
        for ty in self.clauses().flat_map(|x| {
            x.params()
                .iter()
                .chain(std::iter::once(x.return_type()))
                .chain(x.guard_types())
        }) {
            if let Some(target) = ty.find_target_if_contains(text, position) {
                return Some(target);
            }
        }
        None
    }
}

impl FindTarget for efmt_core::items::forms::TypeDecl {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        if self.type_name().contains(position) {
            return Some(Target::type_name(
                self.type_name().start_position(),
                None,
                self.type_name().value().to_owned(),
                self.params().len(),
            ));
        }
        for param in self.params() {
            if param.contains(position) {
                let target = Target {
                    name: param.value().to_owned(),
                    kind: ItemKind::Variable,
                    position: param.start_position(),
                };
                return Some(target);
            }
        }
        self.type_value().find_target_if_contains(text, position)
    }
}

impl FindDefinition for efmt_core::items::forms::TypeDecl {
    fn find_definition(&self, _text: &str, item: &ItemKind) -> Option<ItemRange> {
        let ItemKind::TypeName(mfa) = item else {
            return None;
        };
        if self.type_name().value() != mfa.name {
            return None;
        }
        if self.params().len() != mfa.arity {
            return None;
        }
        Some(item_range(self.type_name()))
    }
}

impl FindTarget for efmt_core::items::forms::ModuleAttr {
    fn find_target(&self, _text: &str, position: Position) -> Option<Target> {
        if self.module_name().contains(position) {
            let target = Target::module_name(
                self.module_name().start_position(),
                self.module_name().value().to_owned(),
            );
            Some(target)
        } else {
            None
        }
    }
}

impl FindDefinition for efmt_core::items::forms::ModuleAttr {
    fn find_definition(&self, _text: &str, item: &ItemKind) -> Option<ItemRange> {
        let ItemKind::ModuleName{ module } = item else {
            return None;
        };
        if self.module_name().value() != module {
            return None;
        }
        Some(item_range(self.module_name()))
    }
}

impl FindTarget for efmt_core::items::Type {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        self.children()
            .find_map(|child| child.find_target_if_contains(text, position))
    }
}

impl FindTarget for efmt_core::items::types::NonUnionType {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        match self {
            Self::Base(x) => x.find_target_if_contains(text, position),
            Self::BinaryOp(x) => x.find_target_if_contains(text, position),
        }
    }
}

impl FindTarget for efmt_core::items::types::BaseType {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        match self {
            Self::Mfargs(x) => x.find_target_if_contains(text, position),
            Self::List(x) => x.find_target_if_contains(text, position),
            Self::Tuple(x) => x.find_target_if_contains(text, position),
            Self::Map(x) => x.find_target_if_contains(text, position),
            Self::Record(x) => x.find_target_if_contains(text, position),
            Self::Function(x) => x.find_target_if_contains(text, position),
            Self::Parenthesized(x) => x.get().find_target_if_contains(text, position),
            Self::Annotated(x) => x.find_target_if_contains(text, position),
            Self::Literal(x) => x.find_target_if_contains(text, position),
            Self::Bitstring(_) | Self::UnaryOp(_) => None,
        }
    }
}

impl FindTarget for efmt_core::items::types::BinaryOpType {
    fn find_target(&self, _text: &str, _position: Position) -> Option<Target> {
        // As binary op types are only applied to integer types, they are never renamed.
        None
    }
}

impl FindTarget for efmt_core::items::types::AnnotatedVariableType {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        if self.variable().contains(position) {
            let target = Target {
                name: self.variable().value().to_owned(),
                kind: ItemKind::Variable,
                position: self.variable().start_position(),
            };
            return Some(target);
        }
        self.ty().find_target_if_contains(text, position)
    }
}

impl FindTarget for efmt_core::items::types::FunctionType {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        for param in self.params() {
            if let Some(target) = param.find_target_if_contains(text, position) {
                return Some(target);
            }
        }
        self.return_type()
            .and_then(|x| x.find_target_if_contains(text, position))
    }
}

impl FindTarget for efmt_core::items::types::RecordType {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        if self.name().contains(position) {
            let target = Target {
                name: self.name().value().to_owned(),
                kind: ItemKind::RecordName,
                position: self.name().start_position(),
            };
            return Some(target);
        }
        self.fields().find_map(|(name, field)| {
            if name.contains(position) {
                let target = Target {
                    name: name.value().to_owned(),
                    kind: ItemKind::RecordFieldName,
                    position: name.start_position(),
                };
                Some(target)
            } else {
                field.find_target_if_contains(text, position)
            }
        })
    }
}

impl FindTarget for efmt_core::items::types::MapType {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        self.items().find_map(|(k, v)| {
            k.find_target_if_contains(text, position)
                .or_else(|| v.find_target_if_contains(text, position))
        })
    }
}

impl FindTarget for efmt_core::items::types::LiteralType {
    fn find_target(&self, _text: &str, position: Position) -> Option<Target> {
        match self {
            Self::Variable(x) => {
                if x.contains(position) {
                    let target = Target {
                        name: x.value().to_owned(),
                        kind: ItemKind::Variable,
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

impl FindTarget for efmt_core::items::types::MfargsType {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        if let Some(module_name) = self.module_name() {
            if module_name.contains(position) {
                let target = Target::module_name(
                    module_name.start_position(),
                    module_name.value().to_owned(),
                );
                return Some(target);
            }
        }
        if self.type_name().contains(position) {
            return Some(Target::type_name(
                self.type_name().start_position(),
                self.module_name().map(|x| x.value().to_owned()),
                self.type_name().value().to_owned(),
                self.args().len(),
            ));
        }
        for arg in self.args() {
            if let Some(target) = arg.find_target_if_contains(text, position) {
                return Some(target);
            }
        }
        None
    }
}

impl FindTarget for efmt_core::items::types::TupleType {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        self.items()
            .1
            .find_map(|item| item.find_target_if_contains(text, position))
    }
}

impl FindTarget for efmt_core::items::types::ListType {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        self.item_type()
            .and_then(|item| item.find_target_if_contains(text, position))
    }
}

impl FindTarget for efmt_core::items::Expr {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        self.get().find_target_if_contains(text, position)
    }
}

impl FindTarget for efmt_core::items::expressions::FullExpr {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        match self {
            Self::Base(x) => x.find_target_if_contains(text, position),
            Self::FunctionCall(x) => x.find_target_if_contains(text, position),
            Self::BinaryOpCall(x) => x.find_target_if_contains(text, position),
        }
    }
}

impl FindTarget for efmt_core::items::expressions::BaseExpr {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        match self {
            Self::List(x) => x.find_target_if_contains(text, position),
            Self::Tuple(x) => x.find_target_if_contains(text, position),
            Self::Map(x) => x.find_target_if_contains(text, position),
            Self::RecordConstructOrIndex(x) => x.find_target_if_contains(text, position),
            Self::Bitstring(x) => x.find_target_if_contains(text, position),
            Self::Function(x) => x.find_target_if_contains(text, position),
            Self::UnaryOpCall(x) => x.find_target_if_contains(text, position),
            Self::Parenthesized(x) => x.find_target_if_contains(text, position),
            Self::Literal(x) => x.find_target_if_contains(text, position),
            Self::Block(x) => x.find_target_if_contains(text, position),
            Self::MapUpdate(x) => x.find_target_if_contains(text, position),
            Self::RecordAccessOrUpdate(x) => x.find_target_if_contains(text, position),
        }
    }
}

impl<A: FindTarget, B: FindTarget> FindTarget for efmt_core::items::Either<A, B> {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        match self {
            Self::A(x) => x.find_target(text, position),
            Self::B(x) => x.find_target(text, position),
        }
    }
}

impl<'a, A: FindTarget> FindTarget for &'a A {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        (*self).find_target(text, position)
    }
}

impl FindTarget for efmt_core::items::expressions::BlockExpr {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        self.children()
            .find_map(|child| child.find_target_if_contains(text, position))
    }
}

impl FindTarget for efmt_core::items::expressions::BitstringExpr {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        self.children()
            .find_map(|child| child.find_target_if_contains(text, position))
    }
}

impl FindTarget for efmt_core::items::expressions::MapUpdateExpr {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        self.children()
            .find_map(|child| child.find_target_if_contains(text, position))
    }
}

impl FindTarget for efmt_core::items::expressions::UnaryOpCallExpr {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        self.expr().find_target_if_contains(text, position)
    }
}

impl FindTarget for efmt_core::items::expressions::FunctionExpr {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        if let Some(x) = self.module_name() {
            if x.contains(position) {
                let target = Target::module_name(x.start_position(), x.value().to_owned());
                return Some(target);
            }
        }
        if let Some(x) = self.function_name() {
            if x.contains(position) {
                if let Some(arity) = self.arity().and_then(|x| {
                    x.as_integer_token()
                        .and_then(|x| x.text(text).parse::<usize>().ok())
                }) {
                    let target = Target::function_name(
                        x.start_position(),
                        self.module_name().map(|x| x.value().to_owned()),
                        x.value().to_owned(),
                        arity,
                    );
                    return Some(target);
                } else {
                    return None;
                }
            }
        }
        self.children()
            .find_map(|x| x.find_target_if_contains(text, position))
    }
}

impl FindTarget for efmt_core::items::expressions::MapExpr {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        self.children()
            .find_map(|x| x.find_target_if_contains(text, position))
    }
}

impl FindTarget for efmt_core::items::expressions::ParenthesizedExpr {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        self.get().find_target_if_contains(text, position)
    }
}

impl FindTarget for efmt_core::items::expressions::RecordConstructOrIndexExpr {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        if self.record_name().contains(position) {
            let target = Target {
                name: self.record_name().value().to_owned(),
                kind: ItemKind::RecordName,
                position: self.record_name().start_position(),
            };
            return Some(target);
        }
        for field_name in self.field_names() {
            if field_name.contains(position) {
                let target = Target {
                    name: field_name.value().to_owned(),
                    kind: ItemKind::RecordFieldName,
                    position: field_name.start_position(),
                };
                return Some(target);
            }
        }
        self.children()
            .find_map(|x| x.find_target_if_contains(text, position))
    }
}

impl FindTarget for efmt_core::items::expressions::RecordAccessOrUpdateExpr {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        if self.record_name().contains(position) {
            let target = Target {
                name: self.record_name().value().to_owned(),
                kind: ItemKind::RecordName,
                position: self.record_name().start_position(),
            };
            return Some(target);
        }
        for field_name in self.field_names() {
            if field_name.contains(position) {
                let target = Target {
                    name: field_name.value().to_owned(),
                    kind: ItemKind::RecordFieldName,
                    position: field_name.start_position(),
                };
                return Some(target);
            }
        }
        self.children()
            .find_map(|x| x.find_target_if_contains(text, position))
    }
}

impl FindTarget for efmt_core::items::expressions::TupleExpr {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        self.children()
            .find_map(|x| x.find_target_if_contains(text, position))
    }
}

impl FindTarget for efmt_core::items::expressions::ListExpr {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        self.children()
            .find_map(|x| x.find_target_if_contains(text, position))
    }
}

impl FindTarget for efmt_core::items::expressions::FunctionCallExpr {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        if let Some(x) = self.module_expr() {
            if let Some(x) = x.as_atom_token() {
                if x.contains(position) {
                    let target = Target::module_name(x.start_position(), x.value().to_owned());
                    return Some(target);
                }
            }
            if let Some(target) = x.find_target_if_contains(text, position) {
                return Some(target);
            }
        }
        if let Some(x) = self.function_expr().as_atom_token() {
            if x.contains(position) {
                if self
                    .module_expr()
                    .map_or(false, |x| x.as_atom_token().is_none())
                {
                    return None;
                };
                let target = Target::function_name(
                    x.start_position(),
                    self.module_expr()
                        .and_then(|x| x.as_atom_token())
                        .map(|x| x.value().to_owned()),
                    x.value().to_owned(),
                    self.args().len(),
                );
                return Some(target);
            }
        }
        if let Some(target) = self.function_expr().find_target_if_contains(text, position) {
            return Some(target);
        }
        for arg in self.args() {
            if let Some(target) = arg.find_target_if_contains(text, position) {
                return Some(target);
            }
        }
        None
    }
}

impl FindTarget for efmt_core::items::expressions::BinaryOpCallExpr {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        self.children()
            .find_map(|child| child.find_target_if_contains(text, position))
    }
}

impl FindTarget for efmt_core::items::expressions::LiteralExpr {
    fn find_target(&self, _text: &str, position: Position) -> Option<Target> {
        match self {
            Self::Variable(x) => {
                if x.contains(position) {
                    let target = Target {
                        name: x.value().to_owned(),
                        kind: ItemKind::Variable,
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
pub struct Target {
    pub name: String,
    pub kind: ItemKind,
    pub position: Position,
}

impl Target {
    pub fn module_name(position: Position, module: String) -> Self {
        Self {
            name: module.clone(),
            kind: ItemKind::ModuleName { module },
            position,
        }
    }

    pub fn type_name(
        position: Position,
        module: Option<String>,
        name: String,
        arity: usize,
    ) -> Self {
        Self {
            name: name.clone(),
            kind: ItemKind::TypeName(Mfa {
                module,
                name,
                arity,
            }),
            position,
        }
    }

    pub fn function_name(
        position: Position,
        module: Option<String>,
        name: String,
        arity: usize,
    ) -> Self {
        Self {
            name: name.clone(),
            kind: ItemKind::FunctionName(Mfa {
                module,
                name,
                arity,
            }),
            position,
        }
    }

    pub fn macro_name(position: Position, name: String, arity: Option<usize>) -> Self {
        Self {
            name: name.clone(),
            kind: ItemKind::MacroName { name, arity },
            position,
        }
    }
}

// TODO: remove
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Mfa {
    pub module: Option<String>,
    pub name: String,
    pub arity: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ItemKind {
    ModuleName { module: String },
    TypeName(Mfa),
    FunctionName(Mfa),
    MacroName { name: String, arity: Option<usize> },
    RecordName,
    RecordFieldName, // TODO: record_namee, field_name
    Variable,
}

impl ItemKind {
    pub fn module(&self) -> Option<&str> {
        match self {
            Self::ModuleName { module } => Some(module),
            Self::TypeName(mfa) => mfa.module.as_ref().map(|x| x.as_str()),
            Self::FunctionName(mfa) => mfa.module.as_ref().map(|x| x.as_str()),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn find_target_works() -> orfail::Result<()> {
        macro_rules! assert_rename_target {
            ($start:expr, $end:expr, $name:expr, $kind:pat, $i:expr, $tree:expr) => {
                if ($start..=$end).contains(&$i) {
                    let target = $tree.find_target(offset($i)).or_fail()?;
                    assert_eq!(target.name, $name);
                    assert!(matches!(target.kind, $kind));
                    assert_eq!(target.position.offset(), offset($start).offset());
                    continue;
                }
            };
        }

        let text = r#"-module(foo).

-type foo() :: any().
-type bar(A) :: {A, [b:b()], c}.

-spec foo(foo:foo()) -> ok.
foo(A) ->
    B = A + 2,
    io:format("B=~p~n", [B]),
    {atom1, atom2, Record#record_name.field_name},
    #rec{aaa = (#{bbb => 1}), ccc = fun (A) -> -A end},
    M#{a => 1, b => <<1:4, C/binary>>},
    case 1 + 2 of
        XXX -> baz(XXX)
    end,
    ok.

-export([foo/1]).
-export_type([foo/0]).

-record(rec, { aaa = bbb() :: foo() }).

-define(FOO, foo(B)).
-define(BAR(A), [A]).

bar() ->
    ?FOO + ?BAR(bbb()).
"#;
        let mut tree = SyntaxTree::parse(text.to_owned()).or_fail()?;
        for i in 0..text.len() {
            use ItemKind::*;

            assert_rename_target!(8, 10, "foo", ModuleName { .. }, i, tree);
            assert_rename_target!(21, 23, "foo", TypeName { .. }, i, tree);
            assert_rename_target!(30, 32, "any", TypeName { .. }, i, tree);
            assert_rename_target!(43, 45, "bar", TypeName { .. }, i, tree);
            assert_rename_target!(47, 47, "A", Variable, i, tree);
            assert_rename_target!(54, 54, "A", Variable, i, tree);
            assert_rename_target!(58, 58, "b", ModuleName { .. }, i, tree);
            assert_rename_target!(60, 60, "b", TypeName { .. }, i, tree);
            assert_rename_target!(77, 79, "foo", FunctionName { .. }, i, tree);
            assert_rename_target!(81, 83, "foo", ModuleName { .. }, i, tree);
            assert_rename_target!(85, 87, "foo", TypeName { .. }, i, tree);
            assert_rename_target!(99, 101, "foo", FunctionName { .. }, i, tree);
            assert_rename_target!(103, 103, "A", Variable, i, tree);
            assert_rename_target!(113, 113, "B", Variable, i, tree);
            assert_rename_target!(117, 117, "A", Variable, i, tree);
            assert_rename_target!(128, 129, "io", ModuleName { .. }, i, tree);
            assert_rename_target!(131, 136, "format", FunctionName { .. }, i, tree);
            assert_rename_target!(149, 149, "B", Variable, i, tree);
            assert_rename_target!(173, 178, "Record", Variable, i, tree);
            assert_rename_target!(180, 190, "record_name", RecordName, i, tree);
            assert_rename_target!(192, 201, "field_name", RecordFieldName, i, tree);
            assert_rename_target!(210, 212, "rec", RecordName, i, tree);
            assert_rename_target!(214, 216, "aaa", RecordFieldName, i, tree);
            assert_rename_target!(235, 237, "ccc", RecordFieldName, i, tree);
            assert_rename_target!(246, 246, "A", Variable, i, tree);
            assert_rename_target!(253, 253, "A", Variable, i, tree);
            assert_rename_target!(265, 265, "M", Variable, i, tree);
            assert_rename_target!(288, 288, "C", Variable, i, tree);
            assert_rename_target!(327, 329, "XXX", Variable, i, tree);
            assert_rename_target!(334, 336, "baz", FunctionName { .. }, i, tree);
            assert_rename_target!(338, 340, "XXX", Variable, i, tree);
            assert_rename_target!(370, 372, "foo", FunctionName { .. }, i, tree);
            assert_rename_target!(393, 395, "foo", TypeName { .. }, i, tree);
            assert_rename_target!(411, 413, "rec", RecordName, i, tree);
            assert_rename_target!(418, 420, "aaa", RecordFieldName, i, tree);
            assert_rename_target!(424, 426, "bbb", FunctionName { .. }, i, tree);
            assert_rename_target!(433, 435, "foo", TypeName { .. }, i, tree);
            assert_rename_target!(452, 454, "FOO", MacroName { .. }, i, tree);
            assert_rename_target!(474, 476, "BAR", MacroName { .. }, i, tree);
            assert_rename_target!(489, 491, "bar", FunctionName { .. }, i, tree);
            assert_rename_target!(503, 505, "FOO", MacroName { .. }, i, tree);
            assert_rename_target!(510, 512, "BAR", MacroName { .. }, i, tree);
            assert_rename_target!(514, 516, "bbb", FunctionName { .. }, i, tree);

            assert_eq!(None, tree.find_target(offset(i)));
        }
        Ok(())
    }

    fn offset(offset: usize) -> Position {
        Position::new(offset, 0, 0)
    }
}
