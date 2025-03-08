use efmt_core::{
    items::Macro,
    parse::{TokenOrShebang, TokenStream, Tokenizer},
    span::{Position, Span},
};
use erl_tokenize::values::Symbol;
use orfail::OrFail;
use std::{path::PathBuf, sync::Arc};

pub type ItemRange = std::ops::Range<Position>;

trait ContainsInclusive: Span {
    fn contains_inclusive(&self, position: Position) -> bool {
        self.start_position().offset() <= position.offset()
            && position.offset() <= self.end_position().offset()
    }
}

impl<T: Span> ContainsInclusive for T {}

type ExprOrMaybeFunCall = efmt_core::items::Either<
    (
        efmt_core::items::tokens::AtomToken,
        efmt_core::items::symbols::ColonSymbol,
        efmt_core::items::Maybe<efmt_core::items::tokens::AtomToken>,
    ),
    efmt_core::items::Expr,
>;

// TODO: return position
fn item_range<T: Span>(item: &T) -> ItemRange {
    item.start_position()..item.start_position()
}

#[derive(Debug)]
pub struct SyntaxTree {
    ts: TokenStream,
    module: Option<efmt_core::items::Module>,
    maybe_partial_module: Option<efmt_core::items::Module<true>>,
    expr: Option<ExprOrMaybeFunCall>,
}

impl SyntaxTree {
    pub fn text(&self) -> Arc<String> {
        self.ts.text()
    }

    pub fn parse(text: String) -> orfail::Result<Self> {
        let tokenizer = Tokenizer::new(text);
        // TODO: tokenizer.set_file(path);
        let mut ts = TokenStream::new(tokenizer);
        let module: efmt_core::items::Module = ts.parse().or_fail()?;
        Ok(Self {
            ts,
            module: Some(module),
            maybe_partial_module: None,
            expr: None,
        })
    }

    // TODO: rename
    pub fn parse_as_much_as_possible(text: String) -> orfail::Result<Self> {
        let tokenizer = Tokenizer::new(text);
        let mut ts = TokenStream::new(tokenizer);
        let module: efmt_core::items::Module<true> = ts.parse().or_fail()?;
        Ok(Self {
            ts,
            module: None,
            maybe_partial_module: Some(module),
            expr: None,
        })
    }

    // TODO: rename
    pub fn partial_parse(text: String, position: Position) -> orfail::Result<Self> {
        use erl_tokenize::PositionRange;

        // TODO: optimize (reduce text.clone() count)

        let tokenizer = Tokenizer::new(text.clone());
        let mut token_positions = Vec::new();
        let mut passed = false;
        for token in tokenizer {
            let token = token.or_fail()?;
            let TokenOrShebang::Token(token) = token else {
                continue;
            };

            if !token.is_lexical_token() {
                continue;
            }

            token_positions.push(token.start_position());
            if position.offset() < token.start_position().offset() {
                passed = true;
            }
            if token.as_symbol_token().map(|x| x.value()) == Some(Symbol::Dot) {
                if passed {
                    break;
                } else {
                    token_positions.clear();
                }
            }
        }

        let end_offset = token_positions.last().or_fail()?.offset() + 1;
        for start_position in token_positions {
            let mut tokenizer = Tokenizer::new(text[..end_offset].to_owned());
            tokenizer.set_position(start_position);

            let mut ts = TokenStream::new(tokenizer);
            if let Ok(expr) = ts.parse::<ExprOrMaybeFunCall>() {
                if !expr.contains_inclusive(position) {
                    continue;
                }
                return Ok(Self {
                    ts,
                    module: None,
                    maybe_partial_module: None,
                    expr: Some(expr),
                });
            }
        }

        Err(orfail::Failure::new("failed to parse"))
    }

    pub fn parse_partial_funcall(
        text: String,
        position: Position,
    ) -> Option<(String, Option<String>)> {
        let tree = Self::partial_parse(text, position).ok()?;
        let Some(efmt_core::items::Either::A((module, _, function))) = tree.expr else {
            return None;
        };

        if let Some(offset) = position
            .offset()
            .checked_sub(function.start_position().offset())
        {
            Some((
                module.value().to_owned(),
                function.get().map(|x| x.value()[..offset].to_owned()),
            ))
        } else {
            Some((module.value().to_owned(), None))
        }
    }

    pub fn collect_includes(&self) -> Vec<Include> {
        self.module
            .iter()
            .flat_map(|x| x.children())
            .chain(self.maybe_partial_module.iter().flat_map(|x| x.children()))
            .filter_map(|form| {
                if let efmt_core::items::forms::Form::Include(x) = form.get() {
                    Some(Include {
                        is_lib: x.is_include_lib(),
                        path: PathBuf::from(x.include_path().value()),
                    })
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn iter_exports(&self) -> impl '_ + Iterator<Item = Export> {
        self.module
            .iter()
            .flat_map(|x| x.children())
            .chain(self.maybe_partial_module.iter().flat_map(|x| x.children()))
            .filter_map(|form| {
                if let efmt_core::items::forms::Form::Export(x) = form.get() {
                    Some(x)
                } else {
                    None
                }
            })
            .flat_map(|x| {
                x.exports().iter().map(|e| Export {
                    is_type: !x.is_function(),
                    name: e.name().value().to_owned(),

                    // TODO: handle arity parse error
                    arity: e.arity().text(&self.ts.text()).parse().unwrap_or(0),
                })
            })
    }

    pub fn find_definition(&self, target: &Target, strict: bool) -> Option<ItemRange> {
        let text = self.ts.text();
        let mut ctx = FindDefinitionContext::new(&text);
        ctx.check_arity = strict;
        self.module
            .as_ref()
            .and_then(|x| x.find_definition(&ctx, target))
            .or_else(|| {
                self.maybe_partial_module
                    .as_ref()
                    .and_then(|x| x.find_definition(&ctx, target))
            })
    }

    pub fn find_hover_doc(&self, target: &Target) -> Option<String> {
        let text = self.ts.text();
        self.module
            .as_ref()
            .and_then(|x| x.find_hover_doc(&text, target))
            .or_else(|| {
                self.maybe_partial_module
                    .as_ref()
                    .and_then(|x| x.find_hover_doc(&text, target))
            })
    }

    pub fn find_target(&mut self, position: Position) -> Option<Target> {
        let mut candidate = None;
        for macro_call in self.ts.macros().values() {
            if macro_call.contains_inclusive(position) {
                candidate = Some(macro_call.clone());
                break;
            }
            if position < macro_call.start_position() {
                break;
            }
        }
        if let Some(macro_call) = candidate {
            self.find_target_macro_call(position, macro_call)
        } else if let Some(module) = &self.module {
            module.find_target(&self.ts.text(), position)
        } else if let Some(module) = &self.maybe_partial_module {
            module.find_target(&self.ts.text(), position)
        } else if let Some(expr) = &self.expr {
            expr.find_target(&self.ts.text(), position)
        } else {
            None
        }
    }

    fn find_target_macro_call(&mut self, position: Position, macro_call: Macro) -> Option<Target> {
        if !macro_call.contains_inclusive(position) {
            return None;
        }
        if macro_call.macro_name().contains_inclusive(position) {
            return Some(Target::Macro {
                position: macro_call.macro_name().start_position(),
                macro_name: macro_call.macro_name().value().to_owned(),
                arity: macro_call.arity(),
            });
        }
        if let Some(target) = macro_call
            .args()
            .find(|x| x.contains_inclusive(position))
            .and_then(|x| x.parse_expr(&mut self.ts))
            .and_then(|x| x.find_target_if_contains(&self.ts.text(), position))
        {
            return Some(target);
        }
        None
    }
}

#[derive(Debug)]
pub struct FindDefinitionContext<'a> {
    pub text: &'a str,
    pub check_arity: bool,
}

impl<'a> FindDefinitionContext<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            text,
            check_arity: true,
        }
    }
}

pub trait FindDefinition {
    fn find_definition(&self, ctx: &FindDefinitionContext, target: &Target) -> Option<ItemRange>;
}

pub trait FindTarget {
    fn find_target(&self, text: &str, position: Position) -> Option<Target>;

    fn find_target_if_contains(&self, text: &str, position: Position) -> Option<Target>
    where
        Self: Span,
    {
        if self.contains_inclusive(position) {
            self.find_target(text, position)
        } else {
            None
        }
    }
}

pub trait FindHoverDoc {
    fn find_hover_doc(&self, text: &str, target: &Target) -> Option<String>;
}

impl<const ALLOW_PARTIAL_FAILURE: bool> FindTarget
    for efmt_core::items::Module<ALLOW_PARTIAL_FAILURE>
{
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        for form in self.children() {
            if let Some(target) = form.get().find_target_if_contains(text, position) {
                return Some(target);
            }
        }
        None
    }
}

impl<const ALLOW_PARTIAL_FAILURE: bool> FindHoverDoc
    for efmt_core::items::Module<ALLOW_PARTIAL_FAILURE>
{
    fn find_hover_doc(&self, text: &str, target: &Target) -> Option<String> {
        let check_module = |form: &efmt_core::items::Form| {
            let Some(module_name) = target.module_name() else {
                return Some(());
            };
            match form.get() {
                efmt_core::items::forms::Form::Module(module_attr) => {
                    if module_attr.module_name().value() != module_name {
                        return None;
                    }
                }
                _ => {}
            }
            Some(())
        };

        match target {
            Target::Module { .. } => {
                let mut doc = String::new();
                for form in self.children() {
                    check_module(form)?;
                    match form.get() {
                        efmt_core::items::forms::Form::Attr(attr) => {
                            if !(attr.name() == "moduledoc" && attr.values().len() == 1) {
                                continue;
                            }
                            if !doc.is_empty() {
                                doc.push_str("\n---\n");
                            }
                            if let Some(s) = attr.values()[0].as_string() {
                                doc.push_str(s);
                            } else {
                                // Maybe metadata
                                doc.push_str(attr.text(text));
                            }
                        }
                        _ => {}
                    }
                }
                Some(doc)
            }
            Target::Include { .. } | Target::Variable { .. } => return Some("".to_owned()),
            Target::Type { .. }
            | Target::Function { .. }
            | Target::Macro { .. }
            | Target::Record { .. } => {
                let mut target_forms = Vec::new();
                let mut found = false;
                for form in self.children() {
                    check_module(form)?;
                    match form.get() {
                        efmt_core::items::forms::Form::FunSpec(_)
                        | efmt_core::items::forms::Form::Doc(_) => {
                            target_forms.push(form.get());
                            continue;
                        }
                        efmt_core::items::forms::Form::Define(f) => {
                            if matches!(target, Target::Macro { .. })
                                && target.name() == f.macro_name()
                            {
                                target_forms.push(form.get());
                                found = true;
                                break;
                            }
                        }
                        efmt_core::items::forms::Form::FunDecl(f) => {
                            if matches!(target, Target::Function { .. })
                                && f.clauses()
                                    .next()
                                    .is_some_and(|c| target.name() == c.function_name().value())
                            {
                                found = true;
                                break;
                            }
                        }
                        efmt_core::items::forms::Form::TypeDecl(f) => {
                            if matches!(target, Target::Type { .. })
                                && target.name() == f.type_name().value()
                            {
                                target_forms.push(form.get());
                                found = true;
                                break;
                            }
                        }
                        efmt_core::items::forms::Form::RecordDecl(f) => {
                            if matches!(target, Target::Record { .. })
                                && target.name() == f.record_name().value()
                            {
                                target_forms.push(form.get());
                                found = true;
                                break;
                            }
                        }
                        _ => {}
                    }
                    target_forms.clear();
                }

                if !found {
                    if target.module_name().is_some() {
                        return Some("".to_owned());
                    } else {
                        return None;
                    }
                }

                let mut doc = String::new();
                for form in target_forms {
                    if !doc.is_empty() {
                        doc.push_str("\n---\n");
                    }
                    if let efmt_core::items::forms::Form::Doc(form) = form {
                        // TODO: extract markdown
                        doc.push_str(form.text(text));
                    } else {
                        doc.push_str(form.text(text));
                    }
                }
                Some(doc)
            }
            Target::RecordField {
                record_name,
                field_name,
                ..
            } => {
                for form in self.children() {
                    match form.get() {
                        efmt_core::items::forms::Form::RecordDecl(f)
                            if f.record_name().value() == record_name =>
                        {
                            for field in f.fields() {
                                if field.field_name().value() == field_name {
                                    return Some(field.text(text).to_owned());
                                }
                            }
                            break;
                        }
                        _ => {}
                    }
                }
                None
            }
        }
    }
}

impl<const ALLOW_PARTIAL_FAILURE: bool> FindDefinition
    for efmt_core::items::Module<ALLOW_PARTIAL_FAILURE>
{
    fn find_definition(&self, ctx: &FindDefinitionContext, target: &Target) -> Option<ItemRange> {
        self.children()
            .find_map(|x| x.get().find_definition(ctx, target))
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
            Self::Attr(x) => x.find_target_if_contains(text, position),
            Self::Include(x) => x.find_target_if_contains(text, position),
            Self::Doc(x) => x.find_target_if_contains(text, position),
        }
    }
}

impl FindTarget for efmt_core::items::forms::Attr {
    fn find_target(&self, _text: &str, position: Position) -> Option<Target> {
        if matches!(self.name(), "behaviour" | "behavior") {
            return self
                .values()
                .iter()
                .find(|x| x.contains_inclusive(position))
                .and_then(|x| {
                    x.as_atom().map(|name| Target::Module {
                        position: x.start_position(),
                        module_name: name.to_owned(),
                    })
                });
        }
        None
    }
}

impl FindTarget for efmt_core::items::forms::IncludeDirective {
    fn find_target(&self, _text: &str, position: Position) -> Option<Target> {
        if !self.include_path().contains_inclusive(position) {
            return None;
        }
        Some(Target::Include {
            position: self.include_path().start_position(),
            include: Include {
                is_lib: self.is_include_lib(),
                path: PathBuf::from(self.include_path().value()),
            },
        })
    }
}

impl FindDefinition for efmt_core::items::forms::Form {
    fn find_definition(&self, ctx: &FindDefinitionContext, target: &Target) -> Option<ItemRange> {
        match self {
            Self::Module(x) => x.find_definition(ctx, target),
            Self::TypeDecl(x) => x.find_definition(ctx, target),
            Self::FunDecl(x) => x.find_definition(ctx, target),
            Self::Define(x) => x.find_definition(ctx, target),
            Self::RecordDecl(x) => x.find_definition(ctx, target),
            Self::FunSpec(_)
            | Self::Export(_)
            | Self::Include(_)
            | Self::Attr(_)
            | Self::Doc(_) => None,
        }
    }
}

impl FindTarget for efmt_core::items::forms::DefineDirective {
    fn find_target(&self, _text: &str, position: Position) -> Option<Target> {
        if self.macro_name_token().contains_inclusive(position) {
            return Some(Target::Macro {
                position: self.macro_name_token().start_position(),
                macro_name: self.macro_name_token().value().to_owned(),
                arity: self.variables().map(|x| x.len()),
            });
        }
        None
    }
}

impl FindDefinition for efmt_core::items::forms::DefineDirective {
    fn find_definition(&self, ctx: &FindDefinitionContext, target: &Target) -> Option<ItemRange> {
        let Target::Macro {
            macro_name, arity, ..
        } = target
        else {
            return None;
        };

        if self.macro_name() != macro_name {
            return None;
        }

        if ctx.check_arity && self.variables().map(|x| x.len()) != *arity {
            return None;
        }

        Some(item_range(self.macro_name_token()))
    }
}

impl FindTarget for efmt_core::items::forms::RecordDecl {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        if self.record_name().contains_inclusive(position) {
            return Some(Target::Record {
                position: self.record_name().start_position(),
                record_name: self.record_name().value().to_owned(),
            });
        }
        for field in self.fields() {
            if field.field_name().contains_inclusive(position) {
                return Some(Target::RecordField {
                    position: field.field_name().start_position(),
                    record_name: self.record_name().value().to_owned(),
                    field_name: field.field_name().value().to_owned(),
                });
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

impl FindDefinition for efmt_core::items::forms::RecordDecl {
    fn find_definition(&self, _ctx: &FindDefinitionContext, target: &Target) -> Option<ItemRange> {
        let record_name = match target {
            Target::Record { record_name, .. } | Target::RecordField { record_name, .. } => {
                record_name
            }
            _ => return None,
        };

        if record_name != self.record_name().value() {
            return None;
        }

        if let Target::RecordField { field_name, .. } = target {
            for field in self.fields() {
                if field.field_name().value() == field_name {
                    return Some(item_range(field.field_name()));
                }
            }
            None
        } else {
            Some(item_range(self.record_name()))
        }
    }
}

impl FindTarget for efmt_core::items::forms::ExportAttr {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        for e in self.exports() {
            if e.contains_inclusive(position) {
                let position = e.name().start_position();
                let module_name = None;
                let name = e.name().value().to_owned();
                let arity = e.arity().text(text).parse::<usize>().ok()?;
                let target = if self.is_function() {
                    Target::Function {
                        position,
                        module_name,
                        function_name: name,
                        arity,
                        maybe_type: false,
                    }
                } else {
                    Target::Type {
                        position,
                        module_name,
                        type_name: name,
                        arity,
                    }
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
            if clause.function_name().contains_inclusive(position) {
                return Some(Target::Function {
                    position: clause.function_name().start_position(),
                    module_name: None,
                    function_name: clause.function_name().value().to_owned(),
                    arity: clause.params().len(),
                    maybe_type: false,
                });
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
    fn find_definition(&self, ctx: &FindDefinitionContext, target: &Target) -> Option<ItemRange> {
        // TODO: Handle `ItemKind::Variable`

        let Target::Function {
            function_name,
            arity,
            ..
        } = target
        else {
            return None;
        };

        let clause = self.clauses().next()?;
        if clause.function_name().value() != function_name {
            return None;
        }
        if ctx.check_arity && clause.params().len() != *arity {
            return None;
        }
        Some(item_range(clause.function_name()))
    }
}

impl FindTarget for efmt_core::items::forms::FunSpec {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        if let Some(name) = self.module_name() {
            if name.contains_inclusive(position) {
                return Some(Target::Module {
                    position: name.start_position(),
                    module_name: name.value().to_owned(),
                });
            }
        }
        if self.function_name().contains_inclusive(position) {
            return Some(Target::Function {
                position: self.function_name().start_position(),
                module_name: self.module_name().map(|x| x.value().to_owned()),
                function_name: self.function_name().value().to_owned(),
                arity: self.clauses().next()?.params().len(),
                maybe_type: false,
            });
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
        if self.type_name().contains_inclusive(position) {
            return Some(Target::Type {
                position: self.type_name().start_position(),
                module_name: None,
                type_name: self.type_name().value().to_owned(),
                arity: self.params().len(),
            });
        }
        for param in self.params() {
            if param.contains_inclusive(position) && param.value() != "_" {
                return Some(Target::Variable {
                    position: param.start_position(),
                    variable_name: param.value().to_owned(),
                });
            }
        }
        self.type_value().find_target_if_contains(text, position)
    }
}

impl FindDefinition for efmt_core::items::forms::TypeDecl {
    fn find_definition(&self, ctx: &FindDefinitionContext, target: &Target) -> Option<ItemRange> {
        let (type_name, arity) = match target {
            Target::Type {
                type_name, arity, ..
            } => (type_name, arity),
            Target::Function {
                function_name,
                arity,
                maybe_type: true,
                ..
            } => (function_name, arity),
            _ => return None,
        };
        if self.type_name().value() != type_name {
            return None;
        }
        if ctx.check_arity && self.params().len() != *arity {
            return None;
        }
        Some(item_range(self.type_name()))
    }
}

impl FindTarget for efmt_core::items::forms::ModuleAttr {
    fn find_target(&self, _text: &str, position: Position) -> Option<Target> {
        if self.module_name().contains_inclusive(position) {
            Some(Target::Module {
                position: self.module_name().start_position(),
                module_name: self.module_name().value().to_owned(),
            })
        } else {
            None
        }
    }
}

impl FindDefinition for efmt_core::items::forms::ModuleAttr {
    fn find_definition(&self, _ctx: &FindDefinitionContext, target: &Target) -> Option<ItemRange> {
        let Target::Module { module_name, .. } = target else {
            return None;
        };
        if self.module_name().value() != module_name {
            return None;
        }
        Some(item_range(self.module_name()))
    }
}

impl FindTarget for efmt_core::items::forms::DocAttr {
    fn find_target(&self, _text: &str, _position: Position) -> Option<Target> {
        None
    }
}

impl FindDefinition for efmt_core::items::forms::DocAttr {
    fn find_definition(&self, _ctx: &FindDefinitionContext, _target: &Target) -> Option<ItemRange> {
        None
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
        if self.variable().contains_inclusive(position) {
            return Some(Target::Variable {
                position: self.variable().start_position(),
                variable_name: self.variable().value().to_owned(),
            });
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
        if self.name().contains_inclusive(position) {
            return Some(Target::Record {
                position: self.name().start_position(),
                record_name: self.name().value().to_owned(),
            });
        }
        self.fields().find_map(|(name, field)| {
            if name.contains_inclusive(position) {
                Some(Target::RecordField {
                    position: name.start_position(),
                    record_name: self.name().value().to_owned(),
                    field_name: name.value().to_owned(),
                })
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
                if x.contains_inclusive(position) {
                    return Some(Target::Variable {
                        position: x.start_position(),
                        variable_name: x.value().to_owned(),
                    });
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
            if module_name.contains_inclusive(position) {
                return Some(Target::Module {
                    position: module_name.start_position(),
                    module_name: module_name.value().to_owned(),
                });
            }
        }
        if self.type_name().contains_inclusive(position) {
            return Some(Target::Type {
                position: self.type_name().start_position(),
                module_name: self.module_name().map(|x| x.value().to_owned()),
                type_name: self.type_name().value().to_owned(),
                arity: self.args().len(),
            });
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

impl FindTarget for ExprOrMaybeFunCall {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        match self {
            Self::B(x) => x.find_target_if_contains(text, position),
            Self::A((module_name, _, maybe_function_name)) => {
                if module_name.contains_inclusive(position) {
                    return Some(Target::Module {
                        position: module_name.start_position(),
                        module_name: module_name.value().to_owned(),
                    });
                }
                if let Some(function_name) = maybe_function_name.get() {
                    if function_name.contains_inclusive(position) {
                        return Some(Target::Function {
                            position: function_name.start_position(),
                            module_name: Some(module_name.value().to_owned()),
                            function_name: function_name.value().to_owned(),
                            arity: 0,
                            maybe_type: false,
                        });
                    }
                }
                None
            }
        }
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

impl<A: FindTarget> FindTarget for &A {
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
            if x.contains_inclusive(position) {
                return Some(Target::Module {
                    position: x.start_position(),
                    module_name: x.value().to_owned(),
                });
            }
        }
        if let Some(x) = self.function_name() {
            if self.contains_inclusive(position) {
                if let Some(arity) = self.arity().and_then(|x| {
                    x.as_integer_token()
                        .and_then(|x| x.text(text).parse::<usize>().ok())
                }) {
                    return Some(Target::Function {
                        position: x.start_position(),
                        module_name: self.module_name().map(|x| x.value().to_owned()),
                        function_name: x.value().to_owned(),
                        arity,
                        maybe_type: false,
                    });
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
        if self.record_name().contains_inclusive(position) {
            return Some(Target::Record {
                position: self.record_name().start_position(),
                record_name: self.record_name().value().to_owned(),
            });
        }
        for field_name in self.field_names() {
            if field_name.contains_inclusive(position) {
                return Some(Target::RecordField {
                    position: field_name.start_position(),
                    record_name: self.record_name().value().to_owned(),
                    field_name: field_name.value().to_owned(),
                });
            }
        }
        self.children()
            .find_map(|x| x.find_target_if_contains(text, position))
    }
}

impl FindTarget for efmt_core::items::expressions::RecordAccessOrUpdateExpr {
    fn find_target(&self, text: &str, position: Position) -> Option<Target> {
        if self.record_name().contains_inclusive(position) {
            return Some(Target::Record {
                position: self.record_name().start_position(),
                record_name: self.record_name().value().to_owned(),
            });
        }
        for field_name in self.field_names() {
            if field_name.contains_inclusive(position) {
                return Some(Target::RecordField {
                    position: field_name.start_position(),
                    record_name: self.record_name().value().to_owned(),
                    field_name: field_name.value().to_owned(),
                });
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
                if x.contains_inclusive(position) {
                    return Some(Target::Module {
                        position: x.start_position(),
                        module_name: x.value().to_owned(),
                    });
                }
            }
            if let Some(target) = x.find_target_if_contains(text, position) {
                return Some(target);
            }
        }
        if let Some(x) = self.function_expr().as_atom_token() {
            if x.contains_inclusive(position) {
                if self
                    .module_expr()
                    .is_some_and(|x| x.as_atom_token().is_none())
                {
                    return None;
                };
                return Some(Target::Function {
                    position: x.start_position(),
                    module_name: self
                        .module_expr()
                        .and_then(|x| x.as_atom_token())
                        .map(|x| x.value().to_owned()),
                    function_name: x.value().to_owned(),
                    arity: self.args().len(),
                    maybe_type: false,
                });
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
                if x.contains_inclusive(position) && x.value() != "_" {
                    return Some(Target::Variable {
                        position: x.start_position(),
                        variable_name: x.value().to_owned(),
                    });
                }
            }
            Self::Atom(_)
            | Self::Char(_)
            | Self::Float(_)
            | Self::Integer(_)
            | Self::String(_)
            | Self::SigilString(_) => {}
        }
        None
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Target {
    Module {
        position: Position,
        module_name: String,
    },
    Type {
        position: Position,
        module_name: Option<String>,
        type_name: String,
        arity: usize,
    },
    Function {
        position: Position,
        module_name: Option<String>,
        function_name: String,
        arity: usize,
        maybe_type: bool,
    },
    Macro {
        position: Position,
        macro_name: String,
        arity: Option<usize>,
    },
    Record {
        position: Position,
        record_name: String,
    },
    RecordField {
        position: Position,
        record_name: String,
        field_name: String,
    },
    Variable {
        position: Position,
        variable_name: String,
    },
    Include {
        position: Position,
        include: Include,
    },
}

impl Target {
    pub fn name(&self) -> &str {
        match self {
            Target::Module { module_name, .. } => module_name,
            Target::Type { type_name, .. } => type_name,
            Target::Function { function_name, .. } => function_name,
            Target::Macro { macro_name, .. } => macro_name,
            Target::Record { record_name, .. } => record_name,
            Target::RecordField { field_name, .. } => field_name,
            Target::Variable { variable_name, .. } => variable_name,
            Target::Include { .. } => unreachable!(),
        }
    }

    pub fn position(&self) -> Position {
        match self {
            Target::Module { position, .. } => *position,
            Target::Type { position, .. } => *position,
            Target::Function { position, .. } => *position,
            Target::Macro { position, .. } => *position,
            Target::Record { position, .. } => *position,
            Target::RecordField { position, .. } => *position,
            Target::Variable { position, .. } => *position,
            Target::Include { position, .. } => *position,
        }
    }

    pub fn module_name(&self) -> Option<&str> {
        match self {
            Target::Module { module_name, .. } => Some(module_name.as_str()),
            Target::Type { module_name, .. } | Target::Function { module_name, .. } => {
                module_name.as_ref().map(|s| s.as_str())
            }
            Target::Macro { .. }
            | Target::Record { .. }
            | Target::RecordField { .. }
            | Target::Variable { .. }
            | Target::Include { .. } => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Include {
    pub is_lib: bool,
    pub path: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Export {
    pub is_type: bool,
    pub name: String,
    pub arity: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn find_target_works() -> orfail::Result<()> {
        macro_rules! assert_target {
            ($start:expr, $end:expr, $name:expr, $kind:pat, $i:expr, $tree:expr) => {
                if ($start..=$end).contains(&$i) {
                    let target = $tree.find_target(offset($i)).or_fail()?;
                    assert_eq!(target.name(), $name);
                    assert!(matches!(target, $kind));
                    assert_eq!(target.position().offset(), offset($start).offset());
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
            use Target::*;

            assert_target!(8, 11, "foo", Module { .. }, i, tree);
            assert_target!(21, 24, "foo", Type { .. }, i, tree);
            assert_target!(30, 33, "any", Type { .. }, i, tree);
            assert_target!(43, 46, "bar", Type { .. }, i, tree);
            assert_target!(47, 48, "A", Variable { .. }, i, tree);
            assert_target!(54, 55, "A", Variable { .. }, i, tree);
            assert_target!(58, 59, "b", Module { .. }, i, tree);
            assert_target!(60, 61, "b", Type { .. }, i, tree);
            assert_target!(77, 80, "foo", Function { .. }, i, tree);
            assert_target!(81, 84, "foo", Module { .. }, i, tree);
            assert_target!(85, 88, "foo", Type { .. }, i, tree);
            assert_target!(99, 102, "foo", Function { .. }, i, tree);
            assert_target!(103, 104, "A", Variable { .. }, i, tree);
            assert_target!(113, 114, "B", Variable { .. }, i, tree);
            assert_target!(117, 118, "A", Variable { .. }, i, tree);
            assert_target!(128, 130, "io", Module { .. }, i, tree);
            assert_target!(131, 137, "format", Function { .. }, i, tree);
            assert_target!(149, 150, "B", Variable { .. }, i, tree);
            assert_target!(173, 179, "Record", Variable { .. }, i, tree);
            assert_target!(180, 191, "record_name", Record { .. }, i, tree);
            assert_target!(192, 202, "field_name", RecordField { .. }, i, tree);
            assert_target!(210, 213, "rec", Record { .. }, i, tree);
            assert_target!(214, 217, "aaa", RecordField { .. }, i, tree);
            assert_target!(235, 238, "ccc", RecordField { .. }, i, tree);
            assert_target!(246, 247, "A", Variable { .. }, i, tree);
            assert_target!(253, 254, "A", Variable { .. }, i, tree);
            assert_target!(265, 266, "M", Variable { .. }, i, tree);
            assert_target!(288, 289, "C", Variable { .. }, i, tree);
            assert_target!(327, 330, "XXX", Variable { .. }, i, tree);
            assert_target!(334, 337, "baz", Function { .. }, i, tree);
            assert_target!(338, 341, "XXX", Variable { .. }, i, tree);
            assert_target!(370, 375, "foo", Function { .. }, i, tree);
            assert_target!(393, 398, "foo", Type { .. }, i, tree);
            assert_target!(411, 414, "rec", Record { .. }, i, tree);
            assert_target!(418, 421, "aaa", RecordField { .. }, i, tree);
            assert_target!(424, 427, "bbb", Function { .. }, i, tree);
            assert_target!(433, 436, "foo", Type { .. }, i, tree);
            assert_target!(452, 455, "FOO", Macro { .. }, i, tree);
            assert_target!(474, 477, "BAR", Macro { .. }, i, tree);
            assert_target!(489, 492, "bar", Function { .. }, i, tree);
            assert_target!(503, 506, "FOO", Macro { .. }, i, tree);
            assert_target!(510, 513, "BAR", Macro { .. }, i, tree);
            assert_target!(514, 517, "bbb", Function { .. }, i, tree);

            assert_eq!(None, tree.find_target(offset(i)));
        }
        Ok(())
    }

    fn offset(offset: usize) -> Position {
        Position::new(offset, 0, 0)
    }
}
