use std::collections::VecDeque;

use crate::{
    document::DocumentRepository,
    error::ResponseError,
    fs::FileSystem,
    message::{
        ResponseMessage, SemanticTokenType, SemanticTokens, SemanticTokensParams,
        SemanticTokensRangeParams,
    },
};
use erl_tokenize::Position as TokenizePosition;
use erl_tokenize::{values::Symbol, PositionRange};
use orfail::OrFail;

#[derive(Debug)]
pub struct SemanticTokensProvider;

impl SemanticTokensProvider {
    pub fn options() -> serde_json::Value {
        serde_json::json!({
            "legend": {
                "tokenTypes": [
                    "comment",
                    "string",
                    "keyword",
                    "variable",
                    "number",
                    "class",
                    "function",
                    "macro",
                    "struct"
                ],
                "tokenModifiers": []
            },
            "range": true,
            "full": true
        })
    }

    pub fn handle_full_request<FS: FileSystem>(
        &mut self,
        params: SemanticTokensParams,
        documents: &DocumentRepository<FS>,
    ) -> Result<ResponseMessage, ResponseError> {
        let doc = documents
            .get_from_editings(&params.text_document.uri)
            .or_fail()?;
        self.handle_range_request(
            SemanticTokensRangeParams {
                text_document: params.text_document,
                range: doc.text.range(),
            },
            documents,
        )
    }

    pub fn handle_range_request<FS: FileSystem>(
        &mut self,
        params: SemanticTokensRangeParams,
        documents: &DocumentRepository<FS>,
    ) -> Result<ResponseMessage, ResponseError> {
        let doc = documents
            .get_from_editings(&params.text_document.uri)
            .or_fail()?;
        let text = doc.text.to_range_string(params.range);

        let mut tokenizer = erl_tokenize::Tokenizer::new(&text);
        let mut semantic_tokens = SemanticTokens::default();
        let mut highlighter = Highlighter::new();
        while let Some(token) = tokenizer.next() {
            let Ok(token) = token else {
                tokenizer.consume_char();
                continue;
            };

            if let Some(token) = highlighter.handle_token(token) {
                semantic_tokens.data.extend(token.iter());
            };
        }

        Ok(ResponseMessage::result(semantic_tokens).or_fail()?)
    }
}

#[derive(Debug)]
struct Highlighter {
    last_start_position: TokenizePosition,
    prev_tokens: VecDeque<erl_tokenize::Token>,
}

impl Highlighter {
    fn new() -> Self {
        Self {
            last_start_position: Default::default(),
            prev_tokens: VecDeque::new(),
        }
    }

    fn make_semantic_token(
        &mut self,
        token: &impl PositionRange,
        ty: SemanticTokenType,
    ) -> SemanticToken {
        let semantic_token = SemanticToken::new(token, self.last_start_position.clone(), ty);
        self.last_start_position = token.start_position();
        semantic_token
    }

    fn handle_token(&mut self, token: erl_tokenize::Token) -> Option<SemanticToken> {
        let result = match &token {
            erl_tokenize::Token::Whitespace(_) => None,
            erl_tokenize::Token::Comment(_) => {
                Some(self.make_semantic_token(&token, SemanticTokenType::Comment))
            }
            erl_tokenize::Token::Float(_) | erl_tokenize::Token::Integer(_) => {
                Some(self.make_semantic_token(&token, SemanticTokenType::Number))
            }
            erl_tokenize::Token::Keyword(_) => {
                Some(self.make_semantic_token(&token, SemanticTokenType::Keyword))
            }
            erl_tokenize::Token::Char(_) | erl_tokenize::Token::String(_) => {
                Some(self.make_semantic_token(&token, SemanticTokenType::String))
            }
            erl_tokenize::Token::Symbol(token) => self.handle_symbol_token(token),
            erl_tokenize::Token::Variable(token) => self.handle_variable_token(token),
            erl_tokenize::Token::Atom(token) => self.handle_atom_token(token),
        };

        if !matches!(
            token,
            erl_tokenize::Token::Whitespace(_) | erl_tokenize::Token::Comment(_)
        ) {
            self.prev_tokens.push_back(token);
            if self.prev_tokens.len() > 10 {
                self.prev_tokens.pop_front();
            }
        }

        result
    }

    fn handle_symbol_token(
        &mut self,
        token: &erl_tokenize::tokens::SymbolToken,
    ) -> Option<SemanticToken> {
        if token.value() == Symbol::Slash || token.value() == Symbol::OpenParen {
            if let Some(token) = self
                .prev_tokens
                .back()
                .and_then(|t| t.as_atom_token())
                .cloned()
            {
                return Some(self.make_semantic_token(&token, SemanticTokenType::Function));
            }
        }

        if token.value() == Symbol::Colon {
            if let Some(token) = self
                .prev_tokens
                .back()
                .and_then(|t| t.as_atom_token())
                .cloned()
            {
                return Some(self.make_semantic_token(&token, SemanticTokenType::Class));
            }
        }

        None
    }

    fn handle_variable_token(
        &mut self,
        token: &erl_tokenize::tokens::VariableToken,
    ) -> Option<SemanticToken> {
        if self.is_symbol_eq(0, Symbol::Question) {
            return Some(self.make_semantic_token(token, SemanticTokenType::Macro));
        }

        if self.is_symbol_eq(0, Symbol::OpenParen) && self.is_atom_eq(1, "define") {
            return Some(self.make_semantic_token(token, SemanticTokenType::Macro));
        }

        Some(self.make_semantic_token(token, SemanticTokenType::Variable))
    }

    fn handle_atom_token(
        &mut self,
        token: &erl_tokenize::tokens::AtomToken,
    ) -> Option<SemanticToken> {
        if self.is_symbol_eq(0, Symbol::Hyphen) || self.is_symbol_eq(0, Symbol::Slash) {
            return Some(self.make_semantic_token(token, SemanticTokenType::Keyword));
        }

        if self.is_symbol_eq(0, Symbol::OpenParen)
            && (self.is_atom_eq(1, "module")
                || self.is_atom_eq(1, "behaviour")
                || self.is_atom_eq(1, "behavior"))
        {
            return Some(self.make_semantic_token(token, SemanticTokenType::Class));
        }

        if self.is_symbol_eq(0, Symbol::OpenParen) && self.is_atom_eq(1, "record") {
            return Some(self.make_semantic_token(token, SemanticTokenType::Struct));
        }

        if self.is_symbol_eq(0, Symbol::OpenParen) && self.is_atom_eq(1, "define") {
            return Some(self.make_semantic_token(token, SemanticTokenType::Macro));
        }

        if self.is_symbol_eq(0, Symbol::Question) {
            return Some(self.make_semantic_token(token, SemanticTokenType::Macro));
        }

        if self.is_symbol_eq(0, Symbol::Sharp) {
            return Some(self.make_semantic_token(token, SemanticTokenType::Struct));
        }

        None
    }

    fn is_symbol_eq(&self, prev_n: usize, symbol: Symbol) -> bool {
        self.prev_tokens
            .iter()
            .rev()
            .nth(prev_n)
            .and_then(|token| token.as_symbol_token())
            .map_or(false, |token| token.value() == symbol)
    }

    fn is_atom_eq(&self, prev_n: usize, atom: &str) -> bool {
        self.prev_tokens
            .iter()
            .rev()
            .nth(prev_n)
            .and_then(|token| token.as_atom_token())
            .map_or(false, |token| token.value() == atom)
    }
}

#[derive(Debug, Clone, Copy)]
struct SemanticToken {
    pub delta_line: u32,
    pub delta_start: u32,
    pub length: u32,
    pub token_type: u32,
    pub token_modifiers: u32,
}

impl SemanticToken {
    fn new(
        token: &impl PositionRange,
        last_position: TokenizePosition,
        ty: SemanticTokenType,
    ) -> Self {
        let token_type = match ty {
            SemanticTokenType::Comment => 0,
            SemanticTokenType::String => 1,
            SemanticTokenType::Keyword => 2,
            SemanticTokenType::Variable => 3,
            SemanticTokenType::Number => 4,
            SemanticTokenType::Class => 5,
            SemanticTokenType::Function => 6,
            SemanticTokenType::Macro => 7,
            SemanticTokenType::Struct => 8,
            _ => unreachable!(),
        };
        let delta_line = (token.start_position().line() - last_position.line()) as u32;
        let delta_start = if delta_line == 0 {
            (token.start_position().column() - last_position.column()) as u32
        } else {
            token.start_position().column() as u32 - 1
        };
        Self {
            delta_line,
            delta_start,
            length: (token.end_position().offset() - token.start_position().offset()) as u32,
            token_type,
            token_modifiers: 0,
        }
    }

    fn iter(self) -> impl Iterator<Item = u32> {
        [
            self.delta_line,
            self.delta_start,
            self.length,
            self.token_type,
            self.token_modifiers,
        ]
        .into_iter()
    }
}
