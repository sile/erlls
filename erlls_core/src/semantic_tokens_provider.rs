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
use erl_tokenize::{values::Keyword, Position as TokenizePosition};
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
                    "struct",
                    "operator",
                    "property"
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
        let mut highlighter = Highlighter::new(&text);
        while let Some(token) = tokenizer.next() {
            let Ok(token) = token else {
                tokenizer.consume_char();
                continue;
            };

            highlighter.handle_token(token);
        }

        Ok(ResponseMessage::result(highlighter.semantic_tokens).or_fail()?)
    }
}

#[derive(Debug, Clone, Copy)]
enum BlockType {
    Block,
    Paren,
    Brace,
    Square,
    BitString,
}

#[derive(Debug)]
struct Highlighter<'a> {
    lines: Vec<&'a str>,
    last_start_position: CharPosition,
    prev_tokens: VecDeque<erl_tokenize::Token>,
    semantic_tokens: SemanticTokens,
    block_stack: Vec<BlockType>,
}

impl<'a> Highlighter<'a> {
    fn new(text: &'a str) -> Self {
        Self {
            lines: text.lines().collect(),
            last_start_position: Default::default(),
            prev_tokens: VecDeque::new(),
            semantic_tokens: SemanticTokens::default(),
            block_stack: Vec::new(),
        }
    }

    fn to_char_position(&self, pos: TokenizePosition) -> CharPosition {
        let line = pos.line() as u32 - 1;
        let column = self.lines[line as usize][..pos.column() - 1]
            .chars()
            .count() as u32;
        CharPosition { line, column }
    }

    fn add_semantic_token(&mut self, token: &impl PositionRange, ty: SemanticTokenType) {
        let mut start_position = self.to_char_position(token.start_position());
        let end_position = self.to_char_position(token.end_position());
        while start_position.line <= end_position.line {
            let end_position = if start_position.line == end_position.line {
                end_position
            } else {
                let line = start_position.line;
                let column = self.lines[line as usize].chars().count() as u32;
                CharPosition { line, column }
            };
            let semantic_token =
                SemanticToken::new(start_position, end_position, self.last_start_position, ty);
            self.last_start_position = start_position;
            self.semantic_tokens.data.extend(semantic_token.iter());

            start_position.line += 1;
            start_position.column = 0;
        }
    }

    fn handle_token(&mut self, token: erl_tokenize::Token) {
        match &token {
            erl_tokenize::Token::Whitespace(_) => {}
            erl_tokenize::Token::Comment(_) => {
                self.add_semantic_token(&token, SemanticTokenType::Comment);
            }
            erl_tokenize::Token::Float(_) | erl_tokenize::Token::Integer(_) => {
                self.add_semantic_token(&token, SemanticTokenType::Number);
            }
            erl_tokenize::Token::Keyword(token) => {
                self.handle_keyword_token(token);
            }
            erl_tokenize::Token::Char(_)
            | erl_tokenize::Token::String(_)
            | erl_tokenize::Token::SigilString(_) => {
                self.add_semantic_token(&token, SemanticTokenType::String);
            }
            erl_tokenize::Token::Symbol(token) => self.handle_symbol_token(token),
            erl_tokenize::Token::Variable(token) => self.handle_variable_token(token),
            erl_tokenize::Token::Atom(token) => self.handle_atom_token(token),
        }

        if !matches!(
            token,
            erl_tokenize::Token::Whitespace(_) | erl_tokenize::Token::Comment(_)
        ) {
            self.prev_tokens.push_back(token);
            if self.prev_tokens.len() > 10 {
                self.prev_tokens.pop_front();
            }
        }
    }

    fn handle_keyword_token(&mut self, token: &erl_tokenize::tokens::KeywordToken) {
        match token.value() {
            Keyword::Begin | Keyword::Case | Keyword::Try | Keyword::Receive | Keyword::Maybe => {
                self.block_stack.push(BlockType::Block);
            }
            Keyword::End => {
                // TODO: Check the block type
                self.block_stack.pop();
            }
            _ => {}
        }

        self.add_semantic_token(token, SemanticTokenType::Keyword);
    }

    fn handle_symbol_token(&mut self, token: &erl_tokenize::tokens::SymbolToken) {
        match token.value() {
            Symbol::OpenParen => self.block_stack.push(BlockType::Paren),
            Symbol::OpenBrace => self.block_stack.push(BlockType::Brace),
            Symbol::OpenSquare => self.block_stack.push(BlockType::Square),
            Symbol::DoubleLeftAngle => self.block_stack.push(BlockType::BitString),
            Symbol::CloseParen
            | Symbol::CloseBrace
            | Symbol::CloseSquare
            | Symbol::DoubleRightAngle => {
                // TODO: Check the block type
                self.block_stack.pop();
            }
            _ => {}
        }

        if token.value() == Symbol::Slash || token.value() == Symbol::OpenParen {
            if let Some(token) = self
                .prev_tokens
                .back()
                .and_then(|t| t.as_atom_token())
                .cloned()
            {
                if !(self.is_symbol_eq(1, Symbol::Hyphen)
                    && (self.is_symbol_eq(2, Symbol::Dot) || self.prev_tokens.len() == 2))
                {
                    self.add_semantic_token(&token, SemanticTokenType::Function);
                    return;
                }
            }
        }

        if token.value() == Symbol::Colon {
            if let Some(token) = self
                .prev_tokens
                .back()
                .and_then(|t| t.as_atom_token())
                .cloned()
            {
                self.add_semantic_token(&token, SemanticTokenType::Class);
                return;
            }
        }

        if matches!(token.value(), Symbol::MapMatch | Symbol::DoubleRightArrow) {
            if let Some(token) = self
                .prev_tokens
                .back()
                .and_then(|t| t.as_atom_token())
                .cloned()
            {
                self.add_semantic_token(&token, SemanticTokenType::Property);
            }
        } else if matches!(token.value(), Symbol::Match | Symbol::DoubleColon)
            && matches!(self.block_stack.last(), Some(BlockType::Brace))
        {
            if let Some(token) = self
                .prev_tokens
                .back()
                .and_then(|t| t.as_atom_token())
                .cloned()
            {
                self.add_semantic_token(&token, SemanticTokenType::Property);
            }
        }

        if matches!(
            token.value(),
            Symbol::Slash
                | Symbol::Match
                | Symbol::MapMatch
                | Symbol::VerticalBar
                | Symbol::DoubleVerticalBar
                | Symbol::MaybeMatch
                | Symbol::Not
                | Symbol::Hyphen
                | Symbol::MinusMinus
                | Symbol::Plus
                | Symbol::PlusPlus
                | Symbol::Multiply
                | Symbol::RightArrow
                | Symbol::LeftArrow
                | Symbol::DoubleRightArrow
                | Symbol::DoubleLeftArrow
                | Symbol::DoubleRightAngle
                | Symbol::DoubleLeftAngle
                | Symbol::Eq
                | Symbol::GreaterEq
                | Symbol::Less
                | Symbol::LessEq
        ) {
            self.add_semantic_token(token, SemanticTokenType::Operator);
        }
    }

    fn handle_variable_token(&mut self, token: &erl_tokenize::tokens::VariableToken) {
        if self.is_symbol_eq(0, Symbol::Question) {
            self.add_semantic_token(token, SemanticTokenType::Macro);
            return;
        }

        if self.is_symbol_eq(0, Symbol::OpenParen) && self.is_atom_eq(1, "define") {
            self.add_semantic_token(token, SemanticTokenType::Macro);
            return;
        }

        self.add_semantic_token(token, SemanticTokenType::Variable);
    }

    fn handle_atom_token(&mut self, token: &erl_tokenize::tokens::AtomToken) {
        if self.is_symbol_eq(0, Symbol::Hyphen) || self.is_symbol_eq(0, Symbol::Slash) {
            self.add_semantic_token(token, SemanticTokenType::Keyword);
            return;
        }

        if self.is_symbol_eq(0, Symbol::OpenParen)
            && (self.is_atom_eq(1, "module")
                || self.is_atom_eq(1, "behaviour")
                || self.is_atom_eq(1, "behavior"))
        {
            self.add_semantic_token(token, SemanticTokenType::Class);
            return;
        }

        if self.is_symbol_eq(0, Symbol::OpenParen) && self.is_atom_eq(1, "record") {
            self.add_semantic_token(token, SemanticTokenType::Struct);
            return;
        }

        if self.is_symbol_eq(0, Symbol::OpenParen) && self.is_atom_eq(1, "define") {
            self.add_semantic_token(token, SemanticTokenType::Macro);
            return;
        }

        if self.is_symbol_eq(0, Symbol::Question) {
            self.add_semantic_token(token, SemanticTokenType::Macro);
            return;
        }

        if self.is_symbol_eq(0, Symbol::Sharp) {
            self.add_semantic_token(token, SemanticTokenType::Struct);
            return;
        }

        if self.is_symbol_eq(0, Symbol::Dot)
            && self
                .prev_tokens
                .back()
                .is_some_and(|t| t.end_position().line() == token.start_position().line())
        {
            self.add_semantic_token(token, SemanticTokenType::Property);
        }
    }

    fn is_symbol_eq(&self, prev_n: usize, symbol: Symbol) -> bool {
        self.prev_tokens
            .iter()
            .rev()
            .nth(prev_n)
            .and_then(|token| token.as_symbol_token())
            .is_some_and(|token| token.value() == symbol)
    }

    fn is_atom_eq(&self, prev_n: usize, atom: &str) -> bool {
        self.prev_tokens
            .iter()
            .rev()
            .nth(prev_n)
            .and_then(|token| token.as_atom_token())
            .is_some_and(|token| token.value() == atom)
    }
}

#[derive(Debug, Default, Clone, Copy)]
struct CharPosition {
    line: u32,   // 0 origin
    column: u32, // 0 origin, UTF-16 based position
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
        start_position: CharPosition,
        end_position: CharPosition,
        last_position: CharPosition,
        ty: SemanticTokenType,
    ) -> Self {
        assert_eq!(start_position.line, end_position.line);
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
            SemanticTokenType::Operator => 9,
            SemanticTokenType::Property => 10,
            _ => unreachable!(),
        };
        let delta_line = (start_position.line - last_position.line) as u32;
        let delta_start = if delta_line == 0 {
            start_position.column - last_position.column
        } else {
            start_position.column
        };
        Self {
            delta_line,
            delta_start,
            length: end_position.column - start_position.column,
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
