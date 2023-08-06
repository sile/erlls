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
use erl_tokenize::PositionRange;
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
                    "operator",
                    "namespace"
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
        documents: &mut DocumentRepository<FS>,
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
        documents: &mut DocumentRepository<FS>,
    ) -> Result<ResponseMessage, ResponseError> {
        let doc = documents
            .get_from_editings(&params.text_document.uri)
            .or_fail()?;
        let text = doc.text.to_range_string(params.range);

        let mut tokenizer = erl_tokenize::Tokenizer::new(&text);
        let mut semantic_tokens = SemanticTokens::default();
        let mut highlighter = Highlighter::default();
        while let Some(token) = tokenizer.next() {
            let Ok(token) = token else {
                tokenizer.consume_char();
                continue;
            };

            if let Some(token) = highlighter.handle_raw_token(token) {
                semantic_tokens.data.extend(token.iter());
            };
        }

        Ok(ResponseMessage::result(semantic_tokens).or_fail()?)
    }
}

#[derive(Debug, Default)]
struct Highlighter {
    last_start_position: TokenizePosition,
    prev_tokens: Vec<erl_tokenize::Token>,
    next_atom_is_module: bool,
}

impl Highlighter {
    fn handle_raw_token(&mut self, token: erl_tokenize::Token) -> Option<SemanticToken> {
        let last = self.last_start_position.clone();
        let semantic_token = match &token {
            erl_tokenize::Token::Comment(_) => {
                self.prev_tokens.clear();
                SemanticToken::new(&token, last, SemanticTokenType::Comment)
            }
            erl_tokenize::Token::Float(_) | erl_tokenize::Token::Integer(_) => {
                self.prev_tokens.clear();
                SemanticToken::new(&token, last, SemanticTokenType::Number)
            }
            erl_tokenize::Token::Keyword(_) => {
                self.prev_tokens.clear();
                SemanticToken::new(&token, last, SemanticTokenType::Keyword)
            }
            erl_tokenize::Token::Char(_) | erl_tokenize::Token::String(_) => {
                SemanticToken::new(&token, last, SemanticTokenType::String)
            }
            erl_tokenize::Token::Symbol(_) => {
                self.prev_tokens.push(token);
                return None;
            }
            erl_tokenize::Token::Variable(_) => {
                SemanticToken::new(&token, last, SemanticTokenType::Variable)
            }
            erl_tokenize::Token::Whitespace(_) => {
                self.prev_tokens.clear();
                return None;
            }
            erl_tokenize::Token::Atom(x) => {
                let ty = self.handle_atom(x)?;
                SemanticToken::new(&token, last, ty)
            }
        };
        self.last_start_position = token.start_position();
        Some(semantic_token)
    }

    fn handle_atom(&mut self, a: &erl_tokenize::tokens::AtomToken) -> Option<SemanticTokenType> {
        use erl_tokenize::values::Symbol;
        use erl_tokenize::Token;

        if self.next_atom_is_module {
            self.next_atom_is_module = false;
            self.prev_tokens.clear();
            return Some(SemanticTokenType::Namespace);
        }

        let Some(Token::Symbol(s)) = self.prev_tokens.first() else {
            return None;
        };
        if s.value() == Symbol::Hyphen {
            self.prev_tokens.clear();
            if a.value() == "module" {
                self.next_atom_is_module = true;
            }
            Some(SemanticTokenType::Keyword)
        } else {
            None
        }
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
            SemanticTokenType::Operator => 5,
            SemanticTokenType::Namespace => 6,
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
