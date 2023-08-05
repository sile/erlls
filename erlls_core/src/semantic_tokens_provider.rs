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
    // TODO: provider_options() -> serde_json::Value

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
        let mut last_position = tokenizer.next_position();
        let mut semantic_tokens = SemanticTokens::default();
        while let Some(token) = tokenizer.next() {
            let Ok(token) = token else {
                tokenizer.consume_char();
                continue;
            };

            // if token.end_position().line() != token.start_position().line() {
            //     // TODO
            //     continue;
            // }

            let semantic_token = match token {
                erl_tokenize::Token::Atom(_) => {
                    continue;
                }
                erl_tokenize::Token::Comment(_) => {
                    SemanticToken::new(&token, last_position, SemanticTokenType::Comment)
                }
                erl_tokenize::Token::Float(_) | erl_tokenize::Token::Integer(_) => {
                    SemanticToken::new(&token, last_position, SemanticTokenType::Number)
                }
                erl_tokenize::Token::Keyword(_) => {
                    SemanticToken::new(&token, last_position, SemanticTokenType::Keyword)
                }
                erl_tokenize::Token::Char(_) | erl_tokenize::Token::String(_) => {
                    SemanticToken::new(&token, last_position, SemanticTokenType::String)
                }
                erl_tokenize::Token::Symbol(_) => {
                    SemanticToken::new(&token, last_position, SemanticTokenType::Operator)
                }
                erl_tokenize::Token::Variable(_) => {
                    SemanticToken::new(&token, last_position, SemanticTokenType::Variable)
                }
                erl_tokenize::Token::Whitespace(_) => {
                    continue;
                }
            };
            semantic_tokens.data.extend(semantic_token.iter());
            last_position = token.start_position();
        }

        Ok(ResponseMessage::result(semantic_tokens).or_fail()?)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SemanticToken {
    pub delta_line: u32,
    pub delta_start: u32,
    pub length: u32,
    pub token_type: u32,
    pub token_modifiers: u32,
}

impl SemanticToken {
    pub fn new(
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

    pub fn iter(self) -> impl Iterator<Item = u32> {
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
