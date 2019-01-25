use crate::parser::parse_tree::KotlinFile;
use crate::errors::KtError;
use crate::parser::file::read_file;
use crate::parser::token_cursor::get_token_cursor;
use crate::parser::token_cursor::TokenCursor;
use crate::source_code::SourceCode;
use crate::source_code::Span;
use crate::tokenizer::token::Token;

mod file;
mod token_cursor;
pub mod parse_tree;

pub struct Parser(TokenCursor);

impl Parser {
    pub fn new(code: SourceCode, tokens: Vec<(Span, Token)>) -> Self {
        Parser(get_token_cursor(code, tokens))
    }

    pub fn parse_file(&mut self) -> Result<KotlinFile, KtError> {
        self.0.complete(&read_file)
    }
}
