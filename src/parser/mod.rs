use crate::errors::KtError;
use crate::parser::file::read_file;
use crate::parser::parse_tree::KotlinFile;
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
    pub fn parse_kotlin_file(code: SourceCode, tokens: Vec<(Span, Token)>) -> Result<KotlinFile, KtError> {
        get_token_cursor(code, tokens).complete(&read_file)
    }
}
