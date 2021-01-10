use std::string::ParseError;

use crate::errors::KtError;
use crate::parser::parse_tree::KotlinFile;
use crate::parser::token_cursor::TokenCursor;
use crate::source::{Source, SourceSpan};

use crate::source_cursor::SourceCursor;
use crate::token_stream::{TokenStream, TokenStreamErrorKind};
use crate::token::Token;

// mod file;
mod token_cursor;
pub mod parse_tree;

pub struct Parser {
    cursor: TokenCursor,
    errors: Vec<ParserError>,
}

#[derive(Debug, Clone)]
pub struct ParserError {
    pub span: SourceSpan,
    pub kind: ParserErrorKind,
}

#[derive(Debug, Clone)]
pub enum ParserErrorKind {
    UnexpectedToken { found: Token },
    ExpectedToken { expected: Token, found: Token },
    ExpectedTokenId { found: Token },
    ExpectedTokenOf { found: Token, options: Vec<Token> },
    TokenStreamError { kind: TokenStreamErrorKind },
}

impl Parser {
    pub fn new(cursor: TokenCursor) -> Self {
        Self { cursor, errors: vec![] }
    }

    pub fn from(source: Source) -> Parser {
        Self::new(
            TokenCursor::new(
                TokenStream::new(
                    SourceCursor::new(source)
                )
            )
        )
    }

    pub fn parse_file(self) -> Result<KotlinFile, Vec<ParseError>> {
        todo!()
        // Ok(self.cursor.complete(Self::parse_file))
    }
}
