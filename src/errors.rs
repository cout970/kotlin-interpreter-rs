use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Error;
use std::fmt::Formatter;
use std::fmt::Write;

use crate::source_code::print_code_location;
use crate::source_code::SourceCode;
use crate::source_code::Span;
use crate::source_code::to_str;
use crate::tokenizer::Token;

#[derive(Debug, Clone)]
pub enum TokenizerError {
    UnknownCharacter(u8),
    UnsupportedLiteralPrefix(char),
    ExpectedEndOfString,
    ExpectedEndOfChar,
    UnclosedComment,
    InvalidScapeChar(char),
}

#[derive(Debug, Clone)]
pub enum ParserError {
    ExpectedToken { expected: Token, found: Token },
    ExpectedTokenId { found: Token }
}

#[derive(Clone)]
pub enum KtError {
    Tokenizer { code: SourceCode, span: Span, info: TokenizerError },
    Parser { code: SourceCode, span: Span, info: ParserError },
}

impl Display for KtError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            KtError::Tokenizer { code, span, info } => {
                write!(f, "\n\nAn error occurred: \n")?;
                print_tokenizer_error(f, code, *span, info)?;
                write!(f, "\n\n")?;
            }
            KtError::Parser { code, span, info } => {
                write!(f, "\n\nAn error occurred: \n")?;
                print_parser_error(f, code, *span, info)?;
                write!(f, "\n\n")?;
            }
        }
        Ok(())
    }
}

impl Debug for KtError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self)
    }
}

fn print_tokenizer_error(f: &mut Write, code: &SourceCode, span: Span, error: &TokenizerError) -> Result<(), Error> {
    match error {
        TokenizerError::UnknownCharacter(c) => {
            write!(f, "Found unknown character: '{}' ({})\n", *c as char, *c)?;
            write!(f, "{}", print_code_location(&to_str(code), span))?;
        }
        TokenizerError::UnsupportedLiteralPrefix(c) => {
            write!(f, "Unsupported number prefix: 0{}\n", *c)?;
            write!(f, "{}", print_code_location(&to_str(code), span))?;
        }
        TokenizerError::ExpectedEndOfString => {
            write!(f, "Found end of the line while reading a string\n")?;
            write!(f, "{}", print_code_location(&to_str(code), span))?;
        }
        TokenizerError::ExpectedEndOfChar => {
            write!(f, "Found expecting end of character literal\n")?;
            write!(f, "{}", print_code_location(&to_str(code), span))?;
        }
        TokenizerError::UnclosedComment => {
            write!(f, "Found unclosed comment\n")?;
            write!(f, "{}", print_code_location(&to_str(code), span))?;
        }
        TokenizerError::InvalidScapeChar(c) => {
            write!(f, "Found invalid scape character: '{}' ({})\n", *c, *c as u32)?;
            write!(f, "{}", print_code_location(&to_str(code), span))?;
        }
    }

    Ok(())
}


fn print_parser_error(_f: &mut Write, _code: &SourceCode, _span: Span, _error: &ParserError) -> Result<(), Error> {
//    match error {
//        TokenizerError::UnknownCharacter(c) => {
//            write!(f, "Found unknown character: '{}' ({})\n", *c as char, *c)?;
//            write!(f, "{}", print_code_location(&to_str(code), span))?;
//        }
//        TokenizerError::UnsupportedLiteralPrefix(c) => {
//            write!(f, "Unsupported number prefix: 0{}\n", *c)?;
//            write!(f, "{}", print_code_location(&to_str(code), span))?;
//        }
//        TokenizerError::ExpectedEndOfString => {
//            write!(f, "Found end of the line while reading a string\n")?;
//            write!(f, "{}", print_code_location(&to_str(code), span))?;
//        }
//        TokenizerError::ExpectedEndOfChar => {
//            write!(f, "Found expecting end of character literal\n")?;
//            write!(f, "{}", print_code_location(&to_str(code), span))?;
//        }
//        TokenizerError::UnclosedComment => {
//            write!(f, "Found unclosed comment\n")?;
//            write!(f, "{}", print_code_location(&to_str(code), span))?;
//        }
//        TokenizerError::InvalidScapeChar(c) => {
//            write!(f, "Found invalid scape character: '{}' ({})\n", *c, *c as u32)?;
//            write!(f, "{}", print_code_location(&to_str(code), span))?;
//        }
//    }

    Ok(())
}