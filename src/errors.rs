use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Error;
use std::fmt::Formatter;
use std::fmt::Write;

use crate::source_code::print_code_location;
use crate::source_code::SourceCode;
use crate::source_code::Span;
use crate::source_code::to_str;
use crate::tokenizer::Literal;
use crate::tokenizer::Token;

#[derive(Debug, Clone)]
pub enum TokenizerError {
    UnknownCharacter(u8),
    UnsupportedLiteralPrefix(char),
    ExpectedEndOfString,
    ExpectedEndOfChar,
    UnclosedComment,
    InvalidEscapeChar(char),
    ExpectedEndOfEscapedIdentifier,
}

#[derive(Debug, Clone)]
pub enum ParserError {
    ExpectedToken { expected: Token, found: Token },
    ExpectedTokenId { found: Token },
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
        TokenizerError::InvalidEscapeChar(c) => {
            write!(f, "Found invalid escape character: '{}' ({})\n", *c, *c as u32)?;
            write!(f, "{}", print_code_location(&to_str(code), span))?;
        }
        TokenizerError::ExpectedEndOfEscapedIdentifier => {
            write!(f, "I was expecting a ` to end the escaped identifier: \n")?;
            write!(f, "{}", print_code_location(&to_str(code), span))?;
        }
    }

    Ok(())
}


fn print_parser_error(f: &mut Write, code: &SourceCode, span: Span, error: &ParserError) -> Result<(), Error> {
    match error {
        ParserError::ExpectedToken { expected, found } => {
            write!(f, "Expecting: {} but found: {}\n", expected, found)?;
            write!(f, "{}", print_code_location(&to_str(code), span))?;
        }
        ParserError::ExpectedTokenId { found } => {
            write!(f, "Expecting identifier but found: {}\n", found)?;
            write!(f, "{}", print_code_location(&to_str(code), span))?;
        }
    }

    Ok(())
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Token::Id(it) => { write!(f, "{}", it)?; }
            Token::Literal(it) => { write!(f, "{}", it)?; }
            Token::LitChar(it) => { write!(f, "{}", it)?; }
            Token::LitString(it) => { write!(f, "{}", it)?; }
            Token::Semicolon => { write!(f, ";")?; }
            Token::Newline => { write!(f, "\\n")?; }
            Token::LeftParen => { write!(f, "(")?; }
            Token::RightParen => { write!(f, ")")?; }
            Token::LeftBrace => { write!(f, "{{")?; }
            Token::RightBrace => { write!(f, "}}")?; }
            Token::LeftBracket => { write!(f, "[")?; }
            Token::RightBracket => { write!(f, "]")?; }
            Token::LeftAngleBracket => { write!(f, "<")?; }
            Token::RightAngleBracket => { write!(f, ">")?; }
            Token::At => { write!(f, "@")?; }
            Token::Colon => { write!(f, ":")?; }
            Token::DoubleColon => { write!(f, "::")?; }
            Token::Dollar => { write!(f, "$")?; }
            Token::Dot => { write!(f, ".")?; }
            Token::DoubleDot => { write!(f, "..")?; }
            Token::Comma => { write!(f, ",")?; }
            Token::QuestionMark => { write!(f, "?")?; }
            Token::ExclamationMark => { write!(f, "!")?; }
            Token::NotEquals => { write!(f, "!=")?; }
            Token::NotDoubleEquals => { write!(f, "!==")?; }
            Token::LeftArrow => { write!(f, "->")?; }
            Token::Elvis => { write!(f, "?:")?; }
            Token::Plus => { write!(f, "+")?; }
            Token::DoublePlus => { write!(f, "++")?; }
            Token::Minus => { write!(f, "-")?; }
            Token::DoubleMinus => { write!(f, "--")?; }
            Token::Asterisk => { write!(f, "*")?; }
            Token::Slash => { write!(f, "/")?; }
            Token::Percent => { write!(f, "%")?; }
            Token::Equals => { write!(f, "=")?; }
            Token::DoubleEquals => { write!(f, "==")?; }
            Token::TripleEquals => { write!(f, "===")?; }
            Token::PlusEquals => { write!(f, "+=")?; }
            Token::MinusEquals => { write!(f, "-=")?; }
            Token::TimesEquals => { write!(f, "*=")?; }
            Token::DivEquals => { write!(f, "/=")?; }
            Token::ModEquals => { write!(f, "%=")?; }
            Token::Ampersand => { write!(f, "&")?; }
            Token::DoubleAmpersand => { write!(f, "&&")?; }
            Token::Pipe => { write!(f, "|")?; }
            Token::DoublePipe => { write!(f, "||")?; }
            Token::Underscore => { write!(f, "_")?; }
            Token::EOF => { write!(f, "EOF")?; }
        }
        Ok(())
    }
}


impl Display for Literal {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Literal::Double(it) => { write!(f, "{}", it) }
            Literal::Float(it) => { write!(f, "{}", it) }
            Literal::Int(it) => { write!(f, "{}", it) }
            Literal::Long(it) => { write!(f, "{}", it) }
        }
    }
}