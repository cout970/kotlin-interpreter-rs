use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Error;
use std::fmt::Formatter;
use std::fmt::Write;

use crate::ast::Modifier;
use crate::Number;
use crate::source_code::print_code_location;
use crate::source_code::SourceCode;
use crate::source_code::Span;
use crate::source_code::to_str;
use crate::tokenizer::token::Token;

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
    UnexpectedToken { found: Token },
    ExpectedToken { expected: Token, found: Token },
    ExpectedTokenId { found: Token },
    ExpectedTokenOf { found: Token, options: Vec<Token> },
}

#[derive(Debug, Clone)]
pub enum AnalyserError {
    InvalidModifierUsage { modifier: Modifier, context: String },
    DuplicatedModifier { modifier: Modifier },
    ConflictingImport { name: String },
}

#[derive(Debug, Clone)]
pub enum InterpreterError {}

#[derive(Clone)]
pub enum KtError {
    Tokenizer { code: SourceCode, span: Span, info: TokenizerError },
    Parser { code: SourceCode, span: Span, info: ParserError },
    Analyser { code: SourceCode, span: Span, info: AnalyserError },
    Interpreter { code: SourceCode, span: Span, info: InterpreterError },
    Unimplemented,
}

impl Display for KtError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            KtError::Unimplemented => {
                write!(f, "\n\nUnimplemented\n")?;
            }
            KtError::Tokenizer { code, span, info } => {
                // Use color red
                write!(f, "\x1B[31m")?;

                write!(f, "\n\nAn error occurred: \n")?;
                print_tokenizer_error(f, code, *span, info)?;
                write!(f, "\n\n")?;

                // Reset color
                write!(f, "\x1B[0m")?;
            }
            KtError::Parser { code, span, info } => {
                // Use color red
                write!(f, "\x1B[31m")?;

                write!(f, "\n\nAn error occurred: \n")?;
                print_parser_error(f, code, *span, info)?;
                write!(f, "\n\n")?;

                // Reset color
                write!(f, "\x1B[0m")?;
            }
            KtError::Analyser { code, span, info } => {
                // Use color red
                write!(f, "\x1B[31m")?;

                write!(f, "\n\nAn error occurred: \n")?;
                print_analyser_error(f, code, *span, info)?;
                write!(f, "\n\n")?;

                // Reset color
                write!(f, "\x1B[0m")?;
            }
            KtError::Interpreter { code, span, info } => {
                // Use color red
                write!(f, "\x1B[31m")?;

                write!(f, "\n\nAn error occurred: \n")?;
                print_interpreter_error(f, code, *span, info)?;
                write!(f, "\n\n")?;

                // Reset color
                write!(f, "\x1B[0m")?;
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
        }
        TokenizerError::UnsupportedLiteralPrefix(c) => {
            write!(f, "Unsupported number prefix: 0{}\n", *c)?;
        }
        TokenizerError::ExpectedEndOfString => {
            write!(f, "Found end of the line while reading a string\n")?;
        }
        TokenizerError::ExpectedEndOfChar => {
            write!(f, "Found expecting end of character literal\n")?;
        }
        TokenizerError::UnclosedComment => {
            write!(f, "Found unclosed comment\n")?;
        }
        TokenizerError::InvalidEscapeChar(c) => {
            write!(f, "Found invalid escape character: '{}' ({})\n", *c, *c as u32)?;
        }
        TokenizerError::ExpectedEndOfEscapedIdentifier => {
            write!(f, "I was expecting a ` to end the escaped identifier: \n")?;
        }
    }

    write!(f, "{}", print_code_location(&to_str(code), span))?;
    Ok(())
}

fn print_analyser_error(f: &mut Write, code: &SourceCode, span: Span, error: &AnalyserError) -> Result<(), Error> {
    match error {
        AnalyserError::InvalidModifierUsage { modifier, context } => {
            write!(f, "Modifier '{}' is not applicable in {}\n", modifier.name, context)?;
        }
        AnalyserError::ConflictingImport { name } => {
            write!(f, "Conflicting import, imported name '{}' is ambiguous\n", name)?;
        }
        AnalyserError::DuplicatedModifier { modifier } => {
            write!(f, "Duplicated modifier '{}'\n", modifier.name)?;
        }
    }

    write!(f, "{}", print_code_location(&to_str(code), span))?;
    Ok(())
}

fn print_interpreter_error(f: &mut Write, code: &SourceCode, span: Span, error: &InterpreterError) -> Result<(), Error> {
    match error {
        _ => {}
    }

    Ok(())
}

fn print_parser_error(f: &mut Write, code: &SourceCode, span: Span, error: &ParserError) -> Result<(), Error> {
    match error {
        ParserError::UnexpectedToken { found } => {
            write!(f, "Found unexpected token: {}\n", found)?;
        }
        ParserError::ExpectedToken { expected, found } => {
            write!(f, "Expecting: {} but found: {}\n", expected, found)?;
        }
        ParserError::ExpectedTokenId { found } => {
            write!(f, "Expecting identifier but found: {}\n", found)?;
        }
        ParserError::ExpectedTokenOf { found, options } => {
            write!(f, "Found token {} but I was expecting one of:\n", found)?;
            for x in options {
                write!(f, " - {}\n", x)?;
            }
        }
    }

    write!(f, "{}", print_code_location(&to_str(code), span))?;
    Ok(())
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Token::Id(it) => { write!(f, "{}", it)?; }
            Token::Number(it) => { write!(f, "{}", it)?; }
            Token::Char(it) => { write!(f, "{}", it)?; }
            Token::StringStart => { write!(f, "\"")?; }
            Token::StringEnd => { write!(f, "\"")?; }
            Token::StringTemplateStart => { write!(f, "${{")?; }
            Token::StringTemplateEnd => { write!(f, "}}")?; }
            Token::StringContent(it) => { write!(f, "{}", it)?; }
            Token::StringVariable(it) => { write!(f, "${}", it)?; }
            Token::Semicolon => { write!(f, ";")?; }
            Token::LeftParen => { write!(f, "(")?; }
            Token::RightParen => { write!(f, ")")?; }
            Token::LeftBrace => { write!(f, "{{")?; }
            Token::RightBrace => { write!(f, "}}")?; }
            Token::LeftBracket => { write!(f, "[")?; }
            Token::RightBracket => { write!(f, "]")?; }
            Token::LeftAngleBracket => { write!(f, "<")?; }
            Token::RightAngleBracket => { write!(f, ">")?; }
            Token::LessEquals => { write!(f, "<=")?; }
            Token::GreaterEquals => { write!(f, ">=")?; }
            Token::At => { write!(f, "@")?; }
            Token::Colon => { write!(f, ":")?; }
            Token::DoubleColon => { write!(f, "::")?; }
            Token::Dollar => { write!(f, "$")?; }
            Token::Dot => { write!(f, ".")?; }
            Token::DoubleDot => { write!(f, "..")?; }
            Token::Comma => { write!(f, ",")?; }
            Token::QuestionMark => { write!(f, "?")?; }
            Token::SafeDot => { write!(f, "?.")?; }
            Token::ExclamationMark => { write!(f, "!")?; }
            Token::DoubleExclamationMark => { write!(f, "!!")?; }
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
            Token::As => { write!(f, "as")?; }
            Token::AsQuestionMark => { write!(f, "as?")?; }
            Token::Break => { write!(f, "break")?; }
            Token::Class => { write!(f, "class")?; }
            Token::Continue => { write!(f, "continue")?; }
            Token::Do => { write!(f, "do")?; }
            Token::Else => { write!(f, "else")?; }
            Token::False => { write!(f, "false")?; }
            Token::For => { write!(f, "for")?; }
            Token::Fun => { write!(f, "fun")?; }
            Token::If => { write!(f, "if")?; }
            Token::In => { write!(f, "in")?; }
            Token::NotIn => { write!(f, "!in")?; }
            Token::Is => { write!(f, "is")?; }
            Token::NotIs => { write!(f, "!is")?; }
            Token::Interface => { write!(f, "interface")?; }
            Token::Null => { write!(f, "null")?; }
            Token::Object => { write!(f, "object")?; }
            Token::Package => { write!(f, "package")?; }
            Token::Return => { write!(f, "return")?; }
            Token::Super => { write!(f, "super")?; }
            Token::This => { write!(f, "this")?; }
            Token::Throw => { write!(f, "throw")?; }
            Token::True => { write!(f, "true")?; }
            Token::Try => { write!(f, "try")?; }
            Token::TypeAlias => { write!(f, "typealias")?; }
            Token::Val => { write!(f, "val")?; }
            Token::Var => { write!(f, "var")?; }
            Token::When => { write!(f, "when")?; }
            Token::While => { write!(f, "while")?; }
            Token::EOF => { write!(f, "EOF")?; }
        }
        Ok(())
    }
}


impl Display for Number {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Number::Double(it) => { write!(f, "{}", it) }
            Number::Float(it) => { write!(f, "{}f", it) }
            Number::Byte(it) => { write!(f, "{}", it) }
            Number::Short(it) => { write!(f, "{}", it) }
            Number::Int(it) => { write!(f, "{}", it) }
            Number::Long(it) => { write!(f, "{}L", it) }
        }
    }
}