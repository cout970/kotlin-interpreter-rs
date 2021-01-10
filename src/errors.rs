use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Error;
use std::fmt::Formatter;
use std::fmt::Write;

use crate::parser::{ParserError, ParserErrorKind};
use crate::parser::parse_tree::Modifier;
use crate::parser::parse_tree::ModifierCtx;
use crate::source::{ByteSpan, Source};
use crate::token::{Number, Token};
use crate::token_stream::{TokenStreamError, TokenStreamErrorKind};

#[derive(Debug, Clone)]
pub enum AnalyserError {
    InvalidModifierUsage { modifier: Modifier, context: String },
    DuplicatedModifier { modifier: Modifier },
    MutuallyExclusiveModifier { modifier_1: Modifier, modifier_2: Modifier },
    InvalidModifier { modifier: Modifier, context: ModifierCtx },
    ConflictingImport { name: String },
    DuplicatedTypeParameter { param: String },
    UnknownTypeParameter { param: String },
    DuplicatedFunctionParameter { param: String },
    MultipleInheritance,
    ExtendingNonClass,
    ExtendingFinalClass,
    MissingConstructorCall,
    WhenWithoutEntries,
    WhenElseMustBeLast,
    WhenWithoutArgumentMultipleConditions,
    InvalidWhenCondition,
    NestedTypeAlias,
    DoubleNullableType,
    IncDecToNonVariable,
    InvalidOperatorArguments,
    InvalidOperatorReturn,
    InvalidOperatorFunctionName,
    FunctionParameterInvalidMutability,
    DestructuringInTopLevel,
    InvalidInnerClass,
    UnresolvedReference(String),
    TypeConstrainsNotAllowed,
    EnumWithoutBody,
    ObjectWithConstructor,
    MultipleVarargs,
    MissingReturnType,
    MissingFunctionParameterType,
    ExpectedVariable,
}

#[derive(Clone)]
pub enum KtError {
    Parser(ParserError),
    Analyser { code: Source, span: ByteSpan, info: AnalyserError },
    Unimplemented,
}

impl Display for KtError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            KtError::Unimplemented => {
                write!(f, "\n\nUnimplemented\n")?;
            }
            KtError::Parser(info) => {
                // Use color red
                write!(f, "\x1B[31m")?;

                write!(f, "\n\nAn error occurred: \n{}\n\n", info)?;

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
        }
        Ok(())
    }
}

impl Debug for KtError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self)
    }
}

impl Display for TokenStreamError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            TokenStreamErrorKind::UnknownCharacter(c) => {
                write!(f, "Found unknown character: '{}' ({})\n", *c as char, *c)?;
            }
            TokenStreamErrorKind::UnsupportedLiteralPrefix(c) => {
                write!(f, "Unsupported number prefix: 0{}\n", *c)?;
            }
            TokenStreamErrorKind::ExpectedEndOfString => {
                write!(f, "Found end of the line while reading a string\n")?;
            }
            TokenStreamErrorKind::ExpectedEndOfChar => {
                write!(f, "Found expecting end of character literal\n")?;
            }
            TokenStreamErrorKind::UnclosedComment => {
                write!(f, "Found unclosed comment\n")?;
            }
            TokenStreamErrorKind::InvalidEscapeChar(c) => {
                write!(f, "Found invalid escape character: '{}' ({})\n", *c, *c as u32)?;
            }
            TokenStreamErrorKind::ExpectedEndOfEscapedIdentifier => {
                write!(f, "I was expecting a ` to end the escaped identifier: \n")?;
            }
        }

        write!(f, "{}", self.span)?;
        Ok(())
    }
}

fn print_analyser_error(f: &mut dyn Write, code: &Source, span: ByteSpan, error: &AnalyserError) -> Result<(), Error> {
    match error {
        AnalyserError::InvalidModifierUsage { modifier, context } => {
            write!(f, "Modifier '{:?}' is not applicable in {}\n", modifier, context)?;
        }
        AnalyserError::ConflictingImport { name } => {
            write!(f, "Conflicting import, imported name '{}' is ambiguous\n", name)?;
        }
        AnalyserError::DuplicatedModifier { modifier } => {
            write!(f, "Duplicated modifier '{:?}'\n", modifier)?;
        }
        AnalyserError::DestructuringInTopLevel => {
            write!(f, "Variable destructuring is only available in local properties\n")?;
        }
        AnalyserError::MutuallyExclusiveModifier { modifier_1, modifier_2 } => {
            write!(f, "Modifier '{:?}' is incompatible with '{:?}'\n", modifier_1, modifier_2)?;
        }
        _ => {
            write!(f, "TODO {:?}\n", error)?;
        }
    }

    // TODO
    // write!(f, "{}", print_code_location(&to_str(code), span))?;
    Ok(())
}


impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ParserErrorKind::UnexpectedToken { found } => {
                write!(f, "Found unexpected token: {}\n", found)?;
            }
            ParserErrorKind::ExpectedToken { expected, found } => {
                write!(f, "Expecting: {} but found: {}\n", expected, found)?;
            }
            ParserErrorKind::ExpectedTokenId { found } => {
                write!(f, "Expecting identifier but found: {}\n", found)?;
            }
            ParserErrorKind::ExpectedTokenOf { found, options } => {
                write!(f, "Found token {} but I was expecting one of:\n", found)?;
                for x in options {
                    write!(f, " - {}\n", x)?;
                }
            }
            ParserErrorKind::TokenStreamError { kind } => {
                return write!(f, "{}", TokenStreamError { kind: kind.clone(), span: self.span.clone() });
            }
            // ParserErrorKind::FoundInvalidModifier { found, ctx } => {
            //     let ctx_name = match ctx {
            //         ModifierCtx::TopLevelObject => "top level",
            //         ModifierCtx::TypeParameter => "type parameter",
            //         ModifierCtx::Statement => "statement",
            //         ModifierCtx::Package => "package",
            //         ModifierCtx::Constructor => "constructor",
            //         ModifierCtx::GetterSetter => "getter/setter",
            //         ModifierCtx::ClassMember => "class member",
            //         ModifierCtx::EnumEntry => "enum entry",
            //         ModifierCtx::FunctionParameter => "parameter",
            //         ModifierCtx::ConstructorParameter => "constructor parameter",
            //     };
            //     write!(f, "Modifier '{:?}' is not applicable to '{}'\n", found, ctx_name)?;
            // }
        }

        write!(f, "{}", self.span)?;
        Ok(())
    }
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