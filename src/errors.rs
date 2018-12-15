use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Error;
use std::fmt::Formatter;
use std::fmt::Write;

use crate::source_code::print_code_location;
use crate::source_code::SourceCode;
use crate::source_code::Span;
use crate::source_code::to_str;

#[derive(Debug, Clone)]
pub enum TokenizerError {
    UnknownCharacter(Span, u8)
}

#[derive(Clone)]
pub enum KtError {
    Tokenizer { code: SourceCode, info: TokenizerError }
}

impl Display for KtError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            KtError::Tokenizer { code, info } => {
                write!(f, "\n\nAn error occurred: \n")?;
                print_tokenizer_error(f, code, info)?;
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

fn print_tokenizer_error(f: &mut Write, code: &SourceCode, error: &TokenizerError) -> Result<(), Error> {
    match error {
        TokenizerError::UnknownCharacter(span, c) => {
            write!(f, "Found unknown character: '{}' ({})\n", *c as char, *c)?;
            write!(f, "{}", print_code_location(&to_str(code), *span))?;
        }
    }

    Ok(())
}