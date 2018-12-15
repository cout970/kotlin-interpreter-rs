// Development only {
// cargo watch -s 'clear && cargo test --color always 2>&1'
#![allow(dead_code)]
// }

use std::fs;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use crate::source_code::from_str;
use crate::source_code::SourceCode;

pub mod tokenizer;
pub mod source_code;
pub mod errors;

// https://kotlinlang.org/docs/reference/grammar.html


fn get_all_source_files(path: &Path, result: &mut Vec<SourceCode>) {
    let file = File::open(path);
    let mut file = if let Ok(f) = file { f } else { return; };

    let meta = file.metadata().unwrap();

    if meta.is_dir() {
        let dir = fs::read_dir(path);
        let dir = if let Ok(f) = dir { f } else { return; };

        for p in dir {
            get_all_source_files(&p.unwrap().path(), result);
        }
    } else if meta.is_file() {
        if let Some(ext) = path.extension() {
            if ext == "kt" {
                let ref mut string = String::new();
                println!("Reading file {:?}", path);
                file.read_to_string(string).unwrap();
                result.push(from_str(&string));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::get_token_stream;
    use crate::tokenizer::read_all_tokens;
    use crate::tokenizer::read_token;
    use crate::tokenizer::Token;

    use super::*;

    #[test]
    fn check_basic_tokens() {
        let ref mut s = get_token_stream(from_str("\
; ( ) { } [ ] < > @ : :: $ . .. , ?: ? ! -> .. + ++ - -- * / % = == === != !== += -= *= /= %= & && | ||
  "));

        let expected = vec![
            Token::Semicolon, Token::LeftParen, Token::RightParen, Token::LeftBrace, Token::RightBrace,
            Token::LeftBracket, Token::RightBracket, Token::LeftAngleBracket, Token::RightAngleBracket,
            Token::At, Token::Colon, Token::DoubleColon, Token::Dollar, Token::Dot, Token::DoubleDot,
            Token::Comma, Token::Elvis, Token::QuestionMark, Token::ExclamationMark, Token::LeftArrow,
            Token::DoubleDot, Token::Plus, Token::DoublePlus, Token::Minus, Token::DoubleMinus,
            Token::Asterisk, Token::Slash, Token::Percent, Token::Equals, Token::DoubleEquals,
            Token::TripleEquals, Token::NotEquals, Token::NotDoubleEquals, Token::PlusEquals,
            Token::Minus, Token::Equals, Token::TimesEquals, Token::DivEquals, Token::ModEquals,
            Token::Ampersand, Token::DoubleAmpersand, Token::Pipe, Token::DoublePipe, Token::Newline,
            Token::EOF,
        ];

        let found = read_all_tokens(s).unwrap().into_iter().map(|(_, tk)| tk).collect::<Vec<_>>();

        assert_eq!(expected, found);
    }

    #[test]
    fn get_code() {
        let ref mut codes: Vec<SourceCode> = vec![];
        get_all_source_files("/Data/Dev/Kotlin/ModelLoader/src/".as_ref(), codes);
        assert_eq!(codes.len(), 47);

        for code in codes {
            let ref mut s = get_token_stream(code.clone());
            read_all_tokens(s).expect("Expected: All tokens to be correct");
        }
    }
}
