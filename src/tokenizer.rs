use crate::errors::KtError;
use crate::errors::TokenizerError;
use crate::source_code::SOURCE_CODE_PADDING;
use crate::source_code::SourceCode;
use crate::source_code::Span;

pub struct TokenStream {
    source: SourceCode,
    pos: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Id(String),
    Literal(Literal),
    Semicolon,
    Newline,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    LeftAngleBracket,
    RightAngleBracket,
    At,
    Colon,
    DoubleColon,
    Dollar,
    Dot,
    DoubleDot,
    Comma,
    QuestionMark,
    ExclamationMark,
    NotEquals,
    NotDoubleEquals,
    LeftArrow,
    Elvis,
    Plus,
    DoublePlus,
    Minus,
    DoubleMinus,
    Asterisk,
    Slash,
    Percent,
    Equals,
    DoubleEquals,
    TripleEquals,
    PlusEquals,
    MinusEquals,
    TimesEquals,
    DivEquals,
    ModEquals,
    Ampersand,
    DoubleAmpersand,
    Pipe,
    DoublePipe,
    EOF,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Char(char),
    String(String),
    Double(f64),
    Float(f32),
    Int(i32),
    Long(i64),
}

pub fn get_token_stream(input: SourceCode) -> TokenStream {
    TokenStream { source: input, pos: 0 }
}

pub fn read_token(stream: &mut TokenStream) -> Result<(Span, Token), KtError> {
    trim_spaces(stream);
    let start = stream.pos;
    let token = read_token_aux(stream)?;
    let end = stream.pos;

    Ok(((start, end), token))
}

pub fn read_all_tokens(stream: &mut TokenStream) -> Result<Vec<(Span, Token)>, KtError> {
    let mut tks = vec![];

    loop {
        let pair = read_token(stream)?;
        if let Token::EOF = &pair.1 {
            tks.push(pair);
            break;
        } else {
            tks.push(pair);
        }
    }

    Ok(tks)
}

fn read_token_aux(stream: &mut TokenStream) -> Result<Token, KtError> {
    if (stream.pos as usize + SOURCE_CODE_PADDING) >= stream.source.len() {
        return Ok(Token::EOF);
    }

    // look ahead characters
    let c0 = stream.source[stream.pos as usize];
    let c1 = stream.source[(stream.pos + 1) as usize];

    let tk = match c0 {
        b'\n' => Token::Newline,
        b';' => Token::Semicolon,
        b'(' => Token::LeftParen,
        b')' => Token::RightParen,
        b'{' => Token::LeftBrace,
        b'}' => Token::RightBrace,
        b'[' => Token::LeftBracket,
        b']' => Token::RightBracket,
        b'<' => Token::LeftAngleBracket,
        b'>' => Token::RightAngleBracket,
        b'@' => Token::At,
        b':' => match c1 {
            b':' => {
                stream.pos += 1;
                Token::DoubleColon
            }
            _ => Token::Colon
        },
        b'$' => Token::Dollar,
        b'.' => match c1 {
            b'.' => {
                stream.pos += 1;
                Token::DoubleDot
            }
            _ => Token::Dot
        },
        b',' => Token::Comma,
        b'?' => match c1 {
            b':' => {
                stream.pos += 1;
                Token::Elvis
            }
            _ => Token::QuestionMark
        },
        b'!' => match c1 {
            b'=' => {
                stream.pos += 1;
                match stream.source[(stream.pos + 1) as usize] {
                    b'=' => {
                        stream.pos += 1;
                        Token::NotDoubleEquals
                    }
                    _ => Token::NotEquals
                }
            }
            _ => Token::ExclamationMark
        },
        b'+' => match c1 {
            b'+' => {
                stream.pos += 1;
                Token::DoublePlus
            }
            b'=' => {
                stream.pos += 1;
                Token::PlusEquals
            }
            _ => Token::Plus
        },
        b'-' => match c1 {
            b'-' => {
                stream.pos += 1;
                Token::DoubleMinus
            }
            b'>' => {
                stream.pos += 1;
                Token::LeftArrow
            }
            _ => Token::Minus
        },
        b'*' => match c1 {
            b'=' => {
                stream.pos += 1;
                Token::TimesEquals
            }
            _ => Token::Asterisk
        },
        b'/' => match c1 {
            b'=' => {
                stream.pos += 1;
                Token::DivEquals
            }
            _ => Token::Slash
        },
        b'%' => match c1 {
            b'=' => {
                stream.pos += 1;
                Token::ModEquals
            }
            _ => Token::Percent
        },
        b'=' => match c1 {
            b'=' => {
                stream.pos += 1;
                match stream.source[(stream.pos + 1) as usize] {
                    b'=' => {
                        stream.pos += 1;
                        Token::TripleEquals
                    }
                    _ => Token::DoubleEquals
                }
            }
            _ => Token::Equals
        },
        b'&' => match c1 {
            b'&' => {
                stream.pos += 1;
                Token::DoubleAmpersand
            }
            _ => Token::Ampersand
        },
        b'|' => match c1 {
            b'|' => {
                stream.pos += 1;
                Token::DoublePipe
            }
            _ => Token::Pipe
        },
        _ => return Err(KtError::Tokenizer {
            code: stream.source.clone(),
            info: TokenizerError::UnknownCharacter((stream.pos, stream.pos), c0),
        })
    };

    stream.pos += 1;
    Ok(tk)
}

fn trim_spaces(stream: &mut TokenStream) {
    loop {
        if stream.pos as usize >= stream.source.len() { break; }
        let c = stream.source[stream.pos as usize];

        if c == b' ' || c == b'\t' || c == b'\r' {
            stream.pos += 1;
        } else {
            break;
        }
    }
}