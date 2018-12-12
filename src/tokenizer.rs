use crate::SourceCode;

pub struct TokenStream {
    source: SourceCode,
    pos: u32,
}

pub enum TokenizerError {
    UnknownCharacter(u8)
}

pub type Span = (u32, u32);

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
    Comma,
    QuestionMark,
    ExclamationMark,
    LeftArrow,
    Elvis,
    Range,
    Plus,
    DoublePlus,
    Minus,
    DoubleMinus,
    Asterisk,
    Slash,
    Percent,
    Equals,
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

pub fn read_token(stream: &mut TokenStream) -> Result<(Span, Token), TokenizerError> {
    trim_spaces(stream);
    let start = stream.pos;
    let token = read_token_aux(stream)?;
    let end = stream.pos;

    Ok(((start, end), token))
}

fn read_token_aux(stream: &mut TokenStream) -> Result<Token, TokenizerError> {
    if stream.pos as usize >= stream.source.len() {
        return Ok(Token::EOF);
    }

    let c = stream.source[stream.pos as usize];
    let tk = match c {
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
//        b':' => Token::Colon,
//        b'::' => Token::DoubleColon,
        b'$' => Token::Dollar,
        b'.' => Token::Dot,
//        b'..' => Token::Range,
        b',' => Token::Comma,
//        b'?:' => Token::Elvis,
        b'?' => Token::QuestionMark,
        b'!' => Token::ExclamationMark,
//        b'->' => Token::LeftArrow,
//        b'..' => Token::Range,
        b'+' => Token::Plus,
//        b'--' => Token::DoublePlus,
        b'-' => Token::Minus,
//        b'--' => Token::DoubleMinus,
        b'*' => Token::Asterisk,
        b'/' => Token::Slash,
        b'%' => Token::Percent,
        b'=' => Token::Equals,
//        b'+=' => Token::PlusEquals,
//        b'-=' => Token::MinusEquals,
//        b'*=' => Token::TimesEquals,
//        b'/=' => Token::DivEquals,
//        b'%=' => Token::ModEquals,
        b'&' => Token::Ampersand,
//        b'&&' => Token::DoubleAmpersand,
        b'|' => Token::Pipe,
//        b'||' => Token::DoublePipe,
        _ => return Err(TokenizerError::UnknownCharacter(c))
    };

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