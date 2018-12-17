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
    LitChar(char),
    LitString(String),
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
    Underscore,
    EOF,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Double(f64),
    Float(f32),
    Int(i32),
    Long(i64),
}

impl TokenStream {
    #[inline]
    pub fn read_char(&mut self, index: u32) -> char {
        self.source[(self.pos + index) as usize] as char
    }

    #[inline]
    pub fn read_u8(&mut self, index: u32) -> u8 {
        self.source[(self.pos + index) as usize]
    }

    #[inline]
    pub fn moved(&mut self, index: u32) -> &mut Self {
        self.pos += index;
        self
    }

    #[inline]
    pub fn next(&mut self) {
        self.pos += 1;
    }
}

pub fn get_token_stream(input: SourceCode) -> TokenStream {
    TokenStream { source: input, pos: 0 }
}

pub fn read_token(stream: &mut TokenStream) -> Result<(Span, Token), KtError> {
    trim_spaces(stream);
    trim_comments(stream)?;
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
    let c0 = stream.read_u8(0);
    let c1 = stream.read_u8(1);

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
        b'_' => Token::Underscore,
        b':' => match c1 {
            b':' => {
                stream.next();
                Token::DoubleColon
            }
            _ => Token::Colon
        },
        b'$' => Token::Dollar,
        b'.' => match c1 {
            b'.' => {
                stream.next();
                Token::DoubleDot
            }
            b'0'..=b'9' => read_number(stream)?,
            _ => Token::Dot
        },
        b',' => Token::Comma,
        b'?' => match c1 {
            b':' => {
                stream.next();
                Token::Elvis
            }
            _ => Token::QuestionMark
        },
        b'!' => match c1 {
            b'=' => {
                stream.next();
                match stream.source[(stream.pos + 1) as usize] {
                    b'=' => {
                        stream.next();
                        Token::NotDoubleEquals
                    }
                    _ => Token::NotEquals
                }
            }
            _ => Token::ExclamationMark
        },
        b'+' => match c1 {
            b'+' => {
                stream.next();
                Token::DoublePlus
            }
            b'=' => {
                stream.next();
                Token::PlusEquals
            }
            _ => Token::Plus
        },
        b'-' => match c1 {
            b'-' => {
                stream.next();
                Token::DoubleMinus
            }
            b'>' => {
                stream.next();
                Token::LeftArrow
            }
            _ => Token::Minus
        },
        b'*' => match c1 {
            b'=' => {
                stream.next();
                Token::TimesEquals
            }
            _ => Token::Asterisk
        },
        b'/' => match c1 {
            b'=' => {
                stream.next();
                Token::DivEquals
            }
            _ => Token::Slash
        },
        b'%' => match c1 {
            b'=' => {
                stream.next();
                Token::ModEquals
            }
            _ => Token::Percent
        },
        b'=' => match c1 {
            b'=' => {
                stream.next();
                match stream.source[(stream.pos + 1) as usize] {
                    b'=' => {
                        stream.next();
                        Token::TripleEquals
                    }
                    _ => Token::DoubleEquals
                }
            }
            _ => Token::Equals
        },
        b'&' => match c1 {
            b'&' => {
                stream.next();
                Token::DoubleAmpersand
            }
            _ => Token::Ampersand
        },
        b'|' => match c1 {
            b'|' => {
                stream.next();
                Token::DoublePipe
            }
            _ => Token::Pipe
        },
        b'"' => read_string(stream)?,
        b'\'' => read_char(stream)?,
        b'a'..=b'z' | b'A'..=b'Z' => read_identifier(stream),
        b'0'..=b'9' => read_number(stream)?,
        _ => return Err(KtError::Tokenizer {
            code: stream.source.clone(),
            span: (stream.pos, stream.pos),
            info: TokenizerError::UnknownCharacter(c0),
        })
    };

    stream.next();
    Ok(tk)
}

fn read_identifier(stream: &mut TokenStream) -> Token {
    let mut id = String::new();

    loop {
        let c = stream.read_char(0);
        if !c.is_alphanumeric() {
            break;
        }
        id.push(c);
        stream.next();
    }

    Token::Id(id)
}

fn read_string(stream: &mut TokenStream) -> Result<Token, KtError> {
    let triple = stream.read_char(0) == '"' && stream.read_char(1) == '"' && stream.read_char(2) == '"';
    let mut chars = vec![];
    let start = stream.pos;

    if triple {
        stream.next();
        stream.next();
        stream.next();
        loop {
            let c0 = stream.read_u8(0);
            let c1 = stream.read_u8(1);
            let c2 = stream.read_u8(2);

            if c0 == b'"' && c1 == b'"' && c2 == b'"' {
                stream.next();
                stream.next();
                stream.next();
                break;
            } else if c0 == 0 {
                return Err(KtError::Tokenizer {
                    code: stream.source.clone(),
                    span: (start, stream.pos),
                    info: TokenizerError::ExpectedEndOfString,
                });
            }

            chars.push(c0);
            stream.next()
        }
    } else {
        stream.next();
        loop {
            let c = stream.read_u8(0);

            if c == b'"' {
                stream.next();
                break;
            } else if c == b'\n' {
                return Err(KtError::Tokenizer {
                    code: stream.source.clone(),
                    span: (start, stream.pos),
                    info: TokenizerError::ExpectedEndOfString,
                });
            }

            chars.push(c);
            stream.next()
        }
    }

    Ok(Token::LitString(String::from_utf8_lossy(&chars).to_string()))
}

fn read_char(stream: &mut TokenStream) -> Result<Token, KtError> {
    let c: char;
    let start = stream.pos;

    stream.next(); // skip '
    let c0 = stream.read_char(0);
    stream.next(); // skip char

    if c0 == '\\' {
        let c1 = stream.read_char(0);

        c = match c1 {
            '0'..='3' => {
                // TODO read octal number of 3 digits
                'A'
            }
            'b' => 0x0008 as char,
            't' => '\t',
            'n' => '\n',
            'f' => 0x000C as char,
            'r' => '\r',
            '"' => '"',
            '\'' => '\'',
            '\\' => '\\',
            'u' => {
                // TODO read hex number
                'A'
            },
            _ => {
                return Err(KtError::Tokenizer {
                    code: stream.source.clone(),
                    span: (start, stream.pos),
                    info: TokenizerError::InvalidScapeChar(c1),
                });
            }
        };
    } else {
        c = c0;
    }

    if stream.read_char(0) != '\'' {
        return Err(KtError::Tokenizer {
            code: stream.source.clone(),
            span: (start, stream.pos),
            info: TokenizerError::ExpectedEndOfChar,
        });
    }

    stream.next(); // skip '

    Ok(Token::LitChar(c))
}

fn read_number(stream: &mut TokenStream) -> Result<Token, KtError> {
    let digit = stream.read_char(0);

    let tk = match digit {
        '0' => {
            match stream.read_char(1) {
                'x' => read_hex(stream.moved(2)),
                'o' => {
                    // Octal is not supported in the language
                    return Err(KtError::Tokenizer {
                        code: stream.source.clone(),
                        span: (stream.pos, stream.pos + 1),
                        info: TokenizerError::UnsupportedLiteralPrefix('o'),
                    });
                }
                'b' => read_binary(stream.moved(2)),
                '.' => read_float_or_int(stream),
                '1'..='9' => read_float_or_int(stream),
                '0' => {
                    return Err(KtError::Tokenizer {
                        code: stream.source.clone(),
                        span: (stream.pos, stream.pos + 1),
                        info: TokenizerError::UnsupportedLiteralPrefix('0'),
                    });
                }
                _ => {
                    stream.next();
                    Token::Literal(Literal::Int(0))
                }
            }
        }
        _ => read_float_or_int(stream),
    };
    Ok(tk)
}

fn collect_chars<F: FnMut(char) -> bool>(stream: &mut TokenStream, mut pred: F) -> String {
    let mut res = String::new();
    // TODO add _ but not at start or end

    loop {
        let c = stream.read_char(0);
        if !pred(c) && c != '_' {
            break;
        }

        res.push(c);
        stream.next();
    }
    res
}

fn read_hex(stream: &mut TokenStream) -> Token {
    let res = collect_chars(stream, |c: char| c.is_ascii_hexdigit());
    Token::Literal(from_int_chars(16, &res))
}

fn read_binary(stream: &mut TokenStream) -> Token {
    let res = collect_chars(stream, |c: char| c == '0' || c == '1');
    Token::Literal(from_int_chars(2, &res))
}

fn read_float_or_int(stream: &mut TokenStream) -> Token {
    let pre_dot = collect_chars(stream, |c: char| contains(c, '0', '9'));
    let float = stream.read_char(0) == '.' && stream.read_char(1).is_ascii_digit();

    if float {
        stream.next();
        let post_dot = collect_chars(stream, |c: char| contains(c, '0', '9'));

        let has_e = stream.read_char(0) == 'e';
        let suffix = stream.read_char(1);
        let has_suffix = suffix == '-' || suffix == '+';
        let moved = if has_suffix { 2 } else { 1 };

        let mut exp = if has_e && stream.read_char(moved).is_ascii_digit() {
            collect_chars(stream.moved(moved), |c: char| contains(c, '0', '9'))
        } else {
            String::new()
        };

        if has_suffix {
            let mut s = String::new();
            s.push(suffix);
            for c in exp.chars() {
                s.push(c);
            }
            exp = s;
        }

        return Token::Literal(from_float_chars(&pre_dot, &post_dot, &exp));
    }

    Token::Literal(from_int_chars(10, &pre_dot))
}

fn from_int_chars(radix: u32, digits: &str) -> Literal {
    let chars = digits.chars().filter(|c| *c != '_');
    let mut value: i64 = 0;

    for c in chars {
        value = value * (radix as i64) + digit_to_value(c) as i64;
    }

    if value as u64 & 0xFFFF_FFFF_0000_0000u64 != 0 {
        Literal::Long(value)
    } else {
        Literal::Int(value as i32)
    }
}

fn from_float_chars(pre_dot: &str, post_dot: &str, exp: &str) -> Literal {
    let pre_dot_chars = pre_dot.chars().filter(|c| *c != '_');
    let mut pre_dot_value: f64 = 0.0;

    for c in pre_dot_chars {
        pre_dot_value = pre_dot_value * 10.0 + digit_to_value(c) as f64;
    }

    let post_dot_chars = post_dot.chars().filter(|c| *c != '_').rev();
    let mut post_dot_value: f64 = 0.0;

    for c in post_dot_chars {
        post_dot_value = (post_dot_value + digit_to_value(c) as f64) / 10.0;
    }

    let mut value = pre_dot_value + post_dot_value;

    if !exp.is_empty() {
        let first = exp.chars().nth(0).unwrap();
        let sign: f64 = if first == '-' { -1.0 } else { 1.0 };

        let exp_chars = exp.chars().filter(|c| *c != '_' && *c != '-' && *c != '+').rev();
        let mut exp_value: f64 = 0.0;

        for c in exp_chars {
            exp_value = exp_value * 10.0 + digit_to_value(c) as f64;
        }

        value *= 10.0f64.powf(exp_value * sign);
    }

    Literal::Float(value as f32)
}

fn digit_to_value(c: char) -> u32 {
    match c {
        '0'..='9' => (c as u32) - ('0' as u32),
        'a'..='f' => (c as u32) - ('a' as u32) + 10,
        'A'..='F' => (c as u32) - ('A' as u32) + 10,
        _ => panic!("Invalid digit: '{}' ({})", c, c as u32)
    }
}

fn contains(c: char, start: char, end: char) -> bool {
    c as u32 >= start as u32 && c as u32 <= end as u32
}

fn trim_spaces(stream: &mut TokenStream) {
    loop {
        let c = stream.read_u8(0);

        if c == b' ' || c == b'\t' || c == b'\r' {
            stream.next();
        } else {
            break;
        }
    }
}

fn trim_comments(stream: &mut TokenStream) -> Result<(), KtError> {
    loop {
        let start = stream.pos;
        let c0 = stream.read_char(0);
        let c1 = stream.read_char(1);

        if c0 == '/' && c1 == '/' {
            stream.next();
            stream.next();
            loop {
                let c = stream.read_u8(0);

                if c == 0 || c == b'\n' {
                    break;
                }
                stream.next();
            }

            trim_spaces(stream);
        } else if c0 == '/' && c1 == '*' {
            stream.next();
            stream.next();
            loop {
                let c0 = stream.read_u8(0);
                let c1 = stream.read_u8(1);

                if c0 == 0 {
                    return Err(KtError::Tokenizer {
                        code: stream.source.clone(),
                        span: (start, stream.pos),
                        info: TokenizerError::UnclosedComment,
                    });
                }

                if c0 == b'*' && c1 == b'/' {
                    break;
                }
                stream.next();
            }

            trim_spaces(stream);
        } else {
            break;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::source_code::from_str;
    use crate::tokenizer::get_token_stream;
    use crate::tokenizer::read_token;
    use crate::tokenizer::Token;

    use super::*;

    fn read_single_token(code: &str) -> Token {
        let input = from_str(code);
        let ref mut s = get_token_stream(input);
        read_token(s).unwrap().1
    }

    #[test]
    fn check_float() {
        assert_eq!(Token::Literal(Literal::Float(12345.12345)), read_single_token("12345.12345"));
        assert_eq!(Token::Literal(Literal::Float(1.0)), read_single_token("1.0"));
        assert_eq!(Token::Literal(Literal::Float(0.0)), read_single_token("0.0"));
        assert_eq!(Token::Literal(Literal::Float(0.2e1)), read_single_token("0.2e1"));
        assert_eq!(Token::Literal(Literal::Float(1.2e1)), read_single_token("1.2e1"));
        assert_eq!(Token::Literal(Literal::Float(1.2e+1)), read_single_token("1.2e+1"));
        assert_eq!(Token::Literal(Literal::Float(1.2e-1)), read_single_token("1.2e-1"));
    }

    #[test]
    fn check_int() {
        assert_eq!(Token::Literal(Literal::Int(0)), read_single_token("0"));
        assert_eq!(Token::Literal(Literal::Int(1)), read_single_token("1"));
        assert_eq!(Token::Literal(Literal::Int(123445)), read_single_token("123445"));
        assert_eq!(Token::Literal(Literal::Int(0x1)), read_single_token("0x1"));
        assert_eq!(Token::Literal(Literal::Int(0x0)), read_single_token("0x0"));
        assert_eq!(Token::Literal(Literal::Int(1_000)), read_single_token("1_000"));
        assert_eq!(Token::Literal(Literal::Int(0b010101)), read_single_token("0b010101"));
    }

    #[test]
    fn check_string() {
        assert_eq!(Token::LitString(String::from("abc")), read_single_token("\"abc\""));
        assert_eq!(Token::LitString(String::from("abc")), read_single_token("\"\"\"abc\"\"\""));
    }

    #[test]
    #[ignore]
    fn check_char() {
        assert_eq!(Token::LitChar('a'), read_single_token("'a'"));
        assert_eq!(Token::LitChar('a'), read_single_token("\\u61"));
        assert_eq!(Token::LitChar('a'), read_single_token("\\0141"));
    }
}
