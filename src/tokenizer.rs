use crate::errors::KtError;
use crate::errors::TokenizerError;
use crate::source_code::SOURCE_CODE_PADDING;
use crate::source_code::SourceCode;
use crate::source_code::Span;
use crate::Number;

#[derive(Eq, PartialEq, Copy, Clone)]
enum TokenizerMode {
    Normal,
    String,
    MultilineString,
    StringTemplate,
}

pub struct CodeCursor {
    source: SourceCode,
    pos: u32,
    mode: Vec<(TokenizerMode, i32)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Id(String),
    Number(Number),
    Char(char),
    StringStart,
    StringEnd,
    StringTemplateStart,
    StringTemplateEnd,
    StringContent(String),
    StringVariable(String),
    // Signs
    Semicolon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    LeftAngleBracket,
    RightAngleBracket,
    LessEquals,
    GreaterEquals,
    At,
    Colon,
    DoubleColon,
    Dollar,
    Dot,
    DoubleDot,
    Comma,
    QuestionMark,
    SafeDot,
    ExclamationMark,
    DoubleExclamationMark,
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
    // Keywords
    As,
    AsQuestionMark,
    Break,
    Class,
    Continue,
    Do,
    Else,
    False,
    For,
    Fun,
    If,
    In,
    NotIn,
    Is,
    NotIs,
    Interface,
    Null,
    Object,
    Package,
    Return,
    Super,
    This,
    Throw,
    True,
    Try,
    TypeAlias,
    Val,
    Var,
    When,
    While,
    // End of file
    EOF,
}


impl CodeCursor {
    #[inline]
    fn read_char(&mut self, index: u32) -> char {
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

    #[inline]
    fn mode(&mut self) -> TokenizerMode {
        self.mode.last().unwrap().0
    }

    #[inline]
    fn brace_count(&mut self) -> i32 {
        self.mode.last().unwrap().1
    }

    #[inline]
    fn inc_brace(&mut self) {
        self.mode.last_mut().unwrap().1 += 1;
    }

    #[inline]
    fn dec_brace(&mut self) {
        self.mode.last_mut().unwrap().1 -= 1;
    }

    pub fn code_ref(&mut self) -> SourceCode {
        self.source.clone()
    }
}

pub fn get_code_cursor(input: SourceCode) -> CodeCursor {
    CodeCursor { source: input, pos: 0, mode: vec![(TokenizerMode::Normal, 0)] }
}

pub fn read_token(stream: &mut CodeCursor) -> Result<(Span, Token), KtError> {
    match stream.mode() {
        TokenizerMode::String => read_string_token(stream, false),
        TokenizerMode::MultilineString => read_string_token(stream, true),
        _ => {
            trim_spaces(stream);
            trim_comments(stream)?;
            let start = stream.pos;
            let token = read_token_aux(stream)?;
            let end = stream.pos;

            Ok(((start, end), token))
        }
    }
}

fn read_string_token(stream: &mut CodeCursor, multiline: bool) -> Result<(Span, Token), KtError> {
    // Examples of strings and the generated tokens:
    // "Hello world!!!" => StrStart StrContent("hello world!!!") StrEnd
    // "Hello $world!!!" => StrStart StrContent("hello ") StrVariable("world") StrContent("!!!") StrEnd
    // "Hello ${world}!!!" => StrStart StrContent("hello ") StrTemplateStart Id("world") StrTemplateEnd StrContent("!!!") StrEnd
    let start = stream.pos;

    let tk = match stream.read_char(0) {
        '"' => {
            if multiline {
                stream.next();
                stream.next();
            }
            stream.next();
            stream.mode.pop();
            Token::StringEnd
        }
        '$' => {
            if stream.read_char(1) == '{' {
                stream.next();
                stream.next();
                stream.mode.push((TokenizerMode::StringTemplate, 1));
                Token::StringTemplateStart
            } else {
                read_string_token_variable(stream)?
            }
        }
        _ => {
            read_string_token_content(stream, multiline)?
        }
    };

    let end = stream.pos;

    Ok(((start, end), tk))
}

fn read_string_token_variable(stream: &mut CodeCursor) -> Result<Token, KtError> {
    assert_eq!(stream.read_char(0), '$');
    stream.next();

    let mut content = String::new();

    loop {
        let c0 = stream.read_char(0);

        if !c0.is_alphanumeric() {
            break;
        }

        content.push(c0);
        stream.next();
    }

    Ok(Token::StringVariable(content))
}

fn read_string_token_content(stream: &mut CodeCursor, multiline: bool) -> Result<Token, KtError> {
    let start = stream.pos;
    let mut content = String::new();

    loop {
        let c0 = stream.read_char(0);
        let c1 = stream.read_char(1);
        let c2 = stream.read_char(2);

        if multiline {
            if c0 == '"' && c1 == '"' && c2 == '"' {
                break;
            }
        } else {
            if c0 == '"' {
                break;
            }
        }

        if c0 == '\0' {
            return Err(KtError::Tokenizer {
                code: stream.code_ref(),
                span: (start, stream.pos),
                info: TokenizerError::ExpectedEndOfString
            });
        }

        if !multiline && c0 == '\\' {
            let code = match c1 {
                '"' => '"',
                '\'' => '\'',
                '0' => '\0',
                't' => '\t',
                'r' => '\r',
                'n' => '\n',
                _ => c1
            };

            content.push(code);
            stream.next();
            stream.next();
            continue;
        }

        if c0 == '$' {
            if c1 == '{' || c1.is_alphabetic() {
                break;
            }
        }

        content.push(c0);
        stream.next();
    }

    Ok(Token::StringContent(content))
}


pub fn read_all_tokens(stream: &mut CodeCursor) -> Result<Vec<(Span, Token)>, KtError> {
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

fn read_token_aux(stream: &mut CodeCursor) -> Result<Token, KtError> {
    if (stream.pos as usize + SOURCE_CODE_PADDING) >= stream.source.len() {
        return Ok(Token::EOF);
    }

    // look ahead characters
    let c0 = stream.read_u8(0);
    let c1 = stream.read_u8(1);

    let tk = match c0 {
        b'\n' => Token::Semicolon,
        b';' => Token::Semicolon,
        b'(' => Token::LeftParen,
        b')' => Token::RightParen,
        b'{' => {
            stream.inc_brace();
            Token::LeftBrace
        }
        b'}' => {
            stream.dec_brace();
            if stream.mode() == TokenizerMode::StringTemplate && stream.brace_count() == 0 {
                stream.mode.pop();
                Token::StringTemplateEnd
            } else {
                Token::RightBrace
            }
        }
        b'[' => Token::LeftBracket,
        b']' => Token::RightBracket,
        b'<' => match c1 {
            b'=' => {
                stream.next();
                Token::LessEquals
            }
            _ => Token::LeftAngleBracket
        },
        b'>' => match c1 {
            b'=' => {
                stream.next();
                Token::GreaterEquals
            }
            _ => Token::RightAngleBracket
        },
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
            b'.' => {
                stream.next();
                Token::SafeDot
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
            b'i' => {
                let c2 = stream.read_u8(2);
                match c2 {
                    b'n' => {
                        stream.next();
                        stream.next();
                        Token::NotIn
                    }
                    b's' => {
                        stream.next();
                        stream.next();
                        Token::NotIs
                    }
                    _ => Token::ExclamationMark
                }
            }
            b'!' => {
                stream.next();
                Token::DoubleExclamationMark
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
        b'"' => {
            if c1 == b'"' && stream.read_u8(2) == b'"' {
                stream.mode.push((TokenizerMode::MultilineString, 0));
                stream.next();
                stream.next();
            } else {
                stream.mode.push((TokenizerMode::String, 0));
            }
            Token::StringStart
        }
        b'\'' => { return Ok(read_char(stream)?); }
        b'a'..=b'z' | b'A'..=b'Z' => { return Ok(read_identifier_or_keyword(stream)); }
        b'`' => { return read_escaped_identifier(stream); }
        b'0'..=b'9' => { return Ok(read_number(stream)?); }
        _ => return Err(KtError::Tokenizer {
            code: stream.code_ref(),
            span: (stream.pos, stream.pos),
            info: TokenizerError::UnknownCharacter(c0),
        })
    };

    stream.next();
    Ok(tk)
}

fn read_escaped_identifier(stream: &mut CodeCursor) -> Result<Token, KtError> {
    let start = stream.pos;
    assert_eq!(stream.read_u8(0), b'`');
    stream.next();

    let mut id = String::new();

    loop {
        let c = stream.read_char(0);
        if c == '`' || c == '\0' || c == '\n' {
            break;
        }

        id.push(c);
        stream.next();
    }

    if stream.read_u8(0) != b'`' {
        return Err(KtError::Tokenizer {
            code: stream.code_ref(),
            span: (start, stream.pos),
            info: TokenizerError::ExpectedEndOfEscapedIdentifier,
        });
    }
    stream.next();

    Ok(Token::Id(id))
}

fn read_identifier_or_keyword(stream: &mut CodeCursor) -> Token {
    let mut id = String::new();

    loop {
        let c = stream.read_char(0);
        if !c.is_alphanumeric() {
            break;
        }
        id.push(c);
        stream.next();
    }

    match &id[..] {
        "as" => {
            if stream.read_u8(0) == b'?' {
                stream.next();
                Token::AsQuestionMark
            } else {
                Token::As
            }
        }
        "as?" => Token::AsQuestionMark,
        "break" => Token::Break,
        "class" => Token::Class,
        "continue" => Token::Continue,
        "do" => Token::Do,
        "else" => Token::Else,
        "false" => Token::False,
        "for" => Token::For,
        "fun" => Token::Fun,
        "if" => Token::If,
        "in" => Token::In,
        "interface" => Token::Interface,
        "is" => Token::Is,
        "null" => Token::Null,
        "object" => Token::Object,
        "package" => Token::Package,
        "return" => Token::Return,
        "super" => Token::Super,
        "this" => Token::This,
        "throw" => Token::Throw,
        "true" => Token::True,
        "try" => Token::Try,
        "typealias" => Token::TypeAlias,
        "val" => Token::Val,
        "var" => Token::Var,
        "when" => Token::When,
        "while" => Token::While,
        _ => Token::Id(id)
    }
}

fn read_char(stream: &mut CodeCursor) -> Result<Token, KtError> {
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
            }
            _ => {
                return Err(KtError::Tokenizer {
                    code: stream.code_ref(),
                    span: (start, stream.pos),
                    info: TokenizerError::InvalidEscapeChar(c1),
                });
            }
        };

        stream.next();
    } else {
        c = c0;
    }

    if stream.read_char(0) != '\'' {
        return Err(KtError::Tokenizer {
            code: stream.code_ref(),
            span: (start, stream.pos),
            info: TokenizerError::ExpectedEndOfChar,
        });
    }

    stream.next(); // skip '

    Ok(Token::Char(c))
}

fn read_number(stream: &mut CodeCursor) -> Result<Token, KtError> {
    let digit = stream.read_char(0);

    let tk = match digit {
        '0' => {
            match stream.read_char(1) {
                'x' => read_hex(stream.moved(2)),
                'o' => {
                    // Octal is not supported in the language
                    return Err(KtError::Tokenizer {
                        code: stream.code_ref(),
                        span: (stream.pos, stream.pos + 1),
                        info: TokenizerError::UnsupportedLiteralPrefix('o'),
                    });
                }
                'b' => read_binary(stream.moved(2)),
                '.' => read_float_or_int(stream),
                '1'..='9' => read_float_or_int(stream),
                '0' => {
                    return Err(KtError::Tokenizer {
                        code: stream.code_ref(),
                        span: (stream.pos, stream.pos + 1),
                        info: TokenizerError::UnsupportedLiteralPrefix('0'),
                    });
                }
                _ => {
                    stream.next();
                    Token::Number(Number::Int(0))
                }
            }
        }
        _ => read_float_or_int(stream),
    };
    Ok(tk)
}

fn collect_chars<F: FnMut(char) -> bool>(stream: &mut CodeCursor, mut pred: F) -> String {
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

fn read_hex(stream: &mut CodeCursor) -> Token {
    let res = collect_chars(stream, |c: char| c.is_ascii_hexdigit());
    Token::Number(from_int_chars(16, &res))
}

fn read_binary(stream: &mut CodeCursor) -> Token {
    let res = collect_chars(stream, |c: char| c == '0' || c == '1');
    Token::Number(from_int_chars(2, &res))
}

fn read_float_or_int(stream: &mut CodeCursor) -> Token {
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

        return Token::Number(from_float_chars(&pre_dot, &post_dot, &exp));
    }

    Token::Number(from_int_chars(10, &pre_dot))
}

fn from_int_chars(radix: u32, digits: &str) -> Number {
    let chars = digits.chars().filter(|c| *c != '_');
    let mut value: i64 = 0;

    for c in chars {
        value = value * (radix as i64) + digit_to_value(c) as i64;
    }

    if value as u64 & 0xFFFF_FFFF_0000_0000u64 != 0 {
        Number::Long(value)
    } else {
        Number::Int(value as i32)
    }
}

fn from_float_chars(pre_dot: &str, post_dot: &str, exp: &str) -> Number {
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

    Number::Float(value as f32)
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

fn trim_spaces(stream: &mut CodeCursor) {
    loop {
        let c = stream.read_u8(0);

        if c == b' ' || c == b'\t' || c == b'\r' || c == b'\n' {
            stream.next();
        } else {
            break;
        }
    }
}

fn trim_comments(stream: &mut CodeCursor) -> Result<(), KtError> {
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
                        code: stream.code_ref(),
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
    use crate::tokenizer::get_code_cursor;
    use crate::tokenizer::read_token;
    use crate::tokenizer::Token;

    use super::*;

    fn read_single_token(code: &str) -> Token {
        let input = from_str(code);
        let ref mut s = get_code_cursor(input);
        read_token(s).unwrap().1
    }

    #[test]
    fn check_basic_tokens() {
        let ref mut s = get_code_cursor(from_str("\
; ( ) { } [ ] < > @ : :: $ . .. , ?: ? ! -> .. + ++ - -- * / % = == === != !== += -= *= /= %= & && | ||\
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
            Token::Ampersand, Token::DoubleAmpersand, Token::Pipe, Token::DoublePipe,
            Token::EOF,
        ];

        let found = read_all_tokens(s).unwrap().into_iter().map(|(_, tk)| tk).collect::<Vec<_>>();

        assert_eq!(expected, found);
    }

    #[test]
    fn check_string_variables() {
        let ref mut s = get_code_cursor(from_str("\"Hello $world!!!\""));

        let expected = vec![
            Token::StringStart, Token::StringContent(String::from("Hello ")),
            Token::StringVariable(String::from("world")),
            Token::StringContent(String::from("!!!")), Token::StringEnd,
            Token::EOF,
        ];

        let found = read_all_tokens(s).unwrap().into_iter().map(|(_, tk)| tk).collect::<Vec<_>>();

        assert_eq!(expected, found);
    }

    #[test]
    fn check_string_templates() {
        let ref mut s = get_code_cursor(from_str("\"Hello ${world}!!!\""));

        let expected = vec![
            Token::StringStart, Token::StringContent(String::from("Hello ")),
            Token::StringTemplateStart, Token::Id(String::from("world")), Token::StringTemplateEnd,
            Token::StringContent(String::from("!!!")), Token::StringEnd,
            Token::EOF,
        ];

        let found = read_all_tokens(s).unwrap().into_iter().map(|(_, tk)| tk).collect::<Vec<_>>();

        assert_eq!(expected, found);
    }

    #[test]
    fn check_string_complex_templates() {
        let ref mut s = get_code_cursor(from_str("\"\"\"Hello ${println(\"Real ${1+2} Hello\")}!!!\"\"\""));

        let expected = vec![
            Token::StringStart, Token::StringContent(String::from("Hello ")), Token::StringTemplateStart,    // "Hello ${
            Token::Id(String::from("println")), Token::LeftParen,                                            //     println(
            Token::StringStart, Token::StringContent(String::from("Real ")), Token::StringTemplateStart,     //     "Real ${
            Token::Number(Number::Int(1)), Token::Plus, Token::Number(Number::Int(2)),                     //           1+2
            Token::StringTemplateEnd,                                                                          //      }
            Token::StringContent(String::from(" Hello")), Token::StringEnd, Token::RightParen,               //     Hello")
            Token::StringTemplateEnd,                                                                          // }
            Token::StringContent(String::from("!!!")), Token::StringEnd,                                     // !!!"
            Token::EOF,
        ];

        let found = read_all_tokens(s).unwrap().into_iter().map(|(_, tk)| tk).collect::<Vec<_>>();

        assert_eq!(expected, found);
    }


    #[test]
    fn check_float() {
        assert_eq!(Token::Number(Number::Float(12345.12345)), read_single_token("12345.12345"));
        assert_eq!(Token::Number(Number::Float(1.0)), read_single_token("1.0"));
        assert_eq!(Token::Number(Number::Float(0.0)), read_single_token("0.0"));
        assert_eq!(Token::Number(Number::Float(0.2e1)), read_single_token("0.2e1"));
        assert_eq!(Token::Number(Number::Float(1.2e1)), read_single_token("1.2e1"));
        assert_eq!(Token::Number(Number::Float(1.2e+1)), read_single_token("1.2e+1"));
        assert_eq!(Token::Number(Number::Float(1.2e-1)), read_single_token("1.2e-1"));
    }

    #[test]
    fn check_int() {
        assert_eq!(Token::Number(Number::Int(0)), read_single_token("0"));
        assert_eq!(Token::Number(Number::Int(1)), read_single_token("1"));
        assert_eq!(Token::Number(Number::Int(123445)), read_single_token("123445"));
        assert_eq!(Token::Number(Number::Int(0x1)), read_single_token("0x1"));
        assert_eq!(Token::Number(Number::Int(0x0)), read_single_token("0x0"));
        assert_eq!(Token::Number(Number::Int(1_000)), read_single_token("1_000"));
        assert_eq!(Token::Number(Number::Int(0b010101)), read_single_token("0b010101"));
    }

    #[test]
    #[ignore]
    fn check_char() {
        assert_eq!(Token::Char('a'), read_single_token("'a'"));
        assert_eq!(Token::Char('a'), read_single_token("\\u61"));
        assert_eq!(Token::Char('a'), read_single_token("\\0141"));
    }
}
