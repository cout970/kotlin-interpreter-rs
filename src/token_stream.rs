use crate::source::{ByteSpan, SourceSpan, Source};
use crate::source_cursor::SourceCursor;
use crate::token::{Token, Number};

pub struct TokenStream {
    cursor: SourceCursor,
    mode: Vec<TokenStreamMode>,
    brace_count: Vec<u32>,
}

#[derive(Eq, PartialEq, Copy, Clone)]
pub enum TokenStreamMode {
    Normal,
    String,
    MultilineString,
    StringTemplate,
}

#[derive(Debug, Clone)]
pub struct TokenStreamError {
    pub span: SourceSpan,
    pub kind: TokenStreamErrorKind,
}

#[derive(Debug, Clone)]
pub enum TokenStreamErrorKind {
    UnknownCharacter(u8),
    UnsupportedLiteralPrefix(char),
    ExpectedEndOfString,
    ExpectedEndOfChar,
    UnclosedComment,
    InvalidEscapeChar(char),
    ExpectedEndOfEscapedIdentifier,
}

impl TokenStream {
    pub fn new(cursor: SourceCursor) -> Self {
        Self {
            cursor,
            mode: vec![TokenStreamMode::Normal],
            brace_count: vec![0],
        }
    }

    pub fn source(&self) -> Source {
        self.cursor.source()
    }

    pub fn all(mut self) -> Result<Vec<(Token, ByteSpan)>, TokenStreamError> {
        let mut vec = vec![];

        loop {
            let pair = self.next()?;

            if pair.0 == Token::EOF {
                vec.push(pair);
                break;
            }

            vec.push(pair);
        }

        Ok(vec)
    }

    pub fn next(&mut self) -> Result<(Token, ByteSpan), TokenStreamError> {
        match &self.mode[self.mode.len() - 1] {
            TokenStreamMode::String => self.read_string_token(false),
            TokenStreamMode::MultilineString => self.read_string_token(true),
            _ => self.read_normal_token()
        }
    }

    fn trim_spaces(&mut self) {
        loop {
            let byte = self.cursor.u8();

            if byte == b' ' || byte == b'\t' || byte == b'\r' || byte == b'\n' {
                self.cursor.next_byte();
            } else {
                break;
            }
        }
    }

    fn trim_comments(&mut self) -> Result<(), TokenStreamError> {
        loop {
            let start = self.cursor.pos();
            let c0 = self.cursor.u8();
            let c1 = self.cursor.offset_u8(1);

            if c0 == b'/' && c1 == b'/' {
                self.cursor.next_byte();
                self.cursor.next_byte();

                // Ignore everything until eof or newline
                loop {
                    let c = self.cursor.u8();

                    if c == 0 || c == b'\n' {
                        break;
                    }
                    self.cursor.next_byte();
                }

                self.trim_spaces();
            } else if c0 == b'/' && c1 == b'*' {
                self.cursor.next_byte();
                self.cursor.next_byte();

                loop {
                    let c0 = self.cursor.u8();
                    let c1 = self.cursor.offset_u8(1);

                    if c0 == 0 {
                        return Err(TokenStreamError {
                            span: self.cursor.source_span(start),
                            kind: TokenStreamErrorKind::UnclosedComment,
                        });
                    }

                    if c0 == b'*' && c1 == b'/' {
                        self.cursor.next_byte();
                        self.cursor.next_byte();
                        break;
                    }
                    self.cursor.next_byte();
                }

                self.trim_spaces();
            } else {
                break;
            }
        }

        Ok(())
    }

    /// Examples of strings and the generated tokens:
    /// "Hello world!!!" => StrStart StrContent("hello world!!!") StrEnd
    /// "Hello $world!!!" => StrStart StrContent("hello ") StrVariable("world") StrContent("!!!") StrEnd
    /// "Hello ${world}!!!" => StrStart StrContent("hello ") StrTemplateStart Id("world") StrTemplateEnd StrContent("!!!") StrEnd
    fn read_string_token(&mut self, multiline: bool) -> Result<(Token, ByteSpan), TokenStreamError> {
        let start = self.cursor.pos();

        let tk = match self.cursor.u8() {
            b'"' => {
                if multiline {
                    self.cursor.next_byte();
                    self.cursor.next_byte();
                }
                self.cursor.next_byte();
                self.mode.pop();
                self.brace_count.pop();
                Token::StringEnd
            }
            b'$' => {
                if self.cursor.offset_u8(1) == b'{' {
                    self.cursor.next_byte();
                    self.cursor.next_byte();
                    self.mode.push(TokenStreamMode::StringTemplate);
                    self.brace_count.push(1);
                    Token::StringTemplateStart
                } else {
                    self.read_string_token_variable()?
                }
            }
            _ => {
                self.read_string_token_content(multiline)?
            }
        };

        let span = self.cursor.span(start);

        Ok((tk, span))
    }

    fn read_normal_token(&mut self) -> Result<(Token, ByteSpan), TokenStreamError> {
        self.trim_spaces();
        self.trim_comments()?;
        let start = self.cursor.pos();
        let token = self.next_token()?;
        let span = self.cursor.span(start);

        Ok((token, span))
    }

    /// Read $variable
    fn read_string_token_variable(&mut self) -> Result<Token, TokenStreamError> {
        debug_assert_eq!(self.cursor.u8(), b'$');
        self.cursor.next_byte();

        let mut content = String::new();

        loop {
            let c0 = self.cursor.char();

            if !c0.is_alphanumeric() {
                break;
            }

            content.push(c0);
            self.cursor.next_char();
        }

        Ok(Token::StringVariable(content))
    }

    /// Read ${...}
    fn read_string_token_content(&mut self, multiline: bool) -> Result<Token, TokenStreamError> {
        let start = self.cursor.pos();
        let mut content = String::new();

        loop {
            let c0 = self.cursor.offset_u8(0);
            let c1 = self.cursor.offset_u8(1);
            let c2 = self.cursor.offset_u8(2);

            // End of string
            if multiline {
                if c0 == b'"' && c1 == b'"' && c2 == b'"' {
                    break;
                }
            } else {
                if c0 == b'"' {
                    break;
                }
            }

            // End of file
            if c0 == b'\0' {
                return Err(TokenStreamError {
                    span: self.cursor.source_span(start),
                    kind: TokenStreamErrorKind::ExpectedEndOfString,
                });
            }

            // Escaped char
            if !multiline && c0 == b'\\' {
                self.cursor.next_byte();
                let c1 = self.cursor.char();
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
                self.cursor.next_char();
                continue;
            }

            // Variable
            if c0 == b'$' {
                if c1 == b'{' || c1.is_ascii_alphabetic() {
                    break;
                }
            }
            let c0 = self.cursor.char();

            content.push(c0);
            self.cursor.next_char();
        }

        Ok(Token::StringContent(content))
    }

    fn next_token(&mut self) -> Result<Token, TokenStreamError> {
        if self.cursor.u8() == 0 {
            return Ok(Token::EOF);
        }

        // look ahead characters
        let start = self.cursor.pos();
        let c0 = self.cursor.offset_u8(0);
        let c1 = self.cursor.offset_u8(1);

        let tk = match c0 {
//          b'\n' => Token::Newline,
            b';' => Token::Semicolon,
            b'(' => Token::LeftParen,
            b')' => Token::RightParen,
            b'{' => {
                let end = self.brace_count.len() - 1;
                self.brace_count[end] += 1;
                Token::LeftBrace
            }
            b'}' => {
                let end = self.brace_count.len() - 1;
                self.brace_count[end] -= 1;

                if self.mode[end] == TokenStreamMode::StringTemplate && self.brace_count[end] == 0 {
                    self.mode.pop();
                    self.brace_count.pop();
                    Token::StringTemplateEnd
                } else {
                    Token::RightBrace
                }
            }
            b'[' => Token::LeftBracket,
            b']' => Token::RightBracket,
            b'<' => match c1 {
                b'=' => {
                    self.cursor.next_byte();
                    Token::LessEquals
                }
                _ => Token::LeftAngleBracket
            },
            b'>' => match c1 {
                b'=' => {
                    self.cursor.next_byte();
                    Token::GreaterEquals
                }
                _ => Token::RightAngleBracket
            },
            b'@' => Token::At,
            b':' => match c1 {
                b':' => {
                    self.cursor.next_byte();
                    Token::DoubleColon
                }
                _ => Token::Colon
            },
            b'$' => Token::Dollar,
            b'.' => match c1 {
                b'.' => {
                    self.cursor.next_byte();
                    Token::DoubleDot
                }
                b'0'..=b'9' => self.read_number()?,
                _ => Token::Dot
            },
            b',' => Token::Comma,
            b'?' => match c1 {
                b':' => {
                    self.cursor.next_byte();
                    Token::Elvis
                }
                _ => Token::QuestionMark
            },
            b'!' => match c1 {
                b'=' => {
                    let c2 = self.cursor.offset_u8(2);
                    self.cursor.next_byte();
                    match c2 {
                        b'=' => {
                            self.cursor.next_byte();
                            Token::NotDoubleEquals
                        }
                        _ => Token::NotEquals
                    }
                }
                b'i' => {
                    let c2 = self.cursor.offset_u8(2);
                    let c3 = self.cursor.offset_u8(3);
                    let b = c3.is_ascii_alphabetic() || c3 == b'_';
                    match (c2, b) {
                        (b'n', false) => {
                            self.cursor.next_byte();
                            self.cursor.next_byte();
                            Token::NotIn
                        }
                        (b's', false) => {
                            self.cursor.next_byte();
                            self.cursor.next_byte();
                            Token::NotIs
                        }
                        _ => Token::ExclamationMark
                    }
                }
                b'!' => {
                    self.cursor.next_byte();
                    Token::DoubleExclamationMark
                }
                _ => Token::ExclamationMark
            },
            b'+' => match c1 {
                b'+' => {
                    self.cursor.next_byte();
                    Token::DoublePlus
                }
                b'=' => {
                    self.cursor.next_byte();
                    Token::PlusEquals
                }
                _ => Token::Plus
            },
            b'-' => match c1 {
                b'=' => {
                    self.cursor.next_byte();
                    Token::MinusEquals
                }
                b'-' => {
                    self.cursor.next_byte();
                    Token::DoubleMinus
                }
                b'>' => {
                    self.cursor.next_byte();
                    Token::LeftArrow
                }
                _ => Token::Minus
            },
            b'*' => match c1 {
                b'=' => {
                    self.cursor.next_byte();
                    Token::TimesEquals
                }
                _ => Token::Asterisk
            },
            b'/' => match c1 {
                b'=' => {
                    self.cursor.next_byte();
                    Token::DivEquals
                }
                _ => Token::Slash
            },
            b'%' => match c1 {
                b'=' => {
                    self.cursor.next_byte();
                    Token::ModEquals
                }
                _ => Token::Percent
            },
            b'=' => match c1 {
                b'=' => {
                    let c2 = self.cursor.offset_u8(2);
                    self.cursor.next_byte();
                    match c2 {
                        b'=' => {
                            self.cursor.next_byte();
                            Token::TripleEquals
                        }
                        _ => Token::DoubleEquals
                    }
                }
                _ => Token::Equals
            },
            b'&' => match c1 {
                b'&' => {
                    self.cursor.next_byte();
                    Token::DoubleAmpersand
                }
                _ => Token::Ampersand
            },
            b'|' => match c1 {
                b'|' => {
                    self.cursor.next_byte();
                    Token::DoublePipe
                }
                _ => Token::Pipe
            },
            b'"' => {
                let c2 = self.cursor.offset_u8(2);
                if c1 == b'"' && c2 == b'"' {
                    self.mode.push(TokenStreamMode::MultilineString);
                    self.brace_count.push(0);
                    self.cursor.next_byte();
                    self.cursor.next_byte();
                } else {
                    self.mode.push(TokenStreamMode::String);
                    self.brace_count.push(0);
                }
                Token::StringStart
            }
            b'\'' => {
                return self.read_char();
            }
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                return Ok(self.read_identifier_or_keyword());
            }
            b'`' => {
                return self.read_escaped_identifier();
            }
            b'0'..=b'9' => {
                return Ok(self.read_number()?);
            }
            _ => return Err(TokenStreamError {
                span: self.cursor.source_span(start),
                kind: TokenStreamErrorKind::UnknownCharacter(c0),
            })
        };

        self.cursor.next_byte();
        Ok(tk)
    }

    fn read_identifier_or_keyword(&mut self) -> Token {
        let mut id = String::new();

        loop {
            let c = self.cursor.char();
            if !c.is_alphanumeric() && c != '_' {
                break;
            }
            id.push(c);
            self.cursor.next_char();
        }

        match &id[..] {
            "as" => {
                if self.cursor.u8() == b'?' {
                    self.cursor.next_byte();
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
            "_" => Token::Underscore,
            _ => Token::Id(id)
        }
    }

    fn read_escaped_identifier(&mut self) -> Result<Token, TokenStreamError> {
        let start = self.cursor.pos();
        debug_assert_eq!(self.cursor.u8(), b'`');
        self.cursor.next_byte();

        let mut id = String::new();

        loop {
            let c = self.cursor.char();
            if c == '`' || c == '\0' || c == '\n' {
                break;
            }

            id.push(c);
            self.cursor.next_char();
        }

        if self.cursor.u8() != b'`' {
            return Err(TokenStreamError {
                span: self.cursor.source_span(start),
                kind: TokenStreamErrorKind::ExpectedEndOfEscapedIdentifier,
            });
        }
        self.cursor.next_byte();

        Ok(Token::Id(id))
    }

    fn read_char(&mut self) -> Result<Token, TokenStreamError> {
        let c: char;
        let start = self.cursor.pos();

        self.cursor.next_byte(); // skip '
        let c0 = self.cursor.char();
        self.cursor.next_char(); // skip char

        if c0 == '\\' {
            let c1 = self.cursor.u8();

            c = match c1 {
                b'0'..=b'3' => {
                    // TODO read octal number of 3 digits
                    'A'
                }
                b'b' => 0x08 as char,
                b't' => '\t',
                b'n' => '\n',
                b'f' => 0x0C as char,
                b'r' => '\r',
                b'"' => '"',
                b'\'' => '\'',
                b'\\' => '\\',
                b'u' => {
                    // TODO read hex number
                    'A'
                }
                _ => return Err(TokenStreamError {
                    span: self.cursor.source_span(start),
                    kind: TokenStreamErrorKind::InvalidEscapeChar(c0),
                })
            };

            self.cursor.next_byte();
        } else {
            c = c0;
        }

        if self.cursor.u8() != b'\'' {
            return Err(TokenStreamError {
                span: self.cursor.source_span(start),
                kind: TokenStreamErrorKind::ExpectedEndOfChar,
            });
        }

        self.cursor.next_byte(); // skip '

        Ok(Token::Char(c))
    }

    fn read_number(&mut self) -> Result<Token, TokenStreamError> {
        let start = self.cursor.pos();
        let digit = self.cursor.u8();

        let tk = match digit {
            b'0' => {
                match self.cursor.offset_u8(1) {
                    b'x' => {
                        self.cursor.next_byte();
                        self.cursor.next_byte();
                        self.read_hex()
                    }
                    b'o' => {
                        // Octal is not supported in the language
                        return Err(TokenStreamError {
                            span: self.cursor.source_span(start),
                            kind: TokenStreamErrorKind::UnsupportedLiteralPrefix('o'),
                        });
                    }
                    b'b' => {
                        self.cursor.next_byte();
                        self.cursor.next_byte();
                        self.read_binary()
                    }
                    b'.' => self.read_float_or_int(),
                    b'1'..=b'9' => self.read_float_or_int(),
                    b'0' => {
                        return Err(TokenStreamError {
                            span: self.cursor.source_span(start),
                            kind: TokenStreamErrorKind::UnsupportedLiteralPrefix('0'),
                        });
                    }
                    _ => {
                        self.cursor.next_byte();
                        match self.cursor.u8() {
                            b'L' => {
                                self.cursor.next_byte();
                                Token::Number(Number::Long(0))
                            }
                            b'F' | b'f' => {
                                self.cursor.next_byte();
                                Token::Number(Number::Float(0.0))
                            }
                            _ => Token::Number(Number::Int(0))
                        }
                    }
                }
            }
            _ => self.read_float_or_int(),
        };
        Ok(tk)
    }

    fn collect_chars<F: FnMut(char) -> bool>(&mut self, mut pred: F) -> String {
        let mut res = String::new();

        loop {
            let c = self.cursor.char();

            if pred(c) {
                res.push(c);
            } else if c != '_' {
                break;
            }

            self.cursor.next_char();
        }
        res
    }

    fn read_hex(&mut self) -> Token {
        let res = self.collect_chars(|c: char| c.is_ascii_hexdigit());
        Token::Number(Self::from_int_chars(16, &res))
    }

    fn read_binary(&mut self) -> Token {
        let res = self.collect_chars(|c: char| c == '0' || c == '1');
        Token::Number(Self::from_int_chars(2, &res))
    }

    fn read_float_or_int(&mut self) -> Token {
        let pre_dot = self.collect_chars(|c: char| Self::contains(c, '0', '9'));
        let float = self.cursor.u8() == b'.' && self.cursor.offset_u8(1).is_ascii_digit();

        if float {
            self.cursor.next_byte();
            let post_dot = self.collect_chars(|c: char| Self::contains(c, '0', '9'));

            let has_e = self.cursor.u8() == b'e';
            let suffix = self.cursor.offset_u8(1);
            let has_suffix = suffix == b'-' || suffix == b'+';
            let next_byte = if has_suffix {
                self.cursor.offset_u8(2)
            } else {
                self.cursor.offset_u8(1)
            };

            let mut exp = if has_e && next_byte.is_ascii_digit() {
                self.cursor.next_byte();
                if has_suffix {
                    self.cursor.next_byte();
                }
                self.collect_chars(|c: char| Self::contains(c, '0', '9'))
            } else {
                String::new()
            };

            if has_suffix {
                let mut s = String::new();
                // + or -
                s.push(suffix as char);

                for c in exp.chars() {
                    s.push(c);
                }
                exp = s;
            }

            if self.cursor.u8() == b'f' || self.cursor.u8() == b'F' {
                self.cursor.next_byte();
                let num = Self::from_float_chars(&pre_dot, &post_dot, &exp);

                return match num {
                    Number::Double(n) => Token::Number(Number::Float(n as f32)),
                    Number::Float(n) => Token::Number(Number::Float(n as f32)),
                    Number::Byte(n) => Token::Number(Number::Float(n as f32)),
                    Number::Short(n) => Token::Number(Number::Float(n as f32)),
                    Number::Int(n) => Token::Number(Number::Float(n as f32)),
                    Number::Long(n) => Token::Number(Number::Float(n as f32)),
                };
            }

            return Token::Number(Self::from_float_chars(&pre_dot, &post_dot, &exp));
        }

        if self.cursor.u8() == b'f' || self.cursor.u8() == b'F' {
            self.cursor.next_byte();
            let num = Self::from_float_chars(&pre_dot, "", "");

            return match num {
                Number::Double(n) => Token::Number(Number::Float(n as f32)),
                Number::Float(n) => Token::Number(Number::Float(n as f32)),
                Number::Byte(n) => Token::Number(Number::Float(n as f32)),
                Number::Short(n) => Token::Number(Number::Float(n as f32)),
                Number::Int(n) => Token::Number(Number::Float(n as f32)),
                Number::Long(n) => Token::Number(Number::Float(n as f32)),
            };
        }

        if self.cursor.u8() == b'L' {
            self.cursor.next_byte();
            let num = Self::from_int_chars(10, &pre_dot);

            return match num {
                Number::Double(n) => Token::Number(Number::Long(n as i64)),
                Number::Float(n) => Token::Number(Number::Long(n as i64)),
                Number::Byte(n) => Token::Number(Number::Long(n as i64)),
                Number::Short(n) => Token::Number(Number::Long(n as i64)),
                Number::Int(n) => Token::Number(Number::Long(n as i64)),
                Number::Long(n) => Token::Number(Number::Long(n as i64)),
            };
        }

        Token::Number(Self::from_int_chars(10, &pre_dot))
    }

    fn from_int_chars(radix: u32, digits: &str) -> Number {
        let chars = digits.chars();
        let mut value: i64 = 0;

        for c in chars {
            value = value * (radix as i64) + Self::digit_to_value(c) as i64;
        }

        if value as u64 & 0xFFFF_FFFF_0000_0000u64 != 0 {
            Number::Long(value)
        } else {
            Number::Int(value as i32)
        }
    }

    fn from_float_chars(pre_dot: &str, post_dot: &str, exp: &str) -> Number {
        let pre_dot_chars = pre_dot.chars();
        let mut pre_dot_value: f64 = 0.0;

        for c in pre_dot_chars {
            pre_dot_value = pre_dot_value * 10.0 + Self::digit_to_value(c) as f64;
        }

        let post_dot_chars = post_dot.chars().rev();
        let mut post_dot_value: f64 = 0.0;

        for c in post_dot_chars {
            post_dot_value = (post_dot_value + Self::digit_to_value(c) as f64) / 10.0;
        }

        let mut value = pre_dot_value + post_dot_value;

        if !exp.is_empty() {
            let first = exp.chars().nth(0).unwrap();
            let sign: f64 = if first == '-' { -1.0 } else { 1.0 };

            let exp_chars = exp.chars().filter(|c| *c != '-' && *c != '+').rev();
            let mut exp_value: f64 = 0.0;

            for c in exp_chars {
                exp_value = exp_value * 10.0 + Self::digit_to_value(c) as f64;
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
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::source::Source;

    use super::*;

    fn from_str(s: &str) -> TokenStream {
        TokenStream::new(SourceCursor::new(Source::from_str(s)))
    }

    fn read_single_token(code: &str) -> Token {
        let mut input = from_str(code);

        input.next().unwrap().0
    }

    #[test]
    fn check_basic_tokens() {
        let s = from_str("\
; ( ) { } [ ] < > @ : :: $ . .. , ?: ? ! -> .. + ++ - -- * / % = == === != !== += -= *= /= %= & && | ||\
");

        let expected = vec![
            Token::Semicolon, Token::LeftParen, Token::RightParen, Token::LeftBrace, Token::RightBrace,
            Token::LeftBracket, Token::RightBracket, Token::LeftAngleBracket, Token::RightAngleBracket,
            Token::At, Token::Colon, Token::DoubleColon, Token::Dollar, Token::Dot, Token::DoubleDot,
            Token::Comma, Token::Elvis, Token::QuestionMark, Token::ExclamationMark, Token::LeftArrow,
            Token::DoubleDot, Token::Plus, Token::DoublePlus, Token::Minus, Token::DoubleMinus,
            Token::Asterisk, Token::Slash, Token::Percent, Token::Equals, Token::DoubleEquals,
            Token::TripleEquals, Token::NotEquals, Token::NotDoubleEquals, Token::PlusEquals,
            Token::MinusEquals, Token::TimesEquals, Token::DivEquals, Token::ModEquals,
            Token::Ampersand, Token::DoubleAmpersand, Token::Pipe, Token::DoublePipe,
            Token::EOF,
        ];

        let found = s.all()
            .unwrap()
            .into_iter()
            .map(|(tk, _)| tk)
            .collect::<Vec<_>>();

        assert_eq!(expected, found);
    }

    #[test]
    fn check_tokens_1() {
        let s = from_str("\
moveLayoutSplitterLeft.check(e) -> splitter -= 0.03125f\
");

        let expected = vec![
            Token::Id(String::from("moveLayoutSplitterLeft")),
            Token::Dot,
            Token::Id(String::from("check")),
            Token::LeftParen,
            Token::Id(String::from("e")),
            Token::RightParen,
            Token::LeftArrow,
            Token::Id(String::from("splitter")),
            Token::MinusEquals,
            Token::Number(Number::Float(0.03125)),
            Token::EOF,
        ];

        let found = s.all()
            .unwrap()
            .into_iter()
            .map(|(tk, _)| tk)
            .collect::<Vec<_>>();


        assert_eq!(expected, found);
    }

    #[test]
    fn check_string_variables() {
        let s = from_str("\"Hello $world!!!\"");

        let expected = vec![
            Token::StringStart, Token::StringContent(String::from("Hello ")),
            Token::StringVariable(String::from("world")),
            Token::StringContent(String::from("!!!")), Token::StringEnd,
            Token::EOF,
        ];

        let found = s.all()
            .unwrap()
            .into_iter()
            .map(|(tk, _)| tk)
            .collect::<Vec<_>>();

        assert_eq!(expected, found);
    }

    #[test]
    fn check_string_templates() {
        let s = from_str("\"Hello ${world}!!!\"");

        let expected = vec![
            Token::StringStart, Token::StringContent(String::from("Hello ")),
            Token::StringTemplateStart, Token::Id(String::from("world")), Token::StringTemplateEnd,
            Token::StringContent(String::from("!!!")), Token::StringEnd,
            Token::EOF,
        ];

        let found = s.all()
            .unwrap()
            .into_iter()
            .map(|(tk, _)| tk)
            .collect::<Vec<_>>();

        assert_eq!(expected, found);
    }

    #[test]
    fn check_string_complex_templates() {
        let s = from_str("\"\"\"Hello ${println(\"Real ${1+2} Hello\")}!!!\"\"\"");

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

        let found = s.all()
            .unwrap()
            .into_iter()
            .map(|(tk, _)| tk)
            .collect::<Vec<_>>();

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
        assert_eq!(Token::Number(Number::Float(0f32)), read_single_token("0f"));
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
    fn check_char() {
        assert_eq!(Token::Char('a'), read_single_token("'a'"));
        assert_eq!(Token::Char('a'), read_single_token("\\u61"));
        assert_eq!(Token::Char('a'), read_single_token("\\0141"));
    }
}