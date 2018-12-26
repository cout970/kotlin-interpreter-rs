use crate::errors::KtError;
use crate::errors::ParserError;
use crate::source_code::SourceCode;
use crate::source_code::Span;
use crate::tokenizer::Token;

mod ast;
mod file;

pub struct TokenCursor {
    code: SourceCode,
    tokens: Vec<(Span, Token)>,
    pos: u32,
}

pub fn get_token_cursor(code: SourceCode, tokens: Vec<(Span, Token)>) -> TokenCursor {
    TokenCursor { code, tokens, pos: 0 }
}

impl TokenCursor {
    pub fn read_token(&mut self, offset: usize) -> Token {
        self.tokens[self.pos as usize + offset].1.clone()
    }

    pub fn read_token_span(&mut self, offset: usize) -> Span {
        self.tokens[self.pos as usize + offset].0
    }

    pub fn next(&mut self) {
        self.pos += 1;
    }

    pub fn make_error<T>(&mut self, span: Span, info: ParserError) -> Result<T, KtError> {
        Err(KtError::Parser {
            code: self.code.clone(),
            span,
            info,
        })
    }

    pub fn make_error_expected_of<T>(&mut self, options: Vec<Token>) -> Result<T, KtError> {
        let span = self.read_token_span(0);
        let found = self.read_token(0);
        return self.make_error(span, ParserError::ExpectedTokenOf { found, options });
    }

    fn save(&self) -> u32 {
        self.pos
    }

    fn restore(&mut self, saved: u32) {
        self.pos = saved;
    }

    pub fn many0<T, F>(&mut self, func: &F) -> Result<Vec<T>, KtError>
        where F: Fn(&mut TokenCursor) -> Result<T, KtError> {
        let mut accum: Vec<T> = vec![];

        loop {
            let save = self.save();
            let res = match func(self) {
                Ok(pair) => pair,
                Err(_) => {
                    self.restore(save);
                    break;
                }
            };
            accum.push(res);
        }

        Ok(accum)
    }

    pub fn many1<T, F>(&mut self, func: &F) -> Result<Vec<T>, KtError>
        where F: Fn(&mut TokenCursor) -> Result<T, KtError> {
        let mut accum: Vec<T> = vec![];

        let first = func(self)?;
        accum.push(first);

        loop {
            let save = self.save();
            let res = match func(self) {
                Ok(pair) => pair,
                Err(_) => {
                    self.restore(save);
                    break;
                }
            };
            accum.push(res);
        }

        Ok(accum)
    }

    pub fn separated_by<T, F>(&mut self, tk: Token, func: &F) -> Result<Vec<T>, KtError>
        where F: Fn(&mut TokenCursor) -> Result<T, KtError> {
        let mut accum: Vec<T> = vec![];

        let first = func(self)?;
        accum.push(first);

        loop {
            let save = self.save();
            if !self.optional_expect(tk.clone()) {
                break;
            }
            let res = match func(self) {
                Ok(pair) => pair,
                Err(_) => {
                    self.restore(save);
                    break;
                }
            };
            accum.push(res);
        }

        Ok(accum)
    }

    pub fn optional_separated_by<T, F>(&mut self, tk: Token, func: &F) -> Result<Vec<T>, KtError>
        where F: Fn(&mut TokenCursor) -> Result<T, KtError> {
        let mut accum: Vec<T> = vec![];

        loop {
            let save = self.save();
            let res = match func(self) {
                Ok(pair) => pair,
                Err(_) => {
                    self.restore(save);
                    break;
                }
            };
            accum.push(res);
            if !self.optional_expect(tk.clone()) {
                break;
            }
        }

        Ok(accum)
    }

    pub fn optional<T, F>(&mut self, func: &F) -> Option<T>
        where F: Fn(&mut TokenCursor) -> Result<T, KtError> {
        let save = self.save();
        match func(self) {
            Ok(t) => Some(t),
            Err(_e) => {

//                println!("{}", e);
                self.restore(save);
                None
            }
        }
    }

    pub fn expect(&mut self, tk: Token) -> Result<(), KtError> {
        if tk == self.read_token(0) {
            self.next();
            Ok(())
        } else {
            let span = self.read_token_span(0);
            let found = self.read_token(0);

            return self.make_error(
                span, ParserError::ExpectedToken { expected: tk, found },
            );
        }
    }

    pub fn optional_expect(&mut self, tk: Token) -> bool {
        if tk == self.read_token(0) {
            self.next();
            true
        } else {
            false
        }
    }

    pub fn optional_expect_keyword(&mut self, keyword: &str) -> bool {
        if let Token::Id(name) = self.read_token(0) {
            if keyword == &name {
                self.next();
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    pub fn expect_id(&mut self) -> Result<String, KtError> {
        if let Token::Id(name) = self.read_token(0) {
            self.next();
            Ok(name)
        } else {
            let span = self.read_token_span(0);
            let found = self.read_token(0);

            return self.make_error(
                span, ParserError::ExpectedTokenId { found },
            );
        }
    }

    pub fn expect_keyword(&mut self, keyword: &str) -> Result<String, KtError> {
        if let Token::Id(name) = self.read_token(0) {
            if &name == keyword {
                self.next();
                return Ok(name);
            }

            let span = self.read_token_span(0);
            let found = self.read_token(0);

            return self.make_error(
                span, ParserError::ExpectedTokenId { found },
            );
        } else {
            let span = self.read_token_span(0);
            let found = self.read_token(0);

            return self.make_error(
                span, ParserError::ExpectedTokenId { found },
            );
        }
    }

    pub fn complete<T, F>(&mut self, func: &F) -> Result<T, KtError>
        where F: Fn(&mut TokenCursor) -> Result<T, KtError> {
        let t = func(self)?;
        self.expect(Token::EOF)?;
        Ok(t)
    }
}