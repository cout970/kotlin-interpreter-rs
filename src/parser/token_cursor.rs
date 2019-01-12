use crate::errors::KtError;
use crate::errors::ParserError;
use crate::source_code::from_str;
use crate::source_code::SourceCode;
use crate::source_code::Span;
use crate::tokenizer::token::Token;
use crate::tokenizer::Tokenizer;

pub struct TokenCursor {
    code: SourceCode,
    tokens: Vec<(Span, Token)>,
    pos: u32,
}

pub fn get_token_cursor(code: SourceCode, tokens: Vec<(Span, Token)>) -> TokenCursor {
    TokenCursor { code, tokens, pos: 0 }
}

pub fn get_ast<F, T>(c: &str, func: F) -> T
    where F: Fn(&mut TokenCursor) -> Result<T, KtError> {
    let code = from_str(c);
    let mut code_cursor = Tokenizer::new(code.clone());
    let tks = code_cursor.read_tokens().unwrap();

    // Debug {
    for (_, x) in &tks {
        print!("{} ", x);
    }
    println!();
    // }

    let mut token_cursor = get_token_cursor(code.clone(), tks);
    token_cursor.complete(&func).unwrap()
}

impl TokenCursor {
    pub fn read_token(&mut self, offset: usize) -> Token {
        self.tokens[self.pos as usize + offset].1.clone()
    }

    pub fn read_token_span(&mut self, offset: usize) -> Span {
        self.tokens[self.pos as usize + offset].0
    }

    pub fn start(&mut self) -> u32 {
        self.read_token_span(0).0
    }

    pub fn end(&mut self) -> u32 {
        if self.pos > 0 {
            (self.tokens[(self.pos - 1) as usize].0).1
        } else {
            0
        }
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

    pub fn save(&self) -> u32 {
        self.pos
    }

    pub fn restore(&mut self, saved: u32) {
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

    pub fn optional_separated_by_many1<T, F>(&mut self, tk: Token, func: &F) -> Result<Vec<T>, KtError>
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
            } else {
                while self.optional_expect(tk.clone()) {}
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

    pub fn optional_vec<T, F>(&mut self, func: &F) -> Vec<T>
        where F: Fn(&mut TokenCursor) -> Result<Vec<T>, KtError> {
        let save = self.save();
        match func(self) {
            Ok(t) => t,
            Err(_e) => {
                self.restore(save);
                vec![]
            }
        }
    }

    pub fn chain<F1, F2, OS, OR>(&mut self, operators: &F1, operands: &F2) -> Result<(Vec<OS>, Vec<OR>), KtError>
        where F1: Fn(&mut TokenCursor) -> Result<OR, KtError>,
              F2: Fn(&mut TokenCursor) -> Result<OS, KtError>,
    {
        let mut accum_operands: Vec<OS> = vec![];
        let mut accum_operators: Vec<OR> = vec![];

        accum_operands.push(operands(self)?);

        while let Some(operator) = self.optional(&operators) {
            accum_operators.push(operator);
            accum_operands.push(operands(self)?);
        }

        Ok((accum_operands, accum_operators))
    }

    pub fn expect(&mut self, tk: Token) -> Result<(), KtError> {
        if tk == self.read_token(0) {
            self.next();
            Ok(())
        } else {
            let span = self.read_token_span(0);
            let found = self.read_token(0);

            if tk == Token::EOF {
                return self.make_error(
                    span, ParserError::UnexpectedToken { found },
                );
            } else {
                return self.make_error(
                    span, ParserError::ExpectedToken { expected: tk, found },
                );
            }
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

    pub fn semi(&mut self) {
        self.optional_expect(Token::Semicolon);
    }

    pub fn at_newline(&mut self) -> bool {
        let previous = self.pos.max(1) - 1;
        let this = self.pos;

        let start = (self.tokens[previous as usize].0).1;
        let end = (self.tokens[this as usize].0).0;

        for i in start..end {
            if *self.code.get(i as usize).unwrap() == b'\n' {
                return true;
            }
        }
        false
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