use std::collections::VecDeque;

use crate::parser::{Parser, ParserError, ParserErrorKind};
use crate::source::{BytePos, ByteSpan, Source, SourceSpan};
use crate::source_cursor::SourceCursor;
use crate::token::Token;
use crate::token_stream::{TokenStream, TokenStreamError};
use backtrace::Backtrace;

const TOKEN_LOOK_AHEAD: usize = 4;

pub struct TokenCursor {
    stream: TokenStream,
    tokens: Vec<Token>,
    spans: Vec<ByteSpan>,
    pos: u32,
}

impl TokenCursor {
    pub fn new(stream: TokenStream) -> Self {
        Self {
            stream,
            tokens: Vec::new(),
            spans: Vec::new(),
            pos: 0,
        }
    }

    fn ensure_look_ahead(&mut self) -> Result<(), ParserError> {
        while self.tokens.len() < self.pos as usize + TOKEN_LOOK_AHEAD {
            match self.stream.next() {
                Ok((tk, span)) => {
                    self.tokens.push(tk);
                    self.spans.push(span);
                }
                Err(e) => {
                    return Err(ParserError {
                        span: e.span,
                        kind: ParserErrorKind::TokenStreamError {
                            kind: e.kind
                        },
                    });
                }
            }
        }
        Ok(())
    }

    pub fn init(&mut self) -> Result<(), ParserError> {
        self.ensure_look_ahead()
    }

    pub fn token(&self) -> &Token {
        &self.tokens[self.pos as usize]
    }

    pub fn offset_token(&self, offset: usize) -> &Token {
        debug_assert!(offset <= TOKEN_LOOK_AHEAD);

        &self.tokens[self.pos as usize + offset]
    }

    pub fn span(&self) -> ByteSpan {
        self.spans[self.pos as usize]
    }

    pub fn make_span(&self, start: BytePos) -> ByteSpan {
        ByteSpan::from(start, BytePos(start.0.max(self.end().0)))
    }

    pub fn make_source_span(&self, start: BytePos) -> SourceSpan {
        ByteSpan::from(start, self.end()).to_source_span(self.stream.source())
    }

    pub fn offset_span(&self, offset: usize) -> ByteSpan {
        debug_assert!(offset <= TOKEN_LOOK_AHEAD);
        self.spans[self.pos as usize + offset]
    }

    fn prev_token(&self) -> Token {
        if self.pos > 0 {
            self.tokens[self.pos as usize - 1].clone()
        } else {
            Token::EOF
        }
    }

    fn prev_span(&self) -> ByteSpan {
        if self.pos > 0 {
            self.spans[self.pos as usize - 1]
        } else {
            ByteSpan::new(0, 0)
        }
    }

    pub fn start(&self) -> BytePos {
        self.span().start()
    }

    pub fn end(&self) -> BytePos {
        self.prev_span().end()
    }

    pub fn next(&mut self) -> Result<(), ParserError> {
        debug_assert!(!self.tokens.is_empty());
        self.pos += 1;
        self.ensure_look_ahead()?;
        Ok(())
    }

    pub fn make_error<T>(&mut self, span: ByteSpan, kind: ParserErrorKind) -> Result<T, ParserError> {
        Err(ParserError {
            span: span.to_source_span(self.stream.source()),
            kind,
        })
    }

    pub fn make_error_expected_of<T>(&mut self, options: Vec<Token>) -> Result<T, ParserError> {
        let span = self.span();
        let found = self.token().clone();
        return self.make_error(span, ParserErrorKind::ExpectedTokenOf { found, options });
    }

    pub fn save(&self) -> u32 {
        self.pos
    }

    pub fn restore(&mut self, saved: u32) {
        self.pos = saved;
    }

    pub fn many0<T, F>(&mut self, func: &F) -> Result<Vec<T>, ParserError>
        where F: Fn(&mut TokenCursor) -> Result<T, ParserError> {
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

    pub fn many1<T, F>(&mut self, func: &F) -> Result<Vec<T>, ParserError>
        where F: Fn(&mut TokenCursor) -> Result<T, ParserError> {
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

    pub fn separated_by<T, F>(&mut self, tk: Token, func: &F) -> Result<Vec<T>, ParserError>
        where F: Fn(&mut TokenCursor) -> Result<T, ParserError> {
        let mut accum: Vec<T> = vec![];

        let first = func(self)?;
        accum.push(first);

        loop {
            let save = self.save();
            if !self.optional_expect(tk.clone())? {
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

    pub fn optional_separated_by<T, F>(&mut self, tk: Token, func: &F) -> Result<Vec<T>, ParserError>
        where F: Fn(&mut TokenCursor) -> Result<T, ParserError> {
        let mut accum: Vec<T> = vec![];

        let save = self.save();
        let res = match func(self) {
            Ok(pair) => pair,
            Err(_) => {
                self.restore(save);
                return Ok(vec![]);
            }
        };

        accum.push(res);

        while self.optional_expect(tk.clone())? {
            accum.push(func(self)?);
        }

        Ok(accum)
    }

    pub fn optional_separated_by_many1<T, F>(&mut self, tk: Token, func: &F) -> Result<Vec<T>, ParserError>
        where F: Fn(&mut TokenCursor) -> Result<T, ParserError> {
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
            if !self.optional_expect(tk.clone())? {
                break;
            } else {
                while self.optional_expect(tk.clone())? {}
            }
        }

        Ok(accum)
    }

    pub fn optional<T, F>(&mut self, func: &F) -> Option<T>
        where F: Fn(&mut TokenCursor) -> Result<T, ParserError> {
        let save = self.save();
        match func(self) {
            Ok(t) => Some(t),
            Err(_e) => {
                self.restore(save);
                None
            }
        }
    }

    pub fn optional_vec<T, F>(&mut self, func: &F) -> Vec<T>
        where F: Fn(&mut TokenCursor) -> Result<Vec<T>, ParserError> {
        let save = self.save();
        match func(self) {
            Ok(t) => t,
            Err(_e) => {
                self.restore(save);
                vec![]
            }
        }
    }

    pub fn chain<F1, F2, OS, OR>(&mut self, operators: &F1, operands: &F2) -> Result<(Vec<OS>, Vec<OR>), ParserError>
        where F1: Fn(&mut TokenCursor) -> Result<OR, ParserError>,
              F2: Fn(&mut TokenCursor) -> Result<OS, ParserError>,
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

    pub fn chain_break<F1, F2, OS, OR>(&mut self, operators: &F1, operands: &F2) -> Result<(Vec<OS>, Vec<OR>), ParserError>
        where F1: Fn(&mut TokenCursor) -> Result<OR, ParserError>,
              F2: Fn(&mut TokenCursor) -> Result<OS, ParserError>,
    {
        let mut accum_operands: Vec<OS> = vec![];
        let mut accum_operators: Vec<OR> = vec![];

        accum_operands.push(operands(self)?);

        if !self.at_newline() {
            while let Some(operator) = self.optional(&operators) {
                accum_operators.push(operator);
                accum_operands.push(operands(self)?);
                if self.at_newline() {
                    break;
                }
            }
        }

        Ok((accum_operands, accum_operators))
    }

    pub fn expect(&mut self, tk: Token) -> Result<(), ParserError> {
        if &tk == self.token() {
            // DEBUG
            // {
            //     let backtrace = Backtrace::new();
            //     let symbol_names = backtrace.frames().iter().flat_map(|i| i.symbols()).map(|i| i.name()).collect::<Vec<_>>();
            //     let none = None;
            //     let parent = symbol_names.get(1).unwrap_or(&none);
            //     println!("Expect: {} in {:?}", self.token(), parent);
            // }
            self.next()?;
            Ok(())
        } else {
            let span = self.span();
            let found = self.token().clone();

            return if tk == Token::EOF {
                self.make_error(
                    span, ParserErrorKind::UnexpectedToken { found },
                )
            } else {
                self.make_error(
                    span, ParserErrorKind::ExpectedToken { expected: tk, found },
                )
            };
        }
    }

    pub fn optional_expect(&mut self, tk: Token) -> Result<bool, ParserError> {
        if &tk == self.token() {
            self.next()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub fn semi(&mut self) -> Result<(), ParserError> {
        self.optional_expect(Token::Semicolon)?;
        Ok(())
    }

    pub fn at_newline(&mut self) -> bool {
        let start = self.prev_span().end();
        let end = self.span().start();
        let source = self.stream.source();

        return source.contains_newline(ByteSpan::from(start, end));
    }

    pub fn match_keyword(&self, keyword: &str) -> bool {
        if let Token::Id(name) = self.token() {
            if keyword == name {
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    pub fn match_token(&self, tk: Token) -> bool {
        self.token() == &tk
    }

    pub fn match_id(&self) -> bool {
        if let Token::Id(_) = self.token() {
            true
        } else {
            false
        }
    }

    pub fn eof(&self) -> bool {
        self.match_token(Token::EOF)
    }

    pub fn optional_expect_keyword(&mut self, keyword: &str) -> Result<bool, ParserError> {
        if let Token::Id(name) = self.token() {
            if keyword == name {
                self.next()?;
                Ok(true)
            } else {
                Ok(false)
            }
        } else {
            Ok(false)
        }
    }

    pub fn expect_id(&mut self) -> Result<String, ParserError> {
        if let Token::Id(name) = self.token().clone() {
            self.next()?;
            Ok(name)
        } else {
            let span = self.span();
            let found = self.token().clone();

            return self.make_error(
                span, ParserErrorKind::ExpectedTokenId { found },
            );
        }
    }

    pub fn expect_keyword(&mut self, keyword: &str) -> Result<String, ParserError> {
        if let Token::Id(name) = self.token().clone() {
            if &name == keyword {
                self.next()?;
                return Ok(name);
            }

            let span = self.span();
            let found = self.token().clone();

            self.make_error(
                span, ParserErrorKind::ExpectedTokenId { found },
            )
        } else {
            let span = self.span();
            let found = self.token().clone();

            self.make_error(
                span, ParserErrorKind::ExpectedTokenId { found },
            )
        }
    }

    pub fn complete<T, F>(&mut self, func: &F) -> Result<T, ParserError>
        where F: Fn(&mut TokenCursor) -> Result<T, ParserError> {
        let t = func(self)?;
        self.expect(Token::EOF)?;
        Ok(t)
    }
}

pub fn get_ast<F, T>(c: &str, func: F) -> T
    where F: Fn(&mut TokenCursor) -> Result<T, ParserError> {
    let mut cursor = TokenCursor::new(
        TokenStream::new(
            SourceCursor::new(
                Source::from_str(c)
            )
        )
    );

    func(&mut cursor).expect("Error returned by function")
}