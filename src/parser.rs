use crate::errors::KtError;
use crate::errors::ParserError;
use crate::source_code::SourceCode;
use crate::source_code::Span;
use crate::tokenizer::Token;

pub struct TokenCursor {
    code: SourceCode,
    tokens: Vec<(Span, Token)>,
    pos: u32,
}

fn get_token_cursor(code: SourceCode, tokens: Vec<(Span, Token)>) -> TokenCursor {
    TokenCursor { code, tokens, pos: 0 }
}

impl TokenCursor {
    pub fn read_token(&mut self, offset: usize) -> Token {
        self.tokens[offset].1.clone()
    }

    pub fn read_token_span(&mut self, offset: usize) -> Span {
        self.tokens[offset].0
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

    pub fn many0<T, F>(&mut self, func: &F) -> Result<Vec<T>, KtError>
        where F: Fn(&mut TokenCursor) -> Result<T, KtError> {
        let mut accum: Vec<T> = vec![];

        loop {
            let res = match func(self) {
                Ok(pair) => pair,
                Err(_) => {
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
            let res = match func(self) {
                Ok(pair) => pair,
                Err(_) => {
                    break;
                }
            };
            accum.push(res);
        }

        Ok(accum)
    }

    pub fn optional<T, F>(&mut self, func: &F) -> Option<T>
        where F: Fn(&mut TokenCursor) -> Result<T, KtError> {
        match func(self) {
            Ok(t) => Some(t),
            Err(_) => None
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

    pub fn optional_expect(&mut self, tk: Token) {
        if tk == self.read_token(0) {
            self.next();
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

    pub fn complete<T, F>(&mut self, func: &F) -> Result<T, KtError>
        where F: Fn(&mut TokenCursor) -> Result<T, KtError> {
        let t = func(self)?;
        self.expect(Token::EOF)?;
        Ok(t)
    }
}


//pub fn elem_comma<T, F>(func: &F, input: Input) -> Result<(T, Input), KtError>
//    where F: Fn(Input) -> Result<(T, Input), KtError> {
//    let (res, i) = func(input)?;
//    let i = expect(Token::Comma, i)?;
//    Ok((res, i))
//}
//
//pub fn comma0<T, F>(func: &F, input: Input) -> Result<(Vec<T>, Input), KtError>
//    where F: Fn(Input) -> Result<(T, Input), KtError> {
//    let (first, mut i) = match func(input.clone()) {
//        Ok(pair) => pair,
//        Err(_) => {
//            return Ok((vec![], input));
//        }
//    };
//
//    let mut accum: Vec<T> = vec![first];
//
//    while let Token::Comma = i.read() {
//        let (next, rest) = func(i.next())?;
//        accum.push(next);
//        i = rest;
//    }
//
//    Ok((accum, i))
//}
//
//pub fn comma1<T, F>(func: &F, input: Input) -> Result<(Vec<T>, Input), KtError>
//    where F: Fn(Input) -> Result<(T, Input), KtError> {
//    let (first, mut i): (T, Input) = func(input)?;
//    let mut accum: Vec<T> = vec![first];
//
//    while let Token::Comma = i.read() {
//        let (next, rest) = func(i.next())?;
//        accum.push(next);
//        i = rest;
//    }
//
//    Ok((accum, i))
//}
//
//pub fn pipe1<T, F>(func: &F, input: Input) -> Result<(Vec<T>, Input), KtError>
//    where F: Fn(Input) -> Result<(T, Input), KtError> {
//    let (first, mut i): (T, Input) = func(input)?;
//    let mut accum: Vec<T> = vec![first];
//
//    while let Token::Pipe = i.read() {
//        let (next, rest) = func(i.next())?;
//        accum.push(next);
//        i = rest;
//    }
//
//    Ok((accum, i))
//}


//////// TEST


//pub fn from(c: &str) -> Input {
//    let tokens: Vec<TokenInfo> = tokenize(c.as_bytes()).expect("Tokenizer error");
//    Input::new(c.to_owned(), tokens)
//}
//
//pub fn test_parser<F, T: Debug>(func: F, code: &str)
//    where F: Fn(Input) -> Result<(T, Input), KtError> {
//    let input = from(code);
//    let result = complete(&func, input.clone());
//    match result {
//        Ok(res) => {
//            println!("Value: {:?}\n", res);
//        }
//        Err(error) => {
//            println!("Error: {}\n", ErrorWrapper::KtError(code.to_owned(), error));
//            panic!();
//        }
//    }
//}
//
//pub fn test_parser_result<F, T: Debug + PartialEq>(func: F, code: &str, value: T)
//    where F: Fn(Input) -> Result<(T, Input), KtError> {
//    let input = from(code);
//    let result = complete(&func, input.clone());
//    match result {
//        Ok(res) => {
//            println!("Value: {:?}\n", res);
//            assert_eq!(value, res);
//        }
//        Err(error) => {
//            println!("Error: {}\n", ErrorWrapper::KtError(code.to_owned(), error));
//            panic!();
//        }
//    }
//}
//
//pub fn test_parser_error<F, T: Debug>(func: F, code: &str)
//    where F: Fn(Input) -> Result<(T, Input), KtError> {
//    let input = from(code);
//    let result = complete(&func, input.clone());
//    match result {
//        Ok(res) => {
//            println!("Unexpected success: {:?}\n", res);
//            panic!();
//        }
//        Err(error) => {
//            println!("Error: {}\n", ErrorWrapper::KtError(code.to_owned(), error));
//        }
//    }
//}