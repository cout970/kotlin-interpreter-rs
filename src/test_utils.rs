use crate::errors::KtError;
use crate::parser::Parser;
use crate::source_code::from_str;
use crate::tokenizer::Tokenizer;

pub fn assert_fails(code: &str) -> Vec<KtError> {
    let code = from_str(code);

    let ref mut tokenizer = Tokenizer::new(code.clone());
    let tokens = tokenizer.read_tokens().unwrap();

    let mut parser = Parser::new(code.clone(), tokens);
    let ast = match parser.parse_file() {
        Ok(it) => it,
        Err(it) => {
            println!("ParsingError\n");
            return vec![it];
        }
    };

//    println!("Ast: {:?}\n\n", ast);
//
//    let ctx = Checker::new(code.clone(), ast);
//
//    let errors = ctx.get_errors();
//    assert!(errors.len() > 0);
//    errors

    vec![]
}

pub fn assert_success(code: &str) -> Vec<String> {
    let code = from_str(code);

    let ref mut tokenizer = Tokenizer::new(code.clone());
    let tokens = tokenizer.read_tokens().unwrap();

    let mut parser = Parser::new(code.clone(), tokens);
    let ast = parser.parse_file().unwrap();

//    println!("Ast: {:?}\n\n", ast);

//    let ctx = Checker::new(code.clone(), ast);
//
//    let errors = ctx.get_errors();
//    for x in &errors {
//        println!("{:?}", x);
//    }
//
//    assert_eq!(errors.len(), 0);
//
//    ctx.get_symbols()
    vec![]
}