use crate::errors::KtError;
use crate::source_code::from_str;
use crate::analyzer::semantic_rules::Checker;
use crate::parser::Parser;
use crate::tokenizer::Tokenizer;

pub fn assert_fails(code: &str) -> Vec<KtError> {
    let code = from_str(code);

    let ref mut tokenizer = Tokenizer::new(code.clone());
    let tokens = tokenizer.read_tokens().unwrap();

    let mut parser = Parser::new(code.clone(), tokens);
    let ref mut ast = match parser.parse_file() {
        Ok(it) => it,
        Err(it) => {
            println!("ParsingError\n");
            return vec![it];
        }
    };

    println!("Ast: {:?}\n\n", ast);

    let mut ctx = Checker::new(code.clone());
    ctx.check(ast);

    let errors = ctx.get_errors();
    assert!(errors.len() > 0);
    errors
}

pub fn assert_success(code: &str) {
    let code = from_str(code);

    let ref mut tokenizer = Tokenizer::new(code.clone());
    let tokens = tokenizer.read_tokens().unwrap();

    let mut parser = Parser::new(code.clone(), tokens);
    let ref mut ast = parser.parse_file().unwrap();

    println!("Ast: {:?}\n\n", ast);

    let mut ctx = Checker::new(code.clone());
    ctx.check(ast);

    let errors = ctx.get_errors();
    for x in &errors {
        println!("{:?}", x);
    }

    assert_eq!(errors.len(), 0);
}