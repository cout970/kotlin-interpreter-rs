use crate::errors::KtError;
use crate::source_code::from_str;
use crate::tokenizer::get_code_cursor;
use crate::tokenizer::read_all_tokens;
use crate::analyzer::semantic_rules::Checker;
use crate::parser::Parser;

pub fn assert_fails(code: &str) -> Vec<KtError> {
    let code = from_str(code);

    let ref mut tk_cursor = get_code_cursor(code.clone());
    let tokens = read_all_tokens(tk_cursor).unwrap();

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

    let ref mut tk_cursor = get_code_cursor(code.clone());
    let tokens = read_all_tokens(tk_cursor).unwrap();

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