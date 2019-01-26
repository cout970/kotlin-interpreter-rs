use crate::errors::KtError;
use crate::parser::Parser;
use crate::source_code::from_str;
use crate::tokenizer::Tokenizer;
use crate::analyzer::tree_to_ast::file_to_ast;

pub fn assert_fails(code: &str) {
    let code = from_str(code);

    let ref mut tokenizer = Tokenizer::new(code.clone());
    let tokens = tokenizer.read_tokens().unwrap();

    let mut parser = Parser::new(code.clone(), tokens);
    let file = match parser.parse_file() {
        Ok(it) => it,
        Err(it) => {
            println!("ParsingError: {}\n", it);
            return
        }
    };

    let (ast, errors) = file_to_ast(code, &file);
    dbg!(ast);
    println!("\n\n");

    for x in &errors {
        println!("{:?}", x);
    }

    assert_ne!(errors.len(), 0);
}

pub fn assert_success(code: &str){
    let code = from_str(code);

    let ref mut tokenizer = Tokenizer::new(code.clone());
    let tokens = tokenizer.read_tokens().unwrap();

    let mut parser = Parser::new(code.clone(), tokens);
    let file = parser.parse_file().unwrap();

    let (ast, errors) = file_to_ast(code, &file);
    dbg!(ast);
    println!("\n\n");

    for x in &errors {
        println!("{:?}", x);
    }

    assert_eq!(errors.len(), 0);
}