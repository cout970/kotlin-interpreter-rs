use crate::parser::Parser;
use crate::source_code::from_str;
use crate::tokenizer::Tokenizer;
use crate::analyzer::tree_to_ast::file_to_ast;
use crate::analyzer::typechecker::check_types;
use crate::analyzer::typechecker::CheckedFile;
use crate::source_code::SourceCode;
use crate::analyzer::ast::AstFile;

pub fn assert_fails(code: &str) {
    let code = from_str(code);

    let ref mut tokenizer = Tokenizer::new(code.clone());
    let tokens = tokenizer.read_tokens().unwrap();

    let file = match Parser::parse_kotlin_file(code.clone(), tokens) {
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

    let file = Parser::parse_kotlin_file(code.clone(), tokens).unwrap();

    let (ast, errors) = file_to_ast(code.clone(), &file);
    let ast = dbg!(ast);
    println!("\n\n");

    for x in &errors {
        println!("{:?}", x);
    }

    assert_eq!(errors.len(), 0);

    check_types(vec![CheckedFile{
        path: "<builtin>".to_string(),
        code: code.clone(),
        ast
    }]);
}

pub fn assert_correct_ast(code: &str) -> CheckedFile {
    let code = from_str(code);

    let ref mut tokenizer = Tokenizer::new(code.clone());
    let tokens = tokenizer.read_tokens().unwrap();

    let file = Parser::parse_kotlin_file(code.clone(), tokens).unwrap();

    let (ast, errors) = file_to_ast(code.clone(), &file);
    let ast = dbg!(ast);
    println!("\n\n");

    for x in &errors {
        println!("{:?}", x);
    }

    assert_eq!(errors.len(), 0);

    let (list, errors) = check_types(vec![CheckedFile{
        path: "<builtin>".to_string(),
        code: code.clone(),
        ast
    }]);

    for x in &errors {
        println!("{:?}", x);
    }

    assert_eq!(errors.len(), 0);

    return list.into_iter().next().unwrap();
}

pub fn assert_correct_ast2(code: &str) -> (SourceCode, AstFile) {
    let code = from_str(code);

    let ref mut tokenizer = Tokenizer::new(code.clone());
    let tokens = tokenizer.read_tokens().unwrap();

    let file = Parser::parse_kotlin_file(code.clone(), tokens).unwrap();

    let (ast, errors) = file_to_ast(code.clone(), &file);
    let ast = dbg!(ast);
    println!("\n\n");

    for x in &errors {
        println!("{:?}", x);
    }

    assert_eq!(errors.len(), 0);

    (code, ast)
}