use crate::errors::KtError;
use crate::parser::get_token_cursor;
use crate::parser::parse_file;
use crate::source_code::from_str;
use crate::tokenizer::get_code_cursor;
use crate::tokenizer::read_all_tokens;
use crate::analyzer::semantic_rules::CheckCtx;
use crate::analyzer::semantic_rules::check_ast;

pub fn assert_fails(code: &str) -> Vec<KtError> {
    let code = from_str(code);

    let ref mut tk_cursor = get_code_cursor(code.clone());
    let tokens = read_all_tokens(tk_cursor).unwrap();

    let ref mut cursor = get_token_cursor(code.clone(), tokens);
    let ref mut ast = match parse_file(cursor) {
        Ok(it) => it,
        Err(it) => {
            println!("ParsingError\n");
            return vec![it];
        }
    };

    println!("Ast: {:?}\n\n", ast);

    let mut ctx = CheckCtx::new(code.clone());
    check_ast(&mut ctx, ast);

    assert!(ctx.errors.len() > 0);

    ctx.errors.into_iter()
        .map(|(span, info)| KtError::Analyser { code: code.clone(), span, info })
        .collect()
}

pub fn assert_success(code: &str) {
    let code = from_str(code);

    let ref mut tk_cursor = get_code_cursor(code.clone());
    let tokens = read_all_tokens(tk_cursor).unwrap();

    let ref mut cursor = get_token_cursor(code.clone(), tokens);
    let ref mut ast = parse_file(cursor).unwrap();

    println!("Ast: {:?}\n\n", ast);

    let mut ctx = CheckCtx::new(code.clone());
    check_ast(&mut ctx, ast);

    let len = ctx.errors.len();
    ctx.errors.into_iter()
        .map(|(span, info)| KtError::Analyser { code: code.clone(), span, info })
        .for_each(|it| println!("{:?}", it));

    assert_eq!(len, 0);
}