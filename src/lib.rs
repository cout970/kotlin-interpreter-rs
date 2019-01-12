// Development only {
// cargo watch -s 'clear && cargo test --color always 2>&1'
// cargo watch -c -q -x test
// cargo watch -c -q -s 'cargo rustc -- -Awarnings -Zno-codegen && cargo test'
#![allow(dead_code)]
// }

use std::fs;
use std::fs::File;
use std::io::Read;
//use std::io::stdout;
//use std::io::Write;
use std::path::Path;

use crate::errors::KtError;
use crate::parser::get_token_cursor;
use crate::parser::TokenCursor;
use crate::source_code::from_str;
use crate::source_code::SourceCode;
use crate::tokenizer::get_code_cursor;
use crate::tokenizer::read_all_tokens;

pub mod ast;
pub mod source_code;
pub mod tokenizer;
pub mod parser;
pub mod analyzer;
pub mod interpreter;
pub mod errors;
mod test_utils;

// Lang references:
// - https://kotlinlang.org/docs/kotlin-docs.pdf
// - https://kotlinlang.org/docs/reference/grammar.html
// - https://github.com/JetBrains/kotlin/blob/686cfa6fd29b8e096ea04a2b96e2dc08adced512/compiler/psi/src/org/jetbrains/kotlin/parsing/KotlinParsing.java
// - https://play.kotlinlang.org


#[derive(Clone, PartialEq, Debug)]
pub enum Number {
    Double(f64),
    Float(f32),
    Byte(i8),
    Short(i16),
    Int(i32),
    Long(i64),
}

fn get_all_source_files(path: &Path, result: &mut Vec<SourceCode>) {
    let file = File::open(path);
    let mut file = if let Ok(f) = file { f } else { return; };

    let meta = file.metadata().unwrap();

    if meta.is_dir() {
        let dir = fs::read_dir(path);
        let dir = if let Ok(f) = dir { f } else { return; };

        for p in dir {
            get_all_source_files(&p.unwrap().path(), result);
        }
    } else if meta.is_file() {
        if let Some(ext) = path.extension() {
            if ext == "kt" {
                let ref mut string = String::new();
                let size = file.read_to_string(string).unwrap();
                result.push(from_str(&string));

                println!("Reading file {:?} with size: {}", path, size);
            }
        }
    }
}

fn get_ast<F, T>(c: &str, func: F) -> T
    where F: Fn(&mut TokenCursor) -> Result<T, KtError> {
    let code = from_str(c);
    let mut code_cursor = get_code_cursor(code.clone());
    let tks = read_all_tokens(&mut code_cursor).unwrap();

    // Debug
    for (_, x) in &tks {
        print!("{} ", x);
    }
    println!();

    let mut token_cursor = get_token_cursor(code.clone(), tks);
    token_cursor.complete(&func).unwrap()
}

fn create_vec<T>(first: T, rest: Vec<T>) -> Vec<T> {
    let mut new = Vec::new();
    new.push(first);
    for x in rest.into_iter() {
        new.push(x);
    }
    new
}

#[inline]
fn map<A, B, F: Fn(A) -> B>(src: Vec<A>, func: F) -> Vec<B> {
    src.into_iter().map(func).collect::<Vec<B>>()
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::tokenizer::get_code_cursor;
    use crate::tokenizer::read_all_tokens;

    use super::*;

    #[test]
    #[ignore]
    fn get_code() {
        let ref mut codes: Vec<SourceCode> = vec![];
        get_all_source_files("/Data/Dev/Kotlin/Modeler/src/".as_ref(), codes);
        assert_eq!(codes.len(), 275);

        for code in codes {
            let ref mut s = get_code_cursor(code.clone());
            read_all_tokens(s).expect("Expected: All tokens to be correct");
        }
    }
}
