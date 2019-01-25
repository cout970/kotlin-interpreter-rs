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
use crate::parser::Parser;
use crate::source_code::from_str;
use crate::source_code::SourceCode;

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

// TODO move everything from here to util.rs or something
#[derive(Clone, PartialEq, Debug)]
pub enum Number {
    Double(f64),
    Float(f32),
    Byte(i8),
    Short(i16),
    Int(i32),
    Long(i64),
}

fn get_all_source_files(path: &Path, result: &mut Vec<(String, SourceCode)>) {
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
                let path_str = path.to_string_lossy();
                result.push((path_str.to_string(), from_str(&string)));

                println!("Reading file {:?} with size: {}", path, size);
            }
        }
    }
}

fn create_vec<T>(first: T, rest: Vec<T>) -> Vec<T> {
    let mut new = Vec::new();
    new.push(first);
    for x in rest.into_iter() {
        new.push(x);
    }
    new
}

fn vec_with<T: Clone>(vec: &Vec<T>, new: T) -> Vec<T> {
    let mut new_vec = vec.clone();
    new_vec.push(new);
    new_vec
}


#[inline]
fn map<A, B, F: Fn(A) -> B>(src: Vec<A>, func: F) -> Vec<B> {
    src.into_iter().map(func).collect::<Vec<B>>()
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use crate::tokenizer::Tokenizer;

    use super::*;
    use crate::analyzer::semantic_rules::Checker;
    use crate::analyzer::typechecker::TypeChecker;

    #[test]
    #[ignore]
    fn get_code() {
        let ref mut codes: Vec<(String, SourceCode)> = vec![];
        get_all_source_files("/Data/Dev/Kotlin/Modeler/src/".as_ref(), codes);
        assert_eq!(codes.len(), 275);

        let mut index = 1;
        for (path, code) in codes {
            println!("{}", index);
            let ref mut s = Tokenizer::new(code.clone());
            let tks = s.read_tokens().expect(&format!("Tokenizer error at {}", path));

            let mut parser = Parser::new(code.clone(), tks);
            let ast = parser.parse_file().expect(&format!("Parsing error at {}", path));

            let checker = Checker::new(code.clone(), &ast);

            for errors in checker.get_errors() {
                println!("{}", errors);
            }

            index += 1;
        }
        assert_ne!(0,0);
    }
}
