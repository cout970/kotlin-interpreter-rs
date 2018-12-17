// Development only {
// cargo watch -s 'clear && cargo test --color always 2>&1'
#![allow(dead_code)]
// }

use std::fs;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use crate::source_code::from_str;
use crate::source_code::SourceCode;

pub mod tokenizer;
pub mod source_code;
pub mod errors;
pub mod parser;

// https://kotlinlang.org/docs/reference/grammar.html
// https://play.kotlinlang.org
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

#[cfg(test)]
mod tests {
    use crate::tokenizer::get_code_cursor;
    use crate::tokenizer::read_all_tokens;

    use super::*;

    #[test]
    fn get_code() {
        let ref mut codes: Vec<SourceCode> = vec![];
        get_all_source_files("/Data/Dev/Kotlin/ModelLoader/src/".as_ref(), codes);
        assert_eq!(codes.len(), 47);

        for code in codes {
            let ref mut s = get_code_cursor(code.clone());
            read_all_tokens(s).expect("Expected: All tokens to be correct");
        }
    }
}
