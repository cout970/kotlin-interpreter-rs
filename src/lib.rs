// Development only {
// cargo watch -s 'clear && cargo test --color always 2>&1'
#![allow(dead_code)]
// }

use std::sync::Arc;

mod tokenizer;

pub type SourceCode = Arc<Vec<u8>>;

pub fn from_str(code: &str) -> SourceCode {
    Arc::new(code.into())
}

#[cfg(test)]
mod tests {
    use crate::from_str;
    use crate::tokenizer::get_token_stream;
    use crate::tokenizer::read_token;

    #[test]
    fn it_works() {
        // https://kotlinlang.org/docs/reference/grammar.html
        let ref mut s = get_token_stream(from_str("+-@./$%!?"));
        read_token(s);

        assert_eq!(2 + 2, 4);
    }
}
