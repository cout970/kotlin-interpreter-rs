// Development only {
// cargo watch -s 'clear && cargo test --color always 2>&1'
#![allow(dead_code)]
// }

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
