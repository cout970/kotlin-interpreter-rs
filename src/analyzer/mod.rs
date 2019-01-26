pub mod semantic_rules;
pub mod typechecker;
pub mod ast;
pub mod tree_to_ast;

#[cfg(test)]
mod tests {
    use crate::analyzer::semantic_rules::Checker;
    use crate::analyzer::typechecker::TypeChecker;
    use crate::parser::parse_tree::KotlinFile;
    use crate::parser::Parser;
    use crate::source_code::from_str;
    use crate::source_code::SourceCode;
    use crate::tokenizer::Tokenizer;

    fn get_ast(code: SourceCode) -> KotlinFile {
        let ref mut s = Tokenizer::new(code.clone());
        let tks = s.read_tokens().expect(&format!("Tokenizer error"));

        let mut parser = Parser::new(code.clone(), tks);
        let ast = parser.parse_file().expect(&format!("Parsing error"));

        let checker = Checker::new(code.clone(), &ast);

        dbg!(checker.get_errors());

        ast
    }

    #[test]
    fn check_types() {
        let code = from_str(include_str!("../../examples/Fibonacci.kt"));
        let ast = get_ast(code.clone());
        TypeChecker::run(code.clone(), &ast);
//        assert_eq!(1, 0);
    }
}