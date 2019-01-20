pub mod semantic_rules;
pub mod typechecker;

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::analyzer::semantic_rules::Checker;
    use crate::analyzer::typechecker::TypeChecker;
    use crate::parser::ast::KotlinFile;
    use crate::parser::Parser;
    use crate::source_code::from_str;
    use crate::source_code::SourceCode;
    use crate::tokenizer::Tokenizer;

    use super::*;

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
        let ast = get_ast(from_str(include_str!("../../examples/Fibonacci.kt")));
        TypeChecker::run(&ast);
//        assert_eq!(1, 0);
    }
}