use crate::errors::KtError;
use crate::errors::ParserError;
use crate::parser::ast::Annotation;
use crate::parser::ast::FileAnnotation;
use crate::parser::ast::Modifier;
use crate::parser::ast::PackageHeader;
use crate::parser::ast::Preamble;
use crate::parser::TokenCursor;
use crate::tokenizer::Token;

pub fn read_file(_s: &mut TokenCursor) -> Result<(), KtError> {
    Ok(())
}

fn read_preamble(s: &mut TokenCursor) -> Result<Preamble, KtError> {
    Ok(Preamble {
        file_annotations: s.many0(&read_file_annotation)?,
        package_header: s.optional(&read_package_header),
        imports: Vec::new(),
    })
}

fn read_package_header(s: &mut TokenCursor) -> Result<PackageHeader, KtError> {
    let modifiers = read_modifiers(s)?;
    s.expect_keyword("package")?;
    let path = s.separated_by(Token::Dot, &TokenCursor::expect_id)?;
    s.optional_expect(Token::Semicolon);

    Ok(PackageHeader { modifiers, path })
}

fn read_modifiers(s: &mut TokenCursor) -> Result<Vec<Modifier>, KtError> {
    s.many0(&|s| {
        let res = s.expect_id();
        match res {
            Ok(name) => {
                match name.as_str() {
                    /*"abstract" |*/ /*"final" |*/ "enum" | /*"open" |*/ "annotation" | "sealed" | "data" | // classModifier
                    "override" | "open" | "final" | "abstract" | "lateinit" | // memberModifier
                    "private" | "protected" | "public" | "internal" | // accessModifier
                    "in" | "out" | // varianceAnnotation
                    "noinline" | "crossinline" | "vararg" | // parameterModifier
                    "reified" | // typeParameterModifier
                    "tailrec" | "operator" | "infix" | "inline" | "external" | "suspend" | // functionModifier
                    "const" | // propertyModifier
                    "expect" | "actual" // multiPlatformModifier
                    => {
                        Ok(Modifier { name })
                    }
                    _ => s.make_error((0, 0), ParserError::ExpectedTokenId {
                        found: Token::Id(name.to_owned())
                    })
                }
            }
            Err(e) => Err(e)
        }
    })
}

fn read_file_annotation(s: &mut TokenCursor) -> Result<FileAnnotation, KtError> {
    s.expect(Token::At)?;
    s.expect_keyword("file")?;
    s.expect(Token::Colon)?;

    let annotations = match s.read_token(0) {
        Token::LeftBracket => {
            s.next();
            let n = s.many1(&read_unescaped_annotation)?;
            s.expect(Token::RightBracket)?;
            n
        }
        _ => {
            vec![read_unescaped_annotation(s)?]
        }
    };

    Ok(FileAnnotation { annotations })
}

fn read_unescaped_annotation(s: &mut TokenCursor) -> Result<Annotation, KtError> {
    let names = s.separated_by(Token::Dot, &TokenCursor::expect_id)?;
    Ok(Annotation { names })
}

#[cfg(test)]
mod tests {
    use crate::get_ast;

    use super::*;

    #[test]
    fn test_preamble() {
        println!("{:?}", get_ast("@file:test", read_preamble));
        println!("{:?}", get_ast("@file:[test.t.e.s.t test2]", read_preamble));
        println!("{:?}", get_ast("package com.cout970.main", read_preamble));
//        assert!(false);
    }
}

