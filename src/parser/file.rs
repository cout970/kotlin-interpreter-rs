use crate::errors::KtError;
use crate::errors::ParserError;
use crate::parser::ast::Annotation;
use crate::parser::ast::FileAnnotation;
use crate::parser::ast::Modifier;
use crate::parser::ast::PackageHeader;
use crate::parser::ast::Preamble;
use crate::parser::TokenCursor;
use crate::tokenizer::Token;
use crate::parser::ast::Import;
use crate::parser::ast::KotlinFile;
use crate::parser::ast::TopLevelObject;
use crate::parser::ast::Function;
use crate::parser::ast::TypeParameter;
use crate::parser::ast::Type;
use crate::parser::ast::FunctionBody;
use crate::parser::ast::TypeConstraint;
use crate::parser::ast::Parameter;
use std::sync::Arc;
use crate::parser::ast::TypeReference;
use crate::parser::ast::SimpleUserType;
use crate::parser::ast::FunctionType;
use crate::create_vec;
use crate::parser::ast::CallSiteTypeParams;
use crate::parser::ast::FunctionParameter;
use crate::parser::ast::ParameterMutability;

pub fn read_file(s: &mut TokenCursor) -> Result<KotlinFile, KtError> {
    let preamble = read_preamble(s)?;
    let objects = s.many0(&read_top_level_object)?;
    Ok(KotlinFile { preamble, objects })
}

fn read_top_level_object(s: &mut TokenCursor) -> Result<TopLevelObject, KtError> {
    let modifiers = read_modifiers(s)?;

    let obj = match s.read_token(0) {
        Token::Id(name) => { // name == "fun"
            s.next();
            TopLevelObject::Function(read_function(s, modifiers)?)
        }
        _ => {
            return s.make_error_expected_of(vec![
                Token::Id(String::from("fun")),
            ]);
        }
    };

    Ok(obj)
}

//   : modifiers "fun"
//      typeParameters?
//      (type ".")?
//      SimpleName
//      typeParameters? valueParameters (":" type)?
//      typeConstraints
//      functionBody?
//  ;
fn read_function(s: &mut TokenCursor, modifiers: Vec<Modifier>) -> Result<Function, KtError> {
    let type_parameters = s.optional(&read_type_parameters).unwrap_or(vec![]);

    let save = s.save();
    let receiver = match s.optional(&read_type) {
        Some(ty) => {
            if s.optional_expect(Token::Dot) {
                Some(ty)
            } else {
                s.restore(save);
                None
            }
        }
        None => None
    };

    let name = s.expect_id()?;
    let type_parameters2 = s.optional(&read_type_parameters).unwrap_or(vec![]);
    let value_parameters = read_value_parameters(s)?;

    let return_type = if s.optional_expect(Token::Colon) {
        read_type(s)?
    } else {
        Type {
            annotations: vec![],
            reference: Arc::new(TypeReference::UserType(vec![
                SimpleUserType { name: String::from("Unit"), type_params: vec![] }
            ])),
        }
    };

    let type_constraints = read_type_constraints(s)?;
//    let body = s.optional(&read_function_body);
    let body = None;

    Ok(Function {
        modifiers,
        type_parameters,
        receiver,
        type_parameters2,
        name,
        value_parameters,
        return_type,
        type_constraints,
        body,
    })
}

fn read_type_parameters(s: &mut TokenCursor) -> Result<Vec<TypeParameter>, KtError> {
    s.expect(Token::LeftAngleBracket)?;
    let params = s.separated_by(Token::Comma, &read_type_parameter)?;
    s.expect(Token::RightAngleBracket)?;
    Ok(params)
}

fn read_type_parameter(s: &mut TokenCursor) -> Result<TypeParameter, KtError> {
    let modifiers = read_modifiers(s)?;
    let name = s.expect_id()?;
    let mut user_type = None;

    if s.optional_expect(Token::Colon) {
        user_type = Some(read_type(s)?);
    }

    Ok(TypeParameter {
        modifiers,
        name,
        user_type,
    })
}

fn read_type(s: &mut TokenCursor) -> Result<Type, KtError> {
    let modifiers = s.many0(&read_annotation)?;
    let reference = read_type_reference(s)?;

    Ok(Type { annotations: modifiers, reference })
}

fn read_annotation(s: &mut TokenCursor) -> Result<Annotation, KtError> {
    s.expect(Token::At)?;
    let names = s.separated_by(Token::Dot, &TokenCursor::expect_id)?;
    Ok(Annotation { names })
}

fn read_type_reference(s: &mut TokenCursor) -> Result<Arc<TypeReference>, KtError> {
    let ty = match s.read_token(0) {
        // Options
        //  (Int)           => Int
        //  (Int) -> Int    => func Int to Int
        //  () -> Int       => func that returns Int
        //  ()              => Error
        Token::LeftParen => {
            s.expect(Token::LeftParen)?;

            if s.optional_expect(Token::RightParen) {
                // ()
                s.expect(Token::LeftArrow)?;
                let return_type = read_type_reference(s)?;

                Arc::new(TypeReference::Function(FunctionType { receiver: None, parameters: vec![], return_type }))
            } else {
                // (Int
                let first = read_type(s)?;
                if s.optional_expect(Token::RightParen) {
                    // (Int)
                    if s.optional_expect(Token::LeftArrow) {
                        // (Int) -> // Expecting the return type
                        let return_type = read_type_reference(s)?;

                        Arc::new(TypeReference::Function(FunctionType { receiver: None, parameters: vec![first], return_type }))
                    } else {
                        // It's just parenthesis around a type
                        first.reference
                    }
                } else {
                    // (Int, // Expecting to read more params in this function type
                    s.expect(Token::Comma)?;
                    let rest = s.separated_by(Token::Comma, &read_type)?;
                    s.expect(Token::RightParen)?;
                    s.expect(Token::LeftArrow)?;
                    let return_type = read_type_reference(s)?;

                    Arc::new(TypeReference::Function(FunctionType { receiver: None, parameters: create_vec(first, rest), return_type }))
                }
            }
        }
        // java.lang.Int
        Token::Id(_) => {
            let ty = s.separated_by(Token::Dot, &read_simple_user_type)?;
            Arc::new(TypeReference::UserType(ty))
        }
        _ => {
            return s.make_error_expected_of(vec![
                Token::LeftParen, Token::Id(String::from("Identifier"))
            ]);
        }
    };

    if s.optional_expect(Token::QuestionMark) {
        Ok(Arc::new(TypeReference::Nullable(ty)))
    } else {
        Ok(ty)
    }
}

fn read_parameter_list(s: &mut TokenCursor) -> Result<Vec<Parameter>, KtError> {
    s.separated_by(Token::Comma, &read_parameter)
}

fn read_parameter(s: &mut TokenCursor) -> Result<Parameter, KtError> {
    let name = s.expect_id()?;
    s.expect(Token::Colon)?;
    let ty = read_type(s)?;

    Ok(Parameter { name, ty })
}

fn read_simple_user_type(s: &mut TokenCursor) -> Result<SimpleUserType, KtError> {
    let name = s.expect_id()?;
    let mut type_params = vec![];

    if s.optional_expect(Token::LeftAngleBracket) {
        type_params = s.separated_by(Token::Comma, &read_call_site_type_params)?;
        s.expect(Token::RightAngleBracket)?;
    }

    Ok(SimpleUserType { name, type_params })
}

fn read_call_site_type_params(s: &mut TokenCursor) -> Result<CallSiteTypeParams, KtError> {
    if s.optional_expect(Token::Asterisk) {
        Ok(CallSiteTypeParams::Projection)
    } else {
        let ty = read_type(s)?;
        Ok(CallSiteTypeParams::Type(ty))
    }
}

fn read_function_body(s: &mut TokenCursor) -> Result<FunctionBody, KtError> {
    unimplemented!()
}

fn read_type_constraints(s: &mut TokenCursor) -> Result<Vec<TypeConstraint>, KtError> {
    match s.expect_keyword("where") {
        Ok(_) => {
            let tc = s.separated_by(Token::Comma, &read_type_constraint)?;
            Ok(tc)
        }
        _ => Ok(vec![])
    }
}

fn read_type_constraint(s: &mut TokenCursor) -> Result<TypeConstraint, KtError> {
    let annotations = s.many0(&read_annotation)?;
    let name = s.expect_id()?;
    s.expect(Token::Colon)?;
    let ty = read_type(s)?;
    Ok(TypeConstraint { annotations, name, ty })
}

fn read_value_parameters(s: &mut TokenCursor) -> Result<Vec<FunctionParameter>, KtError> {
    s.expect(Token::LeftParen)?;
    let params = s.optional_separated_by(Token::Comma, &read_function_parameter)?;
    s.expect(Token::RightParen)?;

    Ok(params)
}

fn read_function_parameter(s: &mut TokenCursor) -> Result<FunctionParameter, KtError> {
    let modifiers = read_modifiers(s)?;
    let mut mutability = ParameterMutability::Default;

    if s.optional_expect_keyword("val") {
        mutability = ParameterMutability::Val;
    } else if s.optional_expect_keyword("var") {
        mutability = ParameterMutability::Var;
    }

    let name = s.expect_id()?;
    s.expect(Token::Colon)?;
    let ty = read_type(s)?;

    Ok(FunctionParameter { modifiers, mutability, name, ty })
}

fn read_preamble(s: &mut TokenCursor) -> Result<Preamble, KtError> {
    Ok(Preamble {
        file_annotations: s.many0(&read_file_annotation)?,
        package_header: s.optional(&read_package_header),
        imports: s.many0(&read_import)?,
    })
}

// "import" SimpleName{"."} ("." "*" | "as" SimpleName)? SEMI?
fn read_import(s: &mut TokenCursor) -> Result<Import, KtError> {
    s.expect_keyword("import")?;
    let mut path = s.separated_by(Token::Dot, &TokenCursor::expect_id)?;
    let mut alias = None;

    match s.read_token(0) {
        Token::Dot => {
            s.expect(Token::Dot)?;
            s.expect(Token::Asterisk)?;
            path.push(String::from("*"))
        }
        Token::Id(_) => {
            s.expect_keyword("as")?;
            alias = Some(s.expect_id()?);
        }
        _ => {}
    }
    s.optional_expect(Token::Semicolon);

    Ok(Import { path, alias })
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
        println!("{:?}", get_ast("import com.cout970.main", read_preamble));
        println!("{:?}", get_ast("import com.cout970.main.*", read_preamble));
        println!("{:?}", get_ast("import com.cout970.main as main2", read_preamble));
        println!("{:?}", get_ast("import com.cout970.m1; import com.cout970.m2;", read_preamble));
    }

    #[test]
    fn test_types() {
        println!("{:?}", get_ast("(Int, Int) -> Int", read_type));
        println!("{:?}", get_ast("((Int, Int) -> Int)", read_type));
        println!("{:?}", get_ast("() -> Int", read_type));
        println!("{:?}", get_ast("Int", read_type));
        println!("{:?}", get_ast("(Int)", read_type));
        println!("{:?}", get_ast("((Int) -> Int) -> Int", read_type));
        println!("{:?}", get_ast("Int?", read_type));
        println!("{:?}", get_ast("(Int)?", read_type));
        println!("{:?}", get_ast("(Int) -> Int?", read_type));
        println!("{:?}", get_ast("((Int) -> Int)?", read_type));
        println!("{:?}", get_ast("@Test Int", read_type));
        println!("{:?}", get_ast("List<Int>", read_type));
        println!("{:?}", get_ast("(List<*>) -> List<Int>", read_type));
        println!("{:?}", get_ast("(Map<String, Any>) -> Any", read_type));
    }

    #[test]
    fn test_fun() {
        println!("{:?}", get_ast("fun main()", read_top_level_object));
        println!("{:?}", get_ast("fun main(): Int", read_top_level_object));
        println!("{:?}", get_ast("fun main(a: Int): Int", read_top_level_object));
        println!("{:?}", get_ast("fun main(a: Int, b: Int): Int", read_top_level_object));
        println!("{:?}", get_ast("fun <T> main(c: T): T", read_top_level_object));
        println!("{:?}", get_ast("fun <T> main<T: Int>(c: T): T", read_top_level_object));
        println!("{:?}", get_ast("fun <T> main(c: List<T>): T", read_top_level_object));
//        println!("{:?}", get_ast("fun <T> main(c: Int = 0): T", read_top_level_object));
    }
}

