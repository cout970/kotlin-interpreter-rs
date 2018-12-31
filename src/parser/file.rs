use std::sync::Arc;

use crate::create_vec;
use crate::errors::KtError;
use crate::errors::ParserError;
use crate::map;
use crate::parser::ast::*;
use crate::parser::TokenCursor;
use crate::tokenizer::Literal;
use crate::tokenizer::Token;

macro_rules! create_operator_fun {
    ($name: ident, $(Token::$e: tt => $s: expr,)+) => {
        fn $name(s: &mut TokenCursor) -> Result<String, KtError> {
            let res = match s.read_token(0) {
                $(Token::$e => $s,)+
                _ => {
                    return s.make_error_expected_of(vec![
                        $(Token::$e,)+
                    ]);
                }
            };

            s.next();
            Ok(String::from(res))
        }
    };
}

fn expect_token(tk: Token) -> impl Fn(&mut TokenCursor) -> Result<(), KtError> {
    move |s: &mut TokenCursor| s.expect(tk.clone())
}

pub fn read_file(s: &mut TokenCursor) -> Result<KotlinFile, KtError> {
    let preamble = read_preamble(s)?;
    let objects = s.many0(&read_top_level_object)?;
    Ok(KotlinFile { preamble, objects })
}

fn read_top_level_object(s: &mut TokenCursor) -> Result<TopLevelObject, KtError> {
    let modifiers = read_modifiers(s)?;

    let obj = match s.read_token(0) {
        Token::Fun => {
            s.next();
            TopLevelObject::Function(read_function(s, modifiers)?)
        }
        Token::Val | Token::Var => {
            TopLevelObject::Property(read_property(s, modifiers)?)
        }
        _ => {
            return s.make_error_expected_of(vec![
                Token::Id(String::from("fun")),
            ]);
        }
    };

    Ok(obj)
}

fn read_property(s: &mut TokenCursor, modifiers: Vec<Modifier>) -> Result<Property, KtError> {
//    : modifiers ("val" | "var")
//        typeParameters?
//        (type ".")?
//        (multipleVariableDeclarations | variableDeclarationEntry)
//        typeConstraints
//        ("by" | "=" expression SEMI?)?
//        (getter? setter? | setter? getter?) SEMI?
//    ;

    let mutable = s.optional_expect(Token::Var);
    if !mutable { s.expect(Token::Val)?; }

    let type_parameters = read_type_parameters(s)?;

    let save = s.save();
    let receiver = match s.optional(&read_receiver_type) {
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

    let declarations = if let Token::LeftParen = s.read_token(0) {
        read_multiple_variable_declarations(s)?
    } else {
        vec![read_variable_declaration(s)?]
    };

    let type_constraints = read_type_constraints(s)?;

    let mut initialization = PropertyInitialization::None;

    if s.optional_expect_keyword("by") {
        let e = read_expresion(s)?;
        s.optional_expect(Token::Semicolon);
        initialization = PropertyInitialization::Delegation(e);
    } else if s.optional_expect(Token::Equals) {
        let e = read_expresion(s)?;
//        s.optional_expect(Token::Semicolon);
        initialization = PropertyInitialization::Expr(e);
    }

    let (getter, setter) = s.optional(&read_getter_setter)
        .unwrap_or((None, None));

    Ok(Property {
        modifiers,
        type_parameters,
        receiver,
        declarations,
        type_constraints,
        initialization,
        getter,
        setter,
    })
}

fn read_getter_setter(s: &mut TokenCursor) -> Result<(Option<PropertyGetter>, Option<PropertySetter>), KtError> {
    let modifiers = read_modifiers(s)?;
    let mut getter: Option<PropertyGetter> = None;
    let mut setter: Option<PropertySetter> = None;

    match s.read_token(0) {
        Token::Id(ref t) if t == "get" => {
            getter = Some(read_property_getter(s, modifiers)?);
            let modifiers = read_modifiers(s)?;

            match s.read_token(0) {
                Token::Id(ref t) if t == "set" => {
                    setter = Some(read_property_setter(s, modifiers)?);
                }
                _ => {}
            }
        }
        Token::Id(ref t) if t == "set" => {
            setter = Some(read_property_setter(s, modifiers)?);
            let modifiers = read_modifiers(s)?;

            match s.read_token(0) {
                Token::Id(ref t) if t == "get" => {
                    getter = Some(read_property_getter(s, modifiers)?);
                }
                _ => {}
            }
        }
        _ => {
            return s.make_error_expected_of(vec![Token::Id(String::from("set")), Token::Id(String::from("get"))]);
        }
    }

    Ok((getter, setter))
}

fn read_property_getter(s: &mut TokenCursor, modifiers: Vec<Modifier>) -> Result<PropertyGetter, KtError> {
    s.expect_keyword("get")?;
    if s.optional_expect(Token::LeftParen) {
        s.expect(Token::RightParen)?;

        let mut ty = None;

        if s.optional_expect(Token::Colon) {
            ty = Some(read_type(s)?);
        }

        let body = Some(read_function_body(s)?);

        Ok(PropertyGetter { modifiers, ty, body })
    } else {
        Ok(PropertyGetter { modifiers, ty: None, body: None })
    }
}

fn read_property_setter(s: &mut TokenCursor, modifiers: Vec<Modifier>) -> Result<PropertySetter, KtError> {
    s.expect_keyword("set")?;
    if s.optional_expect(Token::LeftParen) {
        let param_modifiers = read_modifiers(s)?;
        let param_name = Some(s.expect_id()?);
        let mut param_ty = None;

        if s.optional_expect(Token::Colon) {
            param_ty = Some(read_type(s)?);
        }

        s.expect(Token::RightParen)?;
        let body = Some(read_function_body(s)?);

        Ok(PropertySetter { modifiers, param_modifiers, param_name, param_ty, body })
    } else {
        Ok(PropertySetter { modifiers, param_modifiers: vec![], param_name: None, param_ty: None, body: None })
    }
}

fn read_variable_declaration(s: &mut TokenCursor) -> Result<VariableDeclarationEntry, KtError> {
    let name = s.expect_id()?;
    let declared_type = if s.optional_expect(Token::Colon) {
        Some(read_type(s)?)
    } else {
        None
    };

    Ok(VariableDeclarationEntry { name, declared_type })
}

fn read_multiple_variable_declarations(s: &mut TokenCursor) -> Result<Vec<VariableDeclarationEntry>, KtError> {
    s.expect(Token::LeftParen)?;
    let decls = s.separated_by(Token::Comma, &read_variable_declaration)?;
    s.expect(Token::RightParen)?;
    Ok(decls)
}

// BEGIN Expr

create_operator_fun!(
    read_expr_assignment_operator,
    Token::Equals => "=",
    Token::PlusEquals => "+=",
    Token::MinusEquals => "-=",
    Token::TimesEquals => "*=",
    Token::DivEquals => "/=",
    Token::ModEquals => "%=",
);

fn read_expresion(s: &mut TokenCursor) -> Result<Expr, KtError> {
    let (dis, ops) = s.chain(&read_expr_assignment_operator, &read_expr_disjunction)?;
    Ok(Expr::Chain { operands: dis, operators: ops })
}

fn read_expr_disjunction(s: &mut TokenCursor) -> Result<Expr, KtError> {
    let (dis, ops) = s.chain(&expect_token(Token::DoubleAmpersand), &read_expr_conjunction)?;
    Ok(Expr::Chain { operands: dis, operators: map(ops, |_| String::from("&&")) })
}

fn read_expr_conjunction(s: &mut TokenCursor) -> Result<Expr, KtError> {
    let (dis, ops) = s.chain(&expect_token(Token::DoublePipe), &read_expr_equality_comparison)?;
    Ok(Expr::Chain { operands: dis, operators: map(ops, |_| String::from("||")) })
}

create_operator_fun!(
    read_expr_equality_operator,
    Token::DoubleEquals => "==",
    Token::TripleEquals => "===",
    Token::NotEquals => "!=",
    Token::NotDoubleEquals => "!=",
);

fn read_expr_equality_comparison(s: &mut TokenCursor) -> Result<Expr, KtError> {
    let (dis, ops) = s.chain(&read_expr_equality_operator, &read_expr_comparison)?;
    Ok(Expr::Chain { operands: dis, operators: ops })
}

// TODO add more tokens for <= >=
fn read_expr_comparison_operator(s: &mut TokenCursor) -> Result<String, KtError> {
    let res = match s.read_token(0) {
        Token::LeftAngleBracket => {
            if s.read_token(1) == Token::Equals {
                s.next();
                "<="
            } else {
                "<"
            }
        }
        Token::RightAngleBracket => {
            if s.read_token(1) == Token::Equals {
                s.next();
                ">="
            } else {
                ">"
            }
        }
        _ => {
            return s.make_error_expected_of(vec![Token::LeftAngleBracket, Token::RightAngleBracket]);
        }
    };

    s.next();
    Ok(String::from(res))
}

fn read_expr_comparison(s: &mut TokenCursor) -> Result<Expr, KtError> {
    let (dis, ops) = s.chain(&read_expr_comparison_operator, &read_name_infix)?;
    Ok(Expr::Chain { operands: dis, operators: ops })
}

fn read_name_infix(s: &mut TokenCursor) -> Result<Expr, KtError> {
    let expr = read_expr_elvis(s)?;

    match s.read_token(0) {
        Token::Is | Token::NotIs => {
            s.next();
            let ty = read_type(s)?;
            Ok(Expr::Is { expr: Arc::new(expr), ty })
        }
        _ => {
            let mut accum_operands = vec![];
            let mut accum_operators = vec![];

            accum_operands.push(expr);

            while let Some(operator) = s.optional(&read_expr_comparison_operator) {
                accum_operators.push(operator);
                accum_operands.push(read_expr_elvis(s)?);
            }

            Ok(Expr::Chain { operands: accum_operands, operators: accum_operators })
        }
    }
}

fn read_expr_elvis(s: &mut TokenCursor) -> Result<Expr, KtError> {
    let (dis, ops) = s.chain(&expect_token(Token::Elvis), &read_expr_infix_fun_call)?;
    Ok(Expr::Chain { operands: dis, operators: map(ops, |_| String::from(":?")) })
}

fn read_expr_infix_fun_call(s: &mut TokenCursor) -> Result<Expr, KtError> {
    let (dis, ops) = s.chain(&|s| s.expect_id(), &read_expr_range)?;
    Ok(Expr::InfixFun { parameters: dis, functions: ops })
}

fn read_expr_range(s: &mut TokenCursor) -> Result<Expr, KtError> {
    let (dis, ops) = s.chain(&expect_token(Token::DoubleDot), &read_expr_add)?;
    Ok(Expr::Chain { operands: dis, operators: map(ops, |_| String::from("..")) })
}

create_operator_fun!(
    read_expr_add_operator,
    Token::Plus => "+",
    Token::Minus => "-",
);

fn read_expr_add(s: &mut TokenCursor) -> Result<Expr, KtError> {
    let (dis, ops) = s.chain(&read_expr_add_operator, &read_expr_multiply)?;
    Ok(Expr::Chain { operands: dis, operators: ops })
}

create_operator_fun!(
    read_expr_multiply_operator,
    Token::Asterisk => "*",
    Token::Slash => "/",
    Token::Percent => "%",
);

fn read_expr_multiply(s: &mut TokenCursor) -> Result<Expr, KtError> {
    let (dis, ops) = s.chain(&read_expr_multiply_operator, &read_expr_type_rhs)?;
    Ok(Expr::Chain { operands: dis, operators: ops })
}

create_operator_fun!(
    read_expr_as_operator,
    Token::As => "as",
    Token::AsQuestionMark => "as?",
);

fn read_expr_type_rhs(s: &mut TokenCursor) -> Result<Expr, KtError> {
    let (dis, ops) = s.chain(&read_expr_as_operator, &read_expr_prefix_unary)?;
    Ok(Expr::Chain { operands: dis, operators: ops })
}

create_operator_fun!(
    read_expr_prefix_operator,
    Token::Minus => "-",
    Token::Plus => "+",
    Token::DoublePlus => "++",
    Token::DoubleMinus => "--",
    Token::ExclamationMark => "!",
);

fn read_expr_prefix_operation(s: &mut TokenCursor) -> Result<String, KtError> {
//  prefixUnaryOperation
//    : "-" : "+"
//    : "++" : "--"
//    : "!"
//    : annotations
//    : labelDefinition
//  ; TODO
    read_expr_prefix_operator(s)
}

fn read_expr_prefix_unary(s: &mut TokenCursor) -> Result<Expr, KtError> {
    let ops = s.many0(&read_expr_prefix_operation)?;
    let expr = read_expr_postfix_unary(s)?;
    Ok(Expr::Prefix { prefix: ops, expr: Arc::new(expr) })
}

fn read_expr_postfix_operation(s: &mut TokenCursor) -> Result<String, KtError> {
//  postfixUnaryOperation
//    : "++" : "--" : "!!"
//    : callSuffix
//    : arrayAccess
//    : memberAccessOperation postfixUnaryExpression
//  ; TODO
//    match s.read_token(0) {
//        Token::DoublePlus => "++",
//        Token::DoubleMinus => "--",
//        Token::DoubleExclamationMark => "!!",
//        // callSuffix
//        Token::LeftBracket => {
//
//        }
//        _ => panic!()
//    }
    s.make_error_expected_of(vec![])
}

fn read_expr_postfix_unary(s: &mut TokenCursor) -> Result<Expr, KtError> {
    let expr = read_expr_atomic(s)?;
    let ops = s.many0(&read_expr_postfix_operation)?;
    Ok(Expr::Postfix { expr: Arc::new(expr), postfix: ops })
}

fn read_expr_atomic(s: &mut TokenCursor) -> Result<Expr, KtError> {
    match s.read_token(0) {
        Token::LeftParen => {
            s.next();
            let e = read_expresion(s)?;
            s.expect(Token::RightParen)?;
            Ok(e)
        }
        Token::Id(name) => {
            s.next();
            Ok(Expr::Ref(name))
        }
        Token::True => {
            s.next();
            Ok(Expr::Boolean(true))
        }
        Token::False => {
            s.next();
            Ok(Expr::Boolean(false))
        }
        Token::Null => {
            s.next();
            Ok(Expr::Null)
        }
        Token::LitChar(a) => {
            s.next();
            Ok(Expr::Char(a))
        }
        Token::Literal(lit) => {
            s.next();
            match lit {
                Literal::Double(a) => Ok(Expr::Double(a)),
                Literal::Float(a) => Ok(Expr::Float(a)),
                Literal::Int(a) => Ok(Expr::Int(a)),
                Literal::Long(a) => Ok(Expr::Long(a)),
            }
        }
        _ => {
            s.make_error_expected_of(vec![
                Token::Id(String::from("A variable")),
                Token::True, Token::False,
                Token::LitChar('c'),
                Token::Null
            ])
        }
    }
}

// END Expr

fn read_function(s: &mut TokenCursor, modifiers: Vec<Modifier>) -> Result<Function, KtError> {
//   : modifiers "fun"
//      typeParameters?
//      (type ".")?
//      SimpleName
//      typeParameters? valueParameters (":" type)?
//      typeConstraints
//      functionBody?
//  ;
    let type_parameters = read_type_parameters(s)?;

    let save = s.save();
    let receiver = match s.optional(&read_receiver_type) {
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
    let type_parameters2 = read_type_parameters(s)?;
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
    let body = s.optional(&read_function_body);

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
    Ok(s.optional(&read_type_parameters_force).unwrap_or_else(|| vec![]))
}

fn read_type_parameters_force(s: &mut TokenCursor) -> Result<Vec<TypeParameter>, KtError> {
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
    let reference = read_type_reference(s, false)?;

    Ok(Type { annotations: modifiers, reference })
}

fn read_receiver_type(s: &mut TokenCursor) -> Result<Type, KtError> {
    let modifiers = s.many0(&read_annotation)?;
    let reference = read_type_reference(s, true)?;

    Ok(Type { annotations: modifiers, reference })
}

fn read_annotations(s: &mut TokenCursor) -> Result<Vec<Annotation>, KtError> {
    s.many0(&read_annotation)
}

fn read_annotation(s: &mut TokenCursor) -> Result<Annotation, KtError> {
    s.expect(Token::At)?;
    let names = s.separated_by(Token::Dot, &TokenCursor::expect_id)?;
    Ok(Annotation { names })
}

fn read_type_reference(s: &mut TokenCursor, receiver: bool) -> Result<Arc<TypeReference>, KtError> {
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
                let return_type = read_type_reference(s, false)?;

                Arc::new(TypeReference::Function(FunctionType { receiver: None, parameters: vec![], return_type }))
            } else {
                // (Int
                let first = read_type(s)?;
                if s.optional_expect(Token::RightParen) {
                    // (Int)
                    if s.optional_expect(Token::LeftArrow) {
                        // (Int) -> // Expecting the return type
                        let return_type = read_type_reference(s, receiver)?;

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
                    let return_type = read_type_reference(s, receiver)?;

                    Arc::new(TypeReference::Function(FunctionType { receiver: None, parameters: create_vec(first, rest), return_type }))
                }
            }
        }
        // java.lang.Int
        Token::Id(_) => {
            let ty = if !receiver {
                s.separated_by(Token::Dot, &read_simple_user_type)?
            } else {
                let mut sum = vec![];
                sum.push(read_simple_user_type(s)?);

                loop {
                    if s.read_token(0) != Token::Dot {
                        break;
                    }

                    // If we are in  the case `val java.lang.Integer.neg: Int get() = -this`
                    // we need to stop before the name 'neg'
                    if let Token::Id(_) = s.read_token(1) {
                        if s.read_token(2) != Token::Dot {
                            break;
                        }
                    }

                    s.next();
                    sum.push(read_simple_user_type(s)?);
                }
                sum
            };

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
    if s.optional_expect(Token::Equals) {
        Ok(FunctionBody::Expression(read_expresion(s)?))
    } else {
        Ok(FunctionBody::Block(read_block(s)?))
    }
}

fn read_block(s: &mut TokenCursor) -> Result<Vec<Statement>, KtError> {
    s.expect(Token::LeftBrace)?;
    let statements = read_statements(s)?;
    s.expect(Token::RightBrace)?;
    Ok(statements)
}

fn read_statements(s: &mut TokenCursor) -> Result<Vec<Statement>, KtError> {
    s.many0(&expect_token(Token::Semicolon))?;
    let statements = s.optional_separated_by_many1(Token::Semicolon, &read_statement)?;
    s.many0(&expect_token(Token::Semicolon))?;
    Ok(statements)
}

fn read_statement(s: &mut TokenCursor) -> Result<Statement, KtError> {
    match s.optional(&read_declaration) {
        Some(t) => Ok(Statement::Decl(t)),
        None => Ok(Statement::Expr(read_expresion(s)?))
    }
}

fn read_declaration(s: &mut TokenCursor) -> Result<Declaration, KtError> {
    let modifiers = read_modifiers(s)?;

    let res = match s.read_token(0) {
        Token::Fun => Declaration::Function(read_function(s, modifiers)?),
        Token::Val | Token::Var => Declaration::Property(read_property(s, modifiers)?),
        Token::Class | Token::Interface => Declaration::Class(read_class(s, modifiers)?),
        Token::TypeAlias => Declaration::TypeAlias(read_typealias(s, modifiers)?),
        Token::Object => Declaration::Object(read_object(s, modifiers)?),
        _ => {
            return s.make_error_expected_of(vec![
                Token::Fun,
                Token::Val,
                Token::Var,
                Token::Class,
                Token::Interface,
                Token::TypeAlias,
                Token::Object,
            ]);
        }
    };

    Ok(res)
}

fn read_class(s: &mut TokenCursor, modifiers: Vec<Modifier>) -> Result<Class, KtError> {
//    class
//    :   modifiers ("class" | "interface") SimpleName
//        typeParameters?
//        primaryConstructor?
//        (":" annotations delegationSpecifier{","})?
//        typeConstraints
//        (classBody? | enumClassBody)
//    ;

    let interface = s.optional_expect(Token::Interface);
    let mut class_type = ClassType::Interface;

    if !interface {
        s.expect(Token::Class)?;

        if modifiers.iter().any(|m| m.name == "enum") {
            class_type = ClassType::Enum;
        } else if modifiers.iter().any(|m| m.name == "annotation") {
            class_type = ClassType::Annotation;
        } else {
            class_type = ClassType::Class;
        }
    }

    let name = s.expect_id()?;
    let type_parameters = read_type_parameters(s)?;
    let primary_constructor = s.optional(&read_primary_constructor);

    let mut annotations = vec![];
    let mut delegations = vec![];

    if s.optional_expect(Token::Colon) {
        annotations = read_annotations(s)?;
        delegations = s.separated_by(Token::Comma, &read_delegation_specifier)?;
    }

    let type_constraints = read_type_constraints(s)?;

    let body = if class_type == ClassType::Enum {
        Some(read_enum_body(s)?)
    } else {
        s.optional(&read_class_body)
    };

    Ok(Class {
        modifiers,
        class_type,
        name,
        type_parameters,
        primary_constructor,
        annotations,
        delegations,
        type_constraints,
        body,
    })
}

fn read_primary_constructor(s: &mut TokenCursor) -> Result<PrimaryConstructor, KtError> {
    //  (modifiers "constructor")? ("(" functionParameter{","} ")")
    fn primary_constructor_start(s: &mut TokenCursor) -> Result<Vec<Modifier>, KtError> {
        let m = read_modifiers(s)?;
        s.expect_keyword("constructor")?;
        Ok(m)
    }

    let modifiers = s.optional(&primary_constructor_start).unwrap_or(vec![]);
    s.expect(Token::LeftParen)?;
    let params = s.separated_by(Token::Comma, &read_function_parameter)?;
    s.expect(Token::RightParen)?;

    Ok(PrimaryConstructor {
        modifiers,
        params,
    })
}

fn read_delegation_specifier(s: &mut TokenCursor) -> Result<DelegationSpecifier, KtError> {
    let user_type = s.separated_by(Token::Dot, &read_simple_user_type)?;
    let ty = Type{ annotations: vec![], reference: Arc::new(TypeReference::UserType(user_type)) };

    if s.optional_expect_keyword("by") {
        let expr = read_expresion(s)?;
        Ok(DelegationSpecifier::DelegatedBy(ty, expr))
    } else {
        match s.optional(&read_call_suffix) {
            Some(t) => {
                Ok(DelegationSpecifier::FunctionCall(ty))
            },
            None => {
                Ok(DelegationSpecifier::Type(ty))
            }
        }
    }
}

fn read_call_suffix(s: &mut TokenCursor) -> Result<ClassBody, KtError> {
    unimplemented!()
}

fn read_class_body(s: &mut TokenCursor) -> Result<ClassBody, KtError> {
    unimplemented!()
}

fn read_enum_body(s: &mut TokenCursor) -> Result<ClassBody, KtError> {
    unimplemented!()
}

fn read_typealias(s: &mut TokenCursor, modifiers: Vec<Modifier>) -> Result<TypeAlias, KtError> {
    s.expect(Token::TypeAlias)?;
    let name = s.expect_id()?;
    let type_parameters = read_type_parameters(s)?;
    s.expect(Token::Equals)?;
    let ty = read_type(s)?;
    Ok(TypeAlias { name, type_parameters, ty })
}

fn read_object(s: &mut TokenCursor, modifiers: Vec<Modifier>) -> Result<Object, KtError> {
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

    if s.optional_expect(Token::Val) {
        mutability = ParameterMutability::Val;
    } else if s.optional_expect(Token::Var) {
        mutability = ParameterMutability::Var;
    }

    let name = s.expect_id()?;
    s.expect(Token::Colon)?;
    let ty = read_type(s)?;
    let mut default_value = None;

    if s.optional_expect(Token::Equals) {
        default_value = Some(read_expresion(s)?);
    }

    Ok(FunctionParameter { modifiers, mutability, name, ty, default_value })
}

fn read_preamble(s: &mut TokenCursor) -> Result<Preamble, KtError> {
    Ok(Preamble {
        file_annotations: s.many0(&read_file_annotation)?,
        package_header: s.optional(&read_package_header),
        imports: s.many0(&read_import)?,
    })
}

fn read_import(s: &mut TokenCursor) -> Result<Import, KtError> {
    // "import" SimpleName{"."} ("." "*" | "as" SimpleName)? SEMI?
    s.expect_keyword("import")?;
    let mut path = s.separated_by(Token::Dot, &TokenCursor::expect_id)?;
    let mut alias = None;

    match s.read_token(0) {
        Token::Dot => {
            s.expect(Token::Dot)?;
            s.expect(Token::Asterisk)?;
            path.push(String::from("*"))
        }
        Token::As => {
            s.next();
            alias = Some(s.expect_id()?);
        }
        _ => {}
    }
    s.optional_expect(Token::Semicolon);

    Ok(Import { path, alias })
}

fn read_package_header(s: &mut TokenCursor) -> Result<PackageHeader, KtError> {
    let modifiers = read_modifiers(s)?;
    s.expect(Token::Package)?;
    let path = s.separated_by(Token::Dot, &TokenCursor::expect_id)?;
    s.optional_expect(Token::Semicolon);

    Ok(PackageHeader { modifiers, path })
}

fn read_modifiers(s: &mut TokenCursor) -> Result<Vec<Modifier>, KtError> {
    s.many0(&|s| {
        let start = s.pos;
        match s.read_token(0) {
            Token::Id(name) => {
                s.next();
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
                    => Ok(Modifier { name }),
                    _ => s.make_error((start, s.pos), ParserError::ExpectedTokenId {
                        found: Token::Id(name.to_owned())
                    })
                }
            }
            Token::In => Ok(Modifier { name: String::from("in") }),
            _ => {
                let tk = s.read_token(0);
                s.next();
                s.make_error((start, s.pos), ParserError::ExpectedTokenId {
                    found: tk
                })
            }
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
    fn test_complex_code() {
        println!("{:?}", get_ast(
            r#"fun main(args: Array<String>) {
                    val hello = 0
                    val world = 1

                    println // (hello + world)
                 }"#,
            read_file));
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
        println!("{:?}", get_ast("fun <T> java.lang.Integer.main(c: List<T>): T", read_top_level_object));
        println!("{:?}", get_ast("fun <T> main(c: Int = 0): T", read_top_level_object));
    }

    #[test]
    fn test_property() {
        println!("{:?}", get_ast("val a", read_top_level_object));
        println!("{:?}", get_ast("var a", read_top_level_object));
        println!("{:?}", get_ast("val (a, b)", read_top_level_object));
        println!("{:?}", get_ast("val a: Int", read_top_level_object));
        println!("{:?}", get_ast("val Int.a: Int", read_top_level_object));
        println!("{:?}", get_ast("var a: Int internal set", read_top_level_object));
        println!("{:?}", get_ast("var a: Int private set", read_top_level_object));
        println!("{:?}", get_ast("var a: Int private get", read_top_level_object));
        println!("{:?}", get_ast("var a: Int get() { field }", read_top_level_object));
        println!("{:?}", get_ast("var a: Int set(a) { field = a}", read_top_level_object));
        println!("{:?}", get_ast("var a: Int set(a: Int) { field = a}", read_top_level_object));
        println!("{:?}", get_ast("var a: Int set(a: Int) {} get() = 0", read_top_level_object));
    }

    #[test]
    fn test_expr() {
        println!("{:?}", get_ast("a", read_expresion));
        println!("{:?}", get_ast("(a)", read_expresion));
        println!("{:?}", get_ast("a + b", read_expresion));
        println!("{:?}", get_ast("a * b", read_expresion));
        println!("{:?}", get_ast("a plus b", read_expresion));
//        println!("{:?}", get_ast("a++", read_expresion));
//        println!("{:?}", get_ast("a[0]", read_expresion));
        println!("{:?}", get_ast("++a", read_expresion));
        println!("{:?}", get_ast("false || (true && false)", read_expresion));
        println!("{:?}", get_ast("null ?: 1", read_expresion));
        println!("{:?}", get_ast("1 + 2 / 3", read_expresion));
        println!("{:?}", get_ast("1.5 + 6.4", read_expresion));
        println!("{:?}", get_ast("'c'", read_expresion));
        println!("{:?}", get_ast("1 > 2", read_expresion));
        println!("{:?}", get_ast("1 >= 2", read_expresion));
        println!("{:?}", get_ast("1 == 3", read_expresion));
        println!("{:?}", get_ast("1 !== 3", read_expresion));
        println!("{:?}", get_ast("1 === 3", read_expresion));
        println!("{:?}", get_ast("a = 5", read_expresion));
        println!("{:?}", get_ast("a += 5", read_expresion));
    }

    #[test]
    fn test_read_typealias() {
        println!("{:?}", get_ast("public typealias MyList = List", read_statement));
        println!("{:?}", get_ast("typealias Set<T> = Hashmap<T, Any>", read_statement));
    }
}

