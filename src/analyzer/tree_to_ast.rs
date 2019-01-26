use std::rc::Rc;

use crate::analyzer::ast::*;
use crate::analyzer::ast::AstProperty;
use crate::create_vec;
use crate::errors::AnalyserError;
use crate::errors::KtError;
use crate::interpreter::bytecode::Constant;
use crate::Number;
use crate::parser::parse_tree::*;
use crate::source_code::SourceCode;
use crate::source_code::Span;

struct Context {
    code: SourceCode,
    errors: Vec<KtError>,
}

impl Context {
    fn new_error(&mut self, span: Span, info: AnalyserError) {
        self.errors.push(KtError::Analyser {
            code: self.code.clone(),
            span,
            info,
        })
    }
}

fn file_to_ast(code: SourceCode, file: &KotlinFile) -> (AstFile, Vec<KtError>) {
    let mut ctx = Context {
        code: code.clone(),
        errors: vec![],
    };

    let mut classes = vec![];
    let mut functions = vec![];
    let mut properties = vec![];

    for tlo in &file.objects {
        match tlo {
            TopLevelObject::Class(class) => {
                classes.push(class_to_ast(&mut ctx, class));
            }
            TopLevelObject::Object(obj) => {
                classes.push(object_to_ast(&mut ctx, obj));
            }
            TopLevelObject::Function(fun) => {
                functions.push(function_to_ast(&mut ctx, fun));
            }
            TopLevelObject::Property(prop) => {
                properties.push(property_to_ast(&mut ctx, prop));
            }
            TopLevelObject::TypeAlias(_) => {
                unimplemented!()
            }
        }
    }

    (
        AstFile {
            package: file.get_package_str(),
            classes,
            functions,
            properties,
        },
        ctx.errors
    )
}

fn statement_to_ast(ctx: &mut Context, statement: &Statement) -> AstStatement {
    match statement {
        Statement::Expr(e) => AstStatement::Expr(expr_to_ast(ctx, e)),
        Statement::Decl(decl) => {
            match decl {
                Declaration::Class(class) => {
                    AstStatement::Class(class_to_ast(ctx, class))
                }
                Declaration::Object(obj) => {
                    AstStatement::Class(object_to_ast(ctx, obj))
                }
                Declaration::Function(fun) => {
                    AstStatement::Function(function_to_ast(ctx, fun))
                }
                Declaration::Property(prop) => {
                    AstStatement::Property(property_to_local_ast(ctx, prop))
                }
                Declaration::TypeAlias(_) => {
                    panic!("Not supported")
                }
            }
        }
    }
}

fn class_to_ast(ctx: &mut Context, class: &Class) -> AstClass {
    let mut body = vec![];

    if let Some(b) = &class.body {
        for member in &b.members {
            body.push(member_to_ast(ctx, member))
        }
    }

    AstClass {
        name: class.name.to_string(),
        body,
    }
}

fn object_to_ast(ctx: &mut Context, class: &Object) -> AstClass {
    let mut body = vec![];

    if let Some(b) = &class.body {
        for member in &b.members {
            body.push(member_to_ast(ctx, member))
        }
    }

    AstClass {
        name: class.name.to_string(),
        body,
    }
}

fn member_to_ast(ctx: &mut Context, member: &Member) -> AstMember {
    match member {
        Member::Object(obj) => AstMember::Class(object_to_ast(ctx, obj)),
        Member::Function(fun) => AstMember::Function(function_to_ast(ctx, fun)),
        Member::Property(prop) => AstMember::Property(property_to_ast(ctx, prop)),
        Member::Class(class) => AstMember::Class(class_to_ast(ctx, class)),
        Member::CompanionObject(_) => {
            // Ignored
            unimplemented!()
        }
        Member::TypeAlias(_) => {
            panic!("Not supported")
        }
        Member::AnonymousInitializer(_) => {
            // Ignored
            unimplemented!()
        }
        Member::SecondaryConstructor(_) => {
            // Ignored
            unimplemented!()
        }
    }
}


fn property_to_ast(ctx: &mut Context, prop: &Property) -> AstProperty {
    let (delegated, expr) = match &prop.initialization {
        PropertyInitialization::None => (false, None),
        PropertyInitialization::Expr(expr) => (false, Some(expr_to_ast(ctx, expr))),
        PropertyInitialization::Delegation(expr) => (true, Some(expr_to_ast(ctx, expr))),
    };

    let vars = if prop.declarations.len() == 1 {
        let decl = prop.declarations.first().unwrap();
        vec![AstVar {
            name: decl.name.to_owned(),
            ty: decl.declared_type.as_ref().map(|it| type_to_ast(ctx, it)),
            mutable: prop.mutable,
        }]
    } else {
        prop.declarations.iter()
            .map(|decl| var_to_ast(ctx, prop.mutable, decl))
            .collect()
    };

    AstProperty {
        vars,
        expr,
        delegated,
    }
}


fn function_to_ast(ctx: &mut Context, fun: &Function) -> AstFunction {
    let body = fun.body.as_ref().map(|body| {
        match body {
            FunctionBody::Block(e) => block_to_ast(ctx, e),
            FunctionBody::Expression(e) => expr_to_ast(ctx, e),
        }
    });

    let mut args = vec![];
    for param in &fun.value_parameters {
        args.push(AstVar {
            name: param.name.to_owned(),
            ty: Some(type_to_ast(ctx, &param.ty)),
            mutable: false,
        })
        // TODO this should be in the function's block code
//        param.default_value
    }

    AstFunction {
        extension: fun.receiver.is_some(),
        name: fun.name.to_owned(),
        args,
        return_ty: fun.return_type.as_ref().map(|it| type_to_ast(ctx, it)),
        body,
    }
}

fn property_to_local_ast(ctx: &mut Context, prop: &Property) -> AstLocalProperty {
    let (delegated, expr) = match &prop.initialization {
        PropertyInitialization::None => (false, None),
        PropertyInitialization::Expr(expr) => (false, Some(expr_to_ast(ctx, expr))),
        PropertyInitialization::Delegation(expr) => (true, Some(expr_to_ast(ctx, expr))),
    };

    let vars = if prop.declarations.len() == 1 {
        let decl = prop.declarations.first().unwrap();
        vec![AstVar {
            name: decl.name.to_owned(),
            ty: decl.declared_type.as_ref().map(|it| type_to_ast(ctx, it)),
            mutable: prop.mutable,
        }]
    } else {
        prop.declarations.iter()
            .map(|decl| var_to_ast(ctx, prop.mutable, decl))
            .collect()
    };

    AstLocalProperty {
        vars,
        expr,
        delegated,
    }
}

fn expr_to_ast(ctx: &mut Context, expr: &ExprVal) -> AstExpr {
    let span = expr.0;
    match &expr.1 {
        Expr::Null => {
            AstExpr::Constant {
                span,
                value: Constant::Null,
            }
        }
        Expr::Boolean(value) => {
            AstExpr::Constant {
                span,
                value: Constant::Boolean(*value),
            }
        }
        Expr::Char(value) => {
            AstExpr::Constant {
                span,
                value: Constant::Char(*value),
            }
        }
        Expr::Number(num) => {
            let value = match num {
                Number::Double(it) => Constant::Double(*it),
                Number::Float(it) => Constant::Float(*it),
                Number::Byte(it) => Constant::Byte(*it),
                Number::Short(it) => Constant::Short(*it),
                Number::Int(it) => Constant::Int(*it),
                Number::Long(it) => Constant::Long(*it),
            };
            AstExpr::Constant { span, value }
        }
        Expr::This => {
            AstExpr::Ref {
                span,
                name: "this".to_owned(),
            }
        }
        Expr::Super => {
            AstExpr::Ref {
                span,
                name: "super".to_owned(),
            }
        }
        Expr::Ref(name) => {
            AstExpr::Ref {
                span,
                name: name.to_owned(),
            }
        }
        Expr::Chain { operands, operators } => {
            expr_chain_to_tree(ctx, operands, operators)
        }
        Expr::InfixFun { parameters, functions } => {
            expr_chain_to_tree(ctx, parameters, functions)
        }
        Expr::Prefix { prefix, expr } => {
            convert_prefix_to_expr(ctx, span, prefix, expr)
        }
        Expr::Postfix { expr, postfix } => {
            convert_postfix_to_expr(ctx, span, expr, postfix)
        }
        Expr::Is { expr, ty } => {
            AstExpr::Is {
                span,
                expr: Rc::new(expr_to_ast(ctx, expr)),
                ty: type_to_ast(ctx, ty),
            }
        }
        Expr::If { cond, if_true, if_false } => {
            AstExpr::If {
                span,
                cond: Rc::new(expr_to_ast(ctx, cond)),
                if_true: Rc::new(block_to_ast(ctx, if_true)),
                if_false: if_false.as_ref().map(|it| Rc::new(block_to_ast(ctx, &it))),
            }
        }
        Expr::For { variables, expr, body, .. } => {
            let variables = variables.iter().map(|it| var_to_ast(ctx, false, it)).collect::<Vec<_>>();
            AstExpr::For {
                span,
                variables,
                expr: Rc::new(expr_to_ast(ctx, expr)),
                body: Rc::new(block_to_ast(ctx, body)),
            }
        }
        Expr::While { expr, body } => {
            AstExpr::While {
                span,
                expr: Rc::new(expr_to_ast(ctx, expr)),
                body: Rc::new(block_to_ast(ctx, body)),
            }
        }
        Expr::DoWhile { expr, body } => {
            AstExpr::DoWhile {
                span,
                expr: Rc::new(expr_to_ast(ctx, expr)),
                body: Rc::new(block_to_ast(ctx, body)),
            }
        }
        Expr::String(parts) => {
            convert_string_template_to_expr(ctx, span, parts)
        }
        Expr::When { expr, entries } => {
            convert_when_to_ifs(ctx, span, expr, entries)
        }
        Expr::Try { block, catch_blocks, finally } => {
            let catch = catch_blocks.iter()
                .map(|b| (
                    AstVar {
                        name: b.name.to_owned(),
                        ty: Some(simple_user_type_to_ast(ctx, span, &b.ty)),
                        mutable: false,
                    },
                    block_to_ast(ctx, &b.block)
                ))
                .collect();

            AstExpr::Try {
                span,
                body: Rc::new(block_to_ast(ctx, block)),
                catch,
                finally: finally.as_ref().map(|it| block_to_ast(ctx, it)).map(Rc::new),
            }
        }
        Expr::Throw(expr) => {
            AstExpr::Throw {
                span,
                exception: Rc::new(expr_to_ast(ctx, expr)),
            }
        }
        Expr::Return(opt) => {
            AstExpr::Return {
                span,
                value: opt.as_ref().map(|expr| Rc::new(expr_to_ast(ctx, &expr))),
            }
        }
        Expr::Continue => {
            AstExpr::Continue {
                span
            }
        }
        Expr::Break => {
            AstExpr::Break { span }
        }
        // TODO block creation
        Expr::Object { .. } => { unimplemented!() }
        Expr::CallableRef { .. } => { unimplemented!() }
        Expr::Lambda(_lit) => {
            unimplemented!()
        }
    }
}

fn convert_postfix_to_expr(ctx: &mut Context, span: Span, expr: &ExprRef, postfix: &Vec<ExprPostfix>) -> AstExpr {
    let mut ast = expr_to_ast(ctx, expr);
    for post in postfix {
        match post {
            ExprPostfix::Increment | ExprPostfix::Decrement => {
                let variable = match &ast {
                    AstExpr::Ref { name, .. } => name,
                    _ => panic!("Unable to increment/decrement")
                };

                let new_name = match post {
                    ExprPostfix::Increment => "inc",
                    ExprPostfix::Decrement => "dec",
                    _ => panic!("Impossible state")
                };

                ast = AstExpr::Block {
                    span,
                    exprs: vec![
                        AstExpr::WriteRef {
                            span,
                            name: variable.to_owned() + "_",
                            expr: Rc::new(AstExpr::Ref {
                                span,
                                name: variable.to_owned(),
                            }),
                        },
                        AstExpr::WriteRef {
                            span,
                            name: variable.to_owned(),
                            expr: Rc::new(AstExpr::Call {
                                span,
                                function: new_name.to_owned(),
                                args: vec![
                                    AstExpr::Ref {
                                        span,
                                        name: variable.to_owned(),
                                    }
                                ],
                            }),
                        },
                        AstExpr::Ref {
                            span,
                            name: variable.to_owned() + "_",
                        }
                    ],
                };
            }
            ExprPostfix::AssertNonNull => {
                ast = AstExpr::Call {
                    span,
                    function: "assertNonNull".to_owned(),
                    args: vec![ast],
                };
            }
            ExprPostfix::ArrayAccess(index) => {
                let args: Vec<AstExpr> = index.iter()
                    .map(|it| expr_to_ast(ctx, it))
                    .collect();

                ast = AstExpr::Call {
                    span,
                    function: "get".to_owned(),
                    args: create_vec(ast, args),
                };
            }
            ExprPostfix::FunCall(_suffix) => {
                unimplemented!()
                // TODO identify function by type parameters?
            }
            ExprPostfix::MemberAccess { .. } => {
                unimplemented!()
            }
        }
    }
    ast
}

fn convert_prefix_to_expr(ctx: &mut Context, span: Span, prefix: &Vec<String>, expr: &ExprRef) -> AstExpr {
    assert_eq!(prefix.len(), 1);
    let name = prefix.first().unwrap();

    match name.as_str() {
        "++" | "--" => {
            let variable = match &expr.1 {
                Expr::Ref(var) => var,
                _ => panic!("Unable to increment/decrement ")
            };

            let new_name = match name.as_str() {
                "++" => "inc",
                "--" => "dec",
                _ => panic!("Impossible state")
            };

            AstExpr::WriteRef {
                span,
                name: variable.to_owned(),
                expr: Rc::new(AstExpr::Call {
                    span,
                    function: new_name.to_owned(),
                    args: vec![
                        AstExpr::Ref {
                            span,
                            name: variable.to_owned(),
                        }
                    ],
                }),
            }
        }
        _ => {
            let new_name = match name.as_str() {
                "-" => "unaryMinus",
                "+" => "unaryPlus",
                "!" => "not",
                _ => panic!("Unexpected prefix: {}", name)
            };

            let e = expr_to_ast(ctx, expr);
            AstExpr::Call {
                span,
                function: new_name.to_owned(),
                args: vec![e],
            }
        }
    }
}

fn convert_string_template_to_expr(ctx: &mut Context, span: Span, parts: &Vec<StringComponent>) -> AstExpr {
    let mut str_expr = vec![];

    // Extract sub expressions
    for p in parts {
        match p {
            StringComponent::Content(string) => {
                str_expr.push(AstExpr::Constant {
                    span,
                    value: Constant::String(string.to_owned()),
                });
            }
            StringComponent::Variable(name) => {
                str_expr.push(AstExpr::Call {
                    span,
                    function: "toString".to_owned(),
                    args: vec![
                        AstExpr::Ref {
                            span,
                            name: name.to_owned(),
                        }
                    ],
                });
            }
            StringComponent::Template(expr) => {
                str_expr.push(AstExpr::Call {
                    span,
                    function: "toString".to_owned(),
                    args: vec![expr_to_ast(ctx, expr)],
                });
            }
        }
    }

    // Join into tree
    let mut iter = str_expr.into_iter();
    let mut ast = iter.next().unwrap();

    for p in iter {
        ast = AstExpr::Call {
            span,
            function: "plus".to_string(),
            args: vec![ast, p],
        }
    }

    ast
}

fn convert_when_to_ifs(ctx: &mut Context, span: Span, expr: &Option<ExprRef>, entries: &Vec<WhenEntry>) -> AstExpr {
    assert_ne!(entries.len(), 0);
    let mut pairs: Vec<(AstExpr, AstExpr)> = vec![];
    let mut else_value: Option<Rc<AstExpr>> = None;

    if let Some(expr) = expr {
        // When with condition
        let expr = expr_to_ast(ctx, expr);

        // Extract pairs and else
        for (pos, entry) in entries.iter().enumerate() {
            assert_eq!(entry.conditions.len(), 1);
            let cond = entry.conditions.first().unwrap();

            // validation multiple else
            // validation else at the end

            match cond {
                WhenCondition::Else => {
                    if pos != entries.len() - 1 {
                        panic!("Else must be the last entry");
                    }
                    else_value = Some(Rc::new(block_to_ast(ctx, &entry.body)))
                }
                WhenCondition::Expr(cond) => {
                    let cmp = AstExpr::Call {
                        span,
                        function: "equals".to_owned(),
                        args: vec![expr.clone(), expr_to_ast(ctx, cond)],
                    };
                    pairs.push((cmp, block_to_ast(ctx, &entry.body)));
                }
                WhenCondition::In { negated, expr: range } => {
                    let mut cmp = AstExpr::Call {
                        span,
                        function: "contains".to_owned(),
                        args: vec![expr.clone(), expr_to_ast(ctx, range)],
                    };

                    if *negated {
                        cmp = AstExpr::Call {
                            span,
                            function: "not".to_owned(),
                            args: vec![cmp],
                        }
                    }
                    pairs.push((cmp, block_to_ast(ctx, &entry.body)));
                }
                WhenCondition::Is { negated, ty } => {
                    let mut cmp = AstExpr::Is {
                        span,
                        expr: Rc::new(expr.clone()),
                        ty: type_to_ast(ctx, ty),
                    };

                    if *negated {
                        cmp = AstExpr::Call {
                            span,
                            function: "not".to_owned(),
                            args: vec![cmp],
                        }
                    }
                    pairs.push((cmp, block_to_ast(ctx, &entry.body)));
                }
            }
        }
    } else {
        // When without condition

        // Extract pairs and else
        for (pos, entry) in entries.iter().enumerate() {
            assert_eq!(entry.conditions.len(), 1);
            let cond = entry.conditions.first().unwrap();

            // validation multiple else
            // validation else at the end

            match cond {
                WhenCondition::Else => {
                    if pos != entries.len() - 1 {
                        panic!("Else must be the last entry");
                    }
                    else_value = Some(Rc::new(block_to_ast(ctx, &entry.body)))
                }
                WhenCondition::Expr(cond) => {
                    pairs.push((expr_to_ast(ctx, cond), block_to_ast(ctx, &entry.body)));
                }
                WhenCondition::In { .. } => {
                    panic!("Invalid condition");
                }
                WhenCondition::Is { .. } => {
                    panic!("Invalid condition");
                }
            }
        }
    }

    let mut iter = pairs.into_iter().rev();
    let (last_cond, last_block) = iter.next().unwrap();

    let mut ast = AstExpr::If {
        span,
        cond: Rc::new(last_cond),
        if_true: Rc::new(last_block),
        if_false: else_value,
    };

    for (cond, block) in iter {
        ast = AstExpr::If {
            span,
            cond: Rc::new(cond),
            if_true: Rc::new(block),
            if_false: Some(Rc::new(ast)),
        };
    }
    ast
}

fn type_to_ast(ctx: &mut Context, ty: &Type) -> AstType {
    type_reference_to_ast(ctx, ty.span, ty.reference.as_ref())
}

fn type_reference_to_ast(ctx: &mut Context, span: Span, ty: &TypeReference) -> AstType {
    match ty {
        TypeReference::Function(func) => type_func_to_ast(ctx, span, func),
        TypeReference::UserType(user) => simple_user_type_to_ast(ctx, span, user),
        TypeReference::Nullable(reference) => {
            let mut ast = match reference.as_ref() {
                TypeReference::Function(func) => type_func_to_ast(ctx, span, func),
                TypeReference::UserType(user) => simple_user_type_to_ast(ctx, span, user),
                TypeReference::Nullable(_) => {
                    panic!("Double nullable!")
                }
            };
            ast.nullable = true;
            ast
        }
    }
}

fn type_func_to_ast(ctx: &mut Context, span: Span, ty: &FunctionType) -> AstType {
    let mut type_parameters = vec![];

    if let Some(rec) = &ty.receiver {
        type_parameters.push(AstTypeParameter::Type(type_reference_to_ast(ctx, span, rec)));
    }

    for param in &ty.parameters {
        type_parameters.push(AstTypeParameter::Type(type_to_ast(ctx, param)));
    }

    type_parameters.push(AstTypeParameter::Type(type_reference_to_ast(ctx, span, &ty.return_type)));

    AstType {
        span,
        name: "Function".to_string(),
        full_name: "kotlin.Function".to_string(),
        type_parameters,
        nullable: false,
    }
}

fn simple_user_type_to_ast(ctx: &mut Context, span: Span, ty: &Vec<SimpleUserType>) -> AstType {
    let last = ty.last().unwrap();
    let mut full_name = String::new();
    let mut type_parameters = vec![];

    for (i, x) in ty.iter().enumerate() {
        full_name.push_str(&x.name);
        if ty.len() != i + 1 {
            full_name.push('.');
        }
    }

    for x in &last.type_params {
        type_parameters.push(match x {
            CallSiteTypeParams::Projection => AstTypeParameter::Projection,
            CallSiteTypeParams::Type(ty) => AstTypeParameter::Type(type_to_ast(ctx, ty)),
        });
    }

    AstType {
        span,
        name: last.name.to_owned(),
        full_name: "kotlin.Function".to_string(),
        type_parameters,
        nullable: false,
    }
}

fn var_to_ast(ctx: &mut Context, mutable: bool, var: &VariableDeclarationEntry) -> AstVar {
    AstVar {
        name: var.name.to_owned(),
        ty: var.declared_type.as_ref().map(|it| type_to_ast(ctx, it)),
        mutable,
    }
}

fn block_to_ast(ctx: &mut Context, block: &Block) -> AstExpr {
    let mut exprs = vec![];

    for statement in &block.1 {
        match statement {
            Statement::Expr(e) => {
                exprs.push(expr_to_ast(ctx, e))
            }
            Statement::Decl(_) => {}
        }
    }

    AstExpr::Block {
        span: block.0,
        exprs,
    }
}

fn expr_chain_to_tree(ctx: &mut Context, operands: &Vec<ExprVal>, operators: &Vec<String>) -> AstExpr {
    debug_assert_eq!(operands.len(), operators.len() + 1);

    if operators.is_empty() {
        expr_to_ast(ctx, operands.first().unwrap())
    } else {
        let mut iter = operands.iter();
        let mut op_iter = operators.iter();
        let mut tree = expr_to_ast(ctx, iter.next().unwrap());

        while let Some(expr) = iter.next() {
            let op = op_iter.next().unwrap();
            let next = expr_to_ast(ctx, expr);
            let span = (get_span(&tree).0, (expr.0).1);

            tree = AstExpr::Call {
                span,
                function: op.to_owned(),
                args: vec![tree, next],
            }
        }

        tree
    }
}

fn get_span(expr: &AstExpr) -> Span {
    match expr {
        AstExpr::Block { span, .. } => *span,
        AstExpr::Constant { span, .. } => *span,
        AstExpr::Ref { span, .. } => *span,
        AstExpr::Call { span, .. } => *span,
        AstExpr::ReadField { span, .. } => *span,
        AstExpr::WriteRef { span, .. } => *span,
        AstExpr::Is { span, .. } => *span,
        AstExpr::If { span, .. } => *span,
        AstExpr::For { span, .. } => *span,
        AstExpr::While { span, .. } => *span,
        AstExpr::DoWhile { span, .. } => *span,
        AstExpr::Continue { span, .. } => *span,
        AstExpr::Break { span, .. } => *span,
        AstExpr::Throw { span, .. } => *span,
        AstExpr::Return { span, .. } => *span,
        AstExpr::Try { span, .. } => *span,
    }
}
