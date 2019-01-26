use std::rc::Rc;

use crate::analyzer::ast::AstExpr;
use crate::analyzer::ast::AstType;
use crate::analyzer::ast::AstVar;
use crate::create_vec;
use crate::interpreter::bytecode::Constant;
use crate::Number;
use crate::parser::parse_tree::Block;
use crate::parser::parse_tree::Expr;
use crate::parser::parse_tree::ExprPostfix;
use crate::parser::parse_tree::ExprRef;
use crate::parser::parse_tree::ExprVal;
use crate::parser::parse_tree::StringComponent;
use crate::parser::parse_tree::Type;
use crate::parser::parse_tree::VariableDeclarationEntry;
use crate::parser::parse_tree::WhenCondition;
use crate::parser::parse_tree::WhenEntry;
use crate::source_code::Span;
use crate::parser::parse_tree::SimpleUserType;

pub fn expr_to_ast(expr: &ExprVal) -> AstExpr {
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
            expr_chain_to_tree(operands, operators)
        }
        Expr::InfixFun { parameters, functions } => {
            expr_chain_to_tree(parameters, functions)
        }
        Expr::Prefix { prefix, expr } => {
            convert_prefix_to_expr(span, prefix, expr)
        }
        Expr::Postfix { expr, postfix } => {
            convert_postfix_to_expr(span, expr, postfix)
        }
        Expr::Is { expr, ty } => {
            AstExpr::Is {
                span,
                expr: Rc::new(expr_to_ast(expr)),
                ty: type_to_ast(ty),
            }
        }
        Expr::If { cond, if_true, if_false } => {
            AstExpr::If {
                span,
                cond: Rc::new(expr_to_ast(cond)),
                if_true: Rc::new(block_to_ast(if_true)),
                if_false: if_false.as_ref().map(|it| Rc::new(block_to_ast(&it))),
            }
        }
        Expr::For { variables, expr, body, .. } => {
            let variables = variables.iter().map(var_to_ast).collect::<Vec<_>>();
            AstExpr::For {
                span,
                variables,
                expr: Rc::new(expr_to_ast(expr)),
                body: Rc::new(block_to_ast(body)),
            }
        }
        Expr::While { expr, body } => {
            AstExpr::While {
                span,
                expr: Rc::new(expr_to_ast(expr)),
                body: Rc::new(block_to_ast(body)),
            }
        }
        Expr::DoWhile { expr, body } => {
            AstExpr::DoWhile {
                span,
                expr: Rc::new(expr_to_ast(expr)),
                body: Rc::new(block_to_ast(body)),
            }
        }
        Expr::String(parts) => {
            convert_string_template_to_expr(span, parts)
        }
        Expr::When { expr, entries } => {
            convert_when_to_ifs(span, expr, entries)
        }
        Expr::Try { block, catch_blocks, finally } => {
            let catch = catch_blocks.iter()
                .map(|b| (
                    AstVar {
                        name: b.name.to_owned(),
                        ty: Some(simple_user_type_to_ast(&b.ty)),
                        mutable: false,
                    },
                    block_to_ast(&b.block)
                ))
                .collect();

            AstExpr::Try {
                span,
                body: Rc::new(block_to_ast(block)),
                catch,
                finally: finally.as_ref().map(block_to_ast).map(Rc::new),
            }
        }
        Expr::Throw(expr) => {
            AstExpr::Throw {
                span,
                exception: Rc::new(expr_to_ast(expr)),
            }
        }
        Expr::Return(opt) => {
            AstExpr::Return {
                span,
                value: opt.as_ref().map(|expr| Rc::new(expr_to_ast(&expr))),
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
        Expr::Lambda(_) => { unimplemented!() }
    }
}

fn convert_postfix_to_expr(span: Span, expr: &ExprRef, postfix: &Vec<ExprPostfix>) -> AstExpr {
    let mut ast = expr_to_ast(expr);
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

                ast = AstExpr::Block(vec![
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
                ]);
            }
            ExprPostfix::AssertNonNull => {
                ast = AstExpr::Call {
                    span,
                    function: "assertNonNull".to_owned(),
                    args: vec![ast],
                };
            }
            ExprPostfix::ArrayAccess(index) => {
                let args: Vec<AstExpr> = index.iter().map(expr_to_ast).collect();
                ast = AstExpr::Call {
                    span,
                    function: "get".to_owned(),
                    args: create_vec(ast, args),
                };
            }
            ExprPostfix::FunCall(suffix) => {
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

fn convert_prefix_to_expr(span: Span, prefix: &Vec<String>, expr: &ExprRef) -> AstExpr {
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

            let e = expr_to_ast(expr);
            AstExpr::Call {
                span,
                function: new_name.to_owned(),
                args: vec![e],
            }
        }
    }
}

fn convert_string_template_to_expr(span: Span, parts: &Vec<StringComponent>) -> AstExpr {
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
                    args: vec![expr_to_ast(expr)],
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

fn convert_when_to_ifs(span: Span, expr: &Option<ExprRef>, entries: &Vec<WhenEntry>) -> AstExpr {
    assert_ne!(entries.len(), 0);
    let mut pairs: Vec<(AstExpr, AstExpr)> = vec![];
    let mut else_value: Option<Rc<AstExpr>> = None;

    if let Some(expr) = expr {
        // When with condition
        let expr = expr_to_ast(expr);

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
                    else_value = Some(Rc::new(block_to_ast(&entry.body)))
                }
                WhenCondition::Expr(cond) => {
                    let cmp = AstExpr::Call {
                        span,
                        function: "equals".to_owned(),
                        args: vec![expr.clone(), expr_to_ast(cond)],
                    };
                    pairs.push((cmp, block_to_ast(&entry.body)));
                }
                WhenCondition::In { negated, expr: range } => {
                    let mut cmp = AstExpr::Call {
                        span,
                        function: "contains".to_owned(),
                        args: vec![expr.clone(), expr_to_ast(range)],
                    };

                    if *negated {
                        cmp = AstExpr::Call {
                            span,
                            function: "not".to_owned(),
                            args: vec![cmp],
                        }
                    }
                    pairs.push((cmp, block_to_ast(&entry.body)));
                }
                WhenCondition::Is { negated, ty } => {
                    let mut cmp = AstExpr::Is {
                        span,
                        expr: Rc::new(expr.clone()),
                        ty: type_to_ast(ty),
                    };

                    if *negated {
                        cmp = AstExpr::Call {
                            span,
                            function: "not".to_owned(),
                            args: vec![cmp],
                        }
                    }
                    pairs.push((cmp, block_to_ast(&entry.body)));
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
                    else_value = Some(Rc::new(block_to_ast(&entry.body)))
                }
                WhenCondition::Expr(cond) => {
                    pairs.push((expr_to_ast(cond), block_to_ast(&entry.body)));
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

pub fn type_to_ast(ty: &Type) -> AstType {
    unimplemented!()
}

pub fn simple_user_type_to_ast(ty: &Vec<SimpleUserType>) -> AstType {
    unimplemented!()
}

pub fn var_to_ast(var: &VariableDeclarationEntry) -> AstVar {
    unimplemented!()
}

fn block_to_ast(expr: &Block) -> AstExpr {
    unimplemented!()
}

fn expr_chain_to_tree(operands: &Vec<ExprVal>, operators: &Vec<String>) -> AstExpr {
    debug_assert_eq!(operands.len(), operators.len() + 1);

    if operators.is_empty() {
        expr_to_ast(operands.first().unwrap())
    } else {
        let mut iter = operands.iter();
        let mut op_iter = operators.iter();
        let mut tree = expr_to_ast(iter.next().unwrap());

        while let Some(expr) = iter.next() {
            let op = op_iter.next().unwrap();
            let next = expr_to_ast(expr);
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
        AstExpr::Block(list) => {
            (
                get_span(list.first().unwrap()).0,
                get_span(list.last().unwrap()).1,
            )
        }
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

