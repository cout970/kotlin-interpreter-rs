use std::rc::Rc;

use crate::analyzer::ast::AstExpr;
use crate::analyzer::ast::AstType;
use crate::create_vec;
use crate::interpreter::bytecode::Constant;
use crate::Number;
use crate::parser::parse_tree::Block;
use crate::parser::parse_tree::Expr;
use crate::parser::parse_tree::ExprPostfix;
use crate::parser::parse_tree::ExprVal;
use crate::parser::parse_tree::Type;
use crate::source_code::Span;
use crate::analyzer::ast::AstVar;
use crate::parser::parse_tree::VariableDeclarationEntry;

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
        Expr::Postfix { expr, postfix } => {
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
                        // TODO identify function by type parameters?
                    }
                    ExprPostfix::MemberAccess { .. } => {
                        // TODO
                    }
                }
            }
            ast
        }
        Expr::Is { expr, ty } => {
            AstExpr::Is {
                span,
                expr: Rc::new(expr_to_ast(expr)),
                ty: type_to_ast(ty),
            }
        }

        Expr::String(_) => { unimplemented!() }
        Expr::If { cond, if_true, if_false } => {
            AstExpr::If {
                span,
                cond: Rc::new(expr_to_ast(cond)),
                if_true: Rc::new(block_to_ast(if_true)),
                if_false: if_false.as_ref().map(|it| Rc::new(block_to_ast(&it))),
            }
        }
        Expr::Try { .. } => { unimplemented!() }
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
        Expr::When { .. } => { unimplemented!() }
        Expr::Object { .. } => { unimplemented!() }
        Expr::CallableRef { .. } => { unimplemented!() }
        Expr::Lambda(_) => { unimplemented!() }
        Expr::Throw(_) => { unimplemented!() }
        Expr::Return(_) => { unimplemented!() }
        Expr::Continue => { unimplemented!() }
        Expr::Break => {
            AstExpr::Break { span }
        }
    }
}

pub fn type_to_ast(ty: &Type) -> AstType {
    unimplemented!()
}

pub fn var_to_ast(var: &VariableDeclarationEntry) -> AstVar {
    unimplemented!()
}

fn block_to_ast(expr: &Block) -> AstExpr {
    unimplemented!()
}

fn expr_chain_to_tree(operands: &Vec<ExprVal>, operators: &Vec<String>) -> AstExpr {
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

fn get_operator_precedence(op: &str) -> u32 {
    match op {
        "=" | "+=" | "-=" | "*=" | "/=" | "%=" => 12,
        "&&" => 11,
        "||" => 10,
        "==" | "===" | "!=" | "!==" => 9,
        "<" | "<=" | ">" | ">=" => 8,
        "in" | "!in" => 7,
        ":?" => 6,
        ".." => 4,
        "+" | "-" => 3,
        "*" | "/" | "%" => 2,
        "as" | "as?" => 1,
        _ => 5
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
    }
}

