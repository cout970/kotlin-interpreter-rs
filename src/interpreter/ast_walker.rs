use std::collections::HashMap;

use itertools::Itertools;

use crate::analyzer::ast::AstBlock;
use crate::analyzer::ast::AstExpr;
use crate::analyzer::ast::AstStatement;
use crate::analyzer::ast::MutRc;
use crate::interpreter::bytecode::Constant;

struct Context {
    values: HashMap<String, Value>
}

#[derive(Debug, Clone)]
enum Value {
    Primitive(Constant),
    Object(MutRc<HashMap<String, Value>>),
    Nothing,
    Unit,
}

impl Context {
    pub fn new() -> Self {
        Context {
            values: HashMap::new()
        }
    }

    fn get_ref(&self, name: &str) -> Option<Value> {
        self.values.get(name).cloned()
    }

    fn set_ref(&mut self, name: &str, value: Value) {
        *self.values.get_mut(name).unwrap() = value;
    }

    fn new_ref(&mut self, name: &str, value: Value) {
        self.values.insert(name.to_owned(), value);
    }

    fn get_func(&mut self, receiver: &Option<Value>, name: &str) -> Option<String> {
        // TODO
        None
    }
}

fn eval_stm(ctx: &mut Context, ast: &AstStatement) -> Result<Option<Value>, Option<Value>> {
    match ast {
        AstStatement::Expr(e) => {
            return Ok(Some(eval_expr(ctx, e)?));
        }
        AstStatement::Assignment(a, b) => {
            let b_value = eval_expr(ctx, b)?;
            let var = match a {
                AstExpr::Ref { name, .. } => {
                    name
                }
                _ => { panic!("Not a variable"); }
            };

            ctx.set_ref(var, b_value);
        }
        AstStatement::Class(_) => {}
        AstStatement::Function(_) => {}
        AstStatement::Property(prop) => {
            let name = &prop.var.name;
            let e = prop.expr.as_ref().expect("Var initialization");
            let value = eval_expr(ctx, e)?;
            ctx.new_ref(name, value);
        }
        _ => {}
    }
    Ok(None)
}

fn eval_expr(ctx: &mut Context, ast: &AstExpr) -> Result<Value, Option<Value>> {
    match ast {
        AstExpr::Lambda { block, .. } => { /*TODO*/ }
        AstExpr::AnonymousFunction { block, .. } => { /*TODO*/ }
        AstExpr::ObjectLiteral { block, .. } => { /*TODO*/ }
        AstExpr::Constant { value, .. } => {
            return Ok(Value::Primitive(value.clone()));
        }
        AstExpr::Ref { name, .. } => {
            return Ok(ctx.get_ref(name).unwrap());
        }
        AstExpr::Is { .. } => {
            return Ok(Value::Primitive(Constant::Boolean(true)));
        }
        AstExpr::If { cond, if_true, if_false, .. } => {
            if let Value::Primitive(Constant::Boolean(value)) = eval_expr(ctx, &cond.borrow()).unwrap() {
                return eval_block(ctx, if_true);
            } else {
                if let Some(block) = if_false {
                    match eval_block(ctx, block) {
                        Ok(value) => { return Ok(value); }
                        Err(e) => { return Err(e); }
                    }
                } else {
                    return Ok(Value::Unit);
                }
            }
        }
        AstExpr::Continue { .. } => {}
        AstExpr::Break { .. } => {}
        AstExpr::Try { .. } => {}
        AstExpr::Throw { .. } => {
            return Err(None);
        }
        AstExpr::Return { value, .. } => {
            if let Some(expr) = value {
                match eval_expr(ctx, &expr.borrow()) {
                    Ok(value) => { return Ok(value); }
                    Err(e) => { return Err(e); }
                }
            } else {
                return Ok(Value::Unit);
            }
        }
        AstExpr::Call { receiver, function, type_parameters, args, .. } => {
            let rec = if let Some(expr) = receiver {
                match eval_expr(ctx, &expr.borrow()) {
                    Ok(value) => { Some(value) }
                    Err(e) => { return Err(e); }
                }
            } else {
                None
            };

            let mut value_args = vec![];

            for it in args {
                match eval_expr(ctx, it) {
                    Ok(value) => { value_args.push(value); }
                    Err(e) => { return Err(e); }
                }
            }

            let func = ctx.get_func(&rec, function).expect(&format!("Function not found: {}", function));

            return Ok(call_function(ctx, func, rec, value_args));
        }
    }

    Ok(Value::Unit)
}

fn eval_block(ctx: &mut Context, ast: &AstBlock) -> Result<Value, Option<Value>> {
    let mut iter = ast.statements.iter();
    let mut ret = eval_stm(ctx, iter.next().unwrap())?;
    for s in iter {
        ret = eval_stm(ctx, s)?;
    }
    Ok(ret.unwrap_or(Value::Unit))
}

fn call_function(ctx: &mut Context, func: String, rec: Option<Value>, args: Vec<Value>) -> Value {
    Value::Unit
}

fn as_int(a: Value) -> i32 {
    if let Value::Primitive(Constant::Int(it)) = a { it } else { 0 }
}

fn from_int(a: i32) -> Value {
    Value::Primitive(Constant::Int(a))
}

fn add(a: Value, b: Value) -> Value {
    Value::Primitive(Constant::Int(as_int(a) + as_int(b)))
}

//#[cfg(test)]
//mod tests {
//    use crate::interpreter::ast_walker::Context;
//    use crate::interpreter::ast_walker::eval_block;
//    use crate::interpreter::ast_walker::eval_expr;
//    use crate::test_utils::assert_correct_ast;
//
//    #[test]
//    #[ignore]
//    fn sum() {
//        let file = assert_correct_ast(r#"
//            fun test() {
//                val b = 321 + 6 / 3
//                var a = 0
//                a = b
//                a
//            }
//        "#);
//        let main_func = file.ast.functions.first().unwrap();
//        let body = &main_func.body.as_ref().unwrap().clone();
//        let mut ctx = Context::new();
//        let value = eval_block(&mut ctx, body);
//        dbg!(value);
//    }
//}