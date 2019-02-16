use std::collections::HashMap;

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
}

fn eval_stm(ctx: &mut Context, ast: &AstStatement) -> Option<Value> {
    match ast {
        AstStatement::Expr(e) => {
            return eval_expr(ctx, e);
        }
        AstStatement::Class(_) => {}
        AstStatement::Function(_) => {}
        AstStatement::Property(prop) => {
            let name = &prop.vars.first().unwrap().name;
            let e = prop.expr.as_ref().unwrap();
            let value = eval_expr(ctx, e).unwrap();
            ctx.new_ref(name, value);
        }
    }
    None
}

fn eval_expr(ctx: &mut Context, ast: &AstExpr) -> Option<Value> {
    match ast {
        AstExpr::Block { statements, .. } => {
            let mut last = None;
            for x in statements {
                last = eval_stm(ctx, x);
            }
            return last;
        }
        AstExpr::Constant { value, .. } => {
            return Some(Value::Primitive(value.clone()));
        }
        AstExpr::Ref { name, .. } => {
            return ctx.get_ref(name);
        }

        AstExpr::ReadField { field, object, .. } => {
            let value = eval_expr(ctx, &object.borrow()).unwrap();
            if let Value::Object(obj) = value {
                let obj_ref = &obj.borrow();
                let value_ref = obj_ref.get(field).unwrap();
                return Some(value_ref.clone());
            }
        }
        AstExpr::WriteRef { name, expr, .. } => {
            let value = eval_expr(ctx, &expr.borrow()).unwrap();
            ctx.set_ref(name, value);
        }
        AstExpr::Is { .. } => {
            return Some(Value::Primitive(Constant::Boolean(true)));
        }
        AstExpr::If { cond, if_true, if_false, .. } => {
            if let Value::Primitive(Constant::Boolean(value)) = eval_expr(ctx, &cond.borrow()).unwrap() {
                return eval_expr(ctx, &if_true.borrow());
            } else {
                return if_false.clone().and_then(|it| eval_expr(ctx, &it.borrow()));
            }
        }
        AstExpr::For { .. } => {}
        AstExpr::While { .. } => {}
        AstExpr::DoWhile { .. } => {}
        AstExpr::Continue { .. } => {}
        AstExpr::Break { .. } => {}
        AstExpr::Try { .. } => {}
        AstExpr::Throw { .. } => {}
        AstExpr::Return { .. } => {}
        AstExpr::InvokeStatic { function, type_parameters, args, .. } => {
            let a = eval_expr(ctx, &args[0]).unwrap();
            let b = eval_expr(ctx, &args[1]).unwrap();

            match function.as_str() {
                "+" => return Some(from_int(as_int(a) + as_int(b))),
                "-" => return Some(from_int(as_int(a) - as_int(b))),
                "*" => return Some(from_int(as_int(a) * as_int(b))),
                "/" => return Some(from_int(as_int(a) / as_int(b))),
                _ => {}
            }
        }
        AstExpr::InvokeDynamic { .. } => {}
    }

    None
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

#[cfg(test)]
mod tests {
    use crate::interpreter::ast_walker::Context;
    use crate::interpreter::ast_walker::eval_expr;
    use crate::test_utils::assert_correct_ast;

    #[test]
    fn sum() {
        let file = assert_correct_ast(r#"
            fun test() {
                val b = 321 + 6 / 3

                b
            }
        "#);
        let main_func = file.ast.functions.first().unwrap();
        let expr = &main_func.body.as_ref().unwrap().clone();
        let mut ctx = Context::new();
        let value = eval_expr(&mut ctx, expr);
        dbg!(value);
    }
}