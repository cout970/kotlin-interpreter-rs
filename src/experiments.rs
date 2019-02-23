use std::collections::HashMap;

use itertools::Itertools;

use crate::analyzer::ast::*;
use crate::analyzer::tree_to_ast::NOTHING_TYPE;
use crate::analyzer::tree_to_ast::UNIT_TYPE;
use crate::interpreter::bytecode::Constant;
use crate::source_code::SourceCode;

// Signatures: unique identifiers for types and functions
type TypeSig = String;
type FunctionSig = String;

#[derive(Debug)]
struct Scope {
    types: HashMap<String, ()>,
    functions: HashMap<String, String>,
    variables: HashMap<String, String>,
}

#[derive(Debug)]
struct Ctx {
    blocks: Vec<Scope>
}

impl Scope {
    fn new() -> Self {
        Scope {
            types: HashMap::new(),
            functions: HashMap::new(),
            variables: HashMap::new(),
        }
    }
}

impl Ctx {
    fn new() -> Self { Ctx { blocks: vec![Scope::new()] } }

    fn enter_block(&mut self) {
        self.blocks.push(Scope::new());
    }

    fn exit_block(&mut self) {
        self.blocks.pop().expect("Trying to pop global scope");
    }

    fn last_scope(&mut self) -> &mut Scope {
        self.blocks.last_mut().unwrap()
    }

    fn register_function(&mut self, name: &str, signature: &FunctionSig, ret: &str) {
        self.last_scope().functions.insert(signature.to_string(), ret.to_string());
    }

    fn register_variable(&mut self, name: &str, signature: &TypeSig) {
        self.last_scope().variables.insert(name.to_string(), signature.to_string());
    }

    fn get_type_info(&mut self, sig: &str) -> Option<()> {
        for scope in self.blocks.iter().rev() {
            if let Some(info) = scope.types.get(sig) {
                return Some(info.clone());
            }
        }
        None
    }
}

pub fn run_experiments(code: SourceCode, ast: &AstFile) {
    let mut ctx = Ctx::new();

    for func in &ast.functions {
        visit_function(&mut ctx, func);
    }

    dbg!(ctx);
}

fn function_signature_of(name: &str, args: &Vec<TypeSig>) -> FunctionSig {
    let mut sig = String::new();
    sig.push_str(name);
    sig.push('(');
    for (i, arg) in args.iter().enumerate() {
        sig.push_str(arg);
        if i != args.len() - 1 {
            sig.push(',');
        }
    }
    sig.push(')');
    sig
}

fn type_signature_of(name: &str) -> TypeSig {
    name.to_string()
}

fn type_signature_of_constant(constant: &Constant) -> Option<TypeSig> {
    match constant {
        Constant::Null => None,
        Constant::Array(_) => Some(type_signature_of("Array")),
        Constant::Boolean(_) => Some(type_signature_of("Boolean")),
        Constant::Double(_) => Some(type_signature_of("Double")),
        Constant::Float(_) => Some(type_signature_of("Float")),
        Constant::Byte(_) => Some(type_signature_of("Byte")),
        Constant::Short(_) => Some(type_signature_of("Short")),
        Constant::Int(_) => Some(type_signature_of("Int")),
        Constant::Long(_) => Some(type_signature_of("Long")),
        Constant::Char(_) => Some(type_signature_of("Char")),
        Constant::String(_) => Some(type_signature_of("String")),
    }
}

fn visit_function(ctx: &mut Ctx, func: &AstFunction) -> FunctionSig {
    ctx.enter_block();

    let arg_signatures = visit_function_args(ctx, &func.args);
    let sig = function_signature_of(&func.name, &arg_signatures);

    ctx.register_function(&func.name, &sig, "[incomplete]");

    let ret = visit_function_body(ctx, &func.body, &func.return_ty);

    let ret = match ret {
        Some(it) => it,
        None => {
            "_error_".to_string()
        }
    };

    ctx.exit_block();
    ctx.register_function(&func.name, &sig, &ret);

    sig
}

fn visit_function_args(ctx: &mut Ctx, args: &Vec<AstVar>) -> Vec<TypeSig> {
    let mut arg_signatures = vec![];
    for var in args {
        let sig = visit_var(ctx, var);
        arg_signatures.push(sig);
    }
    arg_signatures
}

fn visit_function_body(ctx: &mut Ctx, body: &Option<AstBlock>, return_ty: &Option<AstType>) -> Option<TypeSig> {
    let body = match body {
        Some(body) => body,
        None => {
            return return_ty.as_ref().map(|it| visit_type(ctx, it));
        }
    };

    let mut ret_ty = None;

    if let Some(ty) = return_ty {
        ret_ty = Some(visit_type(ctx, ty));
    }

    visit_block(ctx, body, &ret_ty)
}

fn visit_var(ctx: &mut Ctx, var: &AstVar) -> TypeSig {
    let ty = if let Some(ty) = &var.ty {
        ty
    } else {
        // error
        panic!("Function argument without type!")
    };

    let sig = visit_type(ctx, ty);
    ctx.register_variable(&var.name, &sig);
    sig
}

fn visit_type(ctx: &mut Ctx, ty: &AstType) -> TypeSig {
    let sig = type_signature_of(&ty.name);

    if ctx.get_type_info(&sig).is_none() {
        //error
    }

    sig
}

fn visit_block(ctx: &mut Ctx, block: &AstBlock, expected_return: &Option<TypeSig>) -> Option<TypeSig> {
    let mut ret = None;
    for (i, stm) in block.statements.iter().enumerate() {
        match stm {
            AstStatement::Expr(expr) => {
                let r = visit_expression(ctx, expr, expected_return);

                // Are we in a lambda?
                if i == block.statements.len() - 1 {
                    ret = r;
                }
            }
            AstStatement::Assignment(_, _) => { unimplemented!("assigment") }
            AstStatement::Class(_) => { unimplemented!("class") }
            AstStatement::Function(fun) => { visit_function(ctx, fun); }
            AstStatement::Property(prop) => { visit_local_property(ctx, prop) }
            AstStatement::For { .. } => { unimplemented!("loop") }
            AstStatement::While { .. } => { unimplemented!("loop") }
            AstStatement::DoWhile { .. } => { unimplemented!("loop") }
        }
    }
    ret
}

fn visit_expression(ctx: &mut Ctx, expr: &AstExpr, expected_return: &Option<TypeSig>) -> Option<TypeSig> {
    match expr {
        AstExpr::Constant { value, .. } => {
            type_signature_of_constant(value)
        }
        AstExpr::Ref { .. } => { unimplemented!("ref") }
        AstExpr::Call { .. } => { unimplemented!("call") }
        AstExpr::If { .. } => { unimplemented!("if") }
        AstExpr::Try { .. } => { unimplemented!("try") }
        AstExpr::Is { .. } => { type_signature_of_constant(&Constant::Boolean(false)) }
        AstExpr::Continue { .. } => { Some(visit_type(ctx, &UNIT_TYPE)) }
        AstExpr::Break { .. } => { Some(visit_type(ctx, &UNIT_TYPE)) }
        AstExpr::Throw { .. } => { Some(visit_type(ctx, &NOTHING_TYPE)) }
        AstExpr::Return { value, .. } => {
            match value {
                Some(expr) => {
                    visit_expression(ctx, &expr.borrow(), expected_return)
                }
                None => Some(visit_type(ctx, &NOTHING_TYPE))
            }
        }
        AstExpr::Lambda { .. } => { unimplemented!("Lambda") }
        AstExpr::AnonymousFunction { .. } => { unimplemented!("AnonymousFunction") }
        AstExpr::ObjectLiteral { .. } => { unimplemented!("ObjectLiteral") }
    }
}

fn visit_local_property(ctx: &mut Ctx, prop: &AstLocalProperty) {
    let mut ty = if let Some(ty) = &prop.var.ty {
        Some(visit_type(ctx, ty))
    } else {
        None
    };

    if let Some(expr) = &prop.expr {
        let sig = visit_expression(ctx, expr, &ty);

        match &ty {
            Some(ty) => {
//                if sig != ty {
//                    // error mismatch of types
//                }
            }
            None => {
                match sig {
                    Some(sig) => {
                        ty = Some(sig);
                    }
                    None => {
                        // error, no type defined nor inferred
                    }
                }
            }
        }
    }

    if let Some(ty) = ty {
        ctx.register_variable(&prop.var.name, &ty)
    }
}

#[cfg(test)]
mod tests {
    use crate::test_utils::assert_correct_ast2;

    use super::*;

    #[test]
    fn simple_func() {
        let (code, ast) = assert_correct_ast2(r#"
            fun test() {
                var a = 0
                a
            }
        "#);
        run_experiments(code, &ast);
    }

    #[test]
    fn simple_func2() {
        let (code, ast) = assert_correct_ast2(r#"
            fun test() = 0
        "#);
        run_experiments(code, &ast);
        panic!()
    }
}
