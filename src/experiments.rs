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

#[derive(Debug, Clone, Default)]
struct TypeInfo {
    name: String,
    parent: Option<TypeSig>,
    interfaces: Vec<TypeSig>,
    functions: HashMap<String, TypeSig>,
}

#[derive(Debug)]
struct Scope {
    types: HashMap<String, TypeInfo>,
    functions: HashMap<FunctionSig, (String, TypeSig)>,
    variables: HashMap<String, TypeSig>,
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
        self.last_scope().functions.insert(signature.to_string(), (name.to_string(), ret.to_string()));
    }

    fn register_variable(&mut self, name: &str, signature: &TypeSig) {
        self.last_scope().variables.insert(name.to_string(), signature.to_string());
    }

    fn register_type_info(&mut self, signature: &TypeSig, info: TypeInfo) {
        self.last_scope().types.insert(signature.to_string(), info);
    }

    fn get_type_info(&mut self, sig: &str) -> Option<TypeInfo> {
        for scope in self.blocks.iter().rev() {
            if let Some(info) = scope.types.get(sig) {
                return Some(info.clone());
            }
        }
        None
    }

    fn get_type_signature_for_name(&mut self, name: &str) -> Option<String> {
        for scope in self.blocks.iter().rev() {
            if let Some(sig) = scope.variables.get(name) {
                return Some(sig.clone());
            }
        }
        None
    }

    fn get_function_return_by_signature(&mut self, sig: &str) -> Option<String> {
        for scope in self.blocks.iter().rev() {
            if let Some((name, ret)) = scope.functions.get(sig) {
                return Some(ret.clone());
            }
        }
        None
    }
}

pub fn run_experiments(code: SourceCode, ast: &AstFile) {
    let mut ctx = Ctx::new();

    for class in &ast.classes {
        visit_class(&mut ctx, class);
    }

    for func in &ast.functions {
        visit_function(&mut ctx, func);
    }

    for prop in &ast.properties {
        visit_property(&mut ctx, prop);
    }

    dbg!(ctx);
}

fn visit_class(ctx: &mut Ctx, class: &AstClass) {
    let sig = type_signature_of(&class.name);
    let name = class.name.to_string();
    let parent = class.super_type.as_ref()
        .map(|i| visit_type(ctx, i));

    let mut interfaces = vec![];
    for i in &class.interfaces {
        interfaces.push(visit_type(ctx, i));
    }

    let mut info = TypeInfo { name, parent, interfaces, ..TypeInfo::default() };
    ctx.enter_block();
    ctx.register_variable(&"this".to_string(), &sig);

    for member in &class.body {
        visit_member(ctx, member);
    }

    let scope = ctx.last_scope();

    for (sig, (name, ret)) in scope.functions.iter() {
        info.functions.insert(name.to_string(), sig.to_string());
    }

    ctx.exit_block();
    ctx.register_type_info(&sig, info);

    if class.class_type != AstClassType::Object {
        let ctor = function_signature_of(&class.name, &vec![]);
        ctx.register_function(&class.name, &ctor, &sig);
    }
}

fn visit_member(ctx: &mut Ctx, member: &AstMember) {
    match member {
        AstMember::Class(class) => { visit_class(ctx, class); }
        AstMember::Function(func) => { visit_function(ctx, func); }
        AstMember::Property(prop) => { visit_property(ctx, prop); }
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
            AstStatement::Class(class) => { visit_class(ctx, class) }
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
            Some(type_signature_of_constant(value))
        }
        AstExpr::Ref { obj, name, .. } => {
            match obj {
                Some(obj) => {
                    unimplemented!("member access")
                }
                None => {
                    match ctx.get_type_signature_for_name(name) {
                        Some(sig) => Some(sig),
                        None => {
                            // error, unknown name
                            None
                        }
                    }
                }
            }
        }
        AstExpr::Call { receiver, function, args, .. } => {
            match receiver {
                Some(rec) => {
                    let sig = visit_expression(ctx, &rec.borrow(), &None);
                    match sig {
                        Some(sig) => {
                            match ctx.get_type_info(&sig) {
                                Some(it) => {
                                    match it.functions.get(function) {
                                        Some(sig) => call_function(ctx, sig, args, expected_return),
                                        None => {
                                            // error, missing method
                                            None
                                        }
                                    }
                                }
                                None => {
                                    // error missing type
                                    None
                                }
                            }
                        }
                        None => {
                            // error propagation, probably can be ignored
                            None
                        }
                    }
                }
                None => {
                    call_function(ctx, function, args, expected_return)
                }
            }
        }
        AstExpr::If { .. } => { unimplemented!("if") }
        AstExpr::Try { .. } => { unimplemented!("try") }
        AstExpr::Is { .. } => { Some(type_signature_of_constant(&Constant::Boolean(false))) }
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

fn call_function(ctx: &mut Ctx, function: &String, args: &Vec<AstExpr>, expected_return: &Option<TypeSig>) -> Option<TypeSig> {
    let args_sig = args.iter()
        .map(|a| visit_expression(ctx, a, &None).unwrap())
        .collect_vec();

    let mut sig = function_signature_of(function, &args_sig);

    // If there is a local variable with type function, we call that first
    if let Some(ty_sig) = ctx.get_type_signature_for_name(function) {
        if is_function_type(&ty_sig) {
            sig = function_signature_of("invoke", &args_sig); // type signature of invoke for this type
        }
    }

    match ctx.get_function_return_by_signature(&sig) {
        Some(ret) => {
            Some(ret)
        }
        None => {
            // error
            None
        }
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

fn visit_property(ctx: &mut Ctx, prop: &AstProperty) {
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

fn type_signature_of_constant(constant: &Constant) -> TypeSig {
    match constant {
        Constant::Null => type_signature_of("Nothing?"),
        Constant::Array(_) => type_signature_of("Array"),
        Constant::Boolean(_) => type_signature_of("Boolean"),
        Constant::Double(_) => type_signature_of("Double"),
        Constant::Float(_) => type_signature_of("Float"),
        Constant::Byte(_) => type_signature_of("Byte"),
        Constant::Short(_) => type_signature_of("Short"),
        Constant::Int(_) => type_signature_of("Int"),
        Constant::Long(_) => type_signature_of("Long"),
        Constant::Char(_) => type_signature_of("Char"),
        Constant::String(_) => type_signature_of("String"),
    }
}

fn is_function_type(sig: &TypeSig) -> bool {
    sig.contains("->")
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
                println(a)
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
    }

    #[test]
    fn simple_destructuration() {
        let (code, ast) = assert_correct_ast2(r#"
            fun test() {
                val (a, b) = 10 to 30
                println(a)
            }
        "#);
        run_experiments(code, &ast);
    }

    #[test]
    fn simple_local_var() {
        let (code, ast) = assert_correct_ast2(r#"
            fun test() {
                var a: Int = false
            }
        "#);
        run_experiments(code, &ast);
    }

    #[test]
    fn simple_class() {
        let (code, ast) = assert_correct_ast2(r#"
            class Test
        "#);
        run_experiments(code, &ast);
    }

    #[test]
    fn simple_class2() {
        let (code, ast) = assert_correct_ast2(r#"
            class Test: Test2
        "#);
        run_experiments(code, &ast);
    }

    #[test]
    fn simple_class_with_members() {
        let (code, ast) = assert_correct_ast2(r#"
            class Test {
                fun test(): Int
            }

            fun test2(): Int {
               return Test().test()
            }
        "#);
        run_experiments(code, &ast);
        panic!()
    }
}
