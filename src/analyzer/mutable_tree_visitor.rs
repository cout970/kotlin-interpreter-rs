use crate::analyzer::ast::*;

type Func<'t, S, T> = &'t mut FnMut(&mut S, &mut T);

pub struct Visitor<'t, S> {
    pub pre_visit_file: Option<Func<'t, S, AstFile>>,
    pub post_visit_file: Option<Func<'t, S, AstFile>>,

    pub pre_visit_class: Option<Func<'t, S, AstClass>>,
    pub post_visit_class: Option<Func<'t, S, AstClass>>,

    pub pre_visit_property: Option<Func<'t, S, AstProperty>>,
    pub post_visit_property: Option<Func<'t, S, AstProperty>>,

    pub pre_visit_function: Option<Func<'t, S, AstFunction>>,
    pub post_visit_function: Option<Func<'t, S, AstFunction>>,

    pub pre_visit_typealias: Option<Func<'t, S, AstTypealias>>,
    pub post_visit_typealias: Option<Func<'t, S, AstTypealias>>,

    pub pre_visit_var: Option<Func<'t, S, AstVar>>,
    pub post_visit_var: Option<Func<'t, S, AstVar>>,

    pub pre_visit_type: Option<Func<'t, S, AstType>>,
    pub post_visit_type: Option<Func<'t, S, AstType>>,

    pub pre_visit_expr: Option<Func<'t, S, AstExpr>>,
    pub post_visit_expr: Option<Func<'t, S, AstExpr>>,

    pub pre_visit_statement: Option<Func<'t, S, AstStatement>>,
    pub post_visit_statement: Option<Func<'t, S, AstStatement>>,

    pub pre_visit_local_property: Option<Func<'t, S, AstLocalProperty>>,
    pub post_visit_local_property: Option<Func<'t, S, AstLocalProperty>>,
}

impl<'t, S> Default for Visitor<'t, S> {
    fn default() -> Self {
        Visitor {
            pre_visit_file: None,
            post_visit_file: None,
            pre_visit_class: None,
            post_visit_class: None,
            pre_visit_property: None,
            post_visit_property: None,
            pre_visit_function: None,
            post_visit_function: None,
            pre_visit_typealias: None,
            post_visit_typealias: None,
            pre_visit_var: None,
            post_visit_var: None,
            pre_visit_type: None,
            post_visit_type: None,
            pre_visit_expr: None,
            post_visit_expr: None,
            pre_visit_statement: None,
            post_visit_statement: None,
            pre_visit_local_property: None,
            post_visit_local_property: None,
        }
    }
}

pub fn visit_file<S>(v: &mut Visitor<S>, state: &mut S, ast: &mut AstFile) {
    if let Some(func) = &mut v.pre_visit_file {
        func(state, ast);
    }

    for class in &mut ast.classes {
        visit_class(v, state, class);
    }

    for prop in &mut ast.properties {
        visit_property(v, state, prop);
    }

    for fun in &mut ast.functions {
        visit_function(v, state, fun);
    }

    for alias in &mut ast.typealias {
        visit_typealias(v, state, alias);
    }

    if let Some(func) = &mut v.post_visit_file {
        func(state, ast);
    }
}

pub fn visit_typealias<S>(v: &mut Visitor<S>, state: &mut S, ast: &mut AstTypealias) {
    if let Some(func) = &mut v.pre_visit_typealias {
        func(state, ast);
    }

    visit_type(v, state, &mut ast.ty);

    if let Some(func) = &mut v.post_visit_typealias {
        func(state, ast);
    }
}

pub fn visit_class<S>(v: &mut Visitor<S>, state: &mut S, ast: &mut AstClass) {
    if let Some(func) = &mut v.pre_visit_class {
        func(state, ast);
    }

    visit_opt_type(v, state, &mut ast.super_type);

    for i in &mut ast.interfaces {
        visit_type(v, state, i);
    }

    for member in &mut ast.body {
        match member {
            AstMember::Class(ast) => visit_class(v, state, ast),
            AstMember::Function(ast) => visit_function(v, state, ast),
            AstMember::Property(ast) => visit_property(v, state, ast),
        }
    }

    if let Some(func) = &mut v.post_visit_class {
        func(state, ast);
    }
}

pub fn visit_property<S>(v: &mut Visitor<S>, state: &mut S, ast: &mut AstProperty) {
    if let Some(func) = &mut v.pre_visit_property {
        func(state, ast);
    }

    visit_var(v, state, &mut ast.var);

    if let Some(expr) = &mut ast.expr {
        visit_expr(v, state, expr);
    }

    if let Some(func) = &mut v.post_visit_property {
        func(state, ast);
    }
}

pub fn visit_function<S>(v: &mut Visitor<S>, state: &mut S, ast: &mut AstFunction) {
    if let Some(func) = &mut v.pre_visit_function {
        func(state, ast);
    }

    for var in &mut ast.args {
        visit_var(v, state, var);
    }

    visit_opt_type(v, state, &mut ast.return_ty);

//    if let Some(expr) = &mut ast.body {
//        visit_expr(v, state, expr);
//    }

    if let Some(func) = &mut v.post_visit_function {
        func(state, ast);
    }
}

pub fn visit_var<S>(v: &mut Visitor<S>, state: &mut S, ast: &mut AstVar) {
    if let Some(func) = &mut v.pre_visit_var {
        func(state, ast);
    }

    visit_opt_type(v, state, &mut ast.ty);

    if let Some(func) = &mut v.post_visit_var {
        func(state, ast);
    }
}

pub fn visit_opt_type<S>(v: &mut Visitor<S>, state: &mut S, ast: &mut Option<AstType>) {
    if let Some(ty) = ast {
        visit_type(v, state, ty);
    }
}

pub fn visit_type<S>(v: &mut Visitor<S>, state: &mut S, ast: &mut AstType) {
    if let Some(func) = &mut v.pre_visit_type {
        func(state, ast);
    }

    for param in &mut ast.type_parameters {
        match param {
            AstTypeParameter::Type(ty) => {
                visit_type(v, state, ty);
            }
            AstTypeParameter::Projection => {}
            AstTypeParameter::Parameter(_) => {}
        }
    }

    if let Some(func) = &mut v.post_visit_type {
        func(state, ast);
    }
}

pub fn visit_rc_expr<S>(v: &mut Visitor<S>, state: &mut S, ast: &mut MutRc<AstExpr>) {
    visit_expr(v, state, &mut ast.borrow_mut());
}

pub fn visit_expr<S>(v: &mut Visitor<S>, state: &mut S, ast: &mut AstExpr) {
    if let Some(func) = &mut v.pre_visit_expr {
        func(state, ast);
    }

    match ast {
        AstExpr::Block { block, .. } => {
//            for stmt in statements {
//                visit_statement(v, state, stmt);
//            }
        }
        AstExpr::Constant { .. } => {}
        AstExpr::Ref { .. } => {}
        AstExpr::Call { args, receiver, .. } => {
            if let Some(rec) = receiver {
                visit_rc_expr(v, state, rec)
            }

            for arg in args {
                visit_expr(v, state, arg);
            }
        }
        AstExpr::Is { expr, .. } => {
            visit_rc_expr(v, state, expr);
        }
        AstExpr::If { cond, if_true, if_false, .. } => {
            visit_rc_expr(v, state, cond);
//            visit_rc_expr(v, state, if_true);
//            if let Some(if_false) = if_false {
//                visit_rc_expr(v, state, if_false);
//            }
        }
        AstExpr::For { variables, expr, body, .. } => {
            for var in variables {
                visit_var(v, state, var);
            }
//            visit_rc_expr(v, state, expr);
//            visit_rc_expr(v, state, body);
        }
        AstExpr::While { expr, body, .. } => {
            visit_rc_expr(v, state, expr);
//            visit_rc_expr(v, state, body);
        }
        AstExpr::DoWhile { expr, body, .. } => {
            visit_rc_expr(v, state, expr);
//            visit_rc_expr(v, state, body);
        }
        AstExpr::Continue { .. } => {}
        AstExpr::Break { .. } => {}
        AstExpr::Try { body, catch, finally, .. } => {
//            visit_rc_expr(v, state, body);
//            for (var, expr) in catch {
//                visit_var(v, state, var);
//                visit_expr(v, state, expr);
//            }
//            if let Some(expr) = finally {
//                visit_rc_expr(v, state, expr);
//            }
        }
        AstExpr::Throw { exception, .. } => {
            visit_rc_expr(v, state, exception);
        }
        AstExpr::Return { value, .. } => {
            if let Some(expr) = value {
                visit_rc_expr(v, state, expr);
            }
        }
    }

    if let Some(func) = &mut v.post_visit_expr {
        func(state, ast);
    }
}

pub fn visit_statement<S>(v: &mut Visitor<S>, state: &mut S, ast: &mut AstStatement) {
    if let Some(func) = &mut v.pre_visit_statement {
        func(state, ast);
    }

    match ast {
        AstStatement::Expr(ast) => visit_expr(v, state, ast),
        AstStatement::Assignment(a, b) => {
            visit_expr(v, state, a);
            visit_expr(v, state, b);
        },
        AstStatement::Class(ast) => visit_class(v, state, ast),
        AstStatement::Function(ast) => visit_function(v, state, ast),
        AstStatement::Property(ast) => visit_local_property(v, state, ast),
    }

    if let Some(func) = &mut v.post_visit_statement {
        func(state, ast);
    }
}

pub fn visit_local_property<S>(v: &mut Visitor<S>, state: &mut S, ast: &mut AstLocalProperty) {
    if let Some(func) = &mut v.pre_visit_local_property {
        func(state, ast);
    }

    for var in &mut ast.vars {
        visit_var(v, state, var);
    }

    if let Some(expr) = &mut ast.expr {
        visit_expr(v, state, expr);
    }

    if let Some(func) = &mut v.post_visit_local_property {
        func(state, ast);
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_visitor() {
        let mut file = AstFile {
            package: "".to_string(),
            imports: vec![],
            classes: vec![AstClass {
                name: "ClassName".to_string(),
                ..AstClass::default()
            }],
            functions: vec![],
            properties: vec![],
            typealias: vec![],
        };
        let mut state: Vec<String> = vec![];
        let mut visitor = Visitor::default();

        let a = &mut test_class_visitor;
        visitor.pre_visit_class = Some(a);

        visit_file(&mut visitor, &mut state, &mut file);

        assert_eq!(dbg!(state).len(), 1);
    }

    fn test_class_visitor(s: &mut Vec<String>, class: &mut AstClass) {
        s.push(class.name.to_owned());
    }
}