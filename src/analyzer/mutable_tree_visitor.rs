use crate::analyzer::ast::*;

type Func<'t, S, T> = &'t mut FnMut(&mut S, &mut T);

#[derive(Default)]
pub struct Visitor<'t, S> {
    pub pre_visit_file: Option<Func<'t, S, AstFile>>,
    pub post_visit_file: Option<Func<'t, S, AstFile>>,

    pub pre_visit_class: Option<Func<'t, S, AstClass>>,
    pub post_visit_class: Option<Func<'t, S, AstClass>>,

    pub pre_visit_property: Option<Func<'t, S, AstProperty>>,
    pub post_visit_property: Option<Func<'t, S, AstProperty>>,

    pub pre_visit_function: Option<Func<'t, S, AstFunction>>,
    pub post_visit_function: Option<Func<'t, S, AstFunction>>,

    pub pre_visit_var: Option<Func<'t, S, AstVar>>,
    pub post_visit_var: Option<Func<'t, S, AstVar>>,

    pub pre_visit_type: Option<Func<'t, S, AstType>>,
    pub post_visit_type: Option<Func<'t, S, AstType>>,

    pub pre_visit_expr: Option<Func<'t, S, AstExpr>>,
    pub post_visit_expr: Option<Func<'t, S, AstExpr>>,
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

    if let Some(func) = &mut v.post_visit_file {
        func(state, ast);
    }
}

pub fn visit_class<S>(v: &mut Visitor<S>, state: &mut S, ast: &mut AstClass) {
    if let Some(func) = &mut v.pre_visit_class {
        func(state, ast);
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

    if let Some(expr) = &mut ast.body {
        visit_expr(v, state, expr);
    }

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

pub fn visit_expr<S>(v: &mut Visitor<S>, state: &mut S, ast: &mut AstExpr) {
    if let Some(func) = &mut v.pre_visit_expr {
        func(state, ast);
    }



    if let Some(func) = &mut v.post_visit_expr {
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
                span: (0, 0),
                name: "ClassName".to_string(),
                body: vec![],
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