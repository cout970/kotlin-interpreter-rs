use crate::analyzer::ast::*;
use crate::analyzer::typechecker::FileSymbols;

struct Context {
    info: FileSymbols,
    package: String,
}

pub fn collect_file_symbols(ast: &AstFile) -> FileSymbols {
    let mut ctx = Context {
        info: FileSymbols::new(),
        package: ast.package.clone(),
    };

    for class in &ast.classes {
        visit_class(&mut ctx, class);
    }

    for fun in &ast.functions {
        visit_function(&mut ctx, fun);
    }

    for prop in &ast.properties {
        visit_property(&mut ctx, prop);
    }

    ctx.info
}

fn visit_class(ctx: &mut Context, class: &AstClass) {
    ctx.info.classes.push(ctx.package.clone() + "." + &class.name);
    for member in &class.body {
        match member {
            AstMember::Class(subclass) => visit_member_class(ctx, ctx.package.clone() + &class.name, subclass),
            AstMember::Function(fun) => visit_member_function(ctx, ctx.package.clone() + &class.name, fun),
            AstMember::Property(prop) => visit_member_property(ctx, ctx.package.clone() + &class.name, prop),
        }
    }
}

fn visit_property(ctx: &mut Context, prop: &AstProperty) {
    ctx.info.properties.push(ctx.package.clone() + "." + &prop.var.name);
}

fn visit_function(ctx: &mut Context, fun: &AstFunction) {
    ctx.info.properties.push(ctx.package.clone() + "." + &fun.name);
}

fn visit_member_class(ctx: &mut Context, parent: String, class: &AstClass) {
    for member in &class.body {
        match member {
            AstMember::Class(subclass) => visit_member_class(ctx, parent.clone() + "." + &class.name, subclass),
            AstMember::Function(fun) => visit_member_function(ctx, parent.clone() + "." + &class.name, fun),
            AstMember::Property(prop) => visit_member_property(ctx, parent.clone() + "." + &class.name, prop),
        }
    }
    ctx.info.classes.push(parent + "." + &class.name);
}

fn visit_member_property(ctx: &mut Context, parent: String, prop: &AstProperty) {
    ctx.info.properties.push(parent + "." + &prop.var.name);
}

fn visit_member_function(ctx: &mut Context, parent: String, fun: &AstFunction) {
    ctx.info.properties.push(parent + "." + &fun.name);
}