use std::collections::HashMap;
use std::collections::HashSet;

use crate::analyzer::ast::*;
use crate::analyzer::typechecker::CheckedFile;
use crate::analyzer::typechecker::FileInfo;
use crate::errors::AnalyserError;
use crate::source_code::Span;

type Files = HashMap<String, FileInfo>;

struct Context<'t> {
    files: &'t Files,
    alias: HashMap<String, String>,
    imports: HashMap<String, String>,
    errors: Vec<(Span, AnalyserError)>,
    symbols: Vec<HashSet<String>>,
}

////import com.mcmacker4.raytracer.tracing.HitInfo
////import com.mcmacker4.raytracer.tracing.Ray
////import com.mcmacker4.raytracer.util.*
////import org.joml.Vector3f
////import org.joml.Vector3fc
////import java.lang.Math.pow

impl<'t> Context<'t> {
    fn add_import(&mut self, import: &AstImport) {
        // TODO
    }

    fn resolve_name(&mut self, span: Span, name: &str) -> Option<String> {
        // Local symbol
        if self.has_symbol(name) {
            return None;
        }

        if name.contains(".") {
            // Absolute reference
            let pos = name.chars().rev().position(|c| c == '.').unwrap();
            let path = &name[0..pos];
            let symbol = &name[pos + 1..];

            if let Some(info) = self.files.get(path) {
                // If the symbol is not in the file a error is generated
                if !info.properties.iter().any(|name| name == symbol) &&
                    !info.functions.iter().any(|name| name == symbol) &&
                    !info.classes.iter().any(|name| name == symbol) {
                    self.errors.push((span, AnalyserError::UnresolvedReference(name.to_owned())));
                }
                // The absolute reference doesn't need to be replaced
                None
            } else {
                self.errors.push((span, AnalyserError::UnresolvedReference(name.to_owned())));
                None
            }
        } else {
            // SimpleName
            let real_name = self.alias.get(name).map(|string| string.as_str()).unwrap_or(name);

            if let Some(absolute_path) = self.imports.get(real_name) {
                Some(absolute_path.to_owned())
            } else {
                self.errors.push((span, AnalyserError::UnresolvedReference(name.to_owned())));
                None
            }
        }
    }

    fn enter_block(&mut self) {
        self.symbols.push(HashSet::new());
    }

    fn exit_block(&mut self) {
        self.symbols.pop().unwrap();
    }

    fn add_symbol(&mut self, name: &str) {
        self.symbols.last_mut().unwrap().insert(name.to_owned());
    }

    fn has_symbol(&self, name: &str) -> bool {
        self.symbols.iter().rev().any(|set| set.contains(name))
    }
}

pub fn resolve_imports(files: &Files, path: &str, ast: &mut AstFile) {
    let mut ctx = Context {
        files,
        alias: HashMap::new(),
        imports: HashMap::new(),
        errors: vec![],
        symbols: vec![HashSet::new()],
    };

    // Register imports
    for import in &ast.imports {
        ctx.add_import(import);
    }

    // Register own names
    let info = files.get(path).unwrap();

    for name in &info.classes {
        ctx.add_symbol(name);
    }
    for name in &info.functions {
        ctx.add_symbol(name);
    }
    for name in &info.properties {
        ctx.add_symbol(name);
    }

    // Traverse code to update references
    for class in &mut ast.classes {
        visit_class(&mut ctx, class);
    }
}

fn opt_update_type(ctx: &mut Context, ty: &mut Option<AstType>) {
    if let Some(ty) = ty {
        update_type(ctx, ty);
    }
}

fn update_type(ctx: &mut Context, ty: &mut AstType) {
    if let Some(new_name) = ctx.resolve_name(ty.span, &ty.full_name) {
        ty.full_name = new_name;
    }

    for param in &mut ty.type_parameters {
        match param {
            AstTypeParameter::Type(ty) => {
                update_type(ctx, ty);
            }
            AstTypeParameter::Projection => {}
            AstTypeParameter::Parameter(_) => {
                // TODO check this too?
            }
        }
    }
}

fn visit_class(ctx: &mut Context, class: &mut AstClass) {
    ctx.add_symbol(&class.name);

    // TODO analyze the rest of the class
    ctx.enter_block();
    for member in &mut class.body {
        match member {
            AstMember::Class(subclass) => visit_class(ctx, subclass),
            AstMember::Function(fun) => visit_function(ctx, fun),
            AstMember::Property(prop) => visit_property(ctx, prop),
        }
    }
    ctx.exit_block();
}

fn visit_function(ctx: &mut Context, fun: &mut AstFunction) {
    ctx.add_symbol(&fun.name);

    opt_update_type(ctx, &mut fun.return_ty);

    for var in &mut fun.args {
        opt_update_type(ctx, &mut var.ty);
    }

    if let Some(expr) = &mut fun.body {
        ctx.enter_block();
        visit_expr(ctx, expr);
        ctx.exit_block();
    }
}

fn visit_property(ctx: &mut Context, prop: &mut AstProperty) {
    ctx.add_symbol(&prop.var.name);

    opt_update_type(ctx, &mut prop.var.ty);

    if let Some(expr) = &mut prop.expr {
        ctx.enter_block();
        visit_expr(ctx, expr);
        ctx.exit_block();
    }
}

fn visit_expr(ctx: &mut Context, fun: &mut AstExpr) {
// TODO
}