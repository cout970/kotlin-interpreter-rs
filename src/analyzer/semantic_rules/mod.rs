use std::collections::HashSet;

use crate::ast::Class;
use crate::ast::Function;
use crate::ast::KotlinFile;
use crate::ast::Modifier;
use crate::ast::Preamble;
use crate::ast::TopLevelObject;
use crate::errors::AnalyserError;
use crate::source_code::SourceCode;
use crate::source_code::Span;

pub struct CheckCtx {
    code: SourceCode,
    pub errors: Vec<(Span, AnalyserError)>,
    symbols: Vec<Symbol>,
}

impl CheckCtx {
    pub fn new(code: SourceCode) -> Self {
        CheckCtx { code, errors: vec![], symbols: vec![] }
    }
}

struct Symbol {
    name: String,
    symbol_type: SymbolType,
    path: String,
}

enum SymbolType {
    Type,
    Class,
    Function,
    Property,
}

pub fn check_ast(ctx: &mut CheckCtx, ast: &mut KotlinFile) {
    check_preamble(ctx, &ast.preamble);
    for x in &mut ast.objects {
        check_top_level_object(ctx, x);
    }
}

fn check_preamble(ctx: &mut CheckCtx, preamble: &Preamble) {
    if let Some(pack) = &preamble.package_header {
        if !pack.modifiers.is_empty() {
            for x in &pack.modifiers {
                ctx.errors.push(((0, 0), AnalyserError::InvalidModifierUsage {
                    modifier: x.clone(),
                    context: String::from("package definition"),
                }));
            }
        }
    }

    let mut names: HashSet<String> = HashSet::new();

    for x in &preamble.imports {
        let name = match &x.alias {
            Some(alias) => {
                alias.clone()
            }
            None => {
                let name = x.path.last().unwrap().clone();
                // Ignore * imports, you can have as many of those as you want
                if &name == "*" { continue; }
                name
            }
        };

        if names.contains(&name) {
            ctx.errors.push(((0, 0), AnalyserError::ConflictingImport {
                name: name.clone(),
            }));
        }
        names.insert(name);
    }
}

fn check_top_level_object(ctx: &mut CheckCtx, obj: &mut TopLevelObject) {
    match obj {
        TopLevelObject::Class(it) => { check_class(ctx, it); }
        TopLevelObject::Object(it) => {}
        TopLevelObject::Function(it) => { check_function(ctx, it) }
        TopLevelObject::Property(it) => {}
        TopLevelObject::TypeAlias(it) => {}
    }
}

fn check_class(ctx: &mut CheckCtx, class: &mut Class) {
    report_duplicated_modifiers(ctx, &class.modifiers);
    // TODO check modifiers are applicable to a class
    // check modifiers are applicable to the correct class type: 'enum interface'

    // Duplicated type parameters
    let types_parameters = &class.type_parameters;
    // duplicated delegation specifier
    // missing enum body, for enum classes
}

fn check_function(ctx: &mut CheckCtx, fun: &mut Function) {
    report_duplicated_modifiers(ctx, &fun.modifiers);
    // Duplicated type parameters
    // Types in first type parameters
//    fun.type_parameters;

    // duplicated names in parameters
    // several varargs
    //
}

fn report_duplicated_modifiers(ctx: &mut CheckCtx, mods: &Vec<Modifier>) {
    let mut names: HashSet<String> = HashSet::new();
    for x in mods {
        if names.contains(&x.name) {
            ctx.errors.push(((0, 0), AnalyserError::DuplicatedModifier {
                modifier: x.clone(),
            }));
        }
        names.insert(x.name.clone());
    }
}

#[cfg(test)]
mod tests {
    use crate::test_utils::assert_success;
    use crate::test_utils::assert_fails;

    use super::*;

    #[test]
    fn test_preamble() {
        println!("{:?}", assert_success("package com.cout970.Main"));
        println!("{:?}", assert_fails("public package com.cout970.Main"));
        println!("{:?}", assert_success("import com.cout970.Main"));
        println!("{:?}", assert_fails("import com.cout970.Main; import com.cout970.Main"));
        println!("{:?}", assert_fails("import com.cout970.main.Main; import com.other.Main"));
        println!("{:?}", assert_success("import myPackage.*"));
        println!("{:?}", assert_fails("import *"));
        println!("{:?}", assert_success("import com.cout970.Main as Main2; import com.cout970.Main"));
        println!("{:?}", assert_fails("import com.cout970.Main as Main2; import com.cout970.Main2"));
        println!("{:?}", assert_fails("import com.cout970.A as C; import com.cout970.B as C"));
    }

    #[test]
    fn test_class() {
        println!("{:?}", assert_success("class Test"));
        println!("{:?}", assert_fails("private private class Test"));
        println!("{:?}", assert_success("class Test(){}"));
        println!("{:?}", assert_success("class Test(): Iterable"));
    }
}