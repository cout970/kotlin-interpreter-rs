use std::collections::HashSet;

use crate::analyzer::semantic_rules::modifiers::check_modifiers;
use crate::errors::AnalyserError;
use crate::errors::KtError;
use crate::parser::parse_tree::*;
use crate::source_code::SourceCode;
use crate::source_code::Span;

mod modifiers;

pub struct Checker {
    code: SourceCode,
    errors: Vec<(Span, AnalyserError)>,
    imports: Vec<Path>,
    symbols: Vec<Symbol>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Symbol {
    name: String,
    symbol_type: SymbolType,
    path: Path,
}

#[derive(Clone, Debug, PartialEq)]
pub enum SymbolType {
    Type,
    Class,
    Function,
    Property,
}

impl Checker {
    pub fn new(code: SourceCode, ast: &KotlinFile) -> Self {
        let mut c = Checker { code, errors: vec![], imports: vec![], symbols: vec![] };
        c.check(ast);
        c
    }

    fn check(&mut self, ast: &KotlinFile) {
        check_preamble(self, &ast.preamble);
        for x in &ast.objects {
            check_top_level_object(self, x);
        }
    }

    pub fn get_errors(&self) -> Vec<KtError> {
        self.errors.iter()
            .map(|(span, info)|
                KtError::Analyser { code: self.code.clone(), span: *span, info: info.clone() }
            )
            .collect()
    }

    pub fn get_symbols(&self) -> Vec<Symbol> {
        self.symbols.iter()
            .map(|it| it.clone())
            .collect::<Vec<_>>()
    }
}

fn check_preamble(ctx: &mut Checker, preamble: &Preamble) {
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

        ctx.imports.push(x.path.clone());

        if names.contains(&name) {
            ctx.errors.push(((0, 0), AnalyserError::ConflictingImport {
                name: name.clone(),
            }));
        }
        names.insert(name);
    }
}

fn check_top_level_object(ctx: &mut Checker, obj: &TopLevelObject) {
    match obj {
        TopLevelObject::Class(it) => { check_class(ctx, it, vec![]); }
        TopLevelObject::Object(it) => { check_object(ctx, it, vec![]); }
        TopLevelObject::Function(it) => { check_function(ctx, it, vec![]); }
        TopLevelObject::Property(it) => { check_property(ctx, it, vec![]); }
        TopLevelObject::TypeAlias(it) => { check_typealias(ctx, it, vec![]); }
    }
}

fn check_class(ctx: &mut Checker, class: &Class, path: Path) {
    check_modifiers(ctx, &class.modifiers, ModifierCtx::TopLevelObject);
    // TODO check modifiers are applicable to a class
    // check modifiers are applicable to the correct class type: 'enum interface'

    // Duplicated type parameters
//    let types_parameters = &class.type_parameters;
    // duplicated delegation specifier
    // missing enum body, for enum classes

    ctx.symbols.push(Symbol {
        name: class.name.clone(),
        symbol_type: SymbolType::Class,
        path,
    });
}

fn check_object(ctx: &mut Checker, obj: &Object, path: Path) {
    check_modifiers(ctx, &obj.modifiers, ModifierCtx::TopLevelObject);
    // TODO check modifiers are applicable to a class
    // check modifiers are applicable to the correct class type: 'enum object'

    // objects doesn't have constructors


    if let Some(it) = &obj.body {
        let mut path = path.clone();
        path.push(obj.name.clone());

        for member in &it.members {
            check_member(ctx, member, path.clone());
        }
    }

    ctx.symbols.push(Symbol {
        name: obj.name.clone(),
        symbol_type: SymbolType::Class,
        path,
    });
}

fn check_function(ctx: &mut Checker, fun: &Function, path: Path) {
    check_modifiers(ctx, &fun.modifiers, ModifierCtx::TopLevelObject);
    // Duplicated type parameters
    // Types in first type parameters
//    fun.type_parameters;

    // duplicated names in parameters
    // several varargs
    ctx.symbols.push(Symbol {
        name: fun.name.clone(),
        symbol_type: SymbolType::Function,
        path,
    });
}

fn check_property(ctx: &mut Checker, prop: &Property, path: Path) {
    check_modifiers(ctx, &prop.modifiers, ModifierCtx::TopLevelObject);
    // Duplicated type parameters
    // Types in first type parameters
//    fun.type_parameters;

    if prop.declarations.len() != 1 {
        ctx.errors.push((prop.span, AnalyserError::DestructuringInTopLevel));
    }

    ctx.symbols.push(Symbol {
        name: prop.declarations.first().unwrap().name.clone(),
        symbol_type: SymbolType::Property,
        path,
    });
}

fn check_typealias(ctx: &mut Checker, alias: &TypeAlias, path: Path) {
    check_modifiers(ctx, &alias.modifiers, ModifierCtx::TopLevelObject);
    // Duplicated type parameters
    // Types in first type parameters

    ctx.symbols.push(Symbol {
        name: signature_of_type(&alias.ty),
        symbol_type: SymbolType::Type,
        path,
    });
}

fn check_member(ctx: &mut Checker, member: &Member, path: Path) {
    match member {
        Member::CompanionObject(it) => { check_object(ctx, it, path); }
        Member::Object(it) => { check_object(ctx, it, path); }
        Member::Function(it) => { check_function(ctx, it, path); }
        Member::Property(it) => { check_property(ctx, it, path); }
        Member::Class(it) => { check_class(ctx, it, path); }
        Member::TypeAlias(it) => { check_typealias(ctx, it, path); }
        Member::AnonymousInitializer(_) => {}
        Member::SecondaryConstructor(_) => {}
    }
}

fn signature_of_type(ty: &Type) -> String {
    signature_of_ref(&ty.reference)
}

fn signature_of_ref(ty: &TypeReference) -> String {
    match &ty {
        TypeReference::Function(fun) => {
            // F[Receiver.(Param1,Param2)->Return]
            let mut val = String::new();

            val.push_str("F[");
            if let Some(receiver) = &fun.receiver {
                val.push_str(&signature_of_ref(receiver));
                val.push('.');
            }

            val.push('(');
            for i in 0..fun.parameters.len() {
                val.push_str(&signature_of_type(&fun.parameters[i]));

                if i != fun.parameters.len() - 1 {
                    val.push(',');
                }
            }
            val.push_str(")->");
            val.push_str(&signature_of_ref(&fun.return_type));
            val.push_str("]");
            val
        }
        TypeReference::UserType(user_ty) => {
            // T[A.B.C<D,E>]
            let mut val = String::new();

            val.push_str("T[");
            for i in 0..user_ty.len() {
                val.push_str(&user_ty[i].name);

                let params = &user_ty[i].type_params;
                if !params.is_empty() {
                    val.push('<');
                    for j in 0..params.len() {
                        match &params[j] {
                            CallSiteTypeParams::Projection => {
                                val.push('*');
                            }
                            CallSiteTypeParams::Type(ty) => {
                                val.push_str(&signature_of_type(ty));
                            }
                        }

                        if j != params.len() - 1 {
                            val.push(',');
                        }
                    }
                    val.push('>');
                }

                if i != user_ty.len() - 1 {
                    val.push('.');
                }
            }
            val.push_str("]");
            val
        }
        TypeReference::Nullable(base) => {
            format!("N[{}]", signature_of_ref(&base))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::test_utils::assert_fails;
    use crate::test_utils::assert_success;


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
    #[ignore]
    fn test_class() {
        println!("{:?}", assert_success("class Test"));
        println!("{:?}", assert_fails("private private class Test"));
        println!("{:?}", assert_success("class Test(){}"));
        println!("{:?}", assert_success("class Test(): Iterable"));
        assert_ne!(0, 0);
    }

    #[test]
    #[ignore]
    fn test_files() {
        println!("{:?}", assert_success("class Test"));
        assert_ne!(0, 0);
    }
}