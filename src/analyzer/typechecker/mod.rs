use std::collections::HashMap;

use crate::analyzer::typechecker::type_info_collector::collect_all_types_info;
use crate::analyzer::typechecker::type_reference_getter::get_all_references_to_types;
use crate::parser::ast::*;

mod type_reference_getter;
mod type_info_collector;
mod compiler;

pub struct TypeChecker {
    types: HashMap<String, TypeInfo>
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeInfo {
    pub name: String,
    pub path: Vec<String>,
    pub objects: HashMap<String, ObjectInfo>,
    pub classes: HashMap<String, ClassInfo>,
    pub functions: HashMap<String, FunctionInfo>,
    pub properties: HashMap<String, PropertyInfo>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectInfo {}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassInfo {}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionInfo {}

#[derive(Debug, Clone, PartialEq)]
pub struct PropertyInfo {}

impl ObjectInfo {
    pub fn new(obj: &Object) -> Self {
        Self {}
    }
}

impl ClassInfo {
    pub fn new(class: &Class) -> Self {
        Self {}
    }
}

impl FunctionInfo {
    pub fn new(fun: &Function) -> Self {
        Self {}
    }
    pub fn from_primary_constructor(fun: &PrimaryConstructor) -> Self {
        Self {}
    }
    pub fn from_secondary_constructor(fun: &SecondaryConstructor) -> Self {
        Self {}
    }
}

impl PropertyInfo {
    pub fn new(prop: &Property) -> Self {
        Self {}
    }
}


impl TypeInfo {
    fn new(name: &str, path: &Vec<String>) -> Self {
        TypeInfo {
            name: name.to_owned(),
            path: path.clone(),
            objects: HashMap::new(),
            classes: HashMap::new(),
            functions: HashMap::new(),
            properties: HashMap::new(),
        }
    }
}

impl TypeChecker {
    pub fn run(ast: &KotlinFile) {
        let mut checker = TypeChecker{
            types: HashMap::new()
        };
        let refs = get_all_references_to_types(ast);

        collect_all_types_info(&mut checker.types, ast);

//        TypeResolver::resolve(&checker, &ast);

        dbg!(refs);
    }
}
