#![allow(unused)]

use std::collections::HashMap;

use crate::analyzer::typechecker::compiler::compile_file;
use crate::analyzer::typechecker::type_reference_getter::get_all_references_to_types;
use crate::parser::parse_tree::*;
use crate::source_code::SourceCode;

mod type_reference_getter;
mod type_info_collector;
mod compiler;


pub struct TypeChecker {
    code: SourceCode,
    types: HashMap<String, TypeInfo>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeInfo {
    pub name: String,
    pub path: Vec<String>,
    pub kind: Kind,
    pub objects: HashMap<String, ObjectInfo>,
    pub classes: HashMap<String, ClassInfo>,
    pub functions: HashMap<String, FunctionInfo>,
    pub properties: HashMap<String, PropertyInfo>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Kind {
    Class(ClassInfo),
    Object(ObjectInfo),
    Function,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectInfo {}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassInfo {
    class_type: ClassType,
    reference: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionInfo {}

#[derive(Debug, Clone, PartialEq)]
pub struct PropertyInfo {}

impl ObjectInfo {
    pub fn new(file: &KotlinFile, obj: &Object) -> Self {
        Self {}
    }
}

impl ClassInfo {

    pub fn new(file: &KotlinFile, class: &Class) -> Self {
        Self {
            class_type: class.class_type,
            // TODO reference should use the full path for example com.mypackage.ObjectA.funA.Name
            reference: file.get_package_str() + "." + &class.name,
        }
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
    fn new(name: &str, path: &Vec<String>, kind: Kind) -> Self {
        TypeInfo {
            name: name.to_owned(),
            path: path.clone(),
            kind,
            objects: HashMap::new(),
            classes: HashMap::new(),
            functions: HashMap::new(),
            properties: HashMap::new(),
        }
    }
}

impl TypeChecker {
    pub fn run(code: SourceCode, ast: &KotlinFile) {
        let checker = TypeChecker {
            types: HashMap::new(),
            code
        };
        let refs = get_all_references_to_types(ast);

//        collect_all_types_info(&mut checker.types, ast);

//        compile_file(&mut checker, ast).unwrap();
//        TypeResolver::resolve(&checker, &ast);

        dbg!(refs);
    }

    pub fn get_class_info(&self, ty: &Type) -> Option<&ClassInfo> {
        let info = self.types.get(&signature_of_type(ty))?;

        match &info.kind {
            Kind::Class(info) => Some(info),
            Kind::Object(..) => None,
            Kind::Function => None,
        }
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
