#![allow(unused)]

use std::collections::HashMap;

use crate::analyzer::ast::AstFile;
use crate::analyzer::typechecker::collect_file_symbols::collect_file_symbols;
use crate::analyzer::typechecker::resolve_imports::resolve_imports;
use crate::errors::KtError;
use crate::parser::parse_tree::*;
use crate::source_code::SourceCode;

mod collect_file_symbols;
//mod compiler;
mod resolve_imports;

pub struct TypeChecker {
    code: SourceCode,
}

pub struct FileSymbols {
    pub functions: Vec<String>,
    pub properties: Vec<String>,
    pub classes: Vec<String>,
}

impl FileSymbols {
    pub fn new() -> Self {
        Self {
            functions: vec![],
            properties: vec![],
            classes: vec![],
        }
    }
}

pub struct RawFile {
    pub path: String,
    pub code: SourceCode,
}

pub struct ParsedFile {
    pub path: String,
    pub code: SourceCode,
    pub ast: KotlinFile,
}

pub struct CheckedFile {
    pub path: String,
    pub code: SourceCode,
    pub ast: AstFile,
}

//pub struct AnalyzedFile {
//    pub path: String,
//    pub code: SourceCode,
//    pub ast: AstFile,
//
//}

pub fn check_types(mut files: Vec<CheckedFile>) -> Vec<KtError>{
    let mut file_map: HashMap<String, FileSymbols> = HashMap::new();
    let mut errors = vec![];
    // Collect exposed names from files
    for file in &files {
        // TODO file path or package path?
        file_map.insert(file.path.clone(), collect_file_symbols(&file.ast));
    }

    // Resolve type names from imports
    for file in &mut files {
        for (span, info) in resolve_imports(&file_map, &file.path, &mut file.ast) {
            errors.push(KtError::Analyser { code: file.code.clone(), span, info });
        }
    }



    // Resolve global types
    // - Resolve function types
    // - Resolve property types

    // Resolve local types
    // - Resolve function inferred return
    // - Resolve local properties
    // - Resolve abstract methods

    // Done?
    errors
}

#[cfg(test)]
mod tests {
    use crate::test_utils::assert_success;

    #[test]
    fn update_names(){
        // https://markojerkic.com/algorithms-for-finding-a-list-of-prime-numbers-in-kotlin/
        assert_success(r#"
class Primes() {

    fun getPrimesBrute(n: Int): ArrayList<Int> {
        val result = ArrayList<Int>()

        for (i in (2..n)) {
            var t = true
            for (j in 2..i-1) {
                if (i % j == 0) {
                    t = false
                    break
                }
            }
            if (t) result.add(i)
        }
        return result
    }

}
        "#);
    }
}

// OLD
//
//#[derive(Debug, Clone, PartialEq)]
//pub struct TypeInfo {
//    pub name: String,
//    pub path: Vec<String>,
//    pub kind: Kind,
//    pub objects: HashMap<String, ObjectInfo>,
//    pub classes: HashMap<String, ClassInfo>,
//    pub functions: HashMap<String, FunctionInfo>,
//    pub properties: HashMap<String, PropertyInfo>,
//}
//
//#[derive(Debug, Clone, PartialEq)]
//pub enum Kind {
//    Class(ClassInfo),
//    Object(ObjectInfo),
//    Function,
//}
//
//#[derive(Debug, Clone, PartialEq)]
//pub struct ObjectInfo {}
//
//#[derive(Debug, Clone, PartialEq)]
//pub struct ClassInfo {
//    class_type: ClassType,
//    reference: String,
//}
//
//#[derive(Debug, Clone, PartialEq)]
//pub struct FunctionInfo {}
//
//#[derive(Debug, Clone, PartialEq)]
//pub struct PropertyInfo {}
//
//impl ObjectInfo {
//    pub fn new(file: &KotlinFile, obj: &Object) -> Self {
//        Self {}
//    }
//}
//
//impl ClassInfo {
//    pub fn new(file: &KotlinFile, class: &Class) -> Self {
//        Self {
//            class_type: class.class_type,
//            // TODO reference should use the full path for example com.mypackage.ObjectA.funA.Name
//            reference: file.get_package_str() + "." + &class.name,
//        }
//    }
//}
//
//impl FunctionInfo {
//    pub fn new(fun: &Function) -> Self {
//        Self {}
//    }
//    pub fn from_primary_constructor(fun: &PrimaryConstructor) -> Self {
//        Self {}
//    }
//    pub fn from_secondary_constructor(fun: &SecondaryConstructor) -> Self {
//        Self {}
//    }
//}
//
//impl PropertyInfo {
//    pub fn new(prop: &Property) -> Self {
//        Self {}
//    }
//}
//
//
//impl TypeInfo {
////    fn new(name: &str) -> Self {
////        TypeInfo {
////            name: name.to_owned(),
////            path: path.clone(),
////            objects: HashMap::new(),
////            classes: HashMap::new(),
////            functions: HashMap::new(),
////            properties: HashMap::new(),
////        }
////    }
//}

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
