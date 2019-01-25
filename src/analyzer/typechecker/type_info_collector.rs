use std::collections::HashMap;
use std::collections::HashSet;

use crate::analyzer::typechecker::ClassInfo;
use crate::analyzer::typechecker::FunctionInfo;
use crate::analyzer::typechecker::Kind;
use crate::analyzer::typechecker::ObjectInfo;
use crate::analyzer::typechecker::PropertyInfo;
use crate::analyzer::typechecker::TypeInfo;
use crate::parser::parse_tree::*;
use crate::parser::parse_tree::KotlinFile;
use crate::vec_with;

struct DefinedTypes<'t> {
    types: HashMap<String, TypeInfo>,
    file: &'t KotlinFile,
}

fn collect_all_types_info(types: &mut DefinedTypes, ast: &KotlinFile) {
    let empty = vec![];

    let path = if let Some(pck) = &ast.preamble.package_header {
        &pck.path
    } else {
        &empty
    };

    for obj in &ast.objects {
        collect_top_level_object_info(types, obj, path.clone());
    }
}

fn collect_top_level_object_info(refs: &mut DefinedTypes, obj: &TopLevelObject, path: Vec<String>) {
    match obj {
        TopLevelObject::Object(obj) => {
            collect_object_info(refs, obj, path);
        }
        TopLevelObject::Class(class) => {
            collect_class_info(refs, class, path);
        }
        TopLevelObject::Function(fun) => {}
        TopLevelObject::Property(prop) => {}
        TopLevelObject::TypeAlias(alias) => {}
    }
}

fn collect_class_info(refs: &mut DefinedTypes, class: &Class, path: Vec<String>) {
    let mut ty = TypeInfo::new(&class.name, &path, Kind::Class(ClassInfo::new(refs.file, class)));

    if let Some(ctor) = &class.primary_constructor {
        ty.functions.insert(class.name.to_owned(), FunctionInfo::from_primary_constructor(ctor));
    }

    if let Some(it) = &class.body {
        for member in &it.members {
            match member {
                Member::Object(sub_obj) => {
                    ty.objects.insert(sub_obj.name.to_owned(), ObjectInfo::new(refs.file, sub_obj));
                    collect_object_info(refs, sub_obj, vec_with(&path, class.name.to_owned()));
                }
                Member::Class(sub_obj) => {
                    ty.classes.insert(sub_obj.name.to_owned(), ClassInfo::new(refs.file, sub_obj));
                    collect_class_info(refs, sub_obj, vec_with(&path, class.name.to_owned()));
                }
                Member::Function(fun) => {
                    ty.functions.insert(fun.name.to_owned(), FunctionInfo::new(fun));
                }
                Member::Property(prop) => {
                    debug_assert_eq!(prop.declarations.len(), 1);
                    let decl = prop.declarations.first().unwrap();
                    ty.properties.insert(decl.name.to_owned(), PropertyInfo::new(prop));
                }
                Member::CompanionObject(sub_obj) => {
                    let name = if sub_obj.name.is_empty() {
                        "Companion"
                    } else {
                        &sub_obj.name
                    };
                    ty.objects.insert(name.to_owned(), ObjectInfo::new(refs.file, sub_obj));
                    collect_object_info(refs, sub_obj, vec_with(&path, class.name.to_owned()));
                }
                Member::TypeAlias(_) => {}
                Member::AnonymousInitializer(_) => {}
                Member::SecondaryConstructor(ctor) => {
                    ty.functions.insert(class.name.to_owned(), FunctionInfo::from_secondary_constructor(ctor));
                }
            }
        }
    }

//    refs.insert(class.name.to_owned(), ty);
}

fn collect_object_info(refs: &mut DefinedTypes, obj: &Object, path: Vec<String>) {
    let mut ty = TypeInfo::new(&obj.name, &path, Kind::Object(ObjectInfo::new(refs.file, obj)));

//    if let Some(ctor) = &obj.primary_constructor {
//        ty.functions.insert(fun.name.to_owned(), FunctionInfo::from_primary_constructor(ctor));
//    }

    if let Some(it) = &obj.body {
        for member in &it.members {
            match member {
                Member::Object(sub_obj) => {
                    ty.objects.insert(sub_obj.name.to_owned(), ObjectInfo::new(refs.file, sub_obj));
                    collect_object_info(refs, sub_obj, vec_with(&path, obj.name.to_owned()));
                }
                Member::Class(sub_obj) => {
                    ty.classes.insert(sub_obj.name.to_owned(), ClassInfo::new(refs.file, sub_obj));
                    collect_class_info(refs, sub_obj, vec_with(&path, obj.name.to_owned()));
                }
                Member::Function(fun) => {
                    ty.functions.insert(fun.name.to_owned(), FunctionInfo::new(fun));
                }
                Member::Property(prop) => {
                    debug_assert_eq!(prop.declarations.len(), 1);
                    let decl = prop.declarations.first().unwrap();
                    ty.properties.insert(decl.name.to_owned(), PropertyInfo::new(prop));
                }
                Member::CompanionObject(sub_obj) => {
                    assert_eq!(sub_obj.name.is_empty(), false);
                    ty.objects.insert(sub_obj.name.to_owned(), ObjectInfo::new(refs.file, sub_obj));
                    collect_object_info(refs, sub_obj, vec_with(&path, obj.name.to_owned()));
                }
                Member::TypeAlias(_) => {}
                Member::AnonymousInitializer(_) => {}
                Member::SecondaryConstructor(ctor) => {
                    ty.functions.insert(obj.name.to_owned(), FunctionInfo::from_secondary_constructor(ctor));
                }
            }
        }
    }

//    refs.insert(obj.name.to_owned(), ty);
}