use std::collections::HashMap;

use crate::parser::ast::*;
use crate::errors::KtError;
use crate::interpreter::value::Value;
use crate::source_code::SourceCode;

pub mod value;
mod vm;

pub struct Env {
    code: SourceCode,
    ast: KotlinFile,
    alias: HashMap<String, String>,
    symbols: Vec<HashMap<String, Symbol>>,
}

pub enum Symbol {
    Function,
    Variable,
    Class,
    Object,
    Value,
}

fn run(env: &mut Env, ast: &KotlinFile) -> Result<Value, KtError> {
    for obj in &ast.objects {
        declare_top_level_object(env, obj)?;
    }

    env.call_method(None, "main")
}

fn declare_top_level_object(env: &mut Env, obj: &TopLevelObject) -> Result<(), KtError> {
    match obj {
        TopLevelObject::Class(_) => {}
        TopLevelObject::Object(_) => {}
        TopLevelObject::Function(_) => {}
        TopLevelObject::Property(_) => {}
        TopLevelObject::TypeAlias(TypeAlias { span, modifiers, name, type_parameters, ty }) => {
//            env.alias.insert(name.clone(), signature_of_type(ty));
        }
    }
    unimplemented!()
}


impl Env {
    pub fn call_method(&mut self, instance: Option<Value>, name: &str) -> Result<Value, KtError> {
        unimplemented!()
    }
}