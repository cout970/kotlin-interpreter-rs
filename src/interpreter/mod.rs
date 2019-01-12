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
            env.alias.insert(name.clone(), signature_of_type(ty));
        }
    }
    unimplemented!()
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
                            },
                            CallSiteTypeParams::Type(ty) => {
                                val.push_str(&signature_of_type(ty));
                            },
                        }

                        if j != params.len() - 1 {
                            val.push(',');
                        }
                    }
                    val.push('>');
                }

                if i != params.len() - 1 {
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

impl Env {
    pub fn call_method(&mut self, instance: Option<Value>, name: &str) -> Result<Value, KtError> {
        unimplemented!()
    }
}