use std::collections::HashSet;

use crate::errors::AnalyserError;
use crate::parser::parse_tree::TypeConstraint;
use crate::parser::parse_tree::TypeParameter;

type Result = Vec<AnalyserError>;

pub fn check_type_parameters(params: &Vec<TypeParameter>, constraints: &Vec<TypeConstraint>) -> Vec<AnalyserError> {
    let mut errors = vec![];
    let mut param_names = HashSet::new();

    for param in params {
        if param_names.contains(&param.name) {
            errors.push(AnalyserError::DuplicatedTypeParameter {
                param: param.name.to_owned()
            });
        }
        param_names.insert(param.name.to_owned());
    }

    for constraint in constraints {
        if !param_names.contains(&constraint.name) {
            errors.push(AnalyserError::UnknownTypeParameter {
                param: constraint.name.to_owned()
            });
        }
    }

    if params.is_empty() && !constraints.is_empty(){
        errors.push(AnalyserError::TypeConstrainsNotAllowed);
    }

    errors
}