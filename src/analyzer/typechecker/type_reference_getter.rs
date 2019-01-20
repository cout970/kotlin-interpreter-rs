use std::collections::HashMap;
use std::collections::HashSet;

use crate::parser::ast::*;

struct RefVec {
    pub types: HashSet<String>,
    pub type_params: Vec<HashSet<String>>,
    pub alias: HashMap<String, String>,
}

pub fn get_all_references_to_types(file: &KotlinFile) -> Vec<String> {
    let mut alias = HashMap::new();

    for import in &file.preamble.imports {
        if let Some(a) = &import.alias {
            alias.insert(a.clone(), import.path.last().unwrap().clone());
        }
    }

    let mut refs = RefVec {
        types: HashSet::new(),
        type_params: vec![],
        alias,
    };

    for obj in &file.objects {
        get_top_level_object_references(&mut refs, obj);
    }

    debug_assert_eq!(refs.type_params.len(), 0);
    refs.types.into_iter().collect::<Vec<_>>()
}

fn get_simple_user_type_references(refs: &mut RefVec, user_type: &UserType) {
    let mut name = String::new();

    name.push_str(&user_type[0].name);
    for i in 1..user_type.len() {
        name.push('.');
        name.push_str(&user_type[i].name);
    }

    for ty in user_type {
        for param in &ty.type_params {
            match param {
                CallSiteTypeParams::Projection => {}
                CallSiteTypeParams::Type(ty) => {
                    get_type_references(refs, ty);
                }
            }
        }
    }

    // If the name is the same as a type argument then we ignore it
    for set in &refs.type_params {
        if set.contains(&name) {
            return;
        }
    }

    if let Some(it) = refs.alias.get(&name) {
        refs.types.insert(it.clone());
    } else {
        refs.types.insert(name);
    }
}

fn get_top_level_object_references(refs: &mut RefVec, obj: &TopLevelObject) {
    match obj {
        TopLevelObject::Object(obj) => {
            get_object_references(refs, obj);
        }
        TopLevelObject::Function(fun) => {
            get_function_references(refs, fun);
        }
        TopLevelObject::Property(prop) => {
            get_property_references(refs, prop);
        }
        TopLevelObject::Class(class) => {
            get_class_references(refs, class);
        }
        TopLevelObject::TypeAlias(alias) => {
            get_typealias_references(refs, alias);
        }
    }
}

fn get_property_references(refs: &mut RefVec, prop: &Property) {
    let type_parameters: HashSet<String> = prop.type_parameters.iter()
        .map(|it| it.name.to_owned())
        .collect();

    refs.type_params.push(type_parameters);

    // Receiver
    if let Some(it) = &prop.receiver {
        get_type_references(refs, it);
    }

    // Type parameters
    for param in &prop.type_parameters {
        if let Some(it) = &param.user_type {
            get_type_references(refs, it);
        }
    }

    // Type constraints
    for x in &prop.type_constraints {
        get_type_references(refs, &x.ty);
    }

    // Variables declared
    for entry in &prop.declarations {
        if let Some(it) = &entry.declared_type {
            get_type_references(refs, it);
        }
    }

    // Setter
    if let Some(setter) = &prop.setter {
        if let Some(it) = &setter.param_ty {
            get_type_references(refs, it);
        }

        if let Some(body) = &setter.body {
            match body {
                FunctionBody::Block(block) => {
                    get_block_references(refs, block);
                }
                FunctionBody::Expression((_, expr)) => {
                    get_expr_references(refs, expr);
                }
            }
        }
    }

    // Getter
    if let Some(getter) = &prop.getter {
        if let Some(it) = &getter.ty {
            get_type_references(refs, it);
        }

        if let Some(body) = &getter.body {
            match body {
                FunctionBody::Block(block) => {
                    get_block_references(refs, block);
                }
                FunctionBody::Expression((_, expr)) => {
                    get_expr_references(refs, expr);
                }
            }
        }
    }

    refs.type_params.pop().unwrap();
}

fn get_function_references(refs: &mut RefVec, fun: &Function) {
    let type_parameters: HashSet<String> = fun.type_parameters.iter()
        .map(|it| it.name.to_owned())
        .collect();

    refs.type_params.push(type_parameters);

    // Receiver
    if let Some(it) = &fun.receiver {
        get_type_references(refs, it);
    }

    // Type parameters
    for param in &fun.type_parameters {
        if let Some(it) = &param.user_type {
            get_type_references(refs, it);
        }
    }

    // Type constraints
    for x in &fun.type_constraints {
        get_type_references(refs, &x.ty);
    }

    // Return
    get_type_references(refs, &fun.return_type);

    // Parameters
    for x in &fun.value_parameters {
        get_type_references(refs, &x.ty);

        if let Some((_, expr)) = &x.default_value {
            get_expr_references(refs, expr);
        }
    }

    // Body
    if let Some(body) = &fun.body {
        match body {
            FunctionBody::Block(block) => {
                get_block_references(refs, block);
            }
            FunctionBody::Expression((_, expr)) => {
                get_expr_references(refs, expr);
            }
        }
    }

    refs.type_params.pop().unwrap();
}

fn get_class_references(refs: &mut RefVec, class: &Class) {
    let type_parameters: HashSet<String> = class.type_parameters.iter()
        .map(|it| it.name.to_owned())
        .collect();

    refs.type_params.push(type_parameters);
    refs.types.insert(class.name.to_owned());

    // Type parameters
    for param in &class.type_parameters {
        if let Some(it) = &param.user_type {
            get_type_references(refs, it);
        }
    }

    // Type constraints
    for x in &class.type_constraints {
        get_type_references(refs, &x.ty);
    }

    // Primary constructor
    if let Some(constructor) = &class.primary_constructor {
        for x in &constructor.params {
            get_type_references(refs, &x.ty);
            if let Some((_, expr)) = &x.default_value {
                get_expr_references(refs, expr);
            }
        }
    }

    // Interfaces, Super classes
    for x in &class.delegations {
        get_delegation_specifier_references(refs, x);
    }

    // Body
    if let Some(it) = &class.body {
        for member in &it.members {
            get_member_references(refs, member);
        }
    }

    refs.type_params.pop().unwrap();
}

fn get_class_body_references(refs: &mut RefVec, body: &ClassBody) {
    for member in &body.members {
        get_member_references(refs, member);
    }

    for entry in &body.enum_entries {
        if let Some(it) = &entry.class_body {
            get_class_body_references(refs, it);
        }
        for arg in &entry.value_arguments {
            get_expr_references(refs, &arg.expr.1);
        }
    }
}

fn get_object_references(refs: &mut RefVec, obj: &Object) {
    refs.types.insert(obj.name.to_owned());

    // Primary constructor
    if let Some(constructor) = &obj.primary_constructor {
        for x in &constructor.params {
            get_type_references(refs, &x.ty);
            if let Some((_, expr)) = &x.default_value {
                get_expr_references(refs, expr);
            }
        }
    }

    // Interfaces, Super classes
    for x in &obj.delegations {
        get_delegation_specifier_references(refs, x);
    }

    // Body
    if let Some(it) = &obj.body {
        for member in &it.members {
            get_member_references(refs, member);
        }
    }
}

fn get_delegation_specifier_references(refs: &mut RefVec, delegation: &DelegationSpecifier) {
    match delegation {
        DelegationSpecifier::Type(ty) => {
            get_type_references(refs, ty);
        }
        DelegationSpecifier::DelegatedBy(ty, (_, expr)) => {
            get_type_references(refs, ty);
            get_expr_references(refs, expr);
        }
        DelegationSpecifier::FunctionCall(ty, suffix) => {
            get_type_references(refs, ty);
            get_call_suffix_references(refs, suffix);
        }
    }
}

fn get_call_suffix_references(refs: &mut RefVec, suffix: &CallSuffix) {
    if let Some(lambda) = &suffix.annotated_lambda {
        for statement in &lambda.body.statements {
            get_statement_references(refs, statement);
        }

        for decl in &lambda.body.parameters {
            if let Some(ty) = &decl.declared_type {
                get_type_references(refs, ty);
            }
        }
    }

    for ty in &suffix.type_arguments {
        get_type_references(refs, ty);
    }

    for arg in &suffix.value_arguments {
        get_expr_references(refs, &arg.expr.1);
    }
}

fn get_typealias_references(refs: &mut RefVec, alias: &TypeAlias) {
    let type_parameters: HashSet<String> = alias.type_parameters.iter()
        .map(|it| it.name.to_owned())
        .collect();

    refs.type_params.push(type_parameters);
    refs.types.insert(alias.name.to_owned());

    get_type_references(refs, &alias.ty);
    for param in &alias.type_parameters {
        if let Some(ty) = &param.user_type {
            get_type_references(refs, ty);
        }
    }
    refs.type_params.pop().unwrap();
}

fn get_member_references(refs: &mut RefVec, member: &Member) {
    match member {
        Member::CompanionObject(obj) => {
            get_object_references(refs, obj);
        }
        Member::Object(obj) => {
            get_object_references(refs, obj);
        }
        Member::Function(fun) => {
            get_function_references(refs, fun);
        }
        Member::Property(prop) => {
            get_property_references(refs, prop);
        }
        Member::Class(class) => {
            get_class_references(refs, class);
        }
        Member::TypeAlias(alias) => {
            get_typealias_references(refs, alias);
        }
        Member::AnonymousInitializer(init) => {
            get_block_references(refs, &init.body);
        }
        Member::SecondaryConstructor(constructor) => {
            // Body
            if let Some(it) = &constructor.body {
                get_block_references(refs, it);
            }

            // Call to super
            match &constructor.delegation_call {
                DelegationCall::Super(args) | DelegationCall::This(args) => {
                    for x in args {
                        get_expr_references(refs, &x.expr.1);
                    }
                }
                DelegationCall::None => {}
            }

            // Arguments
            for x in &constructor.parameters {
                get_type_references(refs, &x.ty);

                if let Some((_, expr)) = &x.default_value {
                    get_expr_references(refs, expr);
                }
            }
        }
    }
}

fn get_type_references(refs: &mut RefVec, ty: &Type) {
    get_type_ref_references(refs, &ty.reference);
}

fn get_type_ref_references(refs: &mut RefVec, ty: &TypeReference) {
    match ty {
        TypeReference::Function(fun) => {
            get_type_ref_references(refs, &fun.return_type);
            if let Some(it) = &fun.receiver {
                get_type_ref_references(refs, it);
            }
        }
        TypeReference::UserType(user_type) => {
            get_simple_user_type_references(refs, user_type);
        }
        TypeReference::Nullable(it) => {
            get_type_ref_references(refs, it);
        }
    }
}


fn get_block_references(refs: &mut RefVec, block: &Block) {
    let (_, statements) = block;

    for x in statements {
        get_statement_references(refs, x);
    }
}

fn get_statement_references(refs: &mut RefVec, statement: &Statement) {
    match statement {
        Statement::Expr((_, expr)) => {
            get_expr_references(refs, expr);
        }
        Statement::Decl(decl) => {
            match decl {
                Declaration::Object(obj) => {
                    get_object_references(refs, obj);
                }
                Declaration::Function(fun) => {
                    get_function_references(refs, fun);
                }
                Declaration::Property(prop) => {
                    get_property_references(refs, prop);
                }
                Declaration::Class(class) => {
                    get_class_references(refs, class);
                }
                Declaration::TypeAlias(alias) => {
                    get_typealias_references(refs, alias);
                }
            }
        }
    }
}

fn get_expr_references(refs: &mut RefVec, expr: &Expr) {
    match expr {
        Expr::Chain { operands, .. } => {
            for (_, e) in operands {
                get_expr_references(refs, e);
            }
        }
        Expr::InfixFun { parameters, .. } => {
            for (_, e) in parameters {
                get_expr_references(refs, e);
            }
        }
        Expr::Prefix { expr, .. } => {
            get_expr_references(refs, &expr.1);
        }
        Expr::Postfix { expr, postfix, .. } => {
            get_expr_references(refs, &expr.1);

            for x in postfix {
                match x {
                    ExprPostfix::Increment => {}
                    ExprPostfix::Decrement => {}
                    ExprPostfix::AssertNonNull => {}
                    ExprPostfix::ArrayAccess((_, e)) => {
                        get_expr_references(refs, e);
                    }
                    ExprPostfix::FunCall(suffix) => {
                        get_call_suffix_references(refs, suffix);
                    }
                    ExprPostfix::MemberAccess { next, .. } => {
                        get_expr_references(refs, &next.1);
                    }
                }
            }
        }
        Expr::Is { expr, ty, .. } => {
            get_expr_references(refs, &expr.1);
            get_type_references(refs, ty);
        }
        Expr::String(comps, ..) => {
            for x in comps {
                match x {
                    StringComponent::Content(_) => {}
                    StringComponent::Variable(_) => {}
                    StringComponent::Template(expr) => {
                        get_expr_references(refs, &expr.1);
                    }
                }
            }
        }
        Expr::If { cond, if_true, if_false, .. } => {
            get_expr_references(refs, &cond.1);
            get_block_references(refs, if_true);
            if let Some(it) = if_false {
                get_block_references(refs, it);
            }
        }
        Expr::Try { block, catch_blocks, finally, .. } => {
            get_block_references(refs, block);
            for x in catch_blocks {
                get_simple_user_type_references(refs, &x.ty);
                get_block_references(refs, &x.block);
            }
        }
        Expr::For { variables, expr, body, .. } => {
            for entry in variables {
                if let Some(it) = &entry.declared_type {
                    get_type_references(refs, it);
                }
            }
            get_expr_references(refs, &expr.1);
            get_block_references(refs, body);
        }
        Expr::While { expr, body, .. } => {
            get_expr_references(refs, &expr.1);
            get_block_references(refs, body);
        }
        Expr::DoWhile { expr, body, .. } => {
            get_expr_references(refs, &expr.1);
            get_block_references(refs, body);
        }
        Expr::When { expr, entries, .. } => {
            if let Some(expr) = expr {
                get_expr_references(refs, &expr.1);
            }
            for entry in entries {
                get_block_references(refs, &entry.body);
                for cond in &entry.conditions {
                    match cond {
                        WhenCondition::Else => {}
                        WhenCondition::Expr(expr) => {
                            get_expr_references(refs, &expr.1);
                        }
                        WhenCondition::In { expr, .. } => {
                            get_expr_references(refs, &expr.1);
                        }
                        WhenCondition::Is { ty, .. } => {
                            get_type_references(refs, ty);
                        }
                    }
                }
            }
        }
        Expr::Object { delegation_specifiers, body, .. } => {
            for x in delegation_specifiers {
                get_delegation_specifier_references(refs, x);
            }
            get_class_body_references(refs, body);
        }
        Expr::CallableRef { ty, type_arguments, .. } => {
            get_simple_user_type_references(refs, ty);
            for ty in type_arguments {
                get_type_references(refs, ty);
            }
        }
        Expr::Lambda(fun) => {
            for entry in &fun.parameters {
                if let Some(it) = &entry.declared_type {
                    get_type_references(refs, it);
                }
            }
            for x in &fun.statements {
                get_statement_references(refs, x);
            }
        }
        Expr::Ref(_) => {}
        Expr::Boolean(_) => {}
        Expr::Char(_) => {}
        Expr::Number(_) => {}
        Expr::Null => {}
        Expr::This => {}
        Expr::Super => {}
        Expr::Throw(expr, ..) => {
            get_expr_references(refs, &expr.1);
        }
        Expr::Return(opt) => {
            if let Some(expr) = opt {
                get_expr_references(refs, &expr.1);
            }
        }
        Expr::Continue => {}
        Expr::Break => {}
    }
}
