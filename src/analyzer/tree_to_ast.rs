use lazy_static::*;

use crate::analyzer::ast::*;
use crate::analyzer::modifiers::check_modifiers;
use crate::analyzer::type_parameters::check_type_parameters;
use crate::create_vec;
use crate::errors::AnalyserError;
use crate::errors::KtError;
use crate::parser::parse_tree::*;

use crate::generate_rand_str;
use crate::source::{Source, ByteSpan};
use crate::token::Number;

struct Context {
    code: Source,
    errors: Vec<KtError>,
    modifier_ctx: Vec<ModifierCtx>,
    env_type: Vec<EnvType>,
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum EnvType {
    TopLevel,
    Class,
    Statement,
}

lazy_static! {
    pub static ref UNIT_TYPE: AstType = AstType {
        span: Default::default(),
        name: "Unit".to_string(),
        full_name: "kotlin.Unit".to_string(),
        type_parameters: Vec::new(),
        nullable: false,
    };
    pub static ref NOTHING_TYPE: AstType = AstType {
        span: Default::default(),
        name: "Nothing".to_string(),
        full_name: "kotlin.Nothing".to_string(),
        type_parameters: Vec::new(),
        nullable: false,
    };
}

impl Context {
    fn new_error(&mut self, span: ByteSpan, info: AnalyserError) {
        self.errors.push(KtError::Analyser {
            code: self.code.clone(),
            span,
            info,
        });
    }

    fn add_errors(&mut self, span: ByteSpan, errors: Vec<AnalyserError>) {
        for x in errors {
            self.errors.push(KtError::Analyser {
                code: self.code.clone(),
                span,
                info: x,
            });
        }
    }

    fn check_modifiers(&mut self, span: ByteSpan, modifiers: &Vec<Modifier>, target: ModifierTarget) {
        self.add_errors(span, check_modifiers(modifiers, *self.modifier_ctx.last().unwrap(), target));
    }

    fn check_type_parameters(&mut self, span: ByteSpan, params: &Vec<TypeParameter>, constraints: &Vec<TypeConstraint>) {
        self.add_errors(span, check_type_parameters(params, constraints));
    }
}

pub fn file_to_ast(code: Source, file: &KotlinFile) -> (AstFile, Vec<KtError>) {
    let mut ctx = Context {
        code,
        errors: vec![],
        modifier_ctx: vec![],
        env_type: vec![EnvType::TopLevel],
    };

    let mut classes = vec![];
    let mut functions = vec![];
    let mut properties = vec![];
    let mut typealias = vec![];
    let mut imports = vec![];

    for import in &file.imports {
        imports.push(AstImport {
            span: import.span,
            name: import.path.to_owned().join("."),
            alias: import.alias.clone(),
        })
    }

    ctx.modifier_ctx.push(ModifierCtx::TopLevelObject);

    for tlo in &file.objects {
        match tlo {
            TopLevelObject::Class(class) => {
                classes.push(class_to_ast(&mut ctx, class));
            }
            TopLevelObject::Object(obj) => {
                classes.push(object_to_ast(&mut ctx, obj));
            }
            TopLevelObject::Function(fun) => {
                functions.push(function_to_ast(&mut ctx, fun));
            }
            TopLevelObject::Property(prop) => {
                properties.push(property_to_ast(&mut ctx, prop));
            }
            TopLevelObject::TypeAlias(ta) => {
                typealias.push(AstTypealias {
                    span: ta.span,
                    name: ta.name.to_string(),
                    ty: type_to_ast(&mut ctx, &ta.ty),
                })
            }
        }
    }

    ctx.modifier_ctx.pop().unwrap();

    (
        AstFile {
            package: file.get_package_str(),
            imports,
            classes,
            functions,
            properties,
            typealias,
        },
        ctx.errors
    )
}

fn statement_to_ast(ctx: &mut Context, statement: &Statement) -> Vec<AstStatement> {
    let single = match statement {
        Statement::Expression(e) => AstStatement::Expr(expr_to_ast(ctx, e)),
        Statement::Assignment(first, op, second) => {
            let var = expr_to_ast(ctx, first);
            let mut value = expr_to_ast(ctx, second);

            if op != "=" {
                let operator = op.chars().next().unwrap();

                value = AstExpr::Call {
                    span: get_span(&value),
                    receiver: Some(mut_rc(var.clone())),
                    function: operator.to_string(),
                    type_parameters: vec![],
                    args: vec![value],
                }
            }
            if let AstExpr::Ref { .. } = &var {
                AstStatement::Assignment(var, value)
            } else {
                ctx.new_error(get_span(&var), AnalyserError::ExpectedVariable);
                AstStatement::Expr(value)
            }
        }
        Statement::Declaration(decl) => {
            match decl {
                Declaration::Class(class) => {
                    AstStatement::Class(class_to_ast(ctx, class))
                }
                Declaration::Object(obj) => {
                    AstStatement::Class(object_to_ast(ctx, obj))
                }
                Declaration::Function(fun) => {
                    AstStatement::Function(function_to_ast(ctx, fun))
                }
                Declaration::Property(prop) => {
                    let mut multiple = vec![];
                    for prop in property_to_local_ast(ctx, prop) {
                        multiple.push(AstStatement::Property(prop));
                    }
                    return multiple;
                }
                Declaration::TypeAlias(ta) => {
                    ctx.new_error(ta.span, AnalyserError::NestedTypeAlias);
                    AstStatement::Class(AstClass {
                        span: ta.span,
                        name: "_error_".to_string(),
                        ..AstClass::default()
                    })
                }
            }
        }
        Statement::For { span, variables, expr, body, .. } => {
            let variables = variables.iter().map(|it| var_to_ast(ctx, false, it)).collect::<Vec<_>>();
            AstStatement::For {
                span: *span,
                variables,
                expr: mut_rc(expr_to_ast(ctx, expr)),
                body: block_to_ast(ctx, body),
            }
        }
        Statement::While { span, expr, body } => {
            AstStatement::While {
                span: *span,
                expr: mut_rc(expr_to_ast(ctx, expr)),
                body: block_to_ast(ctx, body),
            }
        }
        Statement::DoWhile { span, expr, body } => {
            AstStatement::DoWhile {
                span: *span,
                expr: mut_rc(expr_to_ast(ctx, expr)),
                body: block_to_ast(ctx, body),
            }
        }
    };

    vec![single]
}

fn class_to_ast(ctx: &mut Context, class: &Class) -> AstClass {
    ctx.check_modifiers(class.span, &class.modifiers, ModifierTarget::Class);
    ctx.check_type_parameters(class.span, &class.type_parameters, &class.type_constraints);
    let mut body = vec![];

    let inner = class.modifiers.contains(&Modifier::Inner);

    if inner && *ctx.env_type.last().unwrap() != EnvType::Class {
        ctx.new_error(class.span, AnalyserError::InvalidInnerClass);
    }

    let class_type = match class.class_type {
        ClassType::Class => {
            if class.modifiers.contains(&Modifier::Inner) {
                AstClassType::Inline
            } else if class.modifiers.contains(&Modifier::Data) {
                AstClassType::Data
            } else {
                AstClassType::Regular
            }
        }
        ClassType::Interface => AstClassType::Interface,
        ClassType::Enum => AstClassType::Enum,
        ClassType::Annotation => AstClassType::Annotation,
    };


    let inheritance_modifier = if class.modifiers.contains(&Modifier::Open) {
        AstInheritanceModifier::Open
    } else if class.modifiers.contains(&Modifier::Sealed) {
        AstInheritanceModifier::Sealed
    } else if class.modifiers.contains(&Modifier::Abstract) {
        AstInheritanceModifier::Abstract
    } else {
        AstInheritanceModifier::Final
    };

    let mut interfaces = vec![];
    let mut super_type = None;

    for del in &class.delegations {
        match del {
            DelegationSpecifier::Type(ty) => {
                interfaces.push(type_to_ast(ctx, ty));
            }
            DelegationSpecifier::DelegatedBy(ty, _) => {
                interfaces.push(type_to_ast(ctx, ty));
                // TODO delegation by expr
            }
            DelegationSpecifier::FunctionCall(ty, _) => {
                if let Some(_) = super_type {
                    ctx.new_error(class.span, AnalyserError::MultipleInheritance);
                }
                super_type = Some(type_to_ast(ctx, ty));
            }
        }
    }

    ctx.env_type.push(EnvType::Class);
    ctx.modifier_ctx.push(ModifierCtx::ClassMember);

    if let Some(b) = &class.body {
        for member in &b.members {
            body.push(member_to_ast(ctx, member))
        }

        if class.class_type == ClassType::Enum {
            enum_entries_to_ast(ctx, class.span, b, &mut body);
        }
    } else if class_type == AstClassType::Enum {
        ctx.new_error(class.span, AnalyserError::EnumWithoutBody);
    }

    ctx.modifier_ctx.pop().unwrap();
    ctx.env_type.pop().unwrap();

    // TODO duplicated interfaces?

    AstClass {
        span: class.span,
        name: class.name.to_string(),
        inner,
        class_type,
        inheritance_modifier,
        interfaces,
        super_type,
        body,
    }
}

fn enum_entries_to_ast(_ctx: &mut Context, span: ByteSpan, class_body: &ClassBody, members: &mut Vec<AstMember>) {
    for entry in &class_body.enum_entries {
        members.push(AstMember::Class(AstClass {
            span,
            name: entry.name.to_string(),
            class_type: AstClassType::Object,
            inheritance_modifier: AstInheritanceModifier::Final,
            ..AstClass::default()
        }));
    }
//    TODO needs initializer blocks
//    let init = AstMember::Function(unimplemented!());
//
//    let companion = members.iter_mut().find_map(|member| {
//        if let AstMember::Class(class) = member {
//            if class.class_type == AstClassType::Object && class.name == "Companion" {
//                Some(class)
//            } else {
//                None
//            }
//        } else {
//            None
//        }
//    });
//
//    if let Some(companion) = companion {
//        companion.body.push(init)
//    } else {
//        members.push(AstMember::Class(AstClass {
//            span,
//            name: "Companion".to_string(),
//            inner: false,
//            class_type: AstClassType::Object,
//            inheritance_modifier: AstInheritanceModifier::Final,
//            body: vec![init],
//        }));
//    }
}

fn object_to_ast(ctx: &mut Context, class: &Object) -> AstClass {
    ctx.check_modifiers(class.span, &class.modifiers, ModifierTarget::Object);
    let mut body = vec![];

    if class.primary_constructor.is_some() {
        ctx.new_error(class.span, AnalyserError::ObjectWithConstructor)
    }

    ctx.env_type.push(EnvType::Class);
    ctx.modifier_ctx.push(ModifierCtx::ClassMember);

    if let Some(b) = &class.body {
        for member in &b.members {
            body.push(member_to_ast(ctx, member))
        }
    }

    ctx.modifier_ctx.pop().unwrap();
    ctx.env_type.pop().unwrap();

    let mut interfaces = vec![];
    let mut super_type = None;

    for del in &class.delegations {
        match del {
            DelegationSpecifier::Type(ty) => {
                interfaces.push(type_to_ast(ctx, ty));
            }
            DelegationSpecifier::DelegatedBy(ty, _) => {
                interfaces.push(type_to_ast(ctx, ty));
                // TODO delegation by expr
            }
            DelegationSpecifier::FunctionCall(ty, _) => {
                if let Some(_) = super_type {
                    ctx.new_error(class.span, AnalyserError::MultipleInheritance);
                }
                super_type = Some(type_to_ast(ctx, ty));
            }
        }
    }

    // TODO duplicated interfaces?

    AstClass {
        span: class.span,
        name: class.name.to_string(),
        inner: false,
        class_type: AstClassType::Object,
        inheritance_modifier: AstInheritanceModifier::Final,
        interfaces,
        super_type,
        body,
    }
}

fn member_to_ast(ctx: &mut Context, member: &Member) -> AstMember {
    match member {
        Member::Object(obj) => AstMember::Class(object_to_ast(ctx, obj)),
        Member::Function(fun) => AstMember::Function(function_to_ast(ctx, fun)),
        Member::Property(prop) => AstMember::Property(property_to_ast(ctx, prop)),
        Member::Class(class) => AstMember::Class(class_to_ast(ctx, class)),
        Member::CompanionObject(_) => {
            // Ignored
            unimplemented!()
        }
        Member::TypeAlias(ta) => {
            ctx.new_error(ta.span, AnalyserError::NestedTypeAlias);
            AstMember::Class(AstClass {
                span: ta.span,
                name: "_error_".to_string(),
                ..AstClass::default()
            })
        }
        Member::AnonymousInitializer(_) => {
            // Ignored
            unimplemented!()
        }
        Member::SecondaryConstructor(_) => {
            // Ignored
            unimplemented!()
        }
    }
}

fn property_to_ast(ctx: &mut Context, prop: &Property) -> AstProperty {
    ctx.check_modifiers(prop.span, &prop.modifiers, ModifierTarget::Property);
    ctx.check_type_parameters(prop.span, &prop.type_parameters, &prop.type_constraints);

    ctx.env_type.push(EnvType::Statement);
    let (delegated, expr) = match &prop.initialization {
        PropertyInitialization::None => (false, None),
        PropertyInitialization::Expr(expr) => (false, Some(expr_to_ast(ctx, expr))),
        PropertyInitialization::Delegation(expr) => (true, Some(expr_to_ast(ctx, expr))),
    };
    ctx.env_type.pop().unwrap();

    if prop.declarations.len() != 1 {
        ctx.new_error(prop.span, AnalyserError::DestructuringInTopLevel);
    }

    let decl = prop.declarations.first().unwrap();
    let var = AstVar {
        name: decl.name.to_owned(),
        ty: decl.declared_type.as_ref().map(|it| type_to_ast(ctx, it)),
        mutable: prop.mutable,
    };

    let getter = if let Some(get) = &prop.getter {
        let mut capitalized = var.name.clone();
        if let Some(r) = capitalized.get_mut(0..1) {
            r.make_ascii_uppercase();
        }
        let name = "get".to_string() + &capitalized;
        let member = *ctx.env_type.last().unwrap() == EnvType::Class;
        let body = function_body_to_ast(ctx, &get.body);

        Some(AstFunction {
            span: get.span,
            extension: false,
            operator: false,
            member,
            name,
            args: vec![],
            return_ty: var.ty.clone(),
            body,
        })
    } else {
        None
    };

    let setter = if let Some(set) = &prop.setter {
        let mut capitalized = var.name.clone();
        if let Some(r) = capitalized.get_mut(0..1) {
            r.make_ascii_uppercase();
        }
        let name = "set".to_string() + &capitalized;
        let member = *ctx.env_type.last().unwrap() == EnvType::Class;
        let body = function_body_to_ast(ctx, &set.body);
        let mut args = vec![];

        args.push(AstVar {
            name: set.param_name.clone().unwrap_or("value".to_string()),
            ty: var.ty.clone(),
            mutable: false,
        });

        Some(AstFunction {
            span: set.span,
            extension: false,
            operator: false,
            member,
            name,
            args,
            return_ty: Some(UNIT_TYPE.clone()),
            body,
        })
    } else {
        None
    };

    AstProperty {
        span: prop.span,
        var,
        expr,
        delegated,
        getter,
        setter,
    }
}

fn function_to_ast(ctx: &mut Context, fun: &Function) -> AstFunction {
    ctx.check_modifiers(fun.span, &fun.modifiers, ModifierTarget::Function);
    ctx.check_type_parameters(fun.span, &fun.type_parameters, &fun.type_constraints);

    let body = function_body_to_ast(ctx, &fun.body);
    let member = *ctx.env_type.last().unwrap() == EnvType::Class;

    let mut args = vec![];

    if let Some(rec) = &fun.receiver {
        args.push(AstVar {
            name: "receiver".to_owned(),
            ty: Some(type_to_ast(ctx, rec)),
            mutable: false,
        });
    }

    let mut vararg = false;
    for param in &fun.value_parameters {
        if args.iter().any(|it| it.name == param.name) {
            ctx.new_error(fun.span, AnalyserError::DuplicatedFunctionParameter {
                param: param.name.to_owned()
            });
        }

        if param.modifiers.contains(&Modifier::Vararg) {
            if vararg {
                ctx.new_error(fun.span, AnalyserError::MultipleVarargs);
            }
            vararg = true;
        }

        args.push(AstVar {
            name: param.name.to_owned(),
            ty: Some(type_to_ast(ctx, &param.ty)),
            mutable: false,
        });

        // TODO this should be in the function's block code
//        param.default_value
    }

    let return_ty = match &fun.return_type {
        Some(ty) => { Some(type_to_ast(ctx, ty)) }
        None => {
            match &fun.body {
                Some(body) => {
                    match *body {
                        FunctionBody::Block(_) => Some(UNIT_TYPE.clone()),
                        FunctionBody::Expression(_) => None,
                    }
                }
                None => Some(UNIT_TYPE.clone()),
            }
        }
    };

    let mut operator = false;

    // Check modifiers
    if fun.modifiers.contains(&Modifier::Operator) {
        // TODO move to typecheck step?
        operator = true;
        match fun.name.as_str() {
            "inc" | "dec" => {
                if args.len() != 1 {
                    ctx.new_error(fun.span, AnalyserError::InvalidOperatorArguments);
                }
                if return_ty.is_none() {
                    ctx.new_error(fun.span, AnalyserError::InvalidOperatorReturn);
                }
            }
            "plus" | "minus" | "times" | "div" | "rem" | "mod" | "rangeTo" | "contains" | "compareTo" => {
                if args.len() != 2 {
                    dbg!(args.len());
                    ctx.new_error(fun.span, AnalyserError::InvalidOperatorArguments);
                }
                if return_ty.is_none() {
                    ctx.new_error(fun.span, AnalyserError::InvalidOperatorReturn);
                }
            }
            "get" => {
                if args.is_empty() {
                    ctx.new_error(fun.span, AnalyserError::InvalidOperatorArguments);
                }
                if return_ty.is_none() {
                    ctx.new_error(fun.span, AnalyserError::InvalidOperatorReturn);
                }
            }
            "set" => {
                if args.len() < 2 {
                    ctx.new_error(fun.span, AnalyserError::InvalidOperatorArguments);
                }
            }
            "invoke" => {
                // Anything is valid
            }
            "plusAssign" | "minusAssign" | "timesAssign" | "divAssign" | "remAssign" | "modAssign" => {
                if args.len() != 2 {
                    ctx.new_error(fun.span, AnalyserError::InvalidOperatorArguments);
                }
                if return_ty.is_none() {
                    ctx.new_error(fun.span, AnalyserError::InvalidOperatorReturn);
                } else if let Some(ret) = &return_ty {
                    if ret.name != "Unit" {
                        ctx.new_error(fun.span, AnalyserError::InvalidOperatorReturn);
                    }
                }
            }
            "provideDelegate" | "getValue" | "setValue" => {
                // TODO this requires more type checking
            }
            _ => {
                operator = false;
                ctx.new_error(fun.span, AnalyserError::InvalidOperatorFunctionName);
            }
        }
    }

    AstFunction {
        span: fun.span,
        extension: fun.receiver.is_some(),
        operator,
        name: fun.name.to_owned(),
        member,
        args,
        return_ty,
        body,
    }
}

fn function_body_to_ast(ctx: &mut Context, body: &Option<FunctionBody>) -> Option<AstBlock> {
    ctx.modifier_ctx.push(ModifierCtx::Statement);
    let res = body.as_ref().map(|body| {
        match body {
            FunctionBody::Block(e) => block_to_ast(ctx, e),
            FunctionBody::Expression(e) => expr_to_block(expr_to_ast(ctx, e)),
        }
    });
    ctx.modifier_ctx.pop().unwrap();
    res
}

fn expr_to_block(e: AstExpr) -> AstBlock {
    AstBlock {
        span: get_span(&e),
        statements: vec![
            AstStatement::Expr(AstExpr::Return {
                span: get_span(&e),
                value: Some(mut_rc(e)),
            })
        ],
    }
}

fn property_to_local_ast(ctx: &mut Context, prop: &Property) -> Vec<AstLocalProperty> {
    ctx.check_modifiers(prop.span, &prop.modifiers, ModifierTarget::Property);
    ctx.check_type_parameters(prop.span, &prop.type_parameters, &prop.type_constraints);

    let (delegated, expr) = match &prop.initialization {
        PropertyInitialization::None => (false, None),
        PropertyInitialization::Expr(expr) => (false, Some(expr_to_ast(ctx, expr))),
        PropertyInitialization::Delegation(expr) => (true, Some(expr_to_ast(ctx, expr))),
    };

    let mut props = vec![];
    let mut first_expr = expr;

    // If there are more than 1 variable, this is a destructuration, so the property is
    // split into several and a new one is created to avoid multiples evaluations
    // of the same expression
    if prop.declarations.len() != 1 {
        if let Some(first) = &first_expr {

            let name = format!("_destruct_{}", generate_rand_str());

            props.push(AstLocalProperty {
                var: AstVar {
                    name: name.to_string(),
                    ty: None,
                    mutable: false,
                },
                delegated: false,
                expr: Some(first.clone()),
            });

            first_expr = Some(AstExpr::Ref {
                span: get_span(first),
                obj: None,
                name,
            });
        }
    }

    for (i, decl) in prop.declarations.iter().enumerate() {
        let var = var_to_ast(ctx, prop.mutable, decl);

        let expr = match &first_expr {
            Some(first) => {

                if prop.declarations.len() == 1 {
                    // if there is only 1 var, use the initialization expression
                    Some(first.clone())
                } else {
                    // if there are more than 1 vars, call componentN() over the initial expression
                    Some(AstExpr::Call {
                        span: get_span(first),
                        receiver: Some(mut_rc(first.clone())),
                        function: format!("component{}", i + 1),
                        type_parameters: vec![],
                        args: vec![],
                    })
                }
            }
            None => None,
        };

        props.push(AstLocalProperty {
            var,
            delegated,
            expr,
        })
    }

    props
}

fn expr_to_ast(ctx: &mut Context, expr: &ExprVal) -> AstExpr {
    let span = expr.0;
    match &expr.1 {
        Expr::Null => {
            AstExpr::Constant {
                span,
                value: Constant::Null,
            }
        }
        Expr::Boolean(value) => {
            AstExpr::Constant {
                span,
                value: Constant::Boolean(*value),
            }
        }
        Expr::Char(value) => {
            AstExpr::Constant {
                span,
                value: Constant::Char(*value),
            }
        }
        Expr::Number(num) => {
            let value = match num {
                Number::Double(it) => Constant::Double(*it),
                Number::Float(it) => Constant::Float(*it),
                Number::Byte(it) => Constant::Byte(*it),
                Number::Short(it) => Constant::Short(*it),
                Number::Int(it) => Constant::Int(*it),
                Number::Long(it) => Constant::Long(*it),
            };
            AstExpr::Constant { span, value }
        }
        Expr::This => {
            AstExpr::Ref {
                span,
                obj: None,
                name: "this".to_owned(),
            }
        }
        Expr::Super => {
            AstExpr::Ref {
                span,
                obj: None,
                name: "super".to_owned(),
            }
        }
        Expr::Ref(name) => {
            AstExpr::Ref {
                span,
                obj: None,
                name: name.to_owned(),
            }
        }
        Expr::Chain { operands, operators } => {
            expr_chain_to_tree(ctx, operands, operators)
        }
        Expr::InfixFun { parameters, functions } => {
            expr_chain_to_tree(ctx, parameters, functions)
        }
        Expr::Prefix { prefix, expr } => {
            convert_prefix_to_expr(ctx, span, prefix, expr)
        }
        Expr::Postfix { expr, postfix } => {
            convert_postfix_to_expr(ctx, span, expr, postfix)
        }
        Expr::Is { expr, ty } => {
            AstExpr::Is {
                span,
                expr: mut_rc(expr_to_ast(ctx, expr)),
                ty: type_to_ast(ctx, ty),
            }
        }
        Expr::If { cond, if_true, if_false } => {
            AstExpr::If {
                span,
                cond: mut_rc(expr_to_ast(ctx, cond)),
                if_true: block_to_ast(ctx, if_true),
                if_false: if_false.as_ref().map(|it| block_to_ast(ctx, &it)),
            }
        }
        Expr::String(parts) => {
            convert_string_template_to_expr(ctx, span, parts)
        }
        Expr::When { expr, entries } => {
            convert_when_to_ifs(ctx, span, expr, entries)
        }
        Expr::Try { block, catch_blocks, finally } => {
            let catch = catch_blocks.iter()
                .map(|b| (
                    AstVar {
                        name: b.name.to_owned(),
                        ty: Some(simple_user_type_to_ast(ctx, span, &b.ty)),
                        mutable: false,
                    },
                    block_to_ast(ctx, &b.block)
                ))
                .collect();

            AstExpr::Try {
                span,
                body: block_to_ast(ctx, block),
                catch,
                finally: finally.as_ref().map(|it| block_to_ast(ctx, it)),
            }
        }
        Expr::Throw(expr) => {
            AstExpr::Throw {
                span,
                exception: mut_rc(expr_to_ast(ctx, expr)),
            }
        }
        Expr::Return(opt) => {
            AstExpr::Return {
                span,
                value: opt.as_ref().map(|expr| mut_rc(expr_to_ast(ctx, &expr))),
            }
        }
        Expr::Continue => {
            AstExpr::Continue {
                span
            }
        }
        Expr::Break => {
            AstExpr::Break { span }
        }
        // TODO block creation
        Expr::Object { .. } => { unimplemented!() }
        Expr::CallableRef { .. } => { unimplemented!() }
        Expr::Lambda(_lit) => {
            unimplemented!()
        }
    }
}

fn convert_postfix_to_expr(ctx: &mut Context, span: ByteSpan, expr: &ExprRef, postfix: &Vec<ExprPostfix>) -> AstExpr {
    let mut ast = expr_to_ast(ctx, expr);
    for post in postfix {
        match post {
            ExprPostfix::Increment | ExprPostfix::Decrement => {
                let variable = match &ast {
                    AstExpr::Ref { name, .. } => name.as_str(),
                    _ => {
                        ctx.new_error(span, AnalyserError::IncDecToNonVariable);
                        "_error_"
                    }
                };

                let new_name = match post {
                    ExprPostfix::Increment => "inc",
                    ExprPostfix::Decrement => "dec",
                    _ => unreachable!()
                };

                // TODO implement this later when the Ast is stabilized and can be safely expanded
            }
            ExprPostfix::AssertNonNull => {
                ast = AstExpr::Call {
                    span,
                    receiver: None,
                    function: "Intrinsics.assertNonNull".to_owned(),
                    type_parameters: vec![],
                    args: vec![ast],
                };
            }
            ExprPostfix::ArrayAccess(index) => {
                let args: Vec<AstExpr> = index.iter()
                    .map(|it| expr_to_ast(ctx, it))
                    .collect();

                ast = AstExpr::Call {
                    span,
                    receiver: Some(mut_rc(ast)),
                    function: "get".to_owned(),
                    type_parameters: vec![],
                    args,
                };
            }
            ExprPostfix::FunCall(suffix) => {
                let mut args = vec![];
                let mut type_parameters = vec![];

                // TODO named arguments
                for x in &suffix.value_arguments {
                    args.push(expr_to_ast(ctx, &x.expr));
                }
                for x in &suffix.type_arguments {
                    // TODO projection not allowed
                    type_parameters.push(type_parameter_to_ast(ctx, x));
                }

                if let AstExpr::Ref { name, .. } = &ast {
                    ast = AstExpr::Call {
                        span: ByteSpan::from(span.start(), suffix.span.end()),
                        receiver: None,
                        function: name.to_string(),
                        type_parameters,
                        args,
                    };
                } else {
                    ast = AstExpr::Call {
                        span: ByteSpan::from(span.start(), suffix.span.end()),
                        receiver: Some(mut_rc(ast)),
                        function: "invoke".to_string(),
                        type_parameters,
                        args,
                    };
                }
            }
            ExprPostfix::MemberAccess { member, .. } => {
                // TODO operator: . ?.
                ast = AstExpr::Ref {
                    span,
                    obj: Some(mut_rc(ast)),
                    name: member.to_string(),
                };
            }
        }
    }
    ast
}

fn convert_prefix_to_expr(ctx: &mut Context, span: ByteSpan, prefix: &Vec<String>, expr: &ExprRef) -> AstExpr {
    if prefix.is_empty() {
        return expr_to_ast(ctx, expr);
    }

    assert_eq!(prefix.len(), 1, "Only one prefix at the time is supported");
    let name = prefix.first().unwrap();

    match name.as_str() {
        "++" | "--" => {
            let variable = match &expr.1 {
                Expr::Ref(var) => var.as_str(),
                _ => {
                    ctx.new_error(span, AnalyserError::IncDecToNonVariable);
                    "_error_"
                }
            };

            let new_name = match name.as_str() {
                "++" => "inc",
                "--" => "dec",
                _ => unreachable!()
            };

            // TODO
            AstExpr::Continue { span: Default::default() }
//            AstExpr::WriteRef {
//                span,
//                name: variable.to_owned(),
//                expr: mut_rc(AstExpr::Call {
//                    span,
//                    receiver: Some(mut_rc(AstExpr::Ref { span, name: variable.to_owned() })),
//                    function: new_name.to_owned(),
//                    type_parameters: vec![],
//                    args: vec![],
//                }),
//            }
        }
        _ => {
            let new_name = match name.as_str() {
                "-" => "unaryMinus",
                "+" => "unaryPlus",
                "!" => "not",
                _ => unreachable!("Unexpected prefix: {}", name)
            };

            let e = expr_to_ast(ctx, expr);
            AstExpr::Call {
                span,
                receiver: Some(mut_rc(e)),
                function: new_name.to_owned(),
                type_parameters: vec![],
                args: vec![],
            }
        }
    }
}

fn convert_string_template_to_expr(ctx: &mut Context, span: ByteSpan, parts: &Vec<StringComponent>) -> AstExpr {
    let mut str_expr = vec![];

    // Extract sub expressions
    for p in parts {
        match p {
            StringComponent::Content(string) => {
                str_expr.push(AstExpr::Constant {
                    span,
                    value: Constant::String(string.to_owned()),
                });
            }
            StringComponent::Variable(name) => {
                str_expr.push(AstExpr::Call {
                    span,
                    receiver: Some(mut_rc(AstExpr::Ref { span, obj: None, name: name.to_owned() })),
                    function: "toString".to_owned(),
                    type_parameters: vec![],
                    args: vec![],
                });
            }
            StringComponent::Template(expr) => {
                str_expr.push(AstExpr::Call {
                    span,
                    receiver: Some(mut_rc(expr_to_ast(ctx, expr))),
                    function: "toString".to_owned(),
                    type_parameters: vec![],
                    args: vec![],
                });
            }
        }
    }

    // Join into tree
    let mut iter = str_expr.into_iter();
    let mut ast = iter.next().unwrap();

    for p in iter {
        ast = AstExpr::Call {
            span,
            receiver: Some(mut_rc(ast)),
            function: "plus".to_string(),
            type_parameters: vec![],
            args: vec![p],
        }
    }

    ast
}

fn convert_when_to_ifs(ctx: &mut Context, span: ByteSpan, expr: &Option<ExprRef>, entries: &Vec<WhenEntry>) -> AstExpr {
    let mut pairs: Vec<(AstExpr, AstBlock)> = vec![];
    let mut else_value: Option<AstBlock> = None;

    if entries.is_empty() {
        ctx.new_error(span, AnalyserError::WhenWithoutEntries);
        return AstExpr::Constant { span, value: Constant::Null };
    }


    if let Some(expr) = expr {
        // When with condition
        let expr = expr_to_ast(ctx, expr);

        // Extract pairs and else
        for (pos, entry) in entries.iter().enumerate() {
            // TODO remove this
            assert_eq!(entry.conditions.len(), 1, "This should be removed");
            let cond = entry.conditions.first().unwrap();

            // validation multiple else
            // validation else at the end

            match cond {
                WhenCondition::Else => {
                    if pos != entries.len() - 1 {
                        ctx.new_error(span, AnalyserError::WhenElseMustBeLast);
                    }
                    else_value = Some(block_to_ast(ctx, &entry.body))
                }
                WhenCondition::Expr(cond) => {
                    let cmp = AstExpr::Call {
                        span,
                        receiver: Some(mut_rc(expr.clone())),
                        function: "equals".to_owned(),
                        type_parameters: vec![],
                        args: vec![expr_to_ast(ctx, cond)],
                    };
                    pairs.push((cmp, block_to_ast(ctx, &entry.body)));
                }
                WhenCondition::In { negated, expr: range } => {
                    let mut cmp = AstExpr::Call {
                        span,
                        receiver: Some(mut_rc(expr.clone())),
                        function: "contains".to_owned(),
                        type_parameters: vec![],
                        args: vec![expr_to_ast(ctx, range)],
                    };

                    if *negated {
                        cmp = AstExpr::Call {
                            span,
                            receiver: Some(mut_rc(cmp)),
                            function: "not".to_owned(),
                            type_parameters: vec![],
                            args: vec![],
                        }
                    }
                    pairs.push((cmp, block_to_ast(ctx, &entry.body)));
                }
                WhenCondition::Is { negated, ty } => {
                    let mut cmp = AstExpr::Is {
                        span,
                        expr: mut_rc(expr.clone()),
                        ty: type_to_ast(ctx, ty),
                    };

                    if *negated {
                        cmp = AstExpr::Call {
                            span,
                            receiver: None,
                            function: "not".to_owned(),
                            type_parameters: vec![],
                            args: vec![cmp],
                        }
                    }
                    pairs.push((cmp, block_to_ast(ctx, &entry.body)));
                }
            }
        }
    } else {
        // When without condition

        // Extract pairs and else
        for (pos, entry) in entries.iter().enumerate() {
            if entry.conditions.len() > 1 {
                ctx.new_error(span, AnalyserError::WhenWithoutArgumentMultipleConditions);
            }
            let cond = entry.conditions.first().unwrap();

            // validation multiple else
            // validation else at the end

            match cond {
                WhenCondition::Else => {
                    if pos != entries.len() - 1 {
                        ctx.new_error(span, AnalyserError::WhenElseMustBeLast);
                    }
                    else_value = Some(block_to_ast(ctx, &entry.body))
                }
                WhenCondition::Expr(cond) => {
                    pairs.push((expr_to_ast(ctx, cond), block_to_ast(ctx, &entry.body)));
                }
                WhenCondition::In { .. } => {
                    ctx.new_error(span, AnalyserError::InvalidWhenCondition);
                }
                WhenCondition::Is { .. } => {
                    ctx.new_error(span, AnalyserError::InvalidWhenCondition);
                }
            }
        }
    }

    let mut iter = pairs.into_iter().rev();
    let (last_cond, last_block) = iter.next().unwrap();

    let mut ast = AstExpr::If {
        span,
        cond: mut_rc(last_cond),
        if_true: last_block,
        if_false: else_value,
    };

    for (cond, block) in iter {
        ast = AstExpr::If {
            span,
            cond: mut_rc(cond),
            if_true: block,
            if_false: Some(expr_to_block(ast)),
        };
    }
    ast
}

fn type_to_ast(ctx: &mut Context, ty: &Type) -> AstType {
    type_reference_to_ast(ctx, ty.span, ty.reference.as_ref())
}

fn type_reference_to_ast(ctx: &mut Context, span: ByteSpan, ty: &TypeReference) -> AstType {
    match ty {
        TypeReference::Function(func) => type_func_to_ast(ctx, span, func),
        TypeReference::UserType(user) => simple_user_type_to_ast(ctx, span, user),
        TypeReference::Nullable(reference) => {
            let mut ast = match reference.as_ref() {
                TypeReference::Function(func) => type_func_to_ast(ctx, span, func),
                TypeReference::UserType(user) => simple_user_type_to_ast(ctx, span, user),
                TypeReference::Nullable(_) => {
                    ctx.new_error(span, AnalyserError::DoubleNullableType);
                    AstType {
                        span,
                        name: "_error_".to_string(),
                        full_name: "_error_".to_string(),
                        type_parameters: vec![],
                        nullable: false,
                    }
                }
            };
            ast.nullable = true;
            ast
        }
    }
}

fn type_func_to_ast(ctx: &mut Context, span: ByteSpan, ty: &FunctionType) -> AstType {
    let mut type_parameters = vec![];

    if let Some(rec) = &ty.receiver {
        type_parameters.push(AstTypeParameter::Type(type_reference_to_ast(ctx, span, rec)));
    }

    for param in &ty.parameters {
        type_parameters.push(AstTypeParameter::Type(type_to_ast(ctx, param)));
    }

    type_parameters.push(AstTypeParameter::Type(type_reference_to_ast(ctx, span, &ty.return_type)));

    AstType {
        span,
        name: "Function".to_string(),
        full_name: "kotlin.Function".to_string(),
        type_parameters,
        nullable: false,
    }
}

fn simple_user_type_to_ast(ctx: &mut Context, span: ByteSpan, ty: &Vec<SimpleUserType>) -> AstType {
    let last = ty.last().unwrap();
    let mut full_name = String::new();
    let mut type_parameters = vec![];

    for (i, x) in ty.iter().enumerate() {
        full_name.push_str(&x.name);
        if ty.len() != i + 1 {
            full_name.push('.');
        }
    }

    for x in &last.type_params {
        type_parameters.push(type_parameter_to_ast(ctx, x));
    }

    AstType {
        span,
        name: last.name.to_owned(),
        full_name,
        type_parameters,
        nullable: false,
    }
}

fn type_parameter_to_ast(ctx: &mut Context, ty: &CallSiteTypeParams) -> AstTypeParameter {
    match ty {
        CallSiteTypeParams::Projection => AstTypeParameter::Projection,
        CallSiteTypeParams::Type(ty) => AstTypeParameter::Type(type_to_ast(ctx, ty)),
    }
}

fn var_to_ast(ctx: &mut Context, mutable: bool, var: &VariableDeclarationEntry) -> AstVar {
    AstVar {
        name: var.name.to_owned(),
        ty: var.declared_type.as_ref().map(|it| type_to_ast(ctx, it)),
        mutable,
    }
}

fn block_to_ast(ctx: &mut Context, block: &Block) -> AstBlock {
    let mut statements = vec![];

    for statement in &block.1 {
        for stm in statement_to_ast(ctx, statement) {
            statements.push(stm);
        }
    }

    AstBlock {
        span: block.0,
        statements,
    }
}

fn expr_chain_to_tree(ctx: &mut Context, operands: &Vec<ExprVal>, operators: &Vec<String>) -> AstExpr {
    debug_assert_eq!(operands.len(), operators.len() + 1);

    if operators.is_empty() {
        expr_to_ast(ctx, operands.first().unwrap())
    } else {
        let mut iter = operands.iter();
        let mut op_iter = operators.iter();
        let mut tree = expr_to_ast(ctx, iter.next().unwrap());

        while let Some(expr) = iter.next() {
            let op = op_iter.next().unwrap();
            let next = expr_to_ast(ctx, expr);

            tree = AstExpr::Call {
                span: ByteSpan::from(get_span(&tree).start(), (expr.0).end()),
                receiver: Some(mut_rc(tree)),
                function: op.to_owned(),
                type_parameters: vec![],
                args: vec![next],
            }
        }

        tree
    }
}

fn get_span(expr: &AstExpr) -> ByteSpan {
    match expr {
        AstExpr::Lambda { span, .. } => *span,
        AstExpr::AnonymousFunction { span, .. } => *span,
        AstExpr::ObjectLiteral { span, .. } => *span,
        AstExpr::Constant { span, .. } => *span,
        AstExpr::Ref { span, .. } => *span,
        AstExpr::Call { span, .. } => *span,
        AstExpr::Is { span, .. } => *span,
        AstExpr::If { span, .. } => *span,
        AstExpr::Continue { span, .. } => *span,
        AstExpr::Break { span, .. } => *span,
        AstExpr::Throw { span, .. } => *span,
        AstExpr::Return { span, .. } => *span,
        AstExpr::Try { span, .. } => *span,
    }
}

