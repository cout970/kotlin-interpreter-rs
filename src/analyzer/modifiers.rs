use std::collections::HashMap;
use std::collections::HashSet;

use crate::errors::AnalyserError;
use crate::parser::parse_tree::Modifier;
use crate::parser::parse_tree::ModifierCtx;
use crate::parser::parse_tree::ModifierTarget;

type Result = Vec<AnalyserError>;

pub fn check_modifiers(mods: &Vec<Modifier>, context: ModifierCtx, target: ModifierTarget) -> Vec<AnalyserError> {
    let mut errors = vec![];
    report_duplicated_modifiers(&mut errors, mods);
    report_mutually_exclusive_modifiers(&mut errors, mods);

    for m in mods {
        if !is_valid_modifier(*m, context, target) {
            errors.push(AnalyserError::InvalidModifier {
                modifier: *m,
                context,
            });
        }
    }

    errors
}

fn is_valid_modifier(modifier: Modifier, context: ModifierCtx, target: ModifierTarget) -> bool {
    match context {
        ModifierCtx::TopLevelObject => {
            match target {
                ModifierTarget::Class => is_visibility_modifier(modifier) || is_class_modifier(modifier) || is_multiplatform_modifier(modifier),
                ModifierTarget::Object => is_visibility_modifier(modifier) || is_multiplatform_modifier(modifier),
                ModifierTarget::Function => is_visibility_modifier(modifier) || is_function_modifier(modifier) || is_multiplatform_modifier(modifier),
                ModifierTarget::Property => is_visibility_modifier(modifier) || is_property_modifier(modifier) || is_multiplatform_modifier(modifier),
                ModifierTarget::Other => is_visibility_modifier(modifier)
            }
        }
        ModifierCtx::TypeParameter => {
            modifier == Modifier::In || modifier == Modifier::Out || modifier == Modifier::Reified
        }
        ModifierCtx::Statement => {
            match target {
                ModifierTarget::Class => is_visibility_modifier(modifier) || is_class_modifier(modifier),
                ModifierTarget::Object => is_visibility_modifier(modifier),
                ModifierTarget::Function => is_visibility_modifier(modifier) || is_function_modifier(modifier),
                ModifierTarget::Property => is_visibility_modifier(modifier) || is_property_modifier(modifier),
                ModifierTarget::Other => is_visibility_modifier(modifier)
            }
        }
        ModifierCtx::Package => false,
        ModifierCtx::Constructor => is_visibility_modifier(modifier),
        ModifierCtx::GetterSetter => is_visibility_modifier(modifier) || modifier == Modifier::Inline,
        ModifierCtx::ClassMember => {
            match target {
                ModifierTarget::Class => is_visibility_modifier(modifier) || is_member_modifier(modifier) || is_class_modifier(modifier) || is_multiplatform_modifier(modifier),
                ModifierTarget::Object => is_visibility_modifier(modifier) || is_member_modifier(modifier) || is_multiplatform_modifier(modifier),
                ModifierTarget::Function => is_visibility_modifier(modifier) || is_member_modifier(modifier) || is_function_modifier(modifier) || is_multiplatform_modifier(modifier),
                ModifierTarget::Property => is_visibility_modifier(modifier) || is_member_modifier(modifier) || is_property_modifier(modifier) || is_multiplatform_modifier(modifier),
                ModifierTarget::Other => is_visibility_modifier(modifier) || is_member_modifier(modifier)
            }
        }
        ModifierCtx::EnumEntry => modifier == Modifier::Inline,
        ModifierCtx::FunctionParameter => {
            modifier == Modifier::Noinline || modifier == Modifier::Crossinline ||
                modifier == Modifier::Vararg
        }
        ModifierCtx::ConstructorParameter => {
            modifier == Modifier::Noinline || modifier == Modifier::Crossinline ||
                modifier == Modifier::Vararg || modifier == Modifier::Override
        }
    }
}

fn is_member_modifier(modifier: Modifier) -> bool {
    modifier == Modifier::Override || modifier == Modifier::Open ||
        modifier == Modifier::Final || modifier == Modifier::Abstract
}

fn is_visibility_modifier(modifier: Modifier) -> bool {
    modifier == Modifier::Public || modifier == Modifier::Private ||
        modifier == Modifier::Protected || modifier == Modifier::Internal
}

fn is_property_modifier(modifier: Modifier) -> bool {
    modifier == Modifier::Lateinit || modifier == Modifier::Inline ||
        modifier == Modifier::Const
}

fn is_function_modifier(modifier: Modifier) -> bool {
    modifier == Modifier::Tailrec || modifier == Modifier::Operator ||
        modifier == Modifier::Infix || modifier == Modifier::Inline ||
        modifier == Modifier::External || modifier == Modifier::Suspend
}

fn is_multiplatform_modifier(modifier: Modifier) -> bool {
    modifier == Modifier::Expect || modifier == Modifier::Actual
}

fn is_class_modifier(modifier: Modifier) -> bool {
    modifier == Modifier::Final || modifier == Modifier::Abstract ||
        modifier == Modifier::Enum || modifier == Modifier::Open ||
        modifier == Modifier::Annotation || modifier == Modifier::Sealed ||
        modifier == Modifier::Data || modifier == Modifier::Inline ||
        modifier == Modifier::External
}

fn report_mutually_exclusive_modifiers(ctx: &mut Result, mods: &Vec<Modifier>) {
    let mut forbidden: HashMap<Modifier, Modifier> = HashMap::new();
    // key = forbidden modifier, value = cause

    for m in mods {
        if let Some(other) = forbidden.get(m) {
            ctx.push(AnalyserError::MutuallyExclusiveModifier {
                modifier_1: *m,
                modifier_2: *other,
            });
        }

        match m {
            Modifier::Enum => {
                forbidden.insert(Modifier::Annotation, *m);
                forbidden.insert(Modifier::Data, *m);
                forbidden.insert(Modifier::Sealed, *m);
                forbidden.insert(Modifier::Inner, *m);
            }
            Modifier::Annotation => {
                forbidden.insert(Modifier::Enum, *m);
                forbidden.insert(Modifier::Data, *m);
                forbidden.insert(Modifier::Sealed, *m);
                forbidden.insert(Modifier::Inner, *m);
            }
            Modifier::Abstract => {
                forbidden.insert(Modifier::Final, *m);
                forbidden.insert(Modifier::Open, *m);
            }
            Modifier::Final => {
                forbidden.insert(Modifier::Abstract, *m);
                forbidden.insert(Modifier::Open, *m);
                forbidden.insert(Modifier::Sealed, *m);
            }
            Modifier::Open => {
                forbidden.insert(Modifier::Abstract, *m);
                forbidden.insert(Modifier::Open, *m);
                forbidden.insert(Modifier::Sealed, *m);
            }
            Modifier::Sealed => {
                forbidden.insert(Modifier::Enum, *m);
                forbidden.insert(Modifier::Data, *m);
                forbidden.insert(Modifier::Annotation, *m);
                forbidden.insert(Modifier::Inner, *m);
            }
            Modifier::Data => {
                forbidden.insert(Modifier::Enum, *m);
                forbidden.insert(Modifier::Sealed, *m);
                forbidden.insert(Modifier::Annotation, *m);
                forbidden.insert(Modifier::Inner, *m);
            }
            Modifier::Inner => {
                forbidden.insert(Modifier::Data, *m);
                forbidden.insert(Modifier::Enum, *m);
                forbidden.insert(Modifier::Sealed, *m);
                forbidden.insert(Modifier::Annotation, *m);
            }
            Modifier::Private => {
                forbidden.insert(Modifier::Protected, *m);
                forbidden.insert(Modifier::Public, *m);
                forbidden.insert(Modifier::Internal, *m);
            }
            Modifier::Protected => {
                forbidden.insert(Modifier::Private, *m);
                forbidden.insert(Modifier::Public, *m);
                forbidden.insert(Modifier::Internal, *m);
            }
            Modifier::Public => {
                forbidden.insert(Modifier::Protected, *m);
                forbidden.insert(Modifier::Private, *m);
                forbidden.insert(Modifier::Internal, *m);
            }
            Modifier::Internal => {
                forbidden.insert(Modifier::Protected, *m);
                forbidden.insert(Modifier::Public, *m);
                forbidden.insert(Modifier::Private, *m);
            }
            Modifier::Inline => {
                forbidden.insert(Modifier::Noinline, *m);
                forbidden.insert(Modifier::Crossinline, *m);
            }
            Modifier::Noinline => {
                forbidden.insert(Modifier::Inline, *m);
                forbidden.insert(Modifier::Crossinline, *m);
            }
            Modifier::Crossinline => {
                forbidden.insert(Modifier::Inline, *m);
                forbidden.insert(Modifier::Noinline, *m);
            }
            Modifier::In => {
                forbidden.insert(Modifier::Out, *m);
            }
            Modifier::Out => {
                forbidden.insert(Modifier::In, *m);
            }
            Modifier::Expect => {
                forbidden.insert(Modifier::Actual, *m);
            }
            Modifier::Actual => {
                forbidden.insert(Modifier::Expect, *m);
            }
            Modifier::Lateinit => {}
            Modifier::Tailrec => {}
            Modifier::Operator => {}
            Modifier::Infix => {}
            Modifier::External => {}
            Modifier::Suspend => {}
            Modifier::Const => {}
            Modifier::Reified => {}
            Modifier::Vararg => {}
            Modifier::Override => {}
        }
    }
}

fn report_duplicated_modifiers(ctx: &mut Result, mods: &Vec<Modifier>) {
    let mut names: HashSet<Modifier> = HashSet::new();

    for x in mods {
        if names.contains(x) {
            ctx.push(AnalyserError::DuplicatedModifier {
                modifier: *x,
            });
        }
        names.insert(*x);
    }
}