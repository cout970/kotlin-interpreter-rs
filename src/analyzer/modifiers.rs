use std::collections::HashMap;
use std::collections::HashSet;

use crate::errors::AnalyserError;
use crate::parser::parse_tree::Modifier;
use crate::parser::parse_tree::ModifierCtx;

type Result = Vec<AnalyserError>;

pub fn check_modifiers(mods: &Vec<Modifier>, _modifier_ctx: ModifierCtx) -> Vec<AnalyserError> {
    let mut ctx = vec![];
    report_duplicated_modifiers(&mut ctx, mods);
    report_mutually_exclusive_modifiers(&mut ctx, mods);
    ctx
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