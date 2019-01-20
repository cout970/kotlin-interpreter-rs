use std::collections::HashSet;

use crate::analyzer::semantic_rules::Checker;
use crate::errors::AnalyserError;
use crate::parser::ast::Modifier;
use crate::parser::ast::ModifierCtx;

pub fn check_modifiers(ctx: &mut Checker, mods: &Vec<Modifier>, modifier_ctx: ModifierCtx) {
    report_duplicated_modifiers(ctx, mods);
}

fn report_duplicated_modifiers(ctx: &mut Checker, mods: &Vec<Modifier>) {
    let mut names: HashSet<Modifier> = HashSet::new();
    for x in mods {
        if names.contains(x) {
            ctx.errors.push(((0, 0), AnalyserError::DuplicatedModifier {
                modifier: *x,
            }));
        }
        names.insert(*x);
    }
}