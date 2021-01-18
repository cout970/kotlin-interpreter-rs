use std::fmt::{Display, Formatter};

use crate::parser::parse_tree::{CallSiteTypeParams, CallSuffix, CatchBlock, Class, ClassBody, DelegationSpecifier, DoWhileStatement, Expr, Expression, ExprSuffix, ForStatement, Function, FunctionBody, FunctionLiteral, FunctionParameter, Import, KotlinFile, Modifier, PackageHeader, PrimaryConstructor, PropertyInitialization, Statement, StatementBlock, StringComponent, TopLevelObject, Type, TypeConstraint, TypeParameter, TypeReference, ValueArgument, Variable, VariableName, VariablePattern, WhenCondition, WhenEntry, WhileStatement};

impl Display for KotlinFile {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(pkg) = &self.package_header {
            writeln!(f, "{}", pkg)?;
            writeln!(f)?;
        }
        for imp in &self.imports {
            writeln!(f, "{}", imp)?;
        }

        for tlo in &self.objects {
            writeln!(f)?;
            writeln!(f, "{}", tlo)?;
        }

        Ok(())
    }
}

impl Display for PackageHeader {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "package {}", self.path.join("."))?;

        Ok(())
    }
}

impl Display for Import {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "import {}", self.path.join("."))?;

        if let Some(alias) = &self.alias {
            write!(f, " as {}", alias)?;
        }

        Ok(())
    }
}

impl Display for TopLevelObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TopLevelObject::Class(e) => write!(f, "{}", e)?,
            // TopLevelObject::Object(e) => write!(f, "{}", e)?,
            TopLevelObject::Function(e) => write!(f, "{}", e)?,
            // TopLevelObject::Property(e) => write!(f, "{}", e)?,
            // TopLevelObject::TypeAlias(e) => write!(f, "{}", e)?,
            _ => {}
        }

        Ok(())
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for m in &self.modifiers {
            write!(f, "{} ", m)?;
        }

        write!(f, "class {}", self.name)?;

        if !self.type_parameters.is_empty() {
            write!(f, "<")?;
            join_comma(f, &self.type_parameters)?;
            write!(f, ">")?;
        }

        if let Some(ctor) = &self.primary_constructor {
            write!(f, "{}", ctor)?;
        }

        if !self.delegation_specifiers.is_empty() {
            write!(f, ": ")?;
            join_comma(f, &self.delegation_specifiers)?;
        }

        if !self.type_constraints.is_empty() {
            write!(f, " where ")?;
            join_comma(f, &self.type_constraints)?;
        }

        if let Some(i) = &self.body {
            write!(f, " {}", i)?;
        }

        Ok(())
    }
}

impl Display for TypeParameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for m in &self.modifiers {
            write!(f, "{} ", m)?;
        }

        write!(f, "{}", self.name)?;

        if let Some(i) = &self.user_type {
            write!(f, ": {}", i)?;
        }

        Ok(())
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.reference)?;

        Ok(())
    }
}

impl Display for TypeReference {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeReference::Function(func) => {
                if func.nullable {
                    write!(f, "(")?;
                }

                if let Some(i) = &func.receiver {
                    write!(f, "{}.", i)?;
                }

                write!(f, "(")?;
                join_comma(f, &func.parameters)?;
                write!(f, ")->{}", func.return_type)?;

                if func.nullable {
                    write!(f, ")?")?;
                }
            }
            TypeReference::SimpleType(ut) => {
                if !ut.package.is_empty() {
                    write!(f, "{}.", ut.package.join("."))?;
                }
                write!(f, "{}", ut.name)?;

                if !ut.type_params.is_empty() {
                    write!(f, "<")?;
                    join_comma(f, &ut.type_params)?;
                    write!(f, ">")?;
                }

                if ut.nullable {
                    write!(f, "?")?;
                }
            }
        }

        Ok(())
    }
}

impl Display for CallSiteTypeParams {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            CallSiteTypeParams::Projection => write!(f, "*")?,
            CallSiteTypeParams::Type(ty) => write!(f, "{}", ty)?,
        }

        Ok(())
    }
}

impl Display for TypeConstraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.ty)?;

        Ok(())
    }
}

impl Display for PrimaryConstructor {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if !self.modifiers.is_empty() {
            write!(f, " ")?;

            for i in &self.modifiers {
                write!(f, "{} ", i)?;
            }
            write!(f, "constructor")?;
        }
        write!(f, "(")?;
        join_comma(f, &self.value_parameters)?;
        write!(f, ")")?;

        Ok(())
    }
}

impl Display for FunctionParameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for x in &self.modifiers {
            write!(f, "{} ", x)?;
        }

        write!(f, "{}: {}", self.name, self.parameter_type)?;

        if let Some(i) = &self.default_value {
            write!(f, " = {}", i)?;
        }

        Ok(())
    }
}

impl Display for DelegationSpecifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            DelegationSpecifier::Type(ty) => write!(f, "{}", ty)?,
            DelegationSpecifier::DelegatedBy(ty, expr) => write!(f, "{} by {}", ty, expr)?,
            DelegationSpecifier::FunctionCall(ty, suff) => write!(f, "{}{}", ty, suff)?,
        }

        Ok(())
    }
}

impl Display for CallSuffix {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        join_comma(f, &self.value_arguments)?;
        write!(f, ")")?;

        Ok(())
    }
}

impl Display for ValueArgument {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(y) = &self.named {
            write!(f, "{} = ", y)?;
        }
        if self.spread {
            write!(f, "*")?;
        }
        write!(f, "{}", self.expr)?;

        Ok(())
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            Expr::Null => write!(f, "null")?,
            Expr::Tree { left, operator, right } => {
                write!(f, "{} {} {}", left, operator, right)?
            }
            Expr::FunctionCall {
                function, type_arguments,
                value_arguments, lambda
            } => {
                write!(f, "{}", function)?;
                if !type_arguments.is_empty() {
                    write!(f, "<")?;
                    join_comma(f, type_arguments)?;
                    write!(f, ">")?;
                }
                write!(f, "(")?;
                join_comma(f, value_arguments)?;
                write!(f, ")")?;
                if let Some(l) = lambda {
                    write!(f, " {}", l)?;
                }
            }
            Expr::PropertyAccess { object, property, safe_call } => {
                let op = if *safe_call { "?." } else { "." };
                write!(f, "{}{}{}", object, op, property)?;
            }
            Expr::UnaryOperator { unary_operators, expr } => {
                write!(f, "{}{}", unary_operators.join(""), expr)?;
            }
            Expr::Suffix { expr, suffix } => {
                write!(f, "{}{}", expr, suffix)?;
            }
            Expr::Is { expr, is_type, negated } => {
                let op = if *negated { "!is" } else { "is" };
                write!(f, "{} {} {}", expr, op, is_type)?;
            }
            Expr::String(vec) => {
                write!(f, "\"")?;
                for x in vec {
                    write!(f, "{}", x)?;
                }
                write!(f, "\"")?;
            }
            Expr::If { cond, if_true, if_false } => {
                write!(f, "if ({}) ", cond)?;
                write!(f, "{}", if_true)?;

                if let Some(i) = if_false {
                    write!(f, " else {}", i)?;
                }
            }
            Expr::Try { block, catch_blocks, finally } => {
                write!(f, "try {} ", block)?;

                if !catch_blocks.is_empty() {
                    for x in catch_blocks {
                        write!(f, "{} ", x)?;
                    }
                }

                if let Some(i) = finally {
                    write!(f, "finally {}", i)?;
                }
            }
            Expr::When { expr, entries } => {
                write!(f, "when ")?;
                if let Some(e) = expr {
                    write!(f, "({}) ", e)?;
                }
                writeln!(f, "{{")?;
                for e in entries {
                    writeln!(f, "  {}", e)?;
                }
                writeln!(f, "}}")?;
            }
            Expr::Object { delegation_specifiers, body } => {
                write!(f, "object")?;

                if !delegation_specifiers.is_empty() {
                    write!(f, ": ")?;
                    join_comma(f, delegation_specifiers)?;
                }

                write!(f, " {}", body)?;
            }
            Expr::ExprCallableRef { receiver_expr, name } => {
                if let Some(expr) = receiver_expr {
                    write!(f, "{}", expr)?;
                }
                write!(f, "::{}", name)?;
            }
            Expr::Lambda(e) => write!(f, "{}", e)?,
            Expr::Ref(e) => write!(f, "{}", e)?,
            Expr::Boolean(e) => write!(f, "{}", e)?,
            Expr::Char(e) => write!(f, "'{}'", e)?,
            Expr::Number(e) => write!(f, "{}", e)?,
            Expr::This => write!(f, "this")?,
            Expr::Super => write!(f, "super")?,
            Expr::Throw(e) => write!(f, "throw {}", e)?,
            Expr::Return(e) => {
                if let Some(i) = e {
                    write!(f, "return {}", i)?
                } else {
                    write!(f, "return")?
                }
            }
            Expr::Continue => write!(f, "continue")?,
            Expr::Break => write!(f, "break")?,
        }

        Ok(())
    }
}

impl Display for ExprSuffix {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprSuffix::Increment => write!(f, "++")?,
            ExprSuffix::Decrement => write!(f, "--")?,
            ExprSuffix::AssertNonNull => write!(f, "!!")?,
            ExprSuffix::ArrayAccess(e) => {
                write!(f, "[")?;
                join_comma(f, e)?;
                write!(f, "]")?;
            }
            ExprSuffix::FunCall(s) => write!(f, "{}", s)?,
            ExprSuffix::MemberAccess { operator, member } => {
                write!(f, "{}{}", operator, member)?;
            }
        }
        Ok(())
    }
}

impl Display for StringComponent {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            StringComponent::Content(c) => write!(f, "{}", c)?,
            StringComponent::Variable(c) => write!(f, "${}", c)?,
            StringComponent::Template(c) => write!(f, "${{ {} }}", c)?,
        }

        Ok(())
    }
}

impl Display for WhenEntry {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        join_comma(f, &self.conditions)?;
        write!(f, " -> ")?;
        let str = match &self.body {
            FunctionBody::Block(e) => format!("{{{}}}", e),
            FunctionBody::Expression(e) => format!("{}", e),
        };

        write!(f, "{}", str.replace("\n", "\n  "))?;

        Ok(())
    }
}

impl Display for WhenCondition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            WhenCondition::Else => write!(f, "else")?,
            WhenCondition::Expr(e) => write!(f, "{}", e)?,
            WhenCondition::In { negated, expr } => {
                let op = if *negated { "!in" } else { "in" };
                write!(f, "{} {}", op, expr)?;
            }
            WhenCondition::Is { negated, is_type } => {
                let op = if *negated { "!is" } else { "is" };
                write!(f, "{} {}", op, is_type)?;
            }
        }

        Ok(())
    }
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        if !self.parameters.is_empty() {
            write!(f, " ")?;
            join_comma(f, &self.parameters)?;
            write!(f, " -> ")?;
        }
        write!(f, "{}", self.body)?;
        write!(f, "}}")?;
        Ok(())
    }
}

impl Display for VariableName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;

        if let Some(ty) = &self.variable_type {
            write!(f, ": {}", ty)?;
        }

        Ok(())
    }
}

impl Display for CatchBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "catch ({}: {}) {}", self.variable, self.exception_type, self.block)?;

        Ok(())
    }
}

impl Display for ClassBody {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{}}")?;

        Ok(())
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for x in &self.modifiers {
            write!(f, "{} ", x)?;
        }

        write!(f, "fun ")?;

        if !self.type_parameters.is_empty() {
            write!(f, "<")?;
            join_comma(f, &self.type_parameters)?;
            write!(f, "> ")?;
        }

        if let Some(i) = &self.receiver {
            if let TypeReference::Function(t) = &i.reference {
                write!(f, "({}).", i)?;
            } else {
                write!(f, "{}.", i)?;
            }
        }

        write!(f, "{}(", self.name)?;
        join_comma(f, &self.value_parameters)?;
        write!(f, ")")?;

        if let Some(i) = &self.return_type {
            write!(f, ": {}", i)?;
        }

        if !self.type_constraints.is_empty() {
            write!(f, " where ")?;
            join_comma(f, &self.type_constraints)?;
        }

        if let Some(i) = &self.body {
            write!(f, " {}", i)?;
        }

        Ok(())
    }
}

impl Display for FunctionBody {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionBody::Block(b) => write!(f, "{{{}}}", b)?,
            FunctionBody::Expression(e) => write!(f, "= {}", e)?,
        }

        Ok(())
    }
}

impl Display for StatementBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use std::fmt::Write;

        if !self.statements.is_empty() {
            let mut str = String::new();
            str.push('\n');
            for x in &self.statements {
                writeln!(str, "{}", x)?;
            }

            let code = str.replace("\n", "\n  ");
            write!(f, "{}", &code[0..code.len() - 2])?;
        }

        Ok(())
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Expression(e) => write!(f, "{}", e)?,
            Statement::Assignment { left, operator, right } => write!(f, "{} {} {}", left, operator, right)?,
            Statement::Class(e) => write!(f, "{}", e)?,
            Statement::Function(e) => write!(f, "{}", e)?,
            Statement::Variable(e) => write!(f, "{}", e)?,
            Statement::For(e) => write!(f, "{}", e)?,
            Statement::While(e) => write!(f, "{}", e)?,
            Statement::DoWhile(e) => write!(f, "{}", e)?,
        }

        Ok(())
    }
}

impl Display for ForStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "for ({} in {}) ", self.variable, self.iterable)?;
        write!(f, "{}", self.body)?;

        Ok(())
    }
}

impl Display for WhileStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "while ({}) ", self.condition)?;
        write!(f, "{}", self.body)?;

        Ok(())
    }
}

impl Display for DoWhileStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "do {} while ({})", self.body, self.condition)?;

        Ok(())
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.mutable {
            write!(f, "var ")?;
        } else {
            write!(f, "val ")?;
        }
        write!(f, "{}", self.variable)?;

        match &self.initialization {
            PropertyInitialization::None => {}
            PropertyInitialization::Expr(e) => write!(f, " = {}", e)?,
            PropertyInitialization::Delegation(e) => write!(f, " by {}", e)?,
        }

        Ok(())
    }
}

impl Display for VariablePattern {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            VariablePattern::Single(e) => write!(f, "{}", e)?,
            VariablePattern::Tuple(vec) => join_comma(f, vec)?,
        }

        Ok(())
    }
}

impl Display for Modifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Modifier::Abstract => write!(f, "abstract")?,
            Modifier::Final => write!(f, "final")?,
            Modifier::Enum => write!(f, "enum")?,
            Modifier::Open => write!(f, "open")?,
            Modifier::Annotation => write!(f, "annotation")?,
            Modifier::Sealed => write!(f, "sealed")?,
            Modifier::Data => write!(f, "data")?,
            Modifier::Lateinit => write!(f, "lateinit")?,
            Modifier::Private => write!(f, "private")?,
            Modifier::Protected => write!(f, "protected")?,
            Modifier::Public => write!(f, "public")?,
            Modifier::Internal => write!(f, "internal")?,
            Modifier::Tailrec => write!(f, "tailrec")?,
            Modifier::Operator => write!(f, "operator")?,
            Modifier::Infix => write!(f, "infix")?,
            Modifier::Inline => write!(f, "inline")?,
            Modifier::External => write!(f, "external")?,
            Modifier::Suspend => write!(f, "suspend")?,
            Modifier::Const => write!(f, "const")?,
            Modifier::Expect => write!(f, "expect")?,
            Modifier::Actual => write!(f, "actual")?,
            Modifier::In => write!(f, "in")?,
            Modifier::Out => write!(f, "out")?,
            Modifier::Reified => write!(f, "reified")?,
            Modifier::Inner => write!(f, "inner")?,
            Modifier::Noinline => write!(f, "noinline")?,
            Modifier::Crossinline => write!(f, "crossinline")?,
            Modifier::Vararg => write!(f, "vararg")?,
            Modifier::Override => write!(f, "override")?,
        }

        Ok(())
    }
}

fn join_comma<T: Display>(f: &mut Formatter, vec: &Vec<T>) -> std::fmt::Result {
    let mut i = vec.len() as i32 - 1;

    for value in vec {
        write!(f, "{}", value)?;
        if i != 0 {
            write!(f, ", ")?;
        }
        i -= 1;
    }

    Ok(())
}