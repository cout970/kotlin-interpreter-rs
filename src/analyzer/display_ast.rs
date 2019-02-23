use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Error;
use std::fmt::Formatter;

use crate::analyzer::ast::*;

impl Debug for AstFile {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let string = format!("{}", self);
        let mut ret = String::new();
        let mut indent = 0;

        for c in string.chars() {
            ret.push(c);
            if c == '{' {
                indent += 1;
            } else if c == '}' {
                indent -= 1;
            } else if c == '\n' {
                for _ in 0..indent {
                    ret.push(' ');
                    ret.push(' ');
                }
            }
        }

        write!(f, "{}", ret)?;
        Ok(())
    }
}

impl Debug for AstClass {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> { write!(f, "{}", self) }
}

impl Debug for AstTypealias {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> { write!(f, "{}", self) }
}

impl Debug for AstFunction {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> { write!(f, "{}", self) }
}

impl Debug for AstMember {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> { write!(f, "{}", self) }
}

impl Debug for AstType {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> { write!(f, "{}", self) }
}

impl Debug for AstExpr {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> { write!(f, "{}", self) }
}

impl Debug for AstProperty {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> { write!(f, "{}", self) }
}

impl Debug for AstLocalProperty {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> { write!(f, "{}", self) }
}

impl Debug for AstVar {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> { write!(f, "{}", self) }
}

impl Debug for AstStatement {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> { write!(f, "{}", self) }
}

impl Debug for AstBlock {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> { write!(f, "{}", self) }
}

impl Debug for AstLambda {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> { write!(f, "{}", self) }
}

impl Display for AstFile {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        if !self.package.is_empty() {
            write!(f, "package {}\n", self.package)?;
        }
        for x in &self.imports {
            write!(f, "import {}", x.name)?;
            if let Some(it) = &x.alias {
                write!(f, " as {}", it)?;
            }
            write!(f, "\n")?;
        }

        for ast in &self.typealias {
            write!(f, "{:?}", ast)?;
        }
        for ast in &self.classes {
            write!(f, "{:?}", ast)?;
        }
        for ast in &self.properties {
            write!(f, "{:?}", ast)?;
        }
        for ast in &self.functions {
            write!(f, "{:?}", ast)?;
        }

        Ok(())
    }
}

impl Display for AstTypealias {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "typealias {} = {:?}", self.name, self.ty)
    }
}

impl Display for AstClass {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self.inheritance_modifier {
            AstInheritanceModifier::Final => { write!(f, "final ")?; }
            AstInheritanceModifier::Open => { write!(f, "open ")?; }
            AstInheritanceModifier::Abstract => { write!(f, "abstract ")?; }
            AstInheritanceModifier::Sealed => { write!(f, "sealed ")?; }
        }

        if self.inner {
            write!(f, "inner ")?;
        }

        match self.class_type {
            AstClassType::Regular => { write!(f, "class ")?; }
            AstClassType::Interface => { write!(f, "interface ")?; }
            AstClassType::Enum => { write!(f, "enum class ")?; }
            AstClassType::Annotation => { write!(f, "annotation class ")?; }
            AstClassType::Data => { write!(f, "data class ")?; }
            AstClassType::Inline => { write!(f, "inline class ")?; }
            AstClassType::Object => { write!(f, "object ")?; }
        }
        write!(f, "{} ", self.name)?;

        if !self.interfaces.is_empty() || self.super_type.is_some() {
            write!(f, ": ")?;
        }

        if let Some(ty) = &self.super_type {
            write!(f, "{:?}() ", ty)?;
        }

        for ty in &self.interfaces {
            write!(f, "{:?} ", ty)?;
        }

        if !self.body.is_empty() {
            write!(f, "{{\n")?;
            for member in &self.body {
                write!(f, "{:?}\n\n", member)?;
            }
            write!(f, "}}\n")?;
        }

        Ok(())
    }
}

impl Display for AstMember {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            AstMember::Class(it) => write!(f, "{:?}", it),
            AstMember::Function(it) => write!(f, "{:?}", it),
            AstMember::Property(it) => write!(f, "{:?}", it),
        }
    }
}

impl Display for AstFunction {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        if self.operator {
            write!(f, "operator ")?;
        }

        write!(f, "fun ")?;

        if self.extension {
            write!(f, "{:?}.", &self.args[0])?;
        }

        write!(f, "{}(", self.name)?;
        write!(f, ")")?;

        if let Some(ret) = &self.return_ty {
            write!(f, ": {:?}", ret)?;
        }

        if let Some(body) = &self.body {
            write!(f, "{{\n")?;
            write!(f, "{}", body)?;
            write!(f, "}}")?;
        }

        Ok(())
    }
}

impl Display for AstProperty {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        if self.var.mutable {
            write!(f, "var")?;
        } else {
            write!(f, "val")?;
        }
        write!(f, " {:?} ", self.var)?;
        if let Some(it) = &self.expr {
            if self.delegated {
                write!(f, "by")?;
            } else {
                write!(f, "=")?;
            }
            write!(f, " {:?} ", it)?;
        }
        Ok(())
    }
}

impl Display for AstLocalProperty {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        if self.vars.first().unwrap().mutable {
            write!(f, "var")?;
        } else {
            write!(f, "val")?;
        }

        if self.vars.len() == 1 {
            write!(f, " {:?} ", self.vars.first().unwrap())?;
        } else {
            write!(f, " (")?;
            for x in &self.vars {
                write!(f, "{:?} ", x)?;
            }
            write!(f, ") ")?;
        }

        if let Some(it) = &self.expr {
            if self.delegated {
                write!(f, "by")?;
            } else {
                write!(f, "=")?;
            }
            write!(f, " {:?} ", it)?;
        }
        Ok(())
    }
}

impl Display for AstType {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self.full_name)?;
        if !self.type_parameters.is_empty() {
            write!(f, "< ")?;
            for param in &self.type_parameters {
                match param {
                    AstTypeParameter::Type(ty) => { write!(f, "{} ", ty)?; }
                    AstTypeParameter::Projection => { write!(f, "* ")?; }
                    AstTypeParameter::Parameter(p) => { write!(f, "{} ", p)?; }
                }
            }
            write!(f, ">")?;
        }
        if self.nullable {
            write!(f, "?")?;
        }
        Ok(())
    }
}

impl Display for AstExpr {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            AstExpr::Lambda { block, .. } => {
                write!(f, "{:#?}\n", block)?;
            }
            AstExpr::AnonymousFunction { block, .. } => {
                write!(f, "{:#?}\n", block)?;
            }
            AstExpr::ObjectLiteral { block, .. } => {
                write!(f, "{:#?}\n", block)?;
            }
            AstExpr::Constant { value, .. } => {
                write!(f, "{:?}", value)?;
            }
            AstExpr::Ref { name, .. } => {
                write!(f, "{}", name)?;
            }
            AstExpr::Call { receiver, function, type_parameters, args, .. } => {
                if let Some(rec) = receiver {
                    write!(f, "({:?}).", rec.borrow())?;
                }
                write!(f, "{}", function)?;
                if !type_parameters.is_empty() {
                    write!(f, "<")?;
                    for param in type_parameters {
                        match param {
                            AstTypeParameter::Type(ty) => { write!(f, " {}", ty)?; }
                            AstTypeParameter::Projection => { write!(f, " *")?; }
                            AstTypeParameter::Parameter(p) => { write!(f, " {}", p)?; }
                        }
                    }
                    write!(f, " >")?;
                }
                write!(f, "( ")?;
                for e in args {
                    write!(f, "{:?} ", e)?;
                }
                write!(f, ")")?;
            }
            AstExpr::Is { expr, ty, .. } => {
                write!(f, "{:?} is {:?}", expr.borrow(), ty)?;
            }
            AstExpr::If { cond, if_true, if_false, .. } => {
                write!(f, "if ({:?}) ", cond.borrow())?;
                write!(f, "{:?} ", if_true)?;
                if let Some(it) = if_false {
                    write!(f, "else {:?} ", it)?;
                }
            }
            AstExpr::Continue { .. } => {
                write!(f, "continue")?;
            }
            AstExpr::Break { .. } => {
                write!(f, "break")?;
            }
            AstExpr::Try { body, catch, finally, .. } => {
                write!(f, "try {:?} ", body);
                for (var, expr) in catch {
                    write!(f, "catch ({:?}) {:?} ", var, expr)?;
                }
                if let Some(it) = finally {
                    write!(f, "finally {:?}", it)?;
                }
            }
            AstExpr::Throw { exception, .. } => {
                write!(f, "throw {:?}", exception.borrow())?;
            }
            AstExpr::Return { value, .. } => {
                write!(f, "return")?;
                if let Some(it) = value {
                    write!(f, " {:?}", it.borrow())?;
                }
            }
        }
        Ok(())
    }
}

impl Display for AstBlock {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        for s in &self.statements {
            write!(f, "{:#?}\n", s)?;
        }
        Ok(())
    }
}

impl Display for AstStatement {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            AstStatement::Expr(it) => { write!(f, "{:#?}", it)?; }
            AstStatement::Class(it) => { write!(f, "{:#?}", it)?; }
            AstStatement::Function(it) => { write!(f, "{:#?}", it)?; }
            AstStatement::Property(it) => { write!(f, "{:#?}", it)?; }
            AstStatement::Assignment(a, b) => { write!(f, "{:#?} = {:#?}", a, b)?; }
            AstStatement::For { variables, expr, body, .. } => {
                write!(f, "for (", )?;
                for x in variables {
                    write!(f, "{:?} ", x)?;
                }
                write!(f, "in {:?}", expr.borrow())?;
                write!(f, ") {:?}", body)?;
            }
            AstStatement::While { expr, body, .. } => {
                write!(f, "while ({:?}) {:?}", expr.borrow(), body)?;
            }
            AstStatement::DoWhile { expr, body, .. } => {
                write!(f, "do {:?} while ({:?})", body, expr.borrow())?;
            }
        }
        Ok(())
    }
}

impl Display for AstVar {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self.name)?;

        if let Some(ty) = &self.ty {
            write!(f, ": {:?}", ty)?;
        }
        Ok(())
    }
}

impl Display for AstLambda {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{{\n")?;
        write!(f, "{}", self.body)?;
        write!(f, "}}")?;
        Ok(())
    }
}