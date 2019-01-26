use std::rc::Rc;

use crate::interpreter::bytecode::Constant;
use crate::source_code::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct AstType {
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstVar {
    pub name: String,
    pub ty: Option<AstType>,
    pub mutable: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstExpr {
    Block(Vec<AstExpr>),
    Constant {
        span: Span,
        value: Constant,
    },
    Ref {
        span: Span,
        name: String,
    },
    Call {
        span: Span,
        function: String,
        args: Vec<AstExpr>,
    },
    ReadField {
        span: Span,
        field: String,
        object: Rc<AstExpr>,
    },
    WriteRef {
        span: Span,
        name: String,
        expr: Rc<AstExpr>,
    },
    Is {
        span: Span,
        expr: Rc<AstExpr>,
        ty: AstType,
    },
    If {
        span: Span,
        cond: Rc<AstExpr>,
        if_true: Rc<AstExpr>,
        if_false: Option<Rc<AstExpr>>,
    },
    For {
        span: Span,
        variables: Vec<AstVar>,
        expr: Rc<AstExpr>,
        body: Rc<AstExpr>,
    },
    While {
        span: Span,
        expr: Rc<AstExpr>,
        body: Rc<AstExpr>,
    },
    DoWhile {
        span: Span,
        expr: Rc<AstExpr>,
        body: Rc<AstExpr>,
    },
    Continue {
        span: Span,
    },
    Break {
        span: Span,
    },
    Try {
        span: Span,
        body: Rc<AstExpr>,
        catch: Vec<(AstVar, AstExpr)>,
        finally: Option<Rc<AstExpr>>,
    },
    Throw {
        span: Span,
        exception: Rc<AstExpr>,
    },
    Return {
        span: Span,
        value: Option<Rc<AstExpr>>,
    },
}