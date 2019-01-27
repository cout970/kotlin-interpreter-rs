use std::rc::Rc;

use crate::interpreter::bytecode::Constant;
use crate::source_code::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct AstType {
    pub span: Span,
    // B
    pub name: String,
    // A.B
    pub full_name: String,
    // A.B<Int>
    pub type_parameters: Vec<AstTypeParameter>,
    // A.B<Int>?
    pub nullable: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstTypeParameter {
    Type(AstType),
    Projection,
    Parameter(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstFile {
    pub package: String,
    pub imports: Vec<AstImport>,
    pub classes: Vec<AstClass>,
    pub functions: Vec<AstFunction>,
    pub properties: Vec<AstProperty>,
    pub typealias: Vec<AstTypealias>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstTypealias {
    pub span: Span,
    pub name: String,
    pub ty: AstType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstImport {
    pub span: Span,
    pub name: String,
    pub alias: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstVar {
    pub name: String,
    pub ty: Option<AstType>,
    pub mutable: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstLocalProperty {
    pub vars: Vec<AstVar>,
    pub delegated: bool,
    pub expr: Option<AstExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstProperty {
    pub span: Span,
    pub var: AstVar,
    pub delegated: bool,
    pub expr: Option<AstExpr>,
    pub getter: Option<AstFunction>,
    pub setter: Option<AstFunction>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstFunction {
    pub span: Span,
    pub extension: bool,
    pub operator: bool,
    pub member: bool,
    pub name: String,
    pub args: Vec<AstVar>,
    pub return_ty: Option<AstType>,
    pub body: Option<AstExpr>,
    // TODO default parameters
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstClass {
    // visibility
    // open, final, abstract, sealed
    // data, enum, annotation, inline, interface
    // inner
    pub span: Span,
    pub name: String,
    pub body: Vec<AstMember>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstStatement {
    Expr(AstExpr),
    Class(AstClass),
    Function(AstFunction),
    Property(AstLocalProperty),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstMember {
    Class(AstClass),
    Function(AstFunction),
    Property(AstProperty),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstExpr {
    Block {
        span: Span,
        statements: Vec<AstStatement>,
    },
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