use std::cell::RefCell;
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

#[derive(Debug, Clone, PartialEq, Default)]
pub struct AstClass {
    // visibility: ignored for now
    pub span: Span,
    pub name: String,
    pub inner: bool,
    pub class_type: AstClassType,
    pub inheritance_modifier: AstInheritanceModifier,
    // Delegation by?
    pub interfaces: Vec<AstType>,
    pub super_type: Option<AstType>,
    pub body: Vec<AstMember>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum AstVisibility {
    Public,
    Private,
    Protected,
    Internal,
}

impl Default for AstVisibility {
    fn default() -> Self { AstVisibility::Public }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum AstClassType {
    Regular,
    Interface,
    Enum,
    Annotation,
    Data,
    Inline,
    Object,
}

impl Default for AstClassType {
    fn default() -> Self { AstClassType::Regular }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum AstInheritanceModifier {
    Final,
    Open,
    Abstract,
    Sealed,
}

impl Default for AstInheritanceModifier {
    fn default() -> Self { AstInheritanceModifier::Final }
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

pub type MutRc<T> = Rc<RefCell<T>>;

pub fn mut_rc<T>(a: T) -> MutRc<T> {
    Rc::new(RefCell::new(a))
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
    CallInvoke {
        span: Span,
        function: MutRc<AstExpr>,
        args: Vec<AstExpr>,
    },
    ReadField {
        span: Span,
        field: String,
        object: MutRc<AstExpr>,
    },
    WriteRef {
        span: Span,
        name: String,
        expr: MutRc<AstExpr>,
    },
    Is {
        span: Span,
        expr: MutRc<AstExpr>,
        ty: AstType,
    },
    If {
        span: Span,
        cond: MutRc<AstExpr>,
        if_true: MutRc<AstExpr>,
        if_false: Option<MutRc<AstExpr>>,
    },
    For {
        span: Span,
        variables: Vec<AstVar>,
        expr: MutRc<AstExpr>,
        body: MutRc<AstExpr>,
    },
    While {
        span: Span,
        expr: MutRc<AstExpr>,
        body: MutRc<AstExpr>,
    },
    DoWhile {
        span: Span,
        expr: MutRc<AstExpr>,
        body: MutRc<AstExpr>,
    },
    Continue {
        span: Span,
    },
    Break {
        span: Span,
    },
    Try {
        span: Span,
        body: MutRc<AstExpr>,
        catch: Vec<(AstVar, AstExpr)>,
        finally: Option<MutRc<AstExpr>>,
    },
    Throw {
        span: Span,
        exception: MutRc<AstExpr>,
    },
    Return {
        span: Span,
        value: Option<MutRc<AstExpr>>,
    },
}