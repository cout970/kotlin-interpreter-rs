use std::cell::RefCell;
use std::rc::Rc;

use crate::interpreter::bytecode::Constant;
use crate::source_code::Span;

#[derive(Clone, PartialEq)]
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

#[derive(Clone, PartialEq)]
pub struct AstFile {
    pub package: String,
    pub imports: Vec<AstImport>,
    pub classes: Vec<AstClass>,
    pub functions: Vec<AstFunction>,
    pub properties: Vec<AstProperty>,
    pub typealias: Vec<AstTypealias>,
}

#[derive(Clone, PartialEq)]
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

#[derive(Clone, PartialEq)]
pub struct AstVar {
    pub name: String,
    pub ty: Option<AstType>,
    pub mutable: bool,
}

#[derive(Clone, PartialEq)]
pub struct AstLocalProperty {
    pub var: AstVar,
    pub delegated: bool,
    pub expr: Option<AstExpr>,
}

#[derive(Clone, PartialEq)]
pub struct AstProperty {
    pub span: Span,
    pub var: AstVar,
    pub delegated: bool,
    pub expr: Option<AstExpr>,
    pub getter: Option<AstFunction>,
    pub setter: Option<AstFunction>,
}

#[derive(Clone, PartialEq)]
pub struct AstFunction {
    pub span: Span,
    pub extension: bool,
    pub operator: bool,
    pub member: bool,
    pub name: String,
    pub args: Vec<AstVar>,
    pub return_ty: Option<AstType>,
    pub body: Option<AstBlock>,
    // TODO default parameters
}

#[derive(Clone, PartialEq)]
pub struct AstLambda {
    pub span: Span,
    pub extension: bool,
    pub args: Vec<AstVar>,
    pub return_ty: Option<AstType>,
    pub body: AstBlock,
}

#[derive(Clone, PartialEq, Default)]
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

#[derive(Clone, PartialEq)]
pub enum AstStatement {
    Expr(AstExpr),
    Assignment(AstExpr, AstExpr),
    Class(AstClass),
    Function(AstFunction),
    Property(AstLocalProperty),
    For {
        span: Span,
        variables: Vec<AstVar>,
        expr: MutRc<AstExpr>,
        body: AstBlock,
    },
    While {
        span: Span,
        expr: MutRc<AstExpr>,
        body: AstBlock,
    },
    DoWhile {
        span: Span,
        expr: MutRc<AstExpr>,
        body: AstBlock,
    },
}

#[derive(Clone, PartialEq)]
pub enum AstMember {
    Class(AstClass),
    Function(AstFunction),
    Property(AstProperty),
}

pub type MutRc<T> = Rc<RefCell<T>>;

pub fn mut_rc<T>(a: T) -> MutRc<T> {
    Rc::new(RefCell::new(a))
}

#[derive(Clone, PartialEq)]
pub enum AstExpr {
    Lambda {
        span: Span,
        block: AstLambda,
    },
    AnonymousFunction {
        span: Span,
        block: AstFunction,
    },
    ObjectLiteral {
        span: Span,
        block: AstClass,
    },
    Constant {
        span: Span,
        value: Constant,
    },
    Ref {
        span: Span,
        obj: Option<MutRc<AstExpr>>,
        name: String,
    },
    Call {
        span: Span,
        receiver: Option<MutRc<AstExpr>>,
        function: String,
        type_parameters: Vec<AstTypeParameter>,
        args: Vec<AstExpr>,
    },
    Is {
        span: Span,
        expr: MutRc<AstExpr>,
        ty: AstType,
    },
    If {
        span: Span,
        cond: MutRc<AstExpr>,
        if_true: AstBlock,
        if_false: Option<AstBlock>,
    },
    Continue {
        span: Span,
    },
    Break {
        span: Span,
    },
    Try {
        span: Span,
        body: AstBlock,
        catch: Vec<(AstVar, AstBlock)>,
        finally: Option<AstBlock>,
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

#[derive(Clone, PartialEq)]
pub struct AstBlock {
    pub span: Span,
    pub statements: Vec<AstStatement>,
}