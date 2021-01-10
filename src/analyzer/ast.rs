use std::cell::RefCell;
use std::rc::Rc;

use crate::source::ByteSpan;

#[derive(Clone, PartialEq, Debug)]
pub enum Constant {
    Null,
    Array(Vec<Constant>),
    Boolean(bool),
    Double(f64),
    Float(f32),
    Byte(i8),
    Short(i16),
    Int(i32),
    Long(i64),
    Char(char),
    String(String),
}

#[derive(Clone, PartialEq)]
pub struct AstType {
    pub span: ByteSpan,
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
    pub span: ByteSpan,
    pub name: String,
    pub ty: AstType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstImport {
    pub span: ByteSpan,
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
    pub span: ByteSpan,
    pub var: AstVar,
    pub delegated: bool,
    pub expr: Option<AstExpr>,
    pub getter: Option<AstFunction>,
    pub setter: Option<AstFunction>,
}

#[derive(Clone, PartialEq)]
pub struct AstFunction {
    pub span: ByteSpan,
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
    pub span: ByteSpan,
    pub extension: bool,
    pub args: Vec<AstVar>,
    pub return_ty: Option<AstType>,
    pub body: AstBlock,
}

#[derive(Clone, PartialEq, Default)]
pub struct AstClass {
    // visibility: ignored for now
    pub span: ByteSpan,
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
        span: ByteSpan,
        variables: Vec<AstVar>,
        expr: MutRc<AstExpr>,
        body: AstBlock,
    },
    While {
        span: ByteSpan,
        expr: MutRc<AstExpr>,
        body: AstBlock,
    },
    DoWhile {
        span: ByteSpan,
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
        span: ByteSpan,
        block: AstLambda,
    },
    AnonymousFunction {
        span: ByteSpan,
        block: AstFunction,
    },
    ObjectLiteral {
        span: ByteSpan,
        block: AstClass,
    },
    Constant {
        span: ByteSpan,
        value: Constant,
    },
    Ref {
        span: ByteSpan,
        obj: Option<MutRc<AstExpr>>,
        name: String,
    },
    Call {
        span: ByteSpan,
        receiver: Option<MutRc<AstExpr>>,
        function: String,
        type_parameters: Vec<AstTypeParameter>,
        args: Vec<AstExpr>,
    },
    Is {
        span: ByteSpan,
        expr: MutRc<AstExpr>,
        ty: AstType,
    },
    If {
        span: ByteSpan,
        cond: MutRc<AstExpr>,
        if_true: AstBlock,
        if_false: Option<AstBlock>,
    },
    Continue {
        span: ByteSpan,
    },
    Break {
        span: ByteSpan,
    },
    Try {
        span: ByteSpan,
        body: AstBlock,
        catch: Vec<(AstVar, AstBlock)>,
        finally: Option<AstBlock>,
    },
    Throw {
        span: ByteSpan,
        exception: MutRc<AstExpr>,
    },
    Return {
        span: ByteSpan,
        value: Option<MutRc<AstExpr>>,
    },
}

#[derive(Clone, PartialEq)]
pub struct AstBlock {
    pub span: ByteSpan,
    pub statements: Vec<AstStatement>,
}