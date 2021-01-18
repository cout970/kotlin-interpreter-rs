use std::fmt::{Debug, Display, Formatter};
use std::sync::Arc;

use crate::source::ByteSpan;
use crate::token::Number;

pub type Path = Vec<String>;

#[derive(Clone, PartialEq, Debug)]
pub struct KotlinFile {
    pub file_annotations: Vec<FileAnnotation>,
    pub package_header: Option<PackageHeader>,
    pub imports: Vec<Import>,
    pub objects: Vec<TopLevelObject>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct FileAnnotation {
    pub span: ByteSpan,
    pub annotations: Vec<Annotation>,
//    type_arguments: Vec<Type>,
//    value_arguments: Option<>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Annotation {
    pub span: ByteSpan,
    pub names: Path,
    pub use_site_target: Option<String>,
    pub type_arguments: Vec<CallSiteTypeParams>,
    pub value_arguments: Vec<ValueArgument>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Import {
    pub span: ByteSpan,
    pub path: Vec<String>,
    pub alias: Option<String>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct PackageHeader {
    pub span: ByteSpan,
    pub path: Vec<String>,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum ModifierCtx {
    TopLevelObject,
    TypeParameter,
    Statement,
    Package,
    Constructor,
    GetterSetter,
    ClassMember,
    EnumEntry,
    FunctionParameter,
    ConstructorParameter,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum ModifierTarget {
    Class,
    Object,
    Function,
    Property,
    Other,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Modifier {
    Abstract,
    Final,
    Enum,
    Open,
    Annotation,
    Sealed,
    Data,
    Lateinit,
    Private,
    Protected,
    Public,
    Internal,
    Tailrec,
    Operator,
    Infix,
    Inline,
    External,
    Suspend,
    Const,
    Expect,
    Actual,
    In,
    Out,
    Reified,
    Inner,
    Noinline,
    Crossinline,
    Vararg,
    Override,
}

#[derive(Clone, PartialEq, Debug)]
pub enum TopLevelObject {
    Class(Class),
    Object(Object),
    Function(Function),
    Property(Property),
    TypeAlias(TypeAlias),
}

#[derive(Clone, PartialEq, Debug)]
pub struct Property {
    pub span: ByteSpan,
    pub modifiers: Vec<Modifier>,
    pub mutable: bool,
    pub type_parameters: Vec<TypeParameter>,
    pub receiver: Option<Type>,
    pub variable: VariableName,
    pub type_constraints: Vec<TypeConstraint>,
    pub initialization: PropertyInitialization,
    pub getter: Option<PropertyGetter>,
    pub setter: Option<PropertySetter>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum PropertyInitialization {
    None,
    Expr(Expression),
    Delegation(Expression),
}

#[derive(Clone, PartialEq, Debug)]
pub struct PropertyGetter {
    pub span: ByteSpan,
    pub modifiers: Vec<Modifier>,
    pub getter_type: Option<Type>,
    pub body: Option<FunctionBody>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct PropertySetter {
    pub span: ByteSpan,
    pub modifiers: Vec<Modifier>,
    pub setter_type: Option<Type>,
    pub parameter: Option<SetterParameter>,
    pub body: Option<StatementBlock>,
}

pub type ExprRef = Arc<(ByteSpan, Expr)>;
pub type Block = (ByteSpan, Vec<Statement>);

#[derive(Clone, PartialEq)]
pub struct Expression {
    pub span: ByteSpan,
    pub kind: Expr,
}

#[derive(Clone, PartialEq, Debug)]
pub struct StatementBlock {
    pub span: ByteSpan,
    pub statements: Vec<Statement>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Expr {
    Null,
    Tree {
        left: Box<Expression>,
        right: Box<Expression>,
        operator: String,
    },
    FunctionCall {
        function: Box<Expression>,
        type_arguments: Vec<Type>,
        value_arguments: Vec<ValueArgument>,
        lambda: Option<FunctionLiteral>,
    },
    PropertyAccess {
        object: Box<Expression>,
        property: String,
        safe_call: bool,
    },
    UnaryOperator {
        unary_operators: Vec<String>,
        expr: Box<Expression>,
    },
    Suffix {
        expr: Box<Expression>,
        suffix: ExprSuffix,
    },
    Is {
        expr: Box<Expression>,
        is_type: Type,
        negated: bool,
    },
    String(Vec<StringComponent>),
    If {
        cond: Box<Expression>,
        if_true: Box<FunctionBody>,
        if_false: Option<Box<FunctionBody>>,
    },
    Try {
        block: Box<FunctionBody>,
        catch_blocks: Vec<CatchBlock>,
        finally: Option<Box<FunctionBody>>,
    },
    When {
        expr: Option<Box<Expression>>,
        entries: Vec<WhenEntry>,
    },
    Object {
        delegation_specifiers: Vec<DelegationSpecifier>,
        body: ClassBody,
    },
    ExprCallableRef {
        receiver_expr: Option<Box<Expression>>,
        name: String,
    },
    Lambda(FunctionLiteral),
    Ref(String),
    Boolean(bool),
    Char(char),
    Number(Number),
    This,
    Super,
    Throw(Box<Expression>),
    Return(Option<Box<Expression>>),
    Continue,
    Break,
}

#[derive(Clone, PartialEq, Debug)]
pub struct WhenEntry {
    pub conditions: Vec<WhenCondition>,
    pub body: FunctionBody,
}

#[derive(Clone, PartialEq, Debug)]
pub enum WhenCondition {
    Else,
    Expr(Expression),
    In { negated: bool, expr: Expression },
    Is { negated: bool, is_type: Type },
}

#[derive(Clone, PartialEq, Debug)]
pub enum StringComponent {
    Content(String),
    Variable(String),
    Template(Expression),
}

#[derive(Clone, PartialEq, Debug)]
pub struct CatchBlock {
    pub annotations: Vec<Annotation>,
    pub variable: String,
    pub exception_type: Type,
    pub block: FunctionBody,
}

#[derive(Clone, PartialEq, Debug)]
pub enum ExprSuffix {
    Increment,
    Decrement,
    AssertNonNull,
    ArrayAccess(Vec<Expression>),
    FunCall(CallSuffix),
    MemberAccess { operator: String, member: String },
}

#[derive(Clone, PartialEq, Debug)]
pub struct Function {
    pub span: ByteSpan,
    pub modifiers: Vec<Modifier>,
    pub type_parameters: Vec<TypeParameter>,
    pub receiver: Option<Type>,
    pub name: String,
    pub value_parameters: Vec<FunctionParameter>,
    pub return_type: Option<Type>,
    pub type_constraints: Vec<TypeConstraint>,
    pub body: Option<FunctionBody>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum FunctionBody {
    Block(StatementBlock),
    Expression(Expression),
}

#[derive(Clone, PartialEq, Debug)]
pub enum Statement {
    Expression(Expression),
    Assignment {
        left: Expression,
        right: Expression,
        operator: String,
    },
    Class(Class),
    Function(Function),
    Variable(Variable),
    For(ForStatement),
    While(WhileStatement),
    DoWhile(DoWhileStatement),
}

#[derive(Clone, PartialEq, Debug)]
pub struct Variable {
    pub span: ByteSpan,
    pub mutable: bool,
    pub variable: VariablePattern,
    pub initialization: PropertyInitialization,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ForStatement {
    pub span: ByteSpan,
    pub annotations: Vec<Annotation>,
    pub variable: VariablePattern,
    pub iterable: Expression,
    pub body: FunctionBody,
}

#[derive(Clone, PartialEq, Debug)]
pub enum VariablePattern {
    Single(VariableName),
    Tuple(Vec<VariableName>),
}

#[derive(Clone, PartialEq, Debug)]
pub struct VariableName {
    pub name: String,
    pub variable_type: Option<Type>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct WhileStatement {
    pub span: ByteSpan,
    pub condition: Expression,
    pub body: FunctionBody,
}

#[derive(Clone, PartialEq, Debug)]
pub struct DoWhileStatement {
    pub span: ByteSpan,
    pub condition: Expression,
    pub body: FunctionBody,
}

#[derive(Clone, PartialEq, Debug)]
pub struct TypeAlias {
    pub span: ByteSpan,
    pub modifiers: Vec<Modifier>,
    pub name: String,
    pub type_parameters: Vec<TypeParameter>,
    pub aliased_type: Type,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Object {
    pub span: ByteSpan,
    pub modifiers: Vec<Modifier>,
    pub name: String,
    pub primary_constructor: Option<PrimaryConstructor>,
    pub annotations: Vec<Annotation>,
    pub delegations: Vec<DelegationSpecifier>,
    pub body: Option<ClassBody>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct TypeConstraint {
    pub annotations: Vec<Annotation>,
    pub name: String,
    pub ty: Type,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Parameter {
    pub name: String,
    pub ty: Type,
}

#[derive(Clone, PartialEq, Debug)]
pub struct FunctionParameter {
    pub modifiers: Vec<Modifier>,
    pub name: String,
    pub parameter_type: Type,
    pub default_value: Option<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct SetterParameter {
    pub modifiers: Vec<Modifier>,
    pub name: String,
    pub parameter_type: Option<Type>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct TypeParameter {
    pub modifiers: Vec<Modifier>,
    pub name: String,
    pub user_type: Option<Type>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Type {
    pub span: ByteSpan,
    pub annotations: Vec<Annotation>,
    pub reference: TypeReference,
}

pub type UserType = Vec<SimpleType>;

#[derive(Clone, PartialEq, Debug)]
pub enum TypeReference {
    Function(FunctionType),
    SimpleType(SimpleType),
}

#[derive(Clone, PartialEq, Debug)]
pub struct SimpleType {
    pub nullable: bool,
    pub name: String,
    pub package: Vec<String>,
    pub type_params: Vec<CallSiteTypeParams>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum CallSiteTypeParams {
    // * -> List<*>
    Projection,
    // Int -> List<Int>
    Type(Type),
}

#[derive(Clone, PartialEq, Debug)]
pub struct FunctionType {
    pub nullable: bool,
    pub receiver: Option<Arc<TypeReference>>,
    pub parameters: Vec<Type>,
    pub return_type: Box<Type>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Class {
    pub span: ByteSpan,
    pub annotations: Vec<Annotation>,
    pub modifiers: Vec<Modifier>,
    pub name: String,
    pub type_parameters: Vec<TypeParameter>,
    pub primary_constructor: Option<PrimaryConstructor>,
    pub delegation_specifiers: Vec<DelegationSpecifier>,
    pub type_constraints: Vec<TypeConstraint>,
    pub body: Option<ClassBody>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ClassBody {
    pub span: ByteSpan,
    pub enum_entries: Vec<EnumEntry>,
    pub members: Vec<Member>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct EnumEntry {
    pub modifiers: Vec<Modifier>,
    pub name: String,
    pub value_arguments: Vec<ValueArgument>,
    pub class_body: Option<ClassBody>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Member {
    CompanionObject(Object),
    Object(Object),
    Function(Function),
    Property(Property),
    Class(Class),
    TypeAlias(TypeAlias),
    AnonymousInitializer(AnonymousInitializer),
    SecondaryConstructor(SecondaryConstructor),
}

#[derive(Clone, PartialEq, Debug)]
pub struct AnonymousInitializer {
    pub span: ByteSpan,
    pub body: Block,
}

#[derive(Clone, PartialEq, Debug)]
pub struct SecondaryConstructor {
    pub modifiers: Vec<Modifier>,
    pub parameters: Vec<FunctionParameter>,
    pub delegation_call: DelegationCall,
    pub body: Option<Block>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum DelegationCall {
    Super(Vec<ValueArgument>),
    This(Vec<ValueArgument>),
    None,
}

#[derive(Clone, PartialEq, Debug)]
pub struct PrimaryConstructor {
    pub span: ByteSpan,
    pub modifiers: Vec<Modifier>,
    pub value_parameters: Vec<FunctionParameter>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum DelegationSpecifier {
    Type(Type),
    DelegatedBy(Type, Expression),
    FunctionCall(Type, CallSuffix),
}

#[derive(Clone, PartialEq, Debug)]
pub struct CallSuffix {
    pub span: ByteSpan,
    pub value_arguments: Vec<ValueArgument>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ValueArgument {
    pub span: ByteSpan,
    pub named: Option<String>,
    pub spread: bool,
    pub expr: Expression,
}

#[derive(Clone, PartialEq, Debug)]
pub struct FunctionLiteral {
    pub annotations: Vec<Annotation>,
    pub parameters: Vec<VariableName>,
    pub body: StatementBlock,
}

impl KotlinFile {
    pub fn get_package_str(&self) -> String {
        match self.package_header.as_ref() {
            Some(it) => it.path.join("."),
            None => String::new()
        }
    }
}

impl Debug for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // Omit span and 1 level of indentation
        write!(f, "Expression::")?;
        self.kind.fmt(f)
    }
}