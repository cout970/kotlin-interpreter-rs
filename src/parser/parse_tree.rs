use std::sync::Arc;

use crate::Number;
use crate::source_code::Span;

pub const SPAN_NONE: Span = (0, 0);

pub type Path = Vec<String>;

#[derive(Clone, PartialEq, Debug)]
pub struct KotlinFile {
    pub preamble: Preamble,
    pub objects: Vec<TopLevelObject>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct KotlinScript {
    pub preamble: Preamble
}

#[derive(Clone, PartialEq, Debug)]
pub struct Preamble {
    pub file_annotations: Vec<FileAnnotation>,
    pub package_header: Option<PackageHeader>,
    pub imports: Vec<Import>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct FileAnnotation {
    pub span: Span,
    pub annotations: Vec<Annotation>,
//    type_arguments: Vec<Type>,
//    value_arguments: Option<>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Annotation {
    pub span: Span,
    pub names: Path,
    pub use_site_target: Option<String>,
    pub type_arguments: Vec<Type>,
    pub value_arguments: Vec<ValueArgument>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Import {
    pub span: Span,
    pub path: Vec<String>,
    pub alias: Option<String>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct PackageHeader {
    pub span: Span,
    pub modifiers: Vec<Modifier>,
    pub path: Vec<String>,
}

//#[derive(Clone, PartialEq, Debug)]
//pub struct Modifier {
//    pub name: String,
//}

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
    pub span: Span,
    pub modifiers: Vec<Modifier>,
    pub mutable: bool,
    pub type_parameters: Vec<TypeParameter>,
    pub receiver: Option<Type>,
    pub declarations: Vec<VariableDeclarationEntry>,
    pub type_constraints: Vec<TypeConstraint>,
    pub initialization: PropertyInitialization,
    pub getter: Option<PropertyGetter>,
    pub setter: Option<PropertySetter>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum PropertyInitialization {
    None,
    Expr(ExprVal),
    Delegation(ExprVal),
}

#[derive(Clone, PartialEq, Debug)]
pub struct PropertyGetter {
    pub span: Span,
    pub modifiers: Vec<Modifier>,
    pub ty: Option<Type>,
    pub body: Option<FunctionBody>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct PropertySetter {
    pub span: Span,
    pub modifiers: Vec<Modifier>,
    pub param_modifiers: Vec<Modifier>,
    pub param_name: Option<String>,
    pub param_ty: Option<Type>,
    pub body: Option<FunctionBody>,
}

pub type ExprVal = (Span, Expr);
pub type ExprRef = Arc<(Span, Expr)>;
pub type Block = (Span, Vec<Statement>);

#[derive(Clone, PartialEq, Debug)]
pub enum Expr {
    Chain {
        operands: Vec<ExprVal>,
        operators: Vec<String>,
    },
    InfixFun {
        parameters: Vec<ExprVal>,
        functions: Vec<String>,
    },
    Prefix {
        prefix: Vec<String>,
        expr: ExprRef,
    },
    Postfix {
        expr: ExprRef,
        postfix: Vec<ExprPostfix>,
    },
    Is {
        expr: ExprRef,
        ty: Type,
    },
    String(Vec<StringComponent>),
    If {
        cond: ExprRef,
        if_true: Block,
        if_false: Option<Block>,
    },
    Try {
        block: Block,
        catch_blocks: Vec<CatchBlock>,
        finally: Option<Block>,
    },
    For {
        annotations: Vec<Annotation>,
        variables: Vec<VariableDeclarationEntry>,
        expr: ExprRef,
        body: Block,
    },
    While {
        expr: ExprRef,
        body: Block,
    },
    DoWhile {
        expr: ExprRef,
        body: Block,
    },
    When {
        expr: Option<ExprRef>,
        entries: Vec<WhenEntry>,
    },
    Object {
        delegation_specifiers: Vec<DelegationSpecifier>,
        body: ClassBody,
    },
    CallableRef {
        name: String,
        ty: UserType,
        type_arguments: Vec<Type>,
    },
    Lambda(FunctionLiteral),
    Ref(String),
    Boolean(bool),
    Char(char),
    Number(Number),
    Null,
    This,
    Super,
    Throw(ExprRef),
    Return(Option<ExprRef>),
    Continue,
    Break,
}

#[derive(Clone, PartialEq, Debug)]
pub struct WhenEntry {
    pub conditions: Vec<WhenCondition>,
    pub body: Block,
}

#[derive(Clone, PartialEq, Debug)]
pub enum WhenCondition {
    Else,
    Expr(ExprVal),
    In { negated: bool, expr: ExprVal },
    Is { negated: bool, ty: Type },
}

#[derive(Clone, PartialEq, Debug)]
pub enum StringComponent {
    Content(String),
    Variable(String),
    Template(ExprVal),
}

#[derive(Clone, PartialEq, Debug)]
pub struct CatchBlock {
    pub annotations: Vec<Annotation>,
    pub name: String,
    pub ty: Vec<SimpleUserType>,
    pub block: Block,
}

#[derive(Clone, PartialEq, Debug)]
pub enum ExprPostfix {
    Increment,
    Decrement,
    AssertNonNull,
    ArrayAccess(Vec<ExprVal>),
    FunCall(CallSuffix),
    MemberAccess { operator: String, next: ExprVal },
}

#[derive(Clone, PartialEq, Debug)]
pub struct VariableDeclarationEntry {
    pub name: String,
    pub declared_type: Option<Type>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Function {
    pub span: Span,
    pub modifiers: Vec<Modifier>,
    pub type_parameters: Vec<TypeParameter>,
    pub receiver: Option<Type>,
    pub type_parameters2: Vec<TypeParameter>,
    pub name: String,
    pub value_parameters: Vec<FunctionParameter>,
    pub return_type: Option<Type>,
    pub type_constraints: Vec<TypeConstraint>,
    pub body: Option<FunctionBody>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum FunctionBody {
    Block(Block),
    Expression(ExprVal),
}

#[derive(Clone, PartialEq, Debug)]
pub enum Statement {
    Expr(ExprVal),
    Decl(Declaration),
}

#[derive(Clone, PartialEq, Debug)]
pub enum Declaration {
    Class(Class),
    Object(Object),
    Function(Function),
    Property(Property),
    TypeAlias(TypeAlias),
}

#[derive(Clone, PartialEq, Debug)]
pub struct TypeAlias {
    pub span: Span,
    pub modifiers: Vec<Modifier>,
    pub name: String,
    pub type_parameters: Vec<TypeParameter>,
    pub ty: Type,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Object {
    pub span: Span,
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
    pub mutability: ParameterMutability,
    pub name: String,
    pub ty: Type,
    pub default_value: Option<ExprVal>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum ParameterMutability {
    Val,
    Var,
    Default,
}

#[derive(Clone, PartialEq, Debug)]
pub struct TypeParameter {
    pub modifiers: Vec<Modifier>,
    pub name: String,
    pub user_type: Option<Type>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Type {
    pub span: Span,
    pub annotations: Vec<Annotation>,
    pub reference: Arc<TypeReference>,
}

pub type UserType = Vec<SimpleUserType>;

#[derive(Clone, PartialEq, Debug)]
pub enum TypeReference {
    Function(FunctionType),
    UserType(UserType),
    Nullable(Arc<TypeReference>),
}


#[derive(Clone, PartialEq, Debug)]
pub struct SimpleUserType {
    pub name: String,
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
    pub receiver: Option<Arc<TypeReference>>,
    pub parameters: Vec<Type>,
    pub return_type: Arc<TypeReference>,
}

#[derive(Clone, PartialEq, Debug, Copy)]
pub enum ClassType {
    Class,
    Interface,
    Enum,
    Annotation,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Class {
    pub span: Span,
    pub modifiers: Vec<Modifier>,
    pub class_type: ClassType,
    pub name: String,
    pub type_parameters: Vec<TypeParameter>,
    pub primary_constructor: Option<PrimaryConstructor>,
    pub annotations: Vec<Annotation>,
    pub delegations: Vec<DelegationSpecifier>,
    pub type_constraints: Vec<TypeConstraint>,
    pub body: Option<ClassBody>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ClassBody {
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
    pub span: Span,
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
    pub modifiers: Vec<Modifier>,
    pub params: Vec<FunctionParameter>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum DelegationSpecifier {
    Type(Type),
    DelegatedBy(Type, ExprVal),
    FunctionCall(Type, CallSuffix),
}

#[derive(Clone, PartialEq, Debug)]
pub struct CallSuffix {
    pub type_arguments: Vec<Type>,
    pub value_arguments: Vec<ValueArgument>,
    pub annotated_lambda: Option<AnnotatedLambda>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ValueArgument {
    pub name: Option<String>,
    pub spread: bool,
    pub expr: ExprVal,
}

#[derive(Clone, PartialEq, Debug)]
pub struct AnnotatedLambda {
    pub annotations: Vec<Annotation>,
    pub body: FunctionLiteral,
}

#[derive(Clone, PartialEq, Debug)]
pub struct FunctionLiteral {
    pub parameters: Vec<VariableDeclarationEntry>,
    pub statements: Vec<Statement>,
}

impl KotlinFile {
    pub fn get_package_str(&self) -> String {
        self.preamble.package_header.as_ref().unwrap().path.join(".")
    }
}