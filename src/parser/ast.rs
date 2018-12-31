use std::sync::Arc;

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
    pub annotations: Vec<Annotation>,
//    type_arguments: Vec<Type>,
//    value_arguments: Option<>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Annotation {
    pub names: Vec<String>,
//    type_arguments: Vec<Type>,
//    value_arguments: Option<>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Import {
    pub path: Vec<String>,
    pub alias: Option<String>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct PackageHeader {
    pub modifiers: Vec<Modifier>,
    pub path: Vec<String>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Modifier {
    pub name: String,
}

#[derive(Clone, PartialEq, Debug)]
pub enum TopLevelObject {
    Class(Class),
    Object,
    Function(Function),
    Property(Property),
    TypeAlias,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Property {
    pub modifiers: Vec<Modifier>,
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
    Expr(Expr),
    Delegation(Expr),
}

#[derive(Clone, PartialEq, Debug)]
pub struct PropertyGetter {
    pub modifiers: Vec<Modifier>,
    pub ty: Option<Type>,
    pub body: Option<FunctionBody>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct PropertySetter {
    pub modifiers: Vec<Modifier>,
    pub param_modifiers: Vec<Modifier>,
    pub param_name: Option<String>,
    pub param_ty: Option<Type>,
    pub body: Option<FunctionBody>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Expr {
    Chain {
        operands: Vec<Expr>,
        operators: Vec<String>,
    },
    InfixFun {
        parameters: Vec<Expr>,
        functions: Vec<String>,
    },
    Prefix {
        prefix: Vec<String>,
        expr: Arc<Expr>,
    },
    Postfix {
        expr: Arc<Expr>,
        postfix: Vec<String>,
    },
    Is {
        expr: Arc<Expr>,
        ty: Type,
    },
    Ref(String),
    Boolean(bool),
    Char(char),
    Double(f64),
    Float(f32),
    Int(i32),
    Long(i64),
    Null,
}

#[derive(Clone, PartialEq, Debug)]
pub struct VariableDeclarationEntry {
    pub name: String,
    pub declared_type: Option<Type>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Function {
    pub modifiers: Vec<Modifier>,
    pub type_parameters: Vec<TypeParameter>,
    pub receiver: Option<Type>,
    pub type_parameters2: Vec<TypeParameter>,
    pub name: String,
    pub value_parameters: Vec<FunctionParameter>,
    pub return_type: Type,
    pub type_constraints: Vec<TypeConstraint>,
    pub body: Option<FunctionBody>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum FunctionBody {
    Block(Vec<Statement>),
    Expression(Expr),
}

#[derive(Clone, PartialEq, Debug)]
pub enum Statement {
    Expr(Expr),
    Decl(Declaration),
}

#[derive(Clone, PartialEq, Debug)]
pub enum Declaration {
    Function(Function),
    Property(Property),
    Class(Class),
    TypeAlias(TypeAlias),
    Object(Object),
}

#[derive(Clone, PartialEq, Debug)]
pub struct TypeAlias {
    pub name: String,
    pub type_parameters: Vec<TypeParameter>,
    pub ty: Type,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Object {}

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
    pub default_value: Option<Expr>,
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
    pub annotations: Vec<Annotation>,
    pub reference: Arc<TypeReference>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum TypeReference {
    Function(FunctionType),
    UserType(Vec<SimpleUserType>),
    Nullable(Arc<TypeReference>),
}

#[derive(Clone, PartialEq, Debug)]
pub struct SimpleUserType {
    pub name: String,
    pub type_params: Vec<CallSiteTypeParams>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum CallSiteTypeParams {
    Projection,
    // * -> List<*>
    Type(Type), // Int -> List<Int>
}

#[derive(Clone, PartialEq, Debug)]
pub struct FunctionType {
    pub receiver: Option<Arc<TypeReference>>,
    pub parameters: Vec<Type>,
    pub return_type: Arc<TypeReference>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum ClassType {
    Class,
    Interface,
    Enum,
    Annotation,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Class {
    pub modifiers: Vec<Modifier>,
    pub class_type: ClassType,
    pub name: String,
    pub type_parameters: Vec<TypeParameter>,
    pub primary_constructor: Option<PrimaryConstructor>,
    pub annotations: Vec<Annotation>,
    pub delegations: Vec<()>,
    pub type_constraints: Vec<TypeConstraint>,
    pub body: Option<ClassBody>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ClassBody {
    pub enum_entries: Option<Vec<()>>,
    pub members: Vec<Statement>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct PrimaryConstructor {
    pub modifiers: Vec<Modifier>,
    pub params: Vec<FunctionParameter>
}

#[derive(Clone, PartialEq, Debug)]
pub enum DelegationSpecifier {
    Type(Type),
    DelegatedBy(Type, Expr),
    FunctionCall(Type)
}