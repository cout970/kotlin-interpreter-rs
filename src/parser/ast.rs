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
pub enum ClassType {
    Class,
    Interface,
}

#[derive(Clone, PartialEq, Debug)]
pub enum TopLevelObject {
    Class(Class),
    Object,
    Function(Function),
    Property,
    TypeAlias,
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
    Block,
    Expression,
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
pub struct Class {
    pub modifiers: Vec<Modifier>,
    pub class_type: ClassType,
    pub name: String,
//    type_parameters: Vec<Type>,
//    primary_constructor: Option<PrimaryConstructor>,
}