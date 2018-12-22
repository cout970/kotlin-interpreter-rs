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
    Function,
    Property,
    TypeAlias,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Class {
    pub modifiers: u32,
    pub class_type: ClassType,
    pub name: String,
//    type_parameters: Vec<Type>,
//    primary_constructor: Option<PrimaryConstructor>,
}