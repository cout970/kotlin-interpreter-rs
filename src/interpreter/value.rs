use std::sync::Arc;

use crate::Number;
use std::collections::HashMap;

//pub enum ValueDescriptor {
//    // Byte
//    B,
//    // Character
//    C,
//    // Double
//    D,
//    // Float
//    F,
//    // Integer
//    I,
//    // Long
//    J,
//    // Short
//    S,
//    // Boolean
//    Z,
//    // Void
//    V,
//    // Object reference
//    L(String),
//    // [ Array
//    Array(Box<ValueDescriptor>),
//}

pub type ObjRef = Arc<ObjectInstance>;

#[derive(PartialEq, Clone, Debug)]
pub enum Value {
    Number(Number),
    Array(Vec<Value>),
    Object(ObjRef),
}

#[derive(PartialEq, Clone, Debug)]
pub struct ObjectInstance {
    pub class: Arc<ClassInstance>,
    pub fields: HashMap<String, Value>
}

#[derive(PartialEq, Clone, Debug)]
pub struct ClassInstance {
    pub fields: Vec<ClassMember>,
    pub methods: Vec<ClassMember>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct ClassMember {
    pub name: String,
    pub descriptor: String,
}

