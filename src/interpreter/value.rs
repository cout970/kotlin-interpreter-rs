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

//pub enum Opcode {
//    Call(String),
//    InstanceOf(String),
//    // If expression
//    If(String, u32),
//    // Inconditional jump
//    Jump,
//    // Create a variable, initialize it or not
//    Def(String, bool),
//    // Read value from variable
//    Read(String),
//    // Write value to variable
//    Write(String),
//    Return,
//    // TODO
//    Try,
//    Throw,
//    Load(Value)
//}