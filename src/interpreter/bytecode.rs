use std::collections::HashMap;
use std::rc::Rc;

pub struct CompiledFile {
    pub constants: HashMap<String, Constant>,
    pub fields: HashMap<String, Rc<CompiledField>>,
    pub classes: HashMap<String, Rc<CompiledClass>>,
    pub functions: HashMap<String, Rc<CompiledFunction>>,
}

pub struct CompiledClass {
    pub name: String,
    pub interfaces: Vec<String>,
    pub super_class: String,
    pub fields: Vec<String>,
}

pub enum CompiledField {
    Member(String),
    Static,
}

pub struct CompiledFunction {
    pub arg_count: u32,
    pub block: Rc<CompiledBlock>,
}

pub struct CompiledBlock {
    pub instructions: Vec<Instruction>
}

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

pub enum Instruction {
    Load(Constant),
    Read(String),
    Write(String),
    JumpIf(i32),
    Jump(i32),
    Call(String),
    New(String),
    Return,
    IsOfType(String),
}