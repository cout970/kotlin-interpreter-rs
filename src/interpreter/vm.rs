use std::borrow::Borrow;
use std::cell::Ref;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::interpreter::bytecode::CompiledFile;
use crate::interpreter::bytecode::CompiledClass;
use crate::interpreter::bytecode::Constant;
use crate::interpreter::bytecode::CompiledField;
use crate::interpreter::bytecode::Instruction;
use crate::interpreter::bytecode::CompiledBlock;

type Obj = Rc<RefCell<RuntimeObj>>;

#[derive(Clone, PartialEq)]
enum Value {
    Ref(Obj),
    Null,
    Array(Vec<Value>),
    Boolean(bool),
    Double(f64),
    Float(f32),
    Byte(i8),
    Short(i16),
    Int(i32),
    Long(i64),
    Char(char),
    ArrayUTF8(String),
}

#[derive(Clone, PartialEq)]
struct RuntimeObj {
    class: String,
    fields: HashMap<String, Value>,
}


struct VM {
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
    code: CompiledFile,
}

#[derive(Debug)]
enum RuntimeError {
    MissingFunction,
    InvalidArgumentCount,
    StackUnderflow,
    MissingField,
    NullPointerException,
    MissingReturn,
    ClassCastException,
    ClassNotFound,
}

impl VM {
    fn new(file: CompiledFile) -> Self {
        let mut vm = VM {
            stack: vec![],
            globals: HashMap::new(),
            code: file,
        };

        vm.register_basics();
        vm
    }

    fn register_basics(&mut self) {
        // Boolean
        let boolean = CompiledClass {
            name: "Boolean".to_owned(),
            super_class: "java.lang.Any".to_owned(),
            interfaces: vec![],
            fields: vec![],
        };

        self.code.classes.insert("java.lang.Boolean".to_owned(), Rc::new(boolean));
    }
}

impl RuntimeObj {
    fn new(class: &CompiledClass) -> Obj {
        let mut fields = HashMap::new();

        for field_name in &class.fields {
            fields.insert(field_name.clone(), Value::Null);
        }

        Rc::new(RefCell::new(RuntimeObj {
            class: class.name.to_owned(),
            fields,
        }))
    }

    fn new_string(content: &str) -> Obj {
        let mut fields = HashMap::new();

        fields.insert("chars".to_owned(), Value::ArrayUTF8(content.to_owned()));

        Rc::new(RefCell::new(RuntimeObj {
            class: "java.lang.String".to_owned(),
            fields,
        }))
    }
}

fn run(file: CompiledFile) -> Result<Option<Value>, RuntimeError> {
    let ref mut vm = VM::new(file);

    call_func(vm, "main", &vec![])
}

fn call_func(vm: &mut VM, func_name: &str, args: &Vec<Value>) -> Result<Option<Value>, RuntimeError> {
    let func = vm.code.functions.get(func_name).ok_or(RuntimeError::MissingFunction)?;

    if func.arg_count != args.len() as u32 {
        return Err(RuntimeError::InvalidArgumentCount);
    }

    for x in args {
        vm.stack.push(x.clone());
    }
    run_block(vm, func.block.clone())?;

    Ok(vm.stack.pop())
}

fn get_ref(obj: &Obj) -> Ref<RuntimeObj> {
    let obj_ref: &RefCell<RuntimeObj> = obj.borrow();
    let obj_ref: Ref<RuntimeObj> = obj_ref.borrow();
    return obj_ref;
}

fn constant_compare(constant: &Constant, value: &Value) -> bool {
    match value {
        Value::Ref(obj) => {
            if let Constant::String(s) = constant {
                let obj_ref = get_ref(obj);
                if obj_ref.class == "String" && obj_ref.fields["chars"] == Value::ArrayUTF8(s.clone()) {
                    true
                } else {
                    false
                }
            } else {
                false
            }
        }
        Value::ArrayUTF8(string) => {
            if let Constant::String(s) = constant {
                string == s
            } else {
                false
            }
        }
        Value::Null => {
            &Constant::Null == constant
        }
        Value::Array(first) => {
            if let Constant::Array(second) = constant {
                if first.len() == second.len() {
                    for i in 0..first.len() {
                        if constant_compare(&second[i], &first[i]) {
                            return false;
                        }
                    }
                    true
                } else { false }
            } else {
                false
            }
        }
        Value::Boolean(first) => {
            if let Constant::Boolean(second) = constant { *first == *second } else { false }
        }
        Value::Double(first) => {
            if let Constant::Double(second) = constant { *first == *second } else { false }
        }
        Value::Float(first) => {
            if let Constant::Float(second) = constant { *first == *second } else { false }
        }
        Value::Byte(first) => {
            if let Constant::Byte(second) = constant { *first == *second } else { false }
        }
        Value::Short(first) => {
            if let Constant::Short(second) = constant { *first == *second } else { false }
        }
        Value::Int(first) => {
            if let Constant::Int(second) = constant { *first == *second } else { false }
        }
        Value::Long(first) => {
            if let Constant::Long(second) = constant { *first == *second } else { false }
        }
        Value::Char(first) => {
            if let Constant::Char(second) = constant { *first == *second } else { false }
        }
    }
}

fn constant_to_value(constant: &Constant) -> Value {
    match constant {
        Constant::Null => Value::Null,
        Constant::Array(it) => {
            let values: Vec<Value> = it.iter().map(constant_to_value).collect();
            Value::Array(values)
        }
        Constant::Boolean(it) => Value::Boolean(*it),
        Constant::Double(it) => Value::Double(*it),
        Constant::Float(it) => Value::Float(*it),
        Constant::Byte(it) => Value::Byte(*it),
        Constant::Short(it) => Value::Short(*it),
        Constant::Int(it) => Value::Int(*it),
        Constant::Long(it) => Value::Long(*it),
        Constant::Char(it) => Value::Char(*it),
        Constant::String(it) => {
            Value::Ref(RuntimeObj::new_string(it))
        }
    }
}

fn run_block(vm: &mut VM, block: Rc<CompiledBlock>) -> Result<(), RuntimeError> {
    let mut pc: i32 = 0;

    while pc >= 0 && pc < block.instructions.len() as i32 {
        match &block.instructions[pc as usize] {
            Instruction::Load(val) => {
                vm.stack.push(constant_to_value(val));
            }
            Instruction::Read(name) => {
                let field = vm.code.fields.get(name)
                    .ok_or(RuntimeError::MissingField)?;

                let val = match Rc::borrow(field) {
                    CompiledField::Member(name) => {
                        let obj = vm.stack.pop().ok_or(RuntimeError::StackUnderflow)?;

                        match obj {
                            Value::Ref(obj) => {
                                get_ref(&obj).fields[name].clone()
                            }
                            Value::Null => {
                                return Err(RuntimeError::NullPointerException);
                            }
                            _ => {
                                return Err(RuntimeError::ClassCastException);
                            }
                        }
                    }
                    CompiledField::Static => {
                        vm.globals.get(name).ok_or(RuntimeError::MissingField)?.clone()
                    }
                };

                vm.stack.push(val);
            }
            Instruction::Write(name) => {
                let field = vm.code.fields.get(name)
                    .ok_or(RuntimeError::MissingField)?;

                let val = vm.stack.pop().ok_or(RuntimeError::StackUnderflow)?;

                match Rc::borrow(field) {
                    CompiledField::Member(name) => {
                        let obj = vm.stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                        match obj {
                            Value::Ref(obj) => {
                                obj.borrow_mut().fields.insert(name.clone(), val);
                            }
                            Value::Null => {
                                return Err(RuntimeError::NullPointerException);
                            }
                            _ => {
                                return Err(RuntimeError::ClassCastException);
                            }
                        }
                    }
                    CompiledField::Static => {
                        vm.globals.get(name).ok_or(RuntimeError::MissingField)?;
                        let ptr = vm.globals.get_mut(name).unwrap();
                        *ptr = val;
                    }
                }
            }
            Instruction::JumpIf(offset) => {
                let val = vm.stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                match val {
                    Value::Ref(obj) => {
                        let obj_ref = get_ref(&obj);

                        if obj_ref.class == "Boolean" {
                            if obj_ref.fields["value"] == Value::Boolean(true) {
                                pc += *offset;
                            }
                        } else {
                            return Err(RuntimeError::ClassCastException);
                        }
                    }
                    Value::Boolean(it) => {
                        if it {
                            pc += *offset;
                        }
                    }
                    Value::Null => {
                        return Err(RuntimeError::NullPointerException);
                    }
                    _ => {
                        return Err(RuntimeError::ClassCastException);
                    }
                }
            }
            Instruction::Jump(offset) => {
                pc += *offset;
            }
            Instruction::Call(name) => {
                let func = vm.code.functions.get(name).ok_or(RuntimeError::MissingFunction)?;

                if (vm.stack.len() as u32) < func.arg_count {
                    return Err(RuntimeError::StackUnderflow);
                }

                run_block(vm, func.block.clone())?;
            }
            Instruction::New(class) => {
                let class = vm.code.classes.get(class).ok_or(RuntimeError::ClassNotFound)?;
                let val = Value::Ref(RuntimeObj::new(class));
                vm.stack.push(val);
            }
            Instruction::IsOfType(signature) => {
                let obj = vm.stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                let is = match obj {
                    Value::Ref(obj) => {
                        signature == &get_ref(&obj).class
                    }
                    Value::Null => {
                        return Err(RuntimeError::NullPointerException);
                    }
                    _ => {
                        return Err(RuntimeError::ClassCastException);
                    }
                };

                vm.stack.push(Value::Boolean(is))
            }
            Instruction::Return => {
                return Ok(());
            }
        }
    }

    Err(RuntimeError::MissingReturn)
}


#[cfg(test)]
mod tests {

    use super::*;
    use crate::interpreter::bytecode::CompiledFunction;

    #[test]
    fn basic_run() {
        let mut file = CompiledFile {
            fields: HashMap::new(),
            classes: HashMap::new(),
            functions: HashMap::new(),
            constants: HashMap::new(),
        };

        file.functions.insert("main".to_owned(), Rc::new(CompiledFunction {
            arg_count: 0,
            block: Rc::new(CompiledBlock { instructions: vec![Instruction::Return] }),
        }));

        let _result = run(file).unwrap();
    }
}