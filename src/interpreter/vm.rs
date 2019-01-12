use std::collections::HashMap;

use crate::interpreter::value::Value;
use crate::Number;

struct CodeBlock {
    labels: HashMap<String, u32>,
    opcodes: Vec<Opcode>,
    param_names: Vec<String>,
}

pub enum Opcode {
    Call { dst: String, fun: String, args: Vec<String> },
    Is { dst: String, src: String, ty: String },
    // If expression
    If { cond: String, label: String },
    // Inconditional jump
    Jump { label: String },
    // Read value from variable
    Move { src: String, dst: String },
    // Write value to variable
    Var { name: String, value: Value },
    // Return content of a variable
    Return { name: Option<String> },
}

#[test]
fn vm_test() {
    let block = CodeBlock {
        labels: HashMap::new(),
        opcodes: vec![
            Opcode::Var { name: String::from("a"), value: Value::Number(Number::Int(42)) },
            Opcode::Call { dst: String::from(""), fun: String::from("println"), args: vec![String::from("a")] }
        ],
        param_names: vec![],
    };
    vm_run(&block, vec![]);
    panic!();
}

fn vm_run(block: &CodeBlock, args: Vec<Value>) -> Option<Value> {
    let mut pc: u32 = 0;
    let mut variables: HashMap<String, Value> = HashMap::new();

    for (name, value) in block.param_names.iter().zip(args) {
        variables.insert(name.clone(), value);
    }

    while pc < (block.opcodes.len() as u32) {
        match &block.opcodes[pc as usize] {
            Opcode::Call { dst, fun, args } => {
                println!("Calling function '{}' with parameters: {:?}", fun, args);
            }
            Opcode::Is { dst, src, ty } => {
                println!("Is '{}' of type '{}'?, result in {}", src, ty, dst);
            }
            Opcode::If { cond, label } => {
                if let Value::Number(num) = variables.get(cond).unwrap() {
                    let value = match num {
                        Number::Double(it) => { *it == 0.0 }
                        Number::Float(it) => { *it == 0.0 }
                        Number::Byte(it) => { *it == 0 }
                        Number::Short(it) => { *it == 0 }
                        Number::Int(it) => { *it == 0 }
                        Number::Long(it) => { *it == 0 }
                    };

                    if value {
                        pc = *block.labels.get(label).unwrap();
                    }
                } else {
                    panic!("Invalid if condition");
                }
            }
            Opcode::Jump { label } => {
                pc = *block.labels.get(label).unwrap();
            }
            Opcode::Move { src, dst } => {
                variables.insert(dst.clone(), variables.get(src).unwrap().clone());
            }
            Opcode::Var { name, value } => {
                variables.insert(name.clone(), value.clone());
            }
            Opcode::Return { name } => {
                match name {
                    Some(it) => {
                        return variables.get(it).map(|it| it.clone());
                    }
                    None => {
                        return None;
                    }
                }
            }
        }
        pc += 1;
    }

    return None;
}