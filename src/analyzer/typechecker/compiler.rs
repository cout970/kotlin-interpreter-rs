// correct modifiers
// non repeated names
// has type
// type is known
// init exp has same type
// has override if needed
// follows the delegation rules
//

use std::collections::HashMap;
use std::rc::Rc;

use crate::analyzer::typechecker::TypeChecker;
use crate::errors::KtError;
use crate::interpreter::bytecode::CompiledClass;
use crate::interpreter::bytecode::CompiledField;
use crate::interpreter::bytecode::CompiledFile;
use crate::interpreter::bytecode::CompiledFunction;
use crate::interpreter::bytecode::Constant;
use crate::parser::ast::KotlinFile;

#[derive()]
struct Compiler<'t> {
    types: &'t mut TypeChecker,
    constants: HashMap<String, Constant>,
    fields: HashMap<String, Rc<CompiledField>>,
    classes: HashMap<String, Rc<CompiledClass>>,
    functions: HashMap<String, Rc<CompiledFunction>>,
}

pub fn compile_file(checker: &mut TypeChecker, file: &KotlinFile) -> Result<CompiledFile, KtError>{
    let mut compiler = Compiler{
        types: checker,
        constants: HashMap::new(),
        fields: HashMap::new(),
        classes: HashMap::new(),
        functions: HashMap::new(),
    };

    Ok(CompiledFile{
        constants: compiler.constants,
        fields: compiler.fields,
        classes: compiler.classes,
        functions: compiler.functions,
    })
}