// correct modifiers
// non repeated names
// has type
// type is known
// init exp has same type
// has override if needed
// follows the delegation rules
//

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::analyzer::typechecker::TypeChecker;
use crate::analyzer::typechecker::TypeInfo;
use crate::errors::AnalyserError;
use crate::errors::KtError;
use crate::interpreter::bytecode::CompiledBlock;
use crate::interpreter::bytecode::CompiledClass;
use crate::interpreter::bytecode::CompiledField;
use crate::interpreter::bytecode::CompiledFile;
use crate::interpreter::bytecode::CompiledFunction;
use crate::interpreter::bytecode::Constant;
use crate::interpreter::bytecode::Instruction;
use crate::Number;
use crate::parser::parse_tree::Class;
use crate::parser::parse_tree::ClassType;
use crate::parser::parse_tree::DelegationSpecifier;
use crate::parser::parse_tree::Expr;
use crate::parser::parse_tree::KotlinFile;
use crate::parser::parse_tree::TopLevelObject;
use crate::parser::parse_tree::Type;
use crate::source_code::SourceCode;

struct Context {
    file: KotlinFile,
    env: Vec<EnvBlock>,
    code: SourceCode,
}

struct EnvBlock {
    types: HashMap<String, TypeInfo>,
    block_type: EnvBlockType,

}

enum EnvBlockType {
    Function,
    Loop,
    Lambda,
}

struct Compiler {
    ctx: Context,
    constants: HashMap<String, Constant>,
    fields: HashMap<String, Rc<CompiledField>>,
    classes: HashMap<String, Rc<CompiledClass>>,
    functions: HashMap<String, Rc<CompiledFunction>>,
    errors: Vec<KtError>,
}

pub fn compile_file(code: SourceCode, file: KotlinFile, expr: &Expr) -> Result<CompiledFile, Vec<KtError>> {
    let ctx = Context { file, code, env: vec![] };

    let mut compiler = Compiler {
        ctx,
        constants: HashMap::new(),
        fields: HashMap::new(),
        classes: HashMap::new(),
        functions: HashMap::new(),
        errors: vec![],
    };

//    for obj in &file.objects {
//        compile_top_level_object(&mut compiler, obj);
//    }

    compile_expr(&mut compiler, expr);

    if compiler.errors.is_empty() {
        Ok(CompiledFile {
            constants: compiler.constants,
            fields: compiler.fields,
            classes: compiler.classes,
            functions: compiler.functions,
        })
    } else {
        Err(compiler.errors)
    }
}

fn compile_expr(comp: &mut Compiler, expr: &Expr) -> PseudoBlock {
    let mut insts = vec![];
    match expr {
        Expr::Chain { .. } => {}
        Expr::InfixFun { .. } => {}
        Expr::Prefix { .. } => {}
        Expr::Postfix { .. } => {}
        Expr::Is { .. } => {}
        Expr::String(val) => {}
        Expr::If { .. } => {}
        Expr::Try { .. } => {}
        Expr::For { .. } => {}
        Expr::While { .. } => {}
        Expr::DoWhile { .. } => {}
        Expr::When { .. } => {}
        Expr::Object { .. } => {}
        Expr::CallableRef { .. } => {}
        Expr::Lambda(_) => {

        }
        Expr::Ref(name) => {
            insts.push(PseudoInstruction::LoadVar(name.clone()));
        }
        Expr::Boolean(val) => {
            insts.push(PseudoInstruction::Load(Constant::Boolean(*val)))
        }
        Expr::Char(val) => {
            insts.push(PseudoInstruction::Load(Constant::Char(*val)))
        }
        Expr::Number(num) => {
            let constant = match num {
                Number::Double(val) => Constant::Double(*val),
                Number::Float(val) => Constant::Float(*val),
                Number::Byte(val) => Constant::Byte(*val),
                Number::Short(val) => Constant::Short(*val),
                Number::Int(val) => Constant::Int(*val),
                Number::Long(val) => Constant::Long(*val),
            };
            insts.push(PseudoInstruction::Load(constant));
        }
        Expr::Null => {
            insts.push(PseudoInstruction::Load(Constant::Null));
        }
        Expr::This => {
            insts.push(PseudoInstruction::LoadVar("this".to_owned()));
        }
        Expr::Return(opt) => {
            if let Some(it) = opt {
                insts.push(PseudoInstruction::Run(compile_expr(comp, &it.1)));
            }
            insts.push(PseudoInstruction::Return);
        }
        Expr::Super => {
            insts.push(PseudoInstruction::LoadVar("super".to_owned()));
        }
        Expr::Throw(_) => {
            // TODO
        }
        Expr::Continue => {

        }
        Expr::Break => {

        }
    }

    unimplemented!()
}

struct PseudoBlock {
    instructions: Vec<PseudoInstruction>
}

enum PseudoInstruction {
    Load(Constant),
    Run(PseudoBlock),
    Jump(u32, RefCell<u32>),
    LoadVar(String),
    Return,
    Break(u32),
    Continue(u32),
}

//pub enum AstExpr {
//    Call(String, Vec<AstExpr>),
//    ReadField(String, AstExpr),
//    WriteField(String, AstExpr, AstExpr),
//    Is(AstExpr, Type),
//    If {
//        cond: AstExpr,
//        if_true: AstExpr,
//        if_false: Option<AstExpr>,
//    },
//    For {
//        variables: Vec<String>,
//        expr: AstExpr,
//        body: AstExpr,
//    },
//    While {
//        expr: AstExpr,
//        body: AstExpr,
//    },
//    DoWhile {
//        expr: AstExpr,
//        body: AstExpr,
//    },
//    Ref(String),
//    Boolean(bool),
//    Char(char),
//    Number(Number),
//    Null,
//    This,
//    Super,
//    Throw(AstExpr),
//    Return(Option<AstExpr>),
//    Continue,
//    Break,
//    Block(Vec<AstExpr>)
//}

//
//fn compile_top_level_object(cmp: &mut Compiler, obj: &TopLevelObject) {
//    match obj {
//        TopLevelObject::Class(ast) => {
//            compile_class(cmp, ast);
//        }
//        TopLevelObject::Object(_) => {}
//        TopLevelObject::Function(_) => {}
//        TopLevelObject::Property(_) => {}
//        TopLevelObject::TypeAlias(_) => {
//            // Ignored
//        }
//    }
//}
//
//fn compile_class(cmp: &mut Compiler, ast: &Class) {
//    let class_ref = cmp.ctx.file.get_package_str() + "." + &ast.name;
//    let (super_class, interfaces, fields) = resolve_class_delegations(cmp, ast);
//
//    cmp.classes.insert(class_ref, Rc::new(CompiledClass {
//        name: ast.name.to_owned(),
//        super_class,
//        interfaces,
//        fields,
//    }));
//}
//
//fn resolve_class_delegations(cmp: &mut Compiler, ast: &Class) -> (String, Vec<String>, Vec<String>) {
//    let mut super_class = "Object";
//    let mut interfaces = vec![];
//    let mut fields = vec![];
//    let mut super_class_count = 0;
//
//    for del in &ast.delegations {
//        match del {
//            DelegationSpecifier::Type(ty) => {
//                resolve_class_delegation_type(cmp, ast, ty, &mut interfaces);
//            }
//            DelegationSpecifier::DelegatedBy(ty, expr) => {
////                resolve_class_delegation_type(cmp, ast, ty, &mut super_class, &mut interfaces)
//            }
//            DelegationSpecifier::FunctionCall(_, _) => {
////                super_class_count += 1;
////
////                // Error extending multiple classes
////                if super_class_count > 1 {
////                    cmp.errors.push(KtError::Analyser {
////                        code: cmp.types.code.clone(),
////                        span: ty.span,
////                        info: AnalyserError::MultipleInheritance,
////                    })
////                }
//            }
//        }
//    }
//
//    (super_class.to_owned(), interfaces, fields)
//}
//
//fn resolve_class_delegation_type(cmp: &mut Compiler, ast: &Class, ty: &Type, interfaces: &mut Vec<String>) {
//    let info = cmp.types.get_class_info(ty);
//
//    match info {
//        None => {
//            cmp.errors.push(KtError::Analyser {
//                code: cmp.types.code.clone(),
//                span: ty.span,
//                info: AnalyserError::ExtendingNonClass,
//            });
//        }
//        Some(info) => {
//            if info.class_type == ClassType::Class {
//                cmp.errors.push(KtError::Analyser {
//                    code: cmp.types.code.clone(),
//                    span: ty.span,
//                    info: AnalyserError::MissingConstructorCall,
//                });
//            } else if info.class_type == ClassType::Interface {
//                // TODO, duplicated interface?, incompatible? invalid type parameters?
//                interfaces.push(info.reference.to_owned());
//            } else {
//                cmp.errors.push(KtError::Analyser {
//                    code: cmp.types.code.clone(),
//                    span: ty.span,
//                    info: AnalyserError::ExtendingFinalClass,
//                });
//            }
//        }
//    }
//}