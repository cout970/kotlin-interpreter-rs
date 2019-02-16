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

use itertools::Itertools;
use rand::Rng;

use crate::analyzer::ast::*;
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
use crate::parser::parse_tree::*;
use crate::source_code::SourceCode;
use crate::source_code::Span;

struct EnvBlock {
    types: HashMap<String, TypeInfo>,
    block_type: EnvBlockType,
}

enum EnvBlockType {
    Function,
    Loop,
    Lambda,
}

struct Env {
    blocks: Vec<EnvBlock>
}

struct Compiler {
    file: KotlinFile,
    env: Env,
    code: SourceCode,
    next_id: u32,
    type_codes: HashMap<String, String>,
    constants: HashMap<String, Constant>,
    fields: HashMap<String, Rc<CompiledField>>,
    classes: HashMap<String, Rc<CompiledClass>>,
    functions: HashMap<String, Rc<CompiledFunction>>,
    errors: Vec<KtError>,
}

struct FuncSignature {
    class: Option<String>,
    receiver: Option<String>,
    args: Vec<String>,
    return_ty: String,
    // metadata
    vararg: Option<u32>,
    arg_names: Vec<String>,
}

/*
Name: sum
Signature: (Int, Int) -> Int
Named Args: a, b,

fun sum(a: Int, b: Int) = a + b
*/


fn compile_function(ctx: &mut Compiler, fun: &AstFunction) {
    let receiver = fun.return_ty.as_ref().map(|ty| &ty.name).cloned();

    let args = fun.args.iter().map(|arg| {
        if let Some(ty) = &arg.ty {
            check_type(ctx, ty)
        } else {
            ctx.new_analyser_error(fun.span, AnalyserError::MissingFunctionParameterType);
            "_error_".to_string()
        }
    }).collect_vec();

    let arg_names = fun.args.iter().map(|arg| &arg.name).cloned().collect_vec();

    let return_ty = if let Some(body) = &fun.body {
        infer_type(ctx, fun.return_ty.as_ref(), body)
    } else {
        if let Some(ret) = &fun.return_ty {
            check_type(ctx, ret)
        } else {
            ctx.new_analyser_error(fun.span, AnalyserError::MissingReturnType);
            "_error_".to_string()
        }
    };

    let signature = FuncSignature {
        class: None,
        receiver,
        args,
        return_ty,
        vararg: None,
        arg_names,
    };

    let body = fun.body.as_ref().map(|e| compile_expr(ctx, e, &fun.name));

    ctx.env.add_function(&fun.name, signature, body);
}

fn compile_expr(ctx: &mut Compiler, expr: &AstExpr, prefix: &str) -> String {
    unimplemented!()
}

fn check_type(ctx: &mut Compiler, ty: &AstType) -> String {
    if let Some(_) = ctx.env.find_type(&ty.full_name) {
        let code = generate_rand_str();
        ctx.type_codes.insert(code.clone(), ty.full_name.to_string());
        return code;
    }

    ctx.new_analyser_error(ty.span, AnalyserError::UnresolvedReference(ty.name.to_string()));

    "_error_".to_string()
}

fn infer_type(ctx: &mut Compiler, ty: Option<&AstType>, block: &AstExpr) -> String {
    generate_rand_str()
//    let name = match block {
//        AstExpr::Constant { value, .. } => {
//            match value {
//                Constant::Null => "kotlin.Nothing",
//                Constant::Array(_) => "[",
//                Constant::Boolean(_) => "java.lang.Boolean",
//                Constant::Double(_) => "java.lang.Double",
//                Constant::Float(_) => "java.lang.Float",
//                Constant::Byte(_) => "java.lang.Byte",
//                Constant::Short(_) => "java.lang.Short",
//                Constant::Int(_) => "java.lang.Integer",
//                Constant::Long(_) => "java.lang.Long",
//                Constant::Char(_) => "java.lang.Character",
//                Constant::String(_) => "java.lang.String",
//            }
//        },
//        AstExpr::Block { .. } => {
//            // type of last expresion
//        },
//        AstExpr::Ref { .. } => {
//            // type of variable
//        },
//        AstExpr::InvokeStatic { .. } => {
//            // return of function
//        },
//        AstExpr::InvokeDynamic { .. } => {
//            // return of function
//        },
//        AstExpr::ReadField { .. } => {
//            // type of field
//        },
//        AstExpr::WriteRef { .. } => {
//            panic!()
//            // is this an expresion?
//        },
//        AstExpr::Is { .. } => {
//            "java.lang.Boolean"
//        },
//        AstExpr::If { if_true, .. } => {
//            infer_type(ctx, ty, &if_true.borrow())
//        },
//        AstExpr::For { .. } => {
//            panic!()
//        },
//        AstExpr::While { .. } => {
//            panic!()
//        },
//        AstExpr::DoWhile { .. } => {
//            panic!()
//        },
//        AstExpr::Continue { .. } => {
//            "kotlin.Unit"
//        },
//        AstExpr::Break { .. } => {
//            "kotlin.Unit"
//        },
//        AstExpr::Try { .. } => {
//            panic!()
//        },
//        AstExpr::Throw { .. } => {
//            "kotlin.Nothing"
//        },
//        AstExpr::Return { .. } => {
//            "kotlin.Unit"
//        },
//    };
//
//    String::from(name)
}

fn generate_rand_str() -> String {
    let mut rng = rand::thread_rng();
    let mut data = String::new();
    let digits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'];

    for _ in 0..8 {
        let d: i32 = rng.gen_range(0, 16);
        data.push(digits[d as usize]);
    }

    data
}


impl Env {
    fn add_function(&mut self, name: &str, signature: FuncSignature, body: Option<String>) {}

    fn last_block(&mut self) -> &mut EnvBlock {
        self.blocks.last_mut().unwrap()
    }

    fn find_type(&mut self, name: &str) -> Option<()> {
        None
    }
}

impl Compiler {
    fn new_analyser_error(&mut self, span: Span, info: AnalyserError) {
        self.errors.push(KtError::Analyser { code: self.code.clone(), span, info });
    }
}

//pub fn compile_file(code: SourceCode, file: KotlinFile, expr: &Expr) -> Result<CompiledFile, Vec<KtError>> {
//    let ctx = Context { file, code, env: vec![] };
//
//    let compiler = Compiler {
//        ctx,
//        constants: HashMap::new(),
//        fields: HashMap::new(),
//        classes: HashMap::new(),
//        functions: HashMap::new(),
//        errors: vec![],
//    };
//
//    for obj in &file.objects {
//        compile_top_level_object(&mut compiler, obj);
//    }
//
//    compile_expr(&mut compiler, expr);
//
//    if compiler.errors.is_empty() {
//        Ok(CompiledFile {
//            constants: compiler.constants,
//            fields: compiler.fields,
//            classes: compiler.classes,
//            functions: compiler.functions,
//        })
//    } else {
//        Err(compiler.errors)
//    }
//}

//fn compile_expr(comp: &mut Compiler, expr: &Expr) -> PseudoBlock {
//    let mut insts = vec![];
//    match expr {
//        Expr::Chain { .. } => {}
//        Expr::InfixFun { .. } => {}
//        Expr::Prefix { .. } => {}
//        Expr::Postfix { .. } => {}
//        Expr::Is { .. } => {}
//        Expr::String(val) => {}
//        Expr::If { .. } => {}
//        Expr::Try { .. } => {}
//        Expr::For { .. } => {}
//        Expr::While { .. } => {}
//        Expr::DoWhile { .. } => {}
//        Expr::When { .. } => {}
//        Expr::Object { .. } => {}
//        Expr::CallableRef { .. } => {}
//        Expr::Lambda(_) => {
//
//        }
//        Expr::Ref(name) => {
//            insts.push(PseudoInstruction::LoadVar(name.clone()));
//        }
//        Expr::Boolean(val) => {
//            insts.push(PseudoInstruction::Load(Constant::Boolean(*val)))
//        }
//        Expr::Char(val) => {
//            insts.push(PseudoInstruction::Load(Constant::Char(*val)))
//        }
//        Expr::Number(num) => {
//            let constant = match num {
//                Number::Double(val) => Constant::Double(*val),
//                Number::Float(val) => Constant::Float(*val),
//                Number::Byte(val) => Constant::Byte(*val),
//                Number::Short(val) => Constant::Short(*val),
//                Number::Int(val) => Constant::Int(*val),
//                Number::Long(val) => Constant::Long(*val),
//            };
//            insts.push(PseudoInstruction::Load(constant));
//        }
//        Expr::Null => {
//            insts.push(PseudoInstruction::Load(Constant::Null));
//        }
//        Expr::This => {
//            insts.push(PseudoInstruction::LoadVar("this".to_owned()));
//        }
//        Expr::Return(opt) => {
//            if let Some(it) = opt {
//                insts.push(PseudoInstruction::Run(compile_expr(comp, &it.1)));
//            }
//            insts.push(PseudoInstruction::Return);
//        }
//        Expr::Super => {
//            insts.push(PseudoInstruction::LoadVar("super".to_owned()));
//        }
//        Expr::Throw(_) => {
//            // TODO
//        }
//        Expr::Continue => {
//
//        }
//        Expr::Break => {
//
//        }
//    }
//
//    unimplemented!()
//}

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