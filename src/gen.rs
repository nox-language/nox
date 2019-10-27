use std::collections::HashMap;

use rlvm::{
    Builder,
    CodeGenFileType,
    CodeGenOptLevel,
    CodeModel,
    Module,
    FunctionPassManager,
    RelocMode,
    Target,
    Value,
    VerifierFailureAction,
    get_default_target_triple,
};
use rlvm::module::Function;
use rlvm::types::{self, Type};
use rlvm::value::constant;

use crate::ast::{
    Declaration,
    Declarations,
    Expr,
    Expr::{FunctionCall, Str},
    FunctionDeclaration,
    Parameter,
};
use crate::error::Result;

pub fn generate(declarations: Declarations) -> Result<String> {
    let object_file = "main.o".to_string();

    let target_triple = get_default_target_triple();
    let target = Target::get_from_triple(&target_triple).expect("get target");

    let target_machine = target.create_target_machine(&target_triple, "generic", "", CodeGenOptLevel::Aggressive, RelocMode::Default, CodeModel::Default);

    let module = Module::new_with_name("module");
    let pass_manager = FunctionPassManager::new_for_module(&module);
    pass_manager.add_promote_memory_to_register_pass();
    pass_manager.add_instruction_combining_pass();
    pass_manager.add_reassociate_pass();
    pass_manager.add_gvn_pass();
    pass_manager.add_cfg_simplification_pass();
    module.set_data_layout(target_machine.create_data_layout());
    module.set_target(target_triple);

    let mut generator = Generator::new(module, pass_manager)?;

    for declaration in declarations {
        generator.declaration(declaration)?;
    }

    if let Err(error) = target_machine.emit_to_file(&generator.module, &object_file, CodeGenFileType::ObjectFile) {
        eprintln!("Cannot emit to object: {}", error);
    }

    Ok(object_file)
}

pub struct Generator {
    builder: Builder,
    module: Module,
    pass_manager: FunctionPassManager,
    values: HashMap<String, Value>,
}

impl Generator {
    pub fn new(module: Module, pass_manager: FunctionPassManager) -> Result<Self> {
        Ok(Self {
            builder: Builder::new(),
            module,
            pass_manager,
            values: HashMap::new(),
        })
    }

    fn create_argument_allocas(&mut self, function: &Function, parameters: &[Parameter]) {
        for (index, parameter) in parameters.iter().enumerate() {
            let arg = function.get_param(index);
            let alloca = self.create_entry_block_alloca(function, &parameter.name.to_string()); // TODO: get name from symbol table.
            self.builder.store(&arg, &alloca);
            self.values.insert(parameter.name.to_string(), alloca); // TODO: get name from symbol table.
        }
    }

    fn create_entry_block_alloca(&self, function: &Function, variable_name: &str) -> Value {
        let basic_block = function.get_entry_basic_block();
        let instruction = basic_block.get_first_instruction();
        let builder = Builder::new();
        builder.position(&basic_block, &instruction);
        builder.alloca(types::double(), variable_name)
    }

    fn declaration(&mut self, declaration: Declaration) -> Result<()> {
        match declaration {
            Declaration::Function(function) => self.function(function)?,
        }
        Ok(())
    }

    fn expr(&mut self, expr: Expr) -> Result<Value> {
        let value =
            match expr {
                Str(string) => self.builder.global_string_ptr(&string, "string"),
                /*Expr::Number(num) => constant::real(types::double(), num),
                Expr::Variable(name) => {
                    match self.values.get(&name) {
                        Some(value) => self.builder.load(types::double(), value, &name),
                        None => return Err(Undefined(format!("variable {}", name))),
                    }
                },
                Expr::Binary(op, left, right) => {
                    if op == BinaryOp::Equal {
                        let name =
                            match *left {
                                Expr::Variable(ref name) => name,
                                _ => return Err(Unexpected("token, expecting variable name")),
                            };

                        let value = self.expr(*right)?;
                        let variable =
                            match self.values.get(name) {
                                Some(value) => value,
                                None => return Err(Undefined(format!("variable {}", name))),
                            };

                        self.builder.store(&value, variable);

                        return Ok(value);
                    }
                    let left = self.expr(*left)?;
                    let right = self.expr(*right)?;
                    match op {
                        BinaryOp::Plus => self.builder.fadd(&left, &right, "result"),
                        BinaryOp::Minus => self.builder.fsub(left, right, "result"),
                        BinaryOp::Times => self.builder.fmul(left, right, "result"),
                        BinaryOp::LessThan => {
                            let boolean = self.builder.fcmp(RealPredicate::UnorderedLesserThan, &left, &right, "cmptmp");
                            self.builder.unsigned_int_to_floating_point(boolean, types::double(), "booltemp")
                        },
                        BinaryOp::Custom(char) => {
                            let callee = format!("binary{}", char);
                            let callee =
                                match self.get_named_function(&callee) {
                                    Some(function) => function,
                                    None => return Err(Undefined(format!("function {}", callee))),
                                };
                            self.builder.call(callee, &[left, right], "binop")
                        },
                        BinaryOp::Equal => unreachable!(),
                    }
                },*/
                FunctionCall { name, arguments } => {
                    let func = self.get_named_function(&name.to_string()).expect("cannot find function"); // TODO: get name from symbol table.
                    assert_eq!(func.param_count(), arguments.len(), "wrong argument count");
                    let arguments: Result<Vec<_>> = arguments.into_iter().map(|arg| self.expr(arg)).collect();
                    let arguments = arguments?;
                    self.builder.call(func, &arguments, "func_call")
                },
                /*Expr::If { condition, then, else_ } => {
                    let condition = self.expr(*condition)?;
                    let condition = self.builder.fcmp(RealPredicate::OrderedNotEqual, &condition, &constant::real(types::double(), 0.0), "ifcond");

                    let start_basic_block = self.builder.get_insert_block();

                    let function = start_basic_block.get_parent();

                    let then_basic_block = BasicBlock::append(&function, "then");

                    self.builder.position_at_end(&then_basic_block);

                    let then_value = self.expr(*then)?;

                    let new_then_basic_block = self.builder.get_insert_block();

                    let else_basic_block = BasicBlock::append(&function, "else");
                    self.builder.position_at_end(&else_basic_block);

                    let else_value = self.expr(*else_)?;

                    let new_else_basic_block = self.builder.get_insert_block();

                    let merge_basic_block = BasicBlock::append(&function, "ifcont");
                    self.builder.position_at_end(&merge_basic_block);

                    let phi = self.builder.phi(types::double(), "result");
                    phi.add_incoming(&[(&then_value, &new_then_basic_block), (&else_value, &new_else_basic_block)]);

                    self.builder.position_at_end(&start_basic_block);
                    self.builder.cond_br(&condition, &then_basic_block, &else_basic_block);

                    self.builder.position_at_end(&new_then_basic_block);
                    self.builder.br(&merge_basic_block);

                    self.builder.position_at_end(&new_else_basic_block);
                    self.builder.br(&merge_basic_block);

                    self.builder.position_at_end(&merge_basic_block);

                    phi
                },
                Expr::For { body, variable_name, init_value, condition, step } => {
                    let function = self.builder.get_insert_block().get_parent();
                    let alloca = self.create_entry_block_alloca(&function, &variable_name);

                    let start_value = self.expr(*init_value)?;

                    self.builder.store(&start_value, &alloca);

                    let loop_basic_block = BasicBlock::append(&function, "loop");

                    self.builder.br(&loop_basic_block);

                    self.builder.position_at_end(&loop_basic_block);

                    let old_value = self.values.insert(variable_name.clone(), alloca.clone());

                    self.expr(*body)?;

                    let step_value =
                        match step {
                            Some(step) => self.expr(*step)?,
                            None => constant::real(types::double(), 1.0),
                        };

                    let end_condition = self.expr(*condition)?;

                    let current_variable = self.builder.load(types::double(), &alloca, &variable_name);
                    let next_variable = self.builder.fadd(&current_variable, &step_value, "nextvar");
                    self.builder.store(&next_variable, &alloca);

                    let zero = constant::real(types::double(), 0.0);
                    let end_condition = self.builder.fcmp(RealPredicate::OrderedNotEqual, &end_condition, &zero, "loopcond");

                    let after_basic_block = BasicBlock::append(&function, "afterloop");

                    self.builder.cond_br(&end_condition, &loop_basic_block, &after_basic_block);

                    self.builder.position_at_end(&after_basic_block);

                    if let Some(old_value) = old_value {
                         self.values.insert(variable_name, old_value);
                    }

                    constant::null(types::double())
                },
                Expr::Unary(operator, operand) => {
                    let operand = self.expr(*operand)?;
                    let callee = format!("unary{}", operator);
                    let callee =
                        match self.get_named_function(&callee) {
                            Some(function) => function,
                            None => return Err(Undefined(format!("function {}", callee))),
                        };
                    self.builder.call(callee, &[operand], "unop")
                },
                Expr::VariableDeclaration { body, declarations } => {
                    let mut old_bindings = vec![];

                    let function = self.builder.get_insert_block().get_parent();

                    for declaration in declarations {
                        let init_value =
                            match declaration.init_value {
                                Some(value) => self.expr(*value)?,
                                None => constant::real(types::double(), 0.0),
                            };

                        let alloca = self.create_entry_block_alloca(&function, &declaration.name);
                        self.builder.store(&init_value, &alloca);

                        if let Some(old_value) = self.values.get(&declaration.name) {
                            old_bindings.push((declaration.name.clone(), old_value.clone()));
                        }

                        self.values.insert(declaration.name.clone(), alloca);
                    }

                    let body_value = self.expr(*body)?;

                    for (variable_name, old_value) in old_bindings {
                        self.values.insert(variable_name, old_value);
                    }

                    body_value
                },*/
            };
        Ok(value)
    }

    pub fn function(&mut self, function: FunctionDeclaration) -> Result<()> {
        let function_type = types::function::new(types::int32(), &[], false);
        let llvm_function = self.add_function("main", function_type); // TODO: get name from symbol table.
        let entry = llvm_function.append_basic_block("entry");
        self.builder.position_at_end(&entry);
        self.values.clear();
        self.create_argument_allocas(&llvm_function, &function.parameters);

        let return_value =
            match self.expr(function.body) {
                Ok(value) => value,
                Err(error) => {
                    llvm_function.delete();
                    return Err(error);
                },
            };

        let function_type = types::function::new(types::int32(), &[types::int32()], false);
        let function = self.add_function("exit", function_type);
        self.builder.call(function, &[constant::int(types::int32(), 0, false)], "func_call");

        self.builder.ret(return_value);
        llvm_function.verify(VerifierFailureAction::AbortProcess);

        self.pass_manager.run(&llvm_function);

        self.module.dump();

        Ok(())
    }

    fn add_function(&self, name: &str, typ: Type) -> Function {
        self.module.add_function(&name, typ)
    }

    fn get_named_function(&mut self, name: &str) -> Option<Function> {
        // TODO: fetch from environment.
        match self.module.get_named_function(&name) {
            Some(function) => Some(function),
            None => {
                let function_type = types::function::new(types::int32(), &[types::pointer::new(types::int8(), 0)], false);
                let function = self.add_function("puts", function_type);
                Some(function)
            },
        }
    }
}
