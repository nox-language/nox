/*
 * Copyright (c) 2018-2019 Boucher, Antoni <bouanto@zoho.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

use std::mem;
use std::path::PathBuf;
use std::rc::Rc;

use rlvm::{
    BasicBlock,
    Builder,
    CodeGenFileType,
    CodeGenOptLevel,
    CodeModel,
    FunctionPassManager,
    IntPredicate,
    Module,
    ModulePassManager,
    RealPredicate,
    RelocMode,
    Target,
    Value,
    VerifierFailureAction,
    get_default_target_triple,
    module::Function,
    target::TargetMachine,
    types,
    value::constant,
};

use ast::Operator;
use symbol::{Strings, Symbol};
use tast::{
    Declaration,
    Expr,
    FuncDeclaration,
    TypedDeclaration,
    TypedVar,
    Var,
};
use types::Type;

fn to_real_op(op: Operator) -> RealPredicate {
    match op {
        Operator::Plus => unimplemented!(),
        Operator::Minus => unimplemented!(),
        Operator::Times => unimplemented!(),
        Operator::And => unimplemented!(),
        Operator::Or => unimplemented!(),
        Operator::Divide => unimplemented!(),
        _ => panic!("{:?} is not a binary operator", op),
    }
}

fn to_real_rel_op(op: Operator) -> RealPredicate {
    match op {
        Operator::Equal => unimplemented!(),
        Operator::Ge => unimplemented!(),
        Operator::Gt => unimplemented!(),
        Operator::Le => unimplemented!(),
        Operator::Lt => unimplemented!(),
        Operator::Neq => unimplemented!(),
        _ => panic!("{:?} is not a relational operator or is not used", op),
    }
}

pub fn to_llvm_type(typ: &Type) -> rlvm::types::Type {
    match *typ {
        Type::Bool => types::integer::int1(),
        Type::Int32 => types::integer::int32(), // TODO: int64?
        Type::String => types::pointer::new(types::int8(), 0),
        Type::Record(_symbol, ref _fields, _) => unimplemented!(),
        Type::Array(ref typ, size, _) => types::array::array(to_llvm_type(typ), size),
        Type::Nil => types::integer::int32(), // TODO: int64 or pointer type?
        Type::Unit => types::void(),
        Type::Name(ref _symbol, ref _type) => unimplemented!(),
        Type::Error => unreachable!("error to llvm type"),
    }
}

pub fn create_entry_block_alloca(function: &Function, variable_name: &str, typ: &Type) -> Value {
    let basic_block = function.get_entry_basic_block();
    let instruction = basic_block.get_first_instruction();
    let builder = Builder::new();
    builder.position(&basic_block, &instruction);
    builder.alloca(to_llvm_type(typ), variable_name)
}

pub fn function(module: &Module, result_type: &Type, params: &[Type], name: Symbol, strings: &Rc<Strings>) -> Function {
    let param_types: Vec<_> = params.iter()
        .map(|typ| to_llvm_type(&typ))
        .collect();
    let function_type = types::function::new(to_llvm_type(&result_type), &param_types, false);
    let function = module.add_function(&strings.get(name).expect("symbol"), function_type);
    function.append_basic_block("entry");
    function
}

pub struct Gen {
    break_label: Option<BasicBlock>,
    builder: Builder,
    function_pass_manager: FunctionPassManager,
    inner_functions: Vec<FuncDeclaration>,
    module: Module,
    module_pass_manager: ModulePassManager,
    target_machine: TargetMachine,
}

impl Gen {
    pub fn new(module: Module) -> Self {
        let target_triple = get_default_target_triple();
        let target = Target::get_from_triple(&target_triple).expect("get target");

        let target_machine = target.create_target_machine(&target_triple, "generic", "", CodeGenOptLevel::Aggressive, RelocMode::Default, CodeModel::Default);

        module.set_data_layout(target_machine.create_data_layout());
        module.set_target(target_triple);

        let module_pass_manager = ModulePassManager::new();
        module_pass_manager.add_function_inlining_pass();

        let function_pass_manager = FunctionPassManager::new_for_module(&module);
        function_pass_manager.add_promote_memory_to_register_pass();
        function_pass_manager.add_instruction_combining_pass();
        function_pass_manager.add_reassociate_pass();
        function_pass_manager.add_gvn_pass();
        function_pass_manager.add_cfg_simplification_pass();

        Self {
            break_label: None,
            builder: Builder::new(),
            function_pass_manager,
            inner_functions: Vec::new(),
            module,
            module_pass_manager,
            target_machine,
        }
    }

    fn create_argument_allocas(&self, llvm_function: &Function, function: &FuncDeclaration) {
        for (index, field) in function.params.iter().enumerate() {
            let arg = llvm_function.get_param(index);
            self.builder.store(&arg, &field.node.value);
        }
    }

    fn declaration(&mut self, declaration: TypedDeclaration, inner: bool) {
        match declaration.node {
            Declaration::Function(function) => {
                if inner {
                    self.inner_functions.push(function.node);
                }
                else {
                    self.function_declaration(function.node);
                }
            },
            Declaration::Variable { init, value, .. } => {
                println!("Var type: {:?}", init.typ);
                let init_value = self.expr(init.expr);
                if let Type::Array(ref typ, size, _) = init.typ {
                    let size = constant::int(types::int32(), (size_of(typ) * size) as u64, true);
                    self.builder.mem_move(&value, align_of(&init.typ), &init_value, align_of(&init.typ), &size);
                }
                else {
                    self.builder.store(&init_value, &value);
                }
            },
            _ => unimplemented!(),
        }
    }

    fn expr(&mut self, expr: Expr) -> Value {
        let value =
            match expr {
                Expr::Array { init, size } => {
                    let typ = types::array::array(to_llvm_type(&init.typ), size);
                    // TODO: set the initial value of the array with a loop.
                    let array = self.builder.alloca(typ, "array");
                    array
                },
                Expr::Assign { expr, var } => {
                    let value = self.expr(expr.expr);
                    let variable =
                        match var.var {
                            Var::Field { .. } => unimplemented!(),
                            Var::Global { .. } => unreachable!(),
                            Var::Simple { value } => value,
                            Var::Subscript { expr, this } => {
                                let index = self.expr(expr.expr);
                                let llvm_type = to_llvm_type(&this.typ);
                                let this = self.variable_address(*this);
                                self.builder.gep(&llvm_type, &this, &[constant::int(types::int32(), 0, true), index], "index")
                            },
                        };
                    self.builder.store(&value, &variable)
                },
                Expr::Bool(value) => {
                    constant::int(types::int1(), value as u64, true)
                },
                Expr::Break => {
                    let label = self.break_label.clone().expect("break label");
                    let val = self.builder.br(&label);
                    let start_basic_block = self.builder.get_insert_block().expect("start basic block");
                    let function = start_basic_block.get_parent();
                    let basic_block = BasicBlock::append(&function, "afterbreak");
                    self.builder.position_at_end(&basic_block);
                    val
                },
                Expr::Call { args, llvm_function } => {
                    let arguments: Vec<_> = args.into_iter().map(|arg| self.expr(arg.expr)).collect();
                    self.builder.call(llvm_function.clone(), &arguments, "")
                },
                Expr::If { condition, then, else_ } => {
                    let condition = self.expr(condition.expr);
                    let condition = self.builder.icmp(IntPredicate::NotEqual, &condition, &constant::int(types::int1(), 0, true), "ifcond");

                    let start_basic_block = self.builder.get_insert_block().expect("start basic block");

                    let function = start_basic_block.get_parent();

                    let then_basic_block = BasicBlock::append(&function, "then");

                    self.builder.position_at_end(&then_basic_block);

                    let then_value = self.expr(then.expr);

                    let new_then_basic_block = self.builder.get_insert_block().expect("new then basic block");

                    let else_basic_block = BasicBlock::append(&function, "else");
                    self.builder.position_at_end(&else_basic_block);

                    let else_value = else_.map(|else_| self.expr(else_.expr));

                    let new_else_basic_block = self.builder.get_insert_block().expect("new else basic block");

                    let merge_basic_block = BasicBlock::append(&function, "ifcont");
                    self.builder.position_at_end(&merge_basic_block);

                    let result =
                        match then.typ {
                            Type::Unit => self.expr(Expr::Nil),
                            _ => {
                                let phi = self.builder.phi(to_llvm_type(&then.typ), "result");
                                if let Some(else_value) = else_value {
                                    phi.add_incoming(&[(&then_value, &new_then_basic_block), (&else_value, &new_else_basic_block)]);
                                }
                                else {
                                    phi.add_incoming(&[(&then_value, &new_then_basic_block)]);
                                }
                                phi
                            },
                        };

                    self.builder.position_at_end(&start_basic_block);
                    self.builder.cond_br(&condition, &then_basic_block, &else_basic_block);

                    self.builder.position_at_end(&new_then_basic_block);
                    self.builder.br(&merge_basic_block);

                    self.builder.position_at_end(&new_else_basic_block);
                    self.builder.br(&merge_basic_block);

                    self.builder.position_at_end(&merge_basic_block);

                    result
                },
                Expr::Int { value } => constant::int(types::integer::int32(), value as u64, true), // TODO: use int64?
                Expr::Decl(declaration) => {
                    self.declaration(*declaration, true);
                    self.expr(Expr::Nil)
                },
                Expr::Nil => constant::int(types::integer::int32(), 0, true), // TODO: use pointer type?
                Expr::Oper { left, oper, right } => {
                    let left = self.expr(left.expr);
                    let right = self.expr(right.expr);
                    match oper.node {
                        Operator::And => unimplemented!(),
                        Operator::Divide => unimplemented!(),
                        Operator::Equal => self.builder.icmp(IntPredicate::Equal, &left, &right, "cmptmp"),
                        Operator::Ge => self.builder.icmp(IntPredicate::SignedGreaterThanOrEqual, &left, &right, "cmptmp"),
                        Operator::Gt => self.builder.icmp(IntPredicate::SignedGreaterThan, &left, &right, "cmptmp"),
                        Operator::Le => self.builder.icmp(IntPredicate::SignedLesserThanOrEqual, &left, &right, "cmptmp"),
                        Operator::Lt => self.builder.icmp(IntPredicate::SignedLesserThan, &left, &right, "cmptmp"),
                        Operator::Minus => self.builder.sub(&left, &right, "diff"),
                        Operator::Neq => self.builder.icmp(IntPredicate::NotEqual, &left, &right, "cmptmp"),
                        Operator::Or => unimplemented!(),
                        Operator::Plus => self.builder.add(&left, &right, "sum"),
                        Operator::Times => unimplemented!(),
                    }
                },
                Expr::Sequence(mut exprs) => {
                    let last_expr = exprs.pop().expect("at least one expression in sequence");
                    for expr in exprs {
                        self.expr(expr.expr);
                    }
                    self.expr(last_expr.expr)
                },
                Expr::Str { ref value } => {
                    self.builder.global_string_ptr(value, "string")
                },
                Expr::Variable(variable) => self.variable(variable),
                Expr::While { body, condition } => {
                    let start_basic_block = self.builder.get_insert_block().expect("start basic block");
                    let function = start_basic_block.get_parent();

                    let start_loop_basic_block = BasicBlock::append(&function, "startloop");
                    self.builder.br(&start_loop_basic_block);
                    self.builder.position_at_end(&start_loop_basic_block);

                    let condition = self.expr(condition.expr);
                    let condition = self.builder.icmp(IntPredicate::NotEqual, &condition, &constant::int(types::int1(), 0, true), "whilecond");

                    let loop_basic_block = BasicBlock::append(&function, "loop");
                    let after_basic_block = BasicBlock::append(&function, "afterloop");
                    self.break_label = Some(after_basic_block.clone());
                    self.builder.cond_br(&condition, &loop_basic_block, &after_basic_block);

                    self.builder.position_at_end(&loop_basic_block);

                    let previous_break_label = self.break_label.clone();
                    self.expr(body.expr);
                    self.break_label = previous_break_label;
                    self.builder.br(&start_loop_basic_block);

                    self.builder.position_at_end(&after_basic_block);

                    constant::null(types::int32())
                },
                _ => unimplemented!("{:?}", expr),
            };
        value
    }

    fn function_declaration(&mut self, function: FuncDeclaration) {
        let entry = function.llvm_function.get_entry_basic_block();
        self.builder.position_at_end(&entry);
        self.create_argument_allocas(&function.llvm_function, &function);

        let return_value = self.expr(function.body.expr);

        if function.body.typ == Type::Unit {
            self.builder.ret_no_value();
        }
        else {
            self.builder.ret(&return_value);
        }
        if function.llvm_function.verify(VerifierFailureAction::AbortProcess) {
            function.llvm_function.dump();
        }

        self.function_pass_manager.run(&function.llvm_function);
    }

    pub fn generate(&mut self, declarations: Vec<TypedDeclaration>, filename: &str) -> PathBuf {
        let mut object_output_path = PathBuf::from(filename);
        object_output_path.set_extension("o");

        for declaration in declarations {
            self.declaration(declaration, false);
        }

        let inner_functions = mem::replace(&mut self.inner_functions, vec![]);
        for function in inner_functions {
            self.function_declaration(function);
        }

        self.module_pass_manager.run(&self.module);
        self.module.dump();

        if let Err(error) = self.target_machine.emit_to_file(&self.module, object_output_path.as_os_str().to_str().expect("filename"), CodeGenFileType::ObjectFile) {
            // TODO: return error instead?
            eprintln!("Cannot emit to object: {}", error);
        }

        object_output_path
    }

    fn variable(&mut self, variable: TypedVar) -> Value {
        let typ = to_llvm_type(&variable.typ);
        match variable.var {
            Var::Field { .. } => unimplemented!(),
            Var::Global { llvm_function } => self.builder.call(llvm_function.clone(), &[], ""),
            Var::Simple { value } => {
                if let Type::Array(_, _, _) = variable.typ {
                    value
                }
                else {
                    self.builder.load(typ, &value, "")
                }
            },
            Var::Subscript { this, expr } => {
                println!("Var: {:?}", variable.typ);
                println!("This: {:?}", this);
                println!("Expr: {:?}", expr.typ);
                let llvm_type = to_llvm_type(&this.typ);
                let this = self.variable_address(*this);
                let index = self.expr(expr.expr);
                println!("gep type:");
                llvm_type.dump();
                println!();
                let pointer = self.builder.gep(&llvm_type, &this, &[constant::int(types::int32(), 0, true), index], "index");
                self.builder.load(typ, &pointer, "")
            },
        }
    }

    fn variable_address(&mut self, variable: TypedVar) -> Value {
        match variable.var {
            Var::Field { .. } => unimplemented!(),
            Var::Global { .. } => unimplemented!(),
            Var::Simple { value } => value,
            Var::Subscript { this, expr } => {
                let llvm_type = to_llvm_type(&variable.typ);
                let this = self.variable_address(*this);
                let index = self.expr(expr.expr);
                self.builder.gep(&llvm_type, &this, &[constant::int(types::int32(), 0, true), index], "index")
            },
        }
    }
}

fn align_of(typ: &Type) -> usize {
    match *typ {
        Type::Array(ref typ, _, _) => size_of(typ),
        Type::Int32 => size_of(typ),
        _ => unimplemented!("align_of {:?}", typ),
    }
}

fn size_of(typ: &Type) -> usize {
    match *typ {
        Type::Int32 => 4,
        _ => unimplemented!("size_of {:?}", typ),
    }
}
