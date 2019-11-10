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

use std::path::PathBuf;
use std::rc::Rc;

use rlvm::{
    Builder,
    CodeGenFileType,
    CodeGenOptLevel,
    CodeModel,
    FunctionPassManager,
    Module,
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
};
use temp::Label;
use types::Type;

pub fn alloc_local() -> Value {
    unimplemented!();
}

pub fn array_subscript(var: Value, subscript: Value) -> Value {
    unimplemented!();
}

pub fn binary_oper(op: Operator, left: Value, right: Value) -> Value {
    unimplemented!();
}

pub fn error() -> Value {
    unimplemented!();
}

pub fn field_access(var: Value, field_index: usize) -> Value {
    unimplemented!();
}

pub fn function_call(label: &Label, mut args: Vec<Value>) -> Value {
    unimplemented!();
}

pub fn goto(label: Label) -> Value {
    unimplemented!();
}

pub fn if_expression(test_expr: Value, if_expr: Value, else_expr: Option<Value>) -> Value {
    unimplemented!();
}

pub fn num(number: i64) -> Value {
    unimplemented!();
}

pub fn record_create(fields: Vec<Value>) -> Value {
    if fields.is_empty() {
        return unit();
    }
    unimplemented!();
}

pub fn relational_oper(op: Operator, left: Value, right: Value) -> Value {
    unimplemented!();
}

pub fn string_equality(oper: Operator, left: Value, right: Value) -> Value {
    //let exp = external_call("stringEqual", &[left, right]);
    unimplemented!();
}

pub fn unit() -> Value {
    unimplemented!();
}

pub fn var_dec(variable: &Value, value: Value) {
    unimplemented!();
}

pub fn while_loop(done_label: &Label, test_expr: Value, body: Value) -> Value {
    unimplemented!();
}

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

pub struct Gen {
    builder: Builder,
    module: Module,
    pass_manager: FunctionPassManager,
    strings: Rc<Strings>,
    target_machine: TargetMachine,
}

pub fn to_llvm_type(typ: &Type) -> rlvm::types::Type {
    match *typ {
        Type::Int => types::integer::int32(), // TODO: int64?
        Type::String => types::pointer::new(types::int8(), 0),
        Type::Record(_symbol, ref _fields, _) => unimplemented!(),
        Type::Array(ref _type, _) => unimplemented!(),
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
    module.add_function(&strings.get(name).expect("symbol"), function_type)
}

impl Gen {
    pub fn new(strings: Rc<Strings>, module: Module) -> Self {
        let target_triple = get_default_target_triple();
        let target = Target::get_from_triple(&target_triple).expect("get target");

        let target_machine = target.create_target_machine(&target_triple, "generic", "", CodeGenOptLevel::Aggressive, RelocMode::Default, CodeModel::Default);

        module.set_data_layout(target_machine.create_data_layout());
        module.set_target(target_triple);

        let pass_manager = FunctionPassManager::new_for_module(&module);
        pass_manager.add_promote_memory_to_register_pass();
        pass_manager.add_instruction_combining_pass();
        pass_manager.add_reassociate_pass();
        pass_manager.add_gvn_pass();
        pass_manager.add_cfg_simplification_pass();

        Self {
            builder: Builder::new(),
            module,
            pass_manager,
            strings,
            target_machine,
        }
    }

    fn create_argument_allocas(&mut self, llvm_function: &Function, function: &FuncDeclaration) {
        for (index, field) in function.params.iter().enumerate() {
            let arg = llvm_function.get_param(index);
            self.builder.store(&arg, &field.node.value);
        }
    }

    fn declaration(&mut self, declaration: &TypedDeclaration) {
        match declaration.node {
            Declaration::Function(ref function) => self.function_declaration(&function.node),
            _ => unimplemented!(),
        }
    }

    fn expr(&self, expr: &Expr) -> Value {
        let value =
            match *expr {
                Expr::Call { ref args, ref llvm_function } => {
                    let arguments: Vec<_> = args.into_iter().map(|arg| self.expr(&arg.expr)).collect();
                    self.builder.call(llvm_function.clone(), &arguments, "")
                },
                Expr::Int { value } => constant::int(types::integer::int32(), value as u64, true), // TODO: use int64?
                Expr::Sequence(ref exprs) => {
                    let (last_expr, exprs) = exprs.split_last().expect("at least one expression in sequence");
                    for expr in exprs {
                        self.expr(&expr.expr);
                    }
                    self.expr(&last_expr.expr)
                },
                Expr::Str { ref value } => {
                    self.builder.global_string_ptr(value, "string")
                },
                _ => unimplemented!("{:?}", expr),
            };
        value
    }

    fn function_declaration(&mut self, function: &FuncDeclaration) {
        let entry = function.llvm_function.append_basic_block("entry");
        self.builder.position_at_end(&entry);
        self.create_argument_allocas(&function.llvm_function, &function);

        let return_value = self.expr(&function.body.expr);

        self.builder.ret(return_value);
        function.llvm_function.verify(VerifierFailureAction::AbortProcess);

        self.pass_manager.run(&function.llvm_function);

        self.module.dump();
    }

    pub fn generate(&mut self, declaration: &TypedDeclaration, filename: &str) -> PathBuf {
        let mut object_output_path = PathBuf::from(filename);
        object_output_path.set_extension("o");

        self.declaration(declaration);
        if let Err(error) = self.target_machine.emit_to_file(&self.module, object_output_path.as_os_str().to_str().expect("filename"), CodeGenFileType::ObjectFile) {
            // TODO: return error instead?
            eprintln!("Cannot emit to object: {}", error);
        }

        object_output_path
    }

    fn symbol(&self, symbol: Symbol) -> String {
        self.strings.get(symbol).expect("symbol")
    }
}
