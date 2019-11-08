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

use rlvm::{
    Builder,
    CodeGenOptLevel,
    CodeModel,
    Module,
    RealPredicate,
    RelocMode,
    Target,
    Value,
    get_default_target_triple,
    module::Function,
    types,
    value::constant,
};

use ast::{FieldWithPos, Operator};
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
}

impl Gen {
    pub fn new() -> Self {
        let target_triple = get_default_target_triple();
        let target = Target::get_from_triple(&target_triple).expect("get target");

        let target_machine = target.create_target_machine(&target_triple, "generic", "", CodeGenOptLevel::Aggressive, RelocMode::Default, CodeModel::Default);

        let module = Module::new_with_name("module");
        module.set_data_layout(target_machine.create_data_layout());
        module.set_target(target_triple);
        Self {
            builder: Builder::new(),
            module,
        }
    }

    pub fn function_declaration(&self, func_name: &str, parameters: &[Type]) -> Function {
        let function_type = types::function::new(types::int32(), &[types::pointer::new(types::int8(), 0)], false);
        self.module.add_function(func_name, function_type)
    }

    pub fn string_literal(&mut self, string: String) -> Value {
        self.builder.global_string_ptr(&string, "string")
    }
}
