/*
 * Copyright (C) 2017-2023  Boucher, Antoni <bouanto@zoho.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

use std::collections::HashMap;
use std::rc::Rc;

use rlvm::{
    Module,
    Value,
    module::Function,
    types,
};

use gen;
use symbol::{Strings, Symbol, Symbols};
use types::Type;

#[derive(Clone)]
pub enum Entry {
    Fun {
        llvm_function: Function,
        parameters: Vec<Type>,
        result: Type,
    },
    Var {
        typ: Type,
        value: Value,
    },
}

pub struct Env {
    type_env: Symbols<Type>,
    var_env: Symbols<Entry>,
}

impl Env {
    pub fn new(strings: &Rc<Strings>, module: &Module) -> Self {
        let mut type_env = Symbols::new(Rc::clone(strings));
        let bool_symbol = type_env.symbol("bool");
        type_env.enter(bool_symbol, Type::Bool);
        let int_symbol = type_env.symbol("int");
        type_env.enter(int_symbol, Type::Int32);
        let string_symbol = type_env.symbol("string");
        type_env.enter(string_symbol, Type::String);

        let var_env = Symbols::new(Rc::clone(strings));
        let mut env = Self {
            type_env,
            var_env,
        };

        for (name, (param_types, return_type)) in external_functions() {
            env.add_function(name, param_types, return_type, module);
        }

        env
    }

    pub fn add_function(&mut self, name: &str, parameters: Vec<Type>, result: Type, module: &Module) {
        let symbol = self.var_env.symbol(name);
        let result_type = gen::to_llvm_type(&result).expect("llvm type");
        let param_types: Vec<_> = parameters.iter()
            .map(|typ| gen::to_llvm_type(&typ).expect("llvm type"))
            .collect();
        let function_type = types::function::new(result_type, &param_types, false);
        let llvm_function = module.add_function(name, function_type);
        let entry = Entry::Fun {
            llvm_function,
            parameters,
            result,
        };
        self.var_env.enter(symbol, entry);
    }

    pub fn begin_scope(&mut self) {
        self.type_env.begin_scope();
        self.var_env.begin_scope();
    }

    pub fn end_scope(&mut self) {
        self.type_env.end_scope();
        self.var_env.end_scope();
    }

    pub fn enter_type(&mut self, symbol: Symbol, typ: Type) {
        self.type_env.enter(symbol, typ);
    }

    pub fn enter_var(&mut self, symbol: Symbol, data: Entry) {
        self.var_env.enter(symbol, data);
    }

    pub fn look_type(&self, symbol: Symbol) -> Option<&Type> {
        self.type_env.look(symbol)
    }

    pub fn look_var(&self, symbol: Symbol) -> Option<&Entry> {
        self.var_env.look(symbol)
    }

    pub fn type_name(&self, symbol: Symbol) -> String {
        self.type_env.name(symbol)
    }

    pub fn var_name(&self, symbol: Symbol) -> String {
        self.var_env.name(symbol)
    }
}

fn external_functions() -> HashMap<&'static str, (Vec<Type>, Type)> {
    let mut functions = HashMap::new();
    functions.insert("print", (vec![Type::String], Type::Unit));
    functions.insert("printi", (vec![Type::Int32], Type::Unit));
    functions.insert("flush", (vec![], Type::Unit));
    functions.insert("getchar", (vec![], Type::String));
    functions.insert("ord", (vec![Type::String], Type::Int32));
    functions.insert("chr", (vec![Type::Int32], Type::String));
    functions.insert("size", (vec![Type::String], Type::Int32));
    functions.insert("substring", (vec![Type::String, Type::Int32, Type::Int32], Type::String));
    functions.insert("concat", (vec![Type::String, Type::String], Type::String));
    functions.insert("not", (vec![Type::Int32], Type::Int32));
    functions.insert("exit", (vec![Type::Int32], Type::Unit));
    functions.insert("stringEqual", (vec![Type::String, Type::String], Type::Int32));

    functions.insert("malloc", (vec![Type::Int32], Type::Int32));
    functions.insert("initArray", (vec![Type::Int32, Type::Int32], Type::Int32));
    functions
}
