/*
 * Copyright (c) 2017-2020 Boucher, Antoni <bouanto@zoho.com>
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

use self::Type::*;
use symbol::{Symbol, Symbols, SymbolWithPos};

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Array(Box<Type>, usize),
    Bool,
    Int32,
    Name(SymbolWithPos, Box<Type>),
    Nil,
    String,
    Struct(Symbol, Vec<(Symbol, Type)>, Unique),
    Unit,
    Void,
    Error,
}

impl Type {
    pub fn show(&self, symbols: &Symbols<()>) -> std::string::String {
        match *self {
            Array(ref typ, size) => {
                format!("[{}; {}]", typ.show(symbols), size)
            },
            Bool => "bool".to_string(),
            Int32 => "int32".to_string(),
            Name(_, ref typ) => {
                typ.show(symbols)
            },
            Nil => "nil".to_string(),
            String => "string".to_string(),
            Struct(symbol, _, _) => format!("struct {}", symbols.name(symbol)),
            Unit => "()".to_string(),
            Void => "void".to_string(),
            Error => "type error".to_string(),
        }
    }
}

static mut UNIQUE_COUNT: u64 = 0;

#[derive(Clone, Debug, PartialEq)]
pub struct Unique(u64);

impl Unique {
    pub fn new() -> Self {
        let value = unsafe { UNIQUE_COUNT };
        unsafe { UNIQUE_COUNT += 1 };
        Unique(value)
    }
}
