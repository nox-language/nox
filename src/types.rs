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
