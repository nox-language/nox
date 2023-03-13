/*
 * Copyright (C) 2019-2023  Boucher, Antoni <bouanto@zoho.com>
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

use rlvm::{Value, module::Function};

use ast::{
    OperatorWithPos,
    TypeAliasDecWithPos,
};
use position::{Pos, WithPos};
use symbol::{Symbol, SymbolWithPos};
use types::Type;

#[derive(Clone, Debug)]
pub enum Declaration {
    Function(TypedFuncDeclaration),
    Struct(StructTypeWithPos),
    TypeAlias(TypeAliasDecWithPos),
    Variable {
        escape: bool,
        init: TypedExpr,
        name: Symbol,
        typ: Option<Type>,
        value: Value,
    },
}

pub type TypedDeclaration = WithPos<Declaration>;

#[derive(Clone, Debug)]
pub enum Expr {
    Array {
        init: Box<TypedExpr>,
        size: usize,
    },
    Assign {
        expr: Box<TypedExpr>,
        var: TypedVar,
    },
    Bool(bool),
    Break,
    Call {
        args: Vec<TypedExpr>,
        llvm_function: Function,
    },
    Decl(Box<TypedDeclaration>),
    EmptyTuple,
    If {
        condition: Box<TypedExpr>,
        else_: Option<Box<TypedExpr>>,
        then: Box<TypedExpr>,
    },
    Int {
        value: i64,
    },
    Nil,
    Oper {
        left: Box<TypedExpr>,
        oper: OperatorWithPos,
        right: Box<TypedExpr>,
    },
    Struct {
        fields: Vec<TypedStructField>,
        typ: SymbolWithPos,
    },
    Sequence(Vec<TypedExpr>),
    Str {
        value: String,
    },
    Variable(TypedVar),
    While {
        body: Box<TypedExpr>,
        condition: Box<TypedExpr>,
    },
}

#[derive(Clone, Debug)]
pub struct TypedExpr {
    pub expr: Expr,
    pub pos: Pos,
    pub typ: Type,
}

#[derive(Clone, Debug)]
pub struct Field {
    pub escape: bool,
    pub name: Symbol,
    pub typ: Type,
}

pub type FieldWithPos = WithPos<Field>;

#[derive(Clone, Debug)]
pub struct Param {
    pub escape: bool,
    pub name: Symbol,
    pub typ: Type,
    pub value: Value,
}

pub type ParamWithPos = WithPos<Param>;

#[derive(Clone, Debug)]
pub struct FuncDeclaration {
    pub body: TypedExpr,
    pub llvm_function: Function,
    pub name: Symbol,
    pub params: Vec<ParamWithPos>,
    pub result_type: Type,
}

pub type TypedFuncDeclaration = WithPos<FuncDeclaration>;

#[derive(Clone, Debug)]
pub struct StructField {
    pub expr: TypedExpr,
    pub ident: Symbol,
}

pub type TypedStructField = WithPos<StructField>;

#[derive(Clone, Debug)]
pub struct StructType {
    pub fields: Vec<FieldWithPos>,
    pub typ: SymbolWithPos,
}

pub type StructTypeWithPos = WithPos<StructType>;

#[derive(Clone, Debug)]
pub enum Var {
    Field {
        index: usize,
        this: Box<TypedVar>,
    },
    Global {
        llvm_function: Function,
    },
    Simple {
        value: Value,
    },
    Subscript {
        expr: Box<TypedExpr>,
        this: Box<TypedVar>,
    },
}

#[derive(Clone, Debug)]
pub struct TypedVar {
    pub pos: Pos,
    pub typ: Type,
    pub var: Var,
}
