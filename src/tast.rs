/*
 * Copyright (c) 2019-2020 Boucher, Antoni <bouanto@zoho.com>
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

use rlvm::{Value, module::Function};

use ast::{
    OperatorWithPos,
    TypeDecWithPos,
};
use position::{Pos, WithPos};
use symbol::{Symbol, SymbolWithPos};
use types::Type;

#[derive(Clone, Debug)]
pub enum Declaration {
    Function(TypedFuncDeclaration),
    Type(TypeDecWithPos),
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
    pub value: Value,
}

pub type FieldWithPos = WithPos<Field>;

#[derive(Clone, Debug)]
pub struct FuncDeclaration {
    pub body: TypedExpr,
    pub llvm_function: Function,
    pub name: Symbol,
    pub params: Vec<FieldWithPos>,
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
