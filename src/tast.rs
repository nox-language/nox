/*
 * Copyright (c) 2019 Boucher, Antoni <bouanto@zoho.com>
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

use ast::{
    FieldWithPos,
    OperatorWithPos,
    TypeDecWithPos,
};
use position::{Pos, WithPos};
use symbol::{Symbol, SymbolWithPos};
use types::Type;

#[derive(Clone, Debug)]
pub enum Declaration {
    Function(Vec<TypedFuncDeclaration>),
    Type(Vec<TypeDecWithPos>),
    VariableDeclaration {
        escape: bool,
        init: TypedExpr,
        name: Symbol,
        typ: Option<SymbolWithPos>,
    },
}

pub type TypedDeclaration = WithPos<Declaration>;

#[derive(Clone, Debug)]
pub enum Expr {
    Array {
        init: Box<TypedExpr>,
        size: Box<TypedExpr>,
        typ: SymbolWithPos,
    },
    Assign {
        expr: Box<TypedExpr>,
        var: TypedVar,
    },
    Break,
    Call {
        args: Vec<TypedExpr>,
        function: Symbol,
    },
    If {
        else_: Option<Box<TypedExpr>>,
        test: Box<TypedExpr>,
        then: Box<TypedExpr>,
    },
    Int {
        value: i64,
    },
    Let {
        body: Box<TypedExpr>,
        declarations: Vec<TypedDeclaration>,
    },
    Nil,
    Oper {
        left: Box<TypedExpr>,
        oper: OperatorWithPos,
        right: Box<TypedExpr>,
    },
    Record {
        fields: Vec<TypedRecordField>,
        typ: SymbolWithPos,
    },
    Sequence(Vec<TypedExpr>),
    Str {
        value: String,
    },
    Variable(TypedVar),
    While {
        body: Box<TypedExpr>,
        test: Box<TypedExpr>,
    },
}

#[derive(Clone, Debug)]
pub struct TypedExpr {
    pub expr: Expr,
    pub pos: Pos,
    pub typ: Type,
}

#[derive(Clone, Debug)]
pub struct FuncDeclaration {
    pub body: TypedExpr,
    pub name: Symbol,
    pub params: Vec<FieldWithPos>,
    pub result: Option<SymbolWithPos>,
}

pub type TypedFuncDeclaration = WithPos<FuncDeclaration>;

#[derive(Clone, Debug)]
pub struct RecordField {
    pub expr: TypedExpr,
    pub ident: Symbol,
}

pub type TypedRecordField = WithPos<RecordField>;

#[derive(Clone, Debug)]
pub enum Var {
    Field {
        ident: SymbolWithPos,
        this: Box<TypedVar>,
    },
    Simple {
        ident: SymbolWithPos,
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
