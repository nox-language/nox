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

use position::WithPos;
use symbol::{Symbol, SymbolWithPos};

#[derive(Clone, Debug)]
pub enum Declaration {
    Extern(ExternFuncDeclarationWithPos),
    Function(FuncDeclarationWithPos),
    Type(TypeDecWithPos),
    Variable {
        escape: bool,
        init: ExprWithPos,
        name: Symbol,
        typ: Option<TyWithPos>,
    },
}

pub type DeclarationWithPos = WithPos<Declaration>;

#[derive(Clone, Debug)]
pub enum Expr {
    Array {
        init: Box<ExprWithPos>,
        size: usize,
    },
    Assign {
        expr: Box<ExprWithPos>,
        var: VarWithPos,
    },
    Bool(bool),
    Break,
    Call {
        args: Vec<ExprWithPos>,
        function: Symbol,
    },
    Decl(Box<DeclarationWithPos>),
    EmptyTuple,
    If {
        condition: Box<ExprWithPos>,
        else_: Option<Box<ExprWithPos>>,
        then: Box<ExprWithPos>,
    },
    Int {
        value: i64,
    },
    Nil,
    Oper {
        left: Box<ExprWithPos>,
        oper: OperatorWithPos,
        right: Box<ExprWithPos>,
    },
    Sequence(Vec<ExprWithPos>),
    Str {
        value: String,
    },
    Struct {
        fields: Vec<StructFieldWithPos>,
        typ: SymbolWithPos,
    },
    Variable(VarWithPos),
    While {
        body: Box<ExprWithPos>,
        condition: Box<ExprWithPos>,
    },
}

pub type ExprWithPos = WithPos<Expr>;

#[derive(Clone, Debug)]
pub struct Field {
    pub escape: bool,
    pub name: Symbol,
    pub typ: TyWithPos,
}

pub type FieldWithPos = WithPos<Field>;

#[derive(Clone, Debug)]
pub struct FuncDeclaration {
    pub body: ExprWithPos,
    pub name: Symbol,
    pub params: Vec<FieldWithPos>,
    pub result: Option<TyWithPos>,
}

pub type FuncDeclarationWithPos = WithPos<FuncDeclaration>;

#[derive(Clone, Debug)]
pub struct ExternFuncDeclaration {
    pub name: Symbol,
    pub params: Vec<FieldWithPos>,
    pub result: Option<TyWithPos>,
}

pub type ExternFuncDeclarationWithPos = WithPos<ExternFuncDeclaration>;

#[derive(Clone, Copy, Debug)]
pub enum Operator {
    And,
    Divide,
    Equal,
    Ge,
    Gt,
    Le,
    Lt,
    Minus,
    Neq,
    Or,
    Plus,
    Times,
}

pub type OperatorWithPos = WithPos<Operator>;

#[derive(Clone, Debug)]
pub struct StructField {
    pub expr: ExprWithPos,
    pub ident: Symbol,
}

pub type StructFieldWithPos = WithPos<StructField>;

#[derive(Clone, Debug)]
pub enum Ty {
    Array {
        size: usize,
        typ: Box<TyWithPos>,
    },
    Name {
        ident: SymbolWithPos,
    },
    Struct {
        fields: Vec<FieldWithPos>,
        typ: SymbolWithPos,
    },
}

#[derive(Clone, Debug)]
pub struct TypeDec {
    pub name: SymbolWithPos,
    pub ty: TyWithPos,
}

pub type TypeDecWithPos = WithPos<TypeDec>;

pub type TyWithPos = WithPos<Ty>;

#[derive(Clone, Debug)]
pub enum Var {
    Field {
        ident: SymbolWithPos,
        this: Box<VarWithPos>,
    },
    Simple {
        ident: SymbolWithPos,
    },
    Subscript {
        expr: Box<ExprWithPos>,
        this: Box<VarWithPos>,
    },
}

pub type VarWithPos = WithPos<Var>;

pub fn dummy_var_expr(symbol: Symbol) -> ExprWithPos {
    WithPos::dummy(Expr::Variable(WithPos::dummy(Var::Simple {
        ident: WithPos::dummy(symbol),
    })))
}
