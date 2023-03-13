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

use position::WithPos;
use symbol::{Symbol, SymbolWithPos};

#[derive(Clone, Debug)]
pub enum Declaration {
    Extern(ExternFuncDeclarationWithPos),
    Function(FuncDeclarationWithPos),
    Struct(StructTypeWithPos),
    TypeAlias(TypeAliasDecWithPos),
    Variable {
        escape: bool,
        init: ExprWithPos,
        name: SymbolWithPos,
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
        function: SymbolWithPos,
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
    pub name: SymbolWithPos,
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
}

pub type TyWithPos = WithPos<Ty>;

#[derive(Clone, Debug)]
pub struct StructType {
    pub fields: Vec<FieldWithPos>,
    pub typ: SymbolWithPos,
}

pub type StructTypeWithPos = WithPos<StructType>;

#[derive(Clone, Debug)]
pub struct TypeAliasDec {
    pub name: SymbolWithPos,
    pub ty: TyWithPos,
}

pub type TypeAliasDecWithPos = WithPos<TypeAliasDec>;

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
