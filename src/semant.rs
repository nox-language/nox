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

mod msg;

use std::collections::HashSet;
use std::rc::Rc;

use rlvm::{
    Module,
    module::Function,
    types,
    value::constant,
};

use ast::{
    Declaration,
    DeclarationWithPos,
    Expr,
    ExprWithPos,
    ExternFuncDeclarationWithPos,
    FieldWithPos,
    FuncDeclarationWithPos,
    Operator,
    OperatorWithPos,
    StructFieldWithPos,
    StructTypeWithPos,
    Ty,
    TyWithPos,
    TypeAliasDecWithPos,
    Var,
    VarWithPos,
};
use env::{Env, Entry};
use error::{Error, Result};
use gen;
use self::msg::call_with_array_help;
use position::{Pos, WithPos};
use self::AddError::*;
use symbol::{Strings, Symbol, SymbolWithPos};
use tast::{
    self,
    TypedDeclaration,
    TypedExpr,
    TypedVar,
};
use types::{Type, Unique};

#[derive(PartialEq)]
enum AddError {
    AddError,
}

fn exp_type_error() -> TypedExpr {
    TypedExpr {
        expr: tast::Expr::Nil,
        pos: Pos::dummy(),
        typ: Type::Error,
    }
}

fn var_type_error() -> TypedVar {
    TypedVar {
        pos: Pos::dummy(),
        typ: Type::Error,
        var: tast::Var::Simple { value: constant::int(types::integer::int32(), 0, true) },
    }
}

pub struct SemanticAnalyzer<'a> {
    current_function: Option<Function>,
    env: &'a mut Env,
    errors: Vec<Error>,
    in_loop: bool,
    module: &'a Module,
    strings: Rc<Strings>,
}

impl<'a> SemanticAnalyzer<'a> {
    pub fn new(env: &'a mut Env, strings: Rc<Strings>, module: &'a Module) -> Self {
        Self {
            current_function: None,
            env,
            errors: vec![],
            in_loop: false,
            module,
            strings,
        }
    }

    fn add_error<T>(&mut self, error: Error, node: T) -> T {
        self.errors.push(error);
        node
    }

    pub fn analyze(mut self, declarations: Vec<DeclarationWithPos>) -> Result<Vec<TypedDeclaration>> {
        let mut result = vec![];
        for declaration in declarations {
            if let Some(declaration) = self.trans_dec(declaration) {
                result.push(declaration);
            }
        }
        if self.errors.is_empty() {
            Ok(result)
        }
        else {
            Err(Error::Multi(self.errors))
        }
    }

    fn actual_ty(&mut self, typ: &Type) -> Type {
        match *typ {
            Type::Name(_, ref typ) => *typ.clone(),
            ref typ => typ.clone(),
        }
    }

    fn actual_ty_var(&mut self, typ: &Type) -> Type {
        let typ =
            match *typ {
                Type::Name(_, ref typ) => *typ.clone(),
                ref typ => typ.clone(),
            };
        typ
    }

    fn check_binary_op(&mut self, oper: OperatorWithPos, left: ExprWithPos, right: ExprWithPos, pos: Pos) -> TypedExpr {
        let left = Box::new(self.trans_exp(left));
        self.check_int(&left, left.pos);
        let right = Box::new(self.trans_exp(right));
        self.check_int(&right, right.pos);
        TypedExpr {
            expr: tast::Expr::Oper { left, oper, right },
            pos,
            typ: Type::Int32,
        }
    }

    fn check_bool(&mut self, expr: &TypedExpr, pos: Pos) {
        if expr.typ != Type::Bool && expr.typ != Type::Error {
            return self.add_error(Error::Type {
                expected: Type::Bool,
                pos,
                unexpected: expr.typ.clone(),
            }, ());
        }
    }

    fn check_defined_type(&mut self, typ: &SymbolWithPos) {
        if self.env.look_type(typ.node).is_some() {
            let ident = self.env.var_name(typ.node).to_string();
            self.add_error(Error::DuplicateType { ident, pos: typ.pos }, ());
        }
    }

    fn check_duplicate_types(&mut self, typ: &TypeAliasDecWithPos) {
        // FIXME: check inner type as well when we support generics.
        if let Ty::Name { ref ident } = typ.node.ty.node {
            if typ.node.name.node == ident.node {
                return self.add_error(Error::Cycle {
                    pos: typ.node.ty.pos,
                }, ());
            }
        }
    }

    fn check_int(&mut self, expr: &TypedExpr, pos: Pos) {
        if expr.typ != Type::Int32 && expr.typ != Type::Error {
            return self.add_error(Error::Type {
                expected: Type::Int32,
                pos,
                unexpected: expr.typ.clone(),
            }, ());
        }
    }

    fn check_types(&mut self, expected: &Type, unexpected: &Type, pos: Pos) {
        let expected = self.actual_ty(expected);
        let unexpected = self.actual_ty(unexpected);
        if expected != unexpected && expected != Type::Error && unexpected != Type::Error {
            if let Type::Struct(_, _, _) = expected {
                if unexpected == Type::Nil {
                    return;
                }
            }
            self.add_error(Error::Type {
                expected: expected.clone(),
                pos,
                unexpected: unexpected.clone(),
            }, ());
        }
    }

    fn get_type(&mut self, symbol: &SymbolWithPos, add: AddError) -> Type {
        if let Some(typ) = self.env.look_type(symbol.node) {
            return typ.clone();
        }
        if add == AddError {
            self.undefined_type(symbol)
        }
        else {
            Type::Error
        }
    }

    fn trans_dec(&mut self, declaration: DeclarationWithPos) -> Option<TypedDeclaration> {
        match declaration.node {
            Declaration::Extern(prototype) => {
                self.trans_extern(prototype);
                None
            },
            Declaration::Function(function) => Some(self.trans_fun(function)),
            Declaration::Struct(struct_type) => {
                self.check_defined_type(&struct_type.node.typ);
                let mut struct_fields = vec![];
                for field in &struct_type.node.fields {
                    let typ = self.trans_ty(&field.node.typ);
                    struct_fields.push((field.node.name, typ));
                }
                let new_type = Type::Struct(struct_type.node.typ.node, struct_fields, Unique::new());
                self.env.enter_type(struct_type.node.typ.node, new_type);
                Some(WithPos::new(tast::Declaration::Struct(self.trans_struct_type(struct_type)), declaration.pos))
            },
            Declaration::TypeAlias(type_declaration) => {
                self.check_duplicate_types(&type_declaration);
                let name = &type_declaration.node.name;
                let new_type = self.trans_ty(&type_declaration.node.ty);
                self.env.enter_type(name.node, new_type);

                Some(WithPos::new(tast::Declaration::TypeAlias(type_declaration), declaration.pos))
            },
            Declaration::Variable { escape, init, name, typ, .. } => {
                let init = self.trans_exp(init);
                let typ =
                    if let Some(ref ident) = typ {
                        let typ = self.trans_ty(ident);
                        self.check_types(&typ, &init.typ, init.pos);
                        Some(typ)
                    }
                    else if init.typ == Type::Nil {
                        return self.add_error(Error::StructType { pos: declaration.pos }, None);
                    }
                    else if init.typ == Type::Error {
                        // TODO: print error here?
                        return None;
                    }
                    else {
                        None
                    };
                let value = gen::create_entry_block_alloca(&self.current_function(), &self.symbol(name), &init.typ);
                self.env.enter_var(name, Entry::Var { typ: init.typ.clone(), value: value.clone() });
                Some(WithPos::new(tast::Declaration::Variable {
                    escape,
                    init,
                    name,
                    typ,
                    value,
                }, declaration.pos))
            },
        }
    }

    pub fn trans_exp(&mut self, expr: ExprWithPos) -> TypedExpr {
        let pos = expr.pos;
        match expr.node {
            Expr::Array { init, size } => {
                let init_expr = self.trans_exp(*init);
                TypedExpr {
                    typ: Type::Array(Box::new(init_expr.typ.clone()), size),
                    expr: tast::Expr::Array { init: Box::new(init_expr), size },
                    pos,
                }
            },
            Expr::Assign { expr, var } => {
                let var = self.trans_var(var);
                let expr = Box::new(self.trans_exp(*expr));
                self.check_types(&var.typ, &expr.typ, expr.pos);
                TypedExpr {
                    expr: tast::Expr::Assign { expr, var },
                    pos,
                    typ: Type::Unit,
                }
            },
            Expr::Bool(value) =>
                TypedExpr {
                    expr: tast::Expr::Bool(value),
                    pos,
                    typ: Type::Bool,
                },
            Expr::Break => {
                if !self.in_loop {
                    return self.add_error(Error::BreakOutsideLoop {
                        pos,
                    }, exp_type_error());
                }
                TypedExpr {
                    expr: tast::Expr::Break,
                    pos,
                    typ: Type::Unit,
                }
            },
            Expr::Call { args, function } => {
                if let Some(entry@Entry::Fun { .. }) = self.env.look_var(function.node).cloned() { // TODO: remove this clone.
                    return match entry {
                        Entry::Fun { ref llvm_function, ref parameters, ref result, .. } => {
                            let mut expr_args = vec![];
                            if parameters.len() != args.len() {
                                self.add_error(Error::InvalidNumberOfParams {
                                    actual: args.len(),
                                    expected: parameters.len(),
                                    pos,
                                }, ());
                            }
                            for (arg, param) in args.into_iter().zip(parameters) {
                                let exp = self.trans_exp(arg);
                                self.check_types(param, &exp.typ, exp.pos);
                                expr_args.push(exp);
                            }
                            TypedExpr {
                                expr: tast::Expr::Call { args: expr_args, llvm_function: llvm_function.clone() },
                                pos,
                                typ: self.actual_ty_var(result),
                            }
                        },
                        _ => unreachable!(),
                    };
                }
                let help = call_with_array_help(&args, &self.env, &function, &self.strings);
                return self.undefined_function(function.node, function.pos, help);
            },
            Expr::EmptyTuple => {
                TypedExpr {
                    expr: tast::Expr::EmptyTuple,
                    pos,
                    typ: Type::Unit,
                }
            },
            Expr::If { else_, condition, then } => {
                let condition = Box::new(self.trans_exp(*condition));
                self.check_bool(&condition, condition.pos);
                let if_expr = Box::new(self.trans_exp(*then));
                let (else_, typ) =
                    match else_ {
                        Some(else_) => {
                            let else_expr = Box::new(self.trans_exp(*else_));
                            self.check_types(&if_expr.typ, &else_expr.typ, else_expr.pos);
                            (Some(else_expr), if_expr.typ.clone())
                        },
                        None => {
                            self.check_types(&Type::Unit, &if_expr.typ, if_expr.pos);
                            (None, Type::Unit)
                        },
                    };
                TypedExpr {
                    expr: tast::Expr::If { else_, condition, then: if_expr },
                    pos,
                    typ,
                }
            },
            Expr::Int { value } =>
                TypedExpr {
                    expr: tast::Expr::Int { value },
                    pos,
                    typ: Type::Int32,
                },
            Expr::Decl(declaration) => {
                let old_in_loop = self.in_loop;
                // TODO: why set in_loop to false here?
                self.in_loop = false;
                let declaration = self.trans_dec(*declaration);
                self.in_loop = old_in_loop;
                match declaration {
                    Some(declaration) =>
                        TypedExpr {
                            expr: tast::Expr::Decl(Box::new(declaration)),
                            pos,
                            typ: Type::Unit,
                        },
                    None => exp_type_error(),
                }
            },
            Expr::Nil =>
                TypedExpr {
                    expr: tast::Expr::Nil,
                    pos,
                    typ: Type::Nil,
                },
            Expr::Oper { left, oper: oper@WithPos { node: Operator::Plus, .. }, right }
            | Expr::Oper { left, oper: oper@WithPos { node: Operator::Minus, .. }, right }
            | Expr::Oper { left, oper: oper@WithPos { node: Operator::Times, .. }, right }
            | Expr::Oper { left, oper: oper@WithPos { node: Operator::And, .. }, right }
            | Expr::Oper { left, oper: oper@WithPos { node: Operator::Or, .. }, right }
            | Expr::Oper { left, oper: oper@WithPos { node: Operator::Divide, .. }, right } =>
                self.check_binary_op(oper, *left, *right, pos),
            Expr::Oper { left, oper: oper@WithPos { node: Operator::Equal, .. }, right }
            | Expr::Oper { left, oper: oper@WithPos { node: Operator::Neq, .. }, right }
            | Expr::Oper { left, oper: oper@WithPos { node: Operator::Lt, .. }, right }
            | Expr::Oper { left, oper: oper@WithPos { node: Operator::Gt, .. }, right }
            | Expr::Oper { left, oper: oper@WithPos { node: Operator::Ge, .. }, right }
            | Expr::Oper { left, oper: oper@WithPos { node: Operator::Le, .. }, right } => {
                let left = Box::new(self.trans_exp(*left));
                let right = Box::new(self.trans_exp(*right));
                self.check_int(&left, left.pos);
                self.check_int(&right, right.pos);
                TypedExpr {
                    expr: tast::Expr::Oper { left, oper: oper.clone(), right },
                    pos,
                    typ: Type::Bool,
                }
            },
            Expr::Struct { mut fields, typ } => {
                let ty = self.get_type(&typ, AddError);
                let mut field_exprs = vec![];
                match ty {
                    Type::Struct(_, ref type_fields, _) => {
                        for &(type_field_name, ref type_field) in type_fields {
                            if let Some(index) = fields.iter().position(|field| field.node.ident == type_field_name) {
                                let field = fields.remove(index);
                                let expr = self.trans_exp(field.node.expr);
                                self.check_types(&type_field, &expr.typ, expr.pos);
                                field_exprs.push(WithPos::new(tast::StructField { expr, ident: field.node.ident }, field.pos));
                            }
                            else {
                                return self.missing_field(type_field_name, &typ);
                            }
                        }

                        for field in fields {
                            let found = type_fields.iter()
                                .any(|&(type_field_name, _)| field.node.ident == type_field_name);
                            if !found {
                                return self.extra_field(&field, &typ);
                            }
                        }
                    },
                    Type::Error => (),
                    _ =>
                        return self.add_error(Error::UnexpectedType {
                            kind: "struct".to_string(),
                            pos: typ.pos,
                        }, exp_type_error()),
                }
                TypedExpr {
                    expr: tast::Expr::Struct { fields: field_exprs, typ },
                    pos,
                    typ: ty,
                }
            },
            Expr::Sequence(exprs) => {
                self.env.begin_scope();
                let new_exprs: Vec<_> = exprs.into_iter()
                    .map(|expr| self.trans_exp(expr))
                    .collect();
                self.env.end_scope();
                let typ = new_exprs.last().cloned().expect("Unexpected empty sequence").typ;
                TypedExpr {
                    expr: tast::Expr::Sequence(new_exprs),
                    pos,
                    typ,
                }
            },
            Expr::Str { ref value } =>
                TypedExpr {
                    expr: tast::Expr::Str { value: value.clone() },
                    pos,
                    typ: Type::String,
                },
            Expr::Variable(var) => {
                let var = self.trans_var(var);
                let typ = var.typ.clone();
                TypedExpr {
                    expr: tast::Expr::Variable(var),
                    pos,
                    typ,
                }
            },
            Expr::While { body, condition } => {
                let condition = Box::new(self.trans_exp(*condition));
                self.check_bool(&condition, condition.pos);
                let old_in_loop = self.in_loop;
                self.in_loop = true;
                let body = Box::new(self.trans_exp(*body));
                self.in_loop = old_in_loop;
                TypedExpr {
                    expr: tast::Expr::While {
                        body,
                        condition,
                    },
                    pos,
                    typ: Type::Unit,
                }
            },
        }
    }

    fn trans_extern(&mut self, prototype: ExternFuncDeclarationWithPos) {
        let result = prototype.node.result.map(|typ| self.trans_ty(&typ)).unwrap_or(Type::Unit);
        let params = prototype.node.params.iter().map(|field| self.trans_ty(&field.node.typ)).collect();
        self.env.add_function(&self.symbol(prototype.node.name), params, result, &self.module)
    }

    fn trans_fun(&mut self, function: FuncDeclarationWithPos) -> TypedDeclaration {
        let pos = function.pos;
        let function = function.node;
        // TODO: error when name already exist?
        let mut param_names = vec![];
        let mut parameters = vec![];
        let mut param_set = HashSet::new();
        for param in &function.params {
            parameters.push(self.trans_ty(&param.node.typ));
            param_names.push(param.node.name);
            if !param_set.insert(param.node.name) {
                self.duplicate_param(&param);
            }
        }

        let result_type =
            if let Some(ref result) = function.result {
                let return_type = self.trans_ty(result);
                if Some(function.name) == self.strings.symbol("main") {
                    self.add_error(Error::Type {
                        expected: Type::Unit,
                        pos,
                        unexpected: return_type.clone(),
                    }, ());
                }
                return_type
            }
            else {
                Type::Unit
            };
        let mut param_names = vec![];
        let mut parameters = vec![];
        for param in &function.params {
            let typ = self.trans_ty(&param.node.typ);
            parameters.push(typ.clone());
            param_names.push(param.node.name);
            }

        let llvm_function = gen::function(&self.module, &result_type, &parameters, function.name, &self.strings);
        let previous_function = self.current_function.clone();
        self.current_function = Some(llvm_function.clone());

        let mut new_params = vec![];
        let mut values = vec![];
        for (param, typ) in function.params.iter().zip(&parameters) {
            let value = gen::create_entry_block_alloca(&llvm_function, &self.symbol(param.node.name), &typ);
            values.push(value.clone());
            new_params.push(WithPos::new(tast::Param {
                escape: param.node.escape,
                name: param.node.name,
                typ: typ.clone(),
                value,
            }, param.pos));
            }

        self.env.enter_var(function.name, Entry::Fun {
            llvm_function: llvm_function.clone(),
            parameters: parameters.clone(),
            result: result_type.clone(),
        });

        self.env.begin_scope();
        for ((param, name), value) in parameters.into_iter().zip(param_names).zip(values) {
            self.env.enter_var(name, Entry::Var { typ: param, value });
            }
        let exp = self.trans_exp(function.body);
        self.check_types(&result_type, &exp.typ, exp.pos);
        self.env.end_scope();
        let function = WithPos::new(tast::FuncDeclaration {
            body: exp,
            llvm_function,
            name: function.name,
            params: new_params,
            result_type,
        }, pos);

        self.current_function = previous_function;

        WithPos::new(tast::Declaration::Function(function), pos)
    }

    fn trans_struct_type(&mut self, struct_type: StructTypeWithPos) -> tast::StructTypeWithPos {
        let mut fields = vec![];
        for field in &struct_type.node.fields {
            fields.push(WithPos::new(tast::Field {
                escape: field.node.escape,
                name: field.node.name,
                typ: self.trans_ty(&field.node.typ),
            }, field.pos));
        }
        WithPos::new(tast::StructType {
            fields,
            typ: struct_type.node.typ,
        }, struct_type.pos)
    }

    fn trans_ty(&mut self, ty: &TyWithPos) -> Type {
        match ty.node {
            Ty::Array { size, ref typ } => {
                Type::Array(Box::new(self.trans_ty(typ)), size)
            },
            Ty::Name { ref ident } => self.get_type(ident, AddError),
        }
    }

    fn trans_var(&mut self, var: VarWithPos) -> TypedVar {
        match var.node {
            Var::Field { ident, this } => {
                let this = Box::new(self.trans_var(*this));
                match this.typ {
                    Type::Struct(struct_type, ref fields, _) => {
                        for (index, &(name, ref typ)) in fields.iter().enumerate() {
                            if name == ident.node {
                                return TypedVar {
                                    pos: var.pos,
                                    typ: typ.clone(),
                                    var: tast::Var::Field {
                                        index,
                                        this,
                                    },
                                };
                            }
                        }
                        self.unexpected_field(&ident, ident.pos, struct_type)
                    },
                    typ =>
                        return self.add_error(Error::NotAStruct {
                            pos: this.pos,
                            typ: typ.to_string(),
                        }, var_type_error()),
                }
            },
            Var::Simple { ident } => {
                match self.env.look_var(ident.node).cloned() { // TODO: remove this clone.
                    Some(Entry::Var { typ, value }) => {
                        return TypedVar {
                            pos: var.pos,
                            typ: self.actual_ty_var(&typ),
                            var: tast::Var::Simple { value },
                        };
                    },
                    Some(Entry::Fun { llvm_function, result, .. }) => {
                        // TODO: check for no parameters?
                        return TypedVar {
                            pos: var.pos,
                            typ: self.actual_ty_var(&result),
                            var: tast::Var::Global { llvm_function },
                        };
                    },
                    _ => self.undefined_variable(ident.node, var.pos),
                }
            },
            Var::Subscript { expr, this } => {
                let var = Box::new(self.trans_var(*this));
                let subscript_expr = Box::new(self.trans_exp(*expr));
                self.check_int(&subscript_expr, subscript_expr.pos);
                match var.typ.clone() {
                    Type::Array(typ, _) => TypedVar {
                        typ: self.actual_ty_var(&typ),
                        pos: var.pos,
                        var: tast::Var::Subscript { expr: subscript_expr, this: var },
                    },
                    Type::Error => var_type_error(),
                    typ =>
                        self.add_error(Error::CannotIndex {
                            pos: var.pos,
                            typ: typ.to_string(),
                        }, var_type_error()),
                }
            },
        }
    }

    fn duplicate_param(&mut self, param: &FieldWithPos) {
        let ident = self.env.var_name(param.node.name).to_string();
        self.add_error(Error::DuplicateParam {
            ident,
            pos: param.pos,
        }, ())
    }

    fn extra_field(&mut self, field: &StructFieldWithPos, typ: &SymbolWithPos) -> TypedExpr {
        let ident = self.env.type_name(field.node.ident);
        let struct_name = self.env.type_name(typ.node);
        self.add_error(Error::ExtraField {
            ident,
            pos: field.pos,
            struct_name,
        }, exp_type_error())
    }

    fn missing_field(&mut self, field_type: Symbol, typ: &SymbolWithPos) -> TypedExpr {
        let ident = self.env.type_name(field_type);
        let struct_name = self.env.type_name(typ.node);
        self.add_error(Error::MissingField {
            ident,
            pos: typ.pos,
            struct_name,
        }, exp_type_error())
    }

    fn symbol(&self, symbol: Symbol) -> String {
        self.strings.get(symbol).expect("symbol")
    }

    fn undefined_function(&mut self, ident: Symbol, pos: Pos, help: Option<String>) -> TypedExpr {
        let ident = self.env.var_name(ident).to_string();
        self.add_error(Error::Undefined {
            help,
            ident,
            item: "function".to_string(),
            pos,
        }, exp_type_error())
    }

    fn undefined_type(&mut self, symbol: &SymbolWithPos) -> Type {
        let ident = self.env.type_name(symbol.node);
        self.add_error(Error::Undefined {
            help: None,
            ident,
            item: "type".to_string(),
            pos: symbol.pos,
        }, Type::Error)
    }

    fn undefined_variable(&mut self, ident: Symbol, pos: Pos) -> TypedVar {
        let ident = self.env.var_name(ident).to_string();
        self.add_error(Error::Undefined {
            help: None,
            ident,
            item: "variable".to_string(),
            pos,
        }, var_type_error())
    }

    fn unexpected_field(&mut self, ident: &SymbolWithPos, pos: Pos, typ: Symbol) -> TypedVar {
        let ident = self.env.type_name(ident.node);
        let struct_name = self.env.type_name(typ);
        self.add_error(Error::UnexpectedField {
            ident,
            pos,
            struct_name,
        }, var_type_error())
    }

    fn current_function(&self) -> Function {
        self.current_function.clone().expect("function")
    }
}
