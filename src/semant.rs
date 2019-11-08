/*
 * Copyright (c) 2017-2019 Boucher, Antoni <bouanto@zoho.com>
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

use std::collections::HashSet;
use std::rc::Rc;

use ast::{
    Declaration,
    DeclarationWithPos,
    Expr,
    ExprWithPos,
    FieldWithPos,
    FuncDeclaration,
    Operator,
    OperatorWithPos,
    RecordFieldWithPos,
    Ty,
    TypeDec,
    TypeDecWithPos,
    TyWithPos,
    Var,
    VarWithPos,
};
use env::{Env, Entry};
use error::{Error, Result};
use position::{Pos, WithPos};
use self::AddError::*;
use symbol::{Strings, Symbol, SymbolWithPos};
use tast::{
    self,
    TypedDeclaration,
    TypedExpr,
    TypedVar,
};
use temp::Label;
use types::{Type, Unique};

#[derive(PartialEq)]
enum AddError {
    AddError,
    DontAddError,
}

fn exp_type_error() -> TypedExpr {
    TypedExpr {
        expr: tast::Expr::Nil,
        pos: Pos::dummy(),
        typ: Type::Error,
    }
}

pub struct SemanticAnalyzer<'a> {
    env: &'a mut Env,
    errors: Vec<Error>,
    in_loop: bool,
    strings: Rc<Strings>,
}

impl<'a> SemanticAnalyzer<'a> {
    pub fn new(env: &'a mut Env, strings: Rc<Strings>) -> Self {
        SemanticAnalyzer {
            env,
            errors: vec![],
            in_loop: false,
            strings,
        }
    }

    fn add_error<T>(&mut self, error: Error, node: T) -> T {
        self.errors.push(error);
        node
    }

    pub fn analyze(mut self, main_symbol: Symbol, expr: ExprWithPos) -> Result<TypedDeclaration> {
        let pos = expr.pos;
        let body = WithPos::new(
            Expr::Sequence(vec![expr, WithPos::new(Expr::Int { value: 0 }, pos)]),
            pos,
        );
        let result = Some(WithPos::new(self.env.type_symbol("int"), pos));
        self.trans_dec(&WithPos::new(Declaration::Function(vec![
            WithPos::new(FuncDeclaration {
                body,
                name: main_symbol,
                params: vec![],
                result,
            }, pos)
        ]), pos), None);
        if self.errors.is_empty() {
            Ok(())
        }
        else {
            Err(Error::Multi(self.errors))
        }
    }

    fn actual_ty(&mut self, typ: &Type) -> Type {
        match *typ {
            Type::Name(_, Some(ref typ)) => *typ.clone(),
            Type::Name(ref symbol, None) => self.get_type(symbol, DontAddError),
            ref typ => typ.clone(),
        }
    }

    fn actual_ty_var(&mut self, typ: &Type) -> Type {
        let typ =
            match *typ {
                Type::Name(_, Some(ref typ)) => *typ.clone(),
                Type::Name(ref symbol, None) =>
                    match self.get_var(symbol) {
                        Entry::Var { ref typ, .. } => typ.clone(),
                        _ => panic!("type should be a variable, not a function"),
                    },
                ref typ => typ.clone(),
            };
        typ
    }

    fn check_binary_op(&mut self, oper: OperatorWithPos, left: &ExprWithPos, right: &ExprWithPos, done_label: Option<Label>, pos: Pos) -> TypedExpr
    {
        let left_pos = left.pos;
        let left = Box::new(self.trans_exp(left, done_label.clone()));
        self.check_int(&left, left_pos);
        let right_pos = right.pos;
        let right = Box::new(self.trans_exp(right, done_label));
        self.check_int(&right, right_pos);
        TypedExpr {
            expr: tast::Expr::Oper { left, oper, right },
            pos,
            typ: Type::Int,
        }
    }

    fn check_duplicate_types(&mut self, types: &[TypeDecWithPos]) {
        let mut names = HashSet::new();
        for typ in types {
            names.insert(typ.node.name.node);
            if let Ty::Name { ref ident } = typ.node.ty.node {
                if names.contains(&ident.node) {
                    return self.add_error(Error::Cycle {
                        pos: typ.node.ty.pos,
                    }, ());
                }
            }
        }
    }

    fn check_int(&mut self, expr: &TypedExpr, pos: Pos) {
        if expr.typ != Type::Int && expr.typ != Type::Error {
            return self.add_error(Error::Type {
                expected: Type::Int,
                pos,
                unexpected: expr.typ.clone(),
            }, ());
        }
    }

    fn check_types(&mut self, expected: &Type, unexpected: &Type, pos: Pos) {
        let expected = self.actual_ty(expected);
        let unexpected = self.actual_ty(unexpected);
        if expected != unexpected && expected != Type::Error && unexpected != Type::Error {
            if let Type::Record(_, _, _) = expected {
                if unexpected == Type::Nil {
                    return;
                }
            }
            return self.add_error(Error::Type {
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

    fn get_var(&mut self, symbol: &SymbolWithPos) -> Entry {
        if let Some(entry) = self.env.look_var(symbol.node) {
            return entry.clone();
        }
        self.undefined_identifier(symbol)
    }

    fn trans_dec(&mut self, declaration: &DeclarationWithPos, done_label: Option<Label>) -> Option<TypedDeclaration> {
        match declaration.node {
            Declaration::Function(ref declarations) => {
                for &WithPos { node: FuncDeclaration { name, ref params, ref result, .. }, .. } in declarations {
                    let result_type =
                        if let Some(ref result) = *result {
                            self.get_type(result, AddError)
                        }
                        else {
                            Type::Unit
                        };
                    // TODO: error when name already exist?
                    let mut param_names = vec![];
                    let mut parameters = vec![];
                    let mut param_set = HashSet::new();
                    for param in params {
                        parameters.push(self.get_type(&param.node.typ, AddError));
                        param_names.push(param.node.name);
                        if !param_set.insert(param.node.name) {
                            self.duplicate_param(param);
                        }
                    }
                    let func_name = self.strings.get(name).expect("strings get");
                    self.env.enter_var(name, Entry::Fun {
                        external: false,
                        label: Label::with_name(&func_name),
                        parameters,
                        result: result_type.clone(),
                    });
                }

                for WithPos { node: FuncDeclaration { ref params, ref body, ref result, .. }, .. } in declarations {
                    let result_type =
                        if let Some(ref result) = *result {
                            self.get_type(result, DontAddError)
                        }
                        else {
                            Type::Unit
                        };
                    let mut param_names = vec![];
                    let mut parameters = vec![];
                    for param in params {
                        parameters.push(self.get_type(&param.node.typ, DontAddError));
                        param_names.push(param.node.name);
                    }
                    self.env.begin_scope();
                    for (param, name) in parameters.into_iter().zip(param_names) {
                        self.env.enter_var(name, Entry::Var { typ: param, value: unimplemented!() });
                    }
                    let exp = self.trans_exp(body, done_label.clone());
                    self.check_types(&result_type, &exp.typ, body.pos);
                    self.env.end_scope();
                }
            },
            Declaration::Type(ref type_declarations) => {
                self.check_duplicate_types(type_declarations);
                for &WithPos { node: TypeDec { ref name, .. }, .. } in type_declarations {
                    self.env.enter_type(name.node, Type::Name(name.clone(), None));
                }

                for &WithPos { node: TypeDec { ref name, ref ty }, .. } in type_declarations {
                    let new_type = self.trans_ty(name.node, ty);
                    self.env.replace_type(name.node, new_type);
                }
            },
            Declaration::VariableDeclaration { escape, ref init, name, ref typ, .. } => {
                let init = self.trans_exp(init, done_label);
                if let Some(ref ident) = *typ {
                    let typ = self.get_type(ident, AddError);
                    self.check_types(&typ, &init.typ, ident.pos);
                } else if init.typ == Type::Nil {
                    return self.add_error(Error::RecordType { pos: declaration.pos }, None);
                }
                self.env.enter_var(name, Entry::Var { typ: init.typ, value: unimplemented!() });
                Some(tast::Declaration::VariableDeclaration {
                    escape,
                    init,
                    name,
                    typ,
                })
            },
        }
    }

    pub fn trans_exp(&mut self, expr: &ExprWithPos, done_label: Option<Label>) -> TypedExpr {
        match expr.node {
            Expr::Array { ref init, ref size, ref typ } => {
                let size_expr = self.trans_exp(size, done_label.clone());
                self.check_int(&size_expr, size.pos);
                let ty = self.get_type(typ, AddError);
                let init_expr = self.trans_exp(init, done_label);
                match ty {
                    Type::Array(ref typ, _) =>
                        self.check_types(typ, &init_expr.typ, init.pos),
                    Type::Error => (),
                    _ =>
                        return self.add_error(Error::UnexpectedType {
                            kind: "array".to_string(),
                            pos: typ.pos,
                        }, exp_type_error()),
                }
                TypedExpr {
                    expr: tast::Expr::Array { init: Box::new(init_expr), size: Box::new(size_expr), typ: typ.clone() },
                    pos: expr.pos,
                    typ: ty,
                }
            },
            Expr::Assign { ref expr, ref var } => {
                let var = self.trans_var(var, done_label.clone());
                let expr_expr = self.trans_exp(expr, done_label);
                self.check_types(&var.ty, &expr_expr.ty, expr.pos);
                TypedExpr {
                    exp: unimplemented!(),
                    ty: Type::Unit,
                }
            },
            Expr::Break => {
                if !self.in_loop {
                    return self.add_error(Error::BreakOutsideLoop {
                        pos: expr.pos,
                    }, exp_type_error());
                }
                TypedExpr {
                    expr: tast::Expr::Break,
                    pos: expr.pos,
                    typ: Type::Unit,
                }
            },
            Expr::Call { ref args, function } => {
                if let Some(entry@Entry::Fun { .. }) = self.env.look_var(function).cloned() { // TODO: remove this clone.
                    return match entry {
                        Entry::Fun { ref label, ref llvm_function, ref parameters, ref result } => {
                            let mut expr_args = vec![];
                            for (arg, param) in args.iter().zip(parameters) {
                                let exp = self.trans_exp(arg, done_label.clone());
                                self.check_types(param, &exp.typ, arg.pos);
                                expr_args.push(exp);
                            }
                            TypedExpr {
                                expr: tast::Expr::Call { args: expr_args, function },
                                pos: expr.pos,
                                typ: self.actual_ty_var(result),
                            }
                        },
                        _ => unreachable!(),
                    };
                }
                return self.undefined_function(function, expr.pos);
            },
            Expr::If { ref else_, ref test, ref then } => {
                let test = Box::new(self.trans_exp(test, done_label.clone()));
                self.check_int(&test, then.pos);
                let if_expr = Box::new(self.trans_exp(then, done_label.clone()));
                let (else_, typ) =
                    match *else_ {
                        Some(ref else_) => {
                            let else_expr = Box::new(self.trans_exp(&else_, done_label));
                            self.check_types(&if_expr.typ, &else_expr.typ, else_.pos);
                            (Some(else_expr), if_expr.typ)
                        },
                        None => {
                            self.check_types(&Type::Unit, &if_expr.typ, then.pos);
                            (None, Type::Unit)
                        },
                    };
                TypedExpr {
                    expr: tast::Expr::If { else_, test, then: if_expr },
                    pos: expr.pos,
                    typ,
                }
            },
            Expr::Int { value } =>
                TypedExpr {
                    expr: tast::Expr::Int { value },
                    pos: expr.pos,
                    typ: Type::Int,
                },
            Expr::Let { ref body, ref declarations } => {
                let old_in_loop = self.in_loop;
                self.in_loop = false;
                self.env.begin_scope();
                for declaration in declarations {
                    self.trans_dec(declaration, done_label.clone());
                }
                self.in_loop = old_in_loop;
                let result = self.trans_exp(body, done_label);
                self.env.end_scope();
                result
            },
            Expr::Nil =>
                TypedExpr {
                    expr: tast::Expr::Nil,
                    pos: expr.pos,
                    typ: Type::Nil,
                },
            Expr::Oper { ref left, oper: oper@WithPos { node: Operator::Plus, .. }, ref right }
            | Expr::Oper { ref left, oper: oper@WithPos { node: Operator::Minus, .. }, ref right }
            | Expr::Oper { ref left, oper: oper@WithPos { node: Operator::Times, .. }, ref right }
            | Expr::Oper { ref left, oper: oper@WithPos { node: Operator::And, .. }, ref right }
            | Expr::Oper { ref left, oper: oper@WithPos { node: Operator::Or, .. }, ref right }
            | Expr::Oper { ref left, oper: oper@WithPos { node: Operator::Divide, .. }, ref right } =>
                self.check_binary_op(oper, left, right, done_label, expr.pos),
            Expr::Oper { ref left, oper: oper@WithPos { node: Operator::Equal, .. }, ref right }
            | Expr::Oper { ref left, oper: oper@WithPos { node: Operator::Neq, .. }, ref right }
            | Expr::Oper { ref left, oper: oper@WithPos { node: Operator::Lt, .. }, ref right }
            | Expr::Oper { ref left, oper: oper@WithPos { node: Operator::Gt, .. }, ref right }
            | Expr::Oper { ref left, oper: oper@WithPos { node: Operator::Ge, .. }, ref right }
            | Expr::Oper { ref left, oper: oper@WithPos { node: Operator::Le, .. }, ref right } => {
                let left = Box::new(self.trans_exp(left, done_label.clone()));
                let right_pos = right.pos;
                let right = Box::new(self.trans_exp(right, done_label));
                self.check_int(&left, left.pos);
                self.check_int(&right, right_pos);
                TypedExpr {
                    expr: tast::Expr::Oper { left, oper, right },
                    pos: expr.pos,
                    typ: Type::Int,
                }
            },
            Expr::Record { ref fields, ref typ } => {
                let ty = self.get_type(typ, AddError);
                let mut field_exprs = vec![];
                match ty {
                    Type::Record(_, ref type_fields, _) => {
                        for &(type_field_name, ref type_field) in type_fields {
                            let mut found = false;
                            for field in fields {
                                if type_field_name == field.node.ident {
                                    found = true;
                                    let expr = self.trans_exp(&field.node.expr, done_label.clone());
                                    self.check_types(&type_field, &expr.typ, field.node.expr.pos);
                                    field_exprs.push(WithPos::new(tast::RecordField { expr, ident: field.node.ident }, field.pos));
                                }
                            }
                            if !found {
                                return self.missing_field(type_field_name, typ);
                            }
                        }

                        for field in fields {
                            let found = type_fields.iter()
                                .any(|&(type_field_name, _)| field.node.ident == type_field_name);
                            if !found {
                                return self.extra_field(field, typ);
                            }
                        }
                    },
                    Type::Error => (),
                    _ =>
                        return self.add_error(Error::UnexpectedType {
                            kind: "record".to_string(),
                            pos: typ.pos,
                        }, exp_type_error()),
                }
                TypedExpr {
                    expr: tast::Expr::Record { fields: field_exprs, typ },
                    pos: expr.pos,
                    typ,
                }
            },
            Expr::Sequence(ref exprs) => {
                if let Some((last_expr, exprs)) = exprs.split_last() {
                    let mut new_exprs = vec![];
                    for expr in exprs {
                        new_exprs.push(self.trans_exp(expr, done_label.clone()));
                    }
                    let last_expr = self.trans_exp(last_expr, done_label);
                    if new_exprs.is_empty() {
                        last_expr
                    }
                    else {
                        unimplemented!();
                    }
                }
                else {
                    panic!("Unexpected empty sequence.");
                }
            },
            Expr::Str { ref value } =>
                TypedExpr {
                    expr: tast::Expr::Str { value: value.clone() },
                    pos: expr.pos,
                    typ: Type::String,
                },
            Expr::Variable(ref var) => self.trans_var(var, done_label),
            Expr::While { ref body, ref test } => {
                let test_expr = self.trans_exp(test, done_label);
                self.check_int(&test_expr, test.pos);
                let old_in_loop = self.in_loop;
                self.in_loop = true;
                let while_done_label = Label::new();
                let result = self.trans_exp(body, Some(while_done_label.clone()));
                self.in_loop = old_in_loop;
                result
            },
        }
    }

    fn trans_ty(&mut self, symbol: Symbol, ty: &TyWithPos) -> Type {
        match ty.node {
            Ty::Array { ref ident } => {
                let ty = self.get_type(ident, AddError);
                Type::Array(Box::new(ty), Unique::new())
            },
            Ty::Name { ref ident } => self.get_type(ident, AddError),
            Ty::Record { ref fields } => {
                let mut record_fields = vec![];
                for field in fields {
                    let typ = self.get_type(&field.node.typ, AddError);
                    record_fields.push((field.node.name, typ));
                }
                Type::Record(symbol, record_fields, Unique::new())
            },
        }
    }

    fn trans_var(&mut self, var: &VarWithPos, done_label: Option<Label>) -> TypedVar {
        match var.node {
            Var::Field { ref ident, ref this } => {
                let this = Box::new(self.trans_var(this, done_label));
                match this.typ {
                    Type::Record(record_type, ref fields, _) => {
                        for (index, &(name, ref typ)) in fields.iter().enumerate() {
                            if name == ident.node {
                                return TypedVar {
                                    pos: var.pos,
                                    typ: typ.clone(),
                                    var: tast::Var::Field {
                                        ident: ident.clone(),
                                        this,
                                    },
                                };
                            }
                        }
                        self.unexpected_field(ident, ident.pos, record_type)
                    },
                    typ =>
                        return self.add_error(Error::NotARecord {
                            pos: this.pos,
                            typ: typ.to_string(),
                        }, exp_type_error()),
                }
            },
            Var::Simple { ref ident } => {
                if let Some(Entry::Var { ref typ, .. }) = self.env.look_var(ident.node).cloned() { // TODO: remove this clone.
                    return TypedExpr {
                        exp: unimplemented!(),
                        ty: self.actual_ty_var(typ),
                    };
                }
                self.undefined_variable(ident.node, var.pos)
            },
            Var::Subscript { ref expr, ref this } => {
                let var = self.trans_var(this, done_label.clone());
                let subscript_expr = self.trans_exp(expr, done_label);
                self.check_int(&subscript_expr, expr.pos);
                match var.ty {
                    Type::Array(typ, _) => TypedExpr {
                        exp: array_subscript(var.exp, subscript_expr.exp),
                        ty: self.actual_ty_var(&typ),
                    },
                    Type::Error => exp_type_error(),
                    typ =>
                        self.add_error(Error::CannotIndex {
                            pos: this.pos,
                            typ: typ.to_string(),
                        }, exp_type_error()),
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

    fn extra_field(&mut self, field: &RecordFieldWithPos, typ: &SymbolWithPos) -> TypedExpr {
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

    fn undefined_function(&mut self, ident: Symbol, pos: Pos) -> TypedExpr {
        let ident = self.env.var_name(ident).to_string();
        self.add_error(Error::Undefined {
            ident,
            item: "function".to_string(),
            pos,
        }, exp_type_error())
    }

    fn undefined_identifier(&mut self, symbol: &SymbolWithPos) -> Entry {
        let ident = self.env.type_name(symbol.node);
        self.add_error(Error::Undefined {
            ident,
            item: "identifier".to_string(),
            pos: symbol.pos,
        }, Entry::Error)
    }

    fn undefined_type(&mut self, symbol: &SymbolWithPos) -> Type {
        let ident = self.env.type_name(symbol.node);
        self.add_error(Error::Undefined {
            ident,
            item: "type".to_string(),
            pos: symbol.pos,
        }, Type::Error)
    }

    fn undefined_variable(&mut self, ident: Symbol, pos: Pos) -> TypedExpr {
        let ident = self.env.var_name(ident).to_string();
        self.add_error(Error::Undefined {
            ident,
            item: "variable".to_string(),
            pos,
        }, exp_type_error())
    }

    fn unexpected_field(&mut self, ident: &SymbolWithPos, pos: Pos, typ: Symbol) -> TypedExpr {
        let ident = self.env.type_name(ident.node);
        let struct_name = self.env.type_name(typ);
        self.add_error(Error::UnexpectedField {
            ident,
            pos,
            struct_name,
        }, exp_type_error())
    }
}
