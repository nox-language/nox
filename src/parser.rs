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

// TODO: use precedence-based parsing.

/*
 * Operator precedence:
 * -
 * * /
 * + -
 * = <> > < >= <=
 * &
 * |
 */

use std::io::Read;
use std::result;

use ast::{
    Declaration,
    DeclarationWithPos,
    Expr,
    ExprWithPos,
    Field,
    FieldWithPos,
    FuncDeclaration,
    FuncDeclarationWithPos,
    Operator,
    RecordField,
    RecordFieldWithPos,
    Ty,
    TypeDec,
    TypeDecWithPos,
    TyWithPos,
    Var,
    VarWithPos,
    dummy_var_expr,
};
use ast::Declaration::Variable;
use error::Error;
use error::Error::UnexpectedToken;
use lexer::Lexer;
use position::{Pos, WithPos};
use symbol::{Symbols, SymbolWithPos};
use token::{Tok, Token};
use token::Tok::*;

use self::Scope::{Global, Local};

macro_rules! eat {
    ($_self:ident, $pat:ident, $var:ident) => {
        match $_self.token() {
            Ok(token) => {
                match token.token {
                    $pat(var) => {
                        $var = var;
                        token.start
                    },
                    tok => return Err(UnexpectedToken {
                        expected: stringify!($pat).to_lowercase(),
                        pos: token.start,
                        unexpected: tok,
                    }),
                }
            },
            Err(error) => return Err(error),
        }
    };
    ($_self:ident, $pat:ident) => {
        eat!($_self, $pat, stringify!($pat).to_lowercase())
    };
    ($_self:ident, $pat:ident, $expected:expr) => {
        match $_self.token() {
            Ok(token) => {
                match token.token {
                    $pat => token.start,
                    tok => return Err(UnexpectedToken {
                        expected: $expected,
                        pos: token.start,
                        unexpected: tok,
                    }),
                }
            },
            Err(error) => return Err(error),
        }
    };
}

macro_rules! fields {
    ($_self:ident, $pat:ident) => {
        match $_self.peek()?.token {
            $pat => vec![],
            _ => $_self.fields()?,
        }
    };
}

#[derive(PartialEq)]
enum Scope {
    Global,
    Local,
}

pub type Result<T> = result::Result<T, Error>;

pub struct Parser<'a, R: Read> {
    lexer: Lexer<R>,
    lookahead: Option<Result<Token>>,
    symbols: &'a mut Symbols<()>,
}

impl<'a, R: Read> Parser<'a, R> {
    pub fn new(lexer: Lexer<R>, symbols: &'a mut Symbols<()>) -> Self {
        Parser {
            lexer,
            lookahead: None,
            symbols,
        }
    }

    fn additive_expr(&mut self) -> Result<ExprWithPos> {
        let mut expr = self.multiplicative_expr()?;
        loop {
            let oper =
                match self.peek_token() {
                    Ok(&Minus) => WithPos::new(Operator::Minus, eat!(self, Minus)),
                    Ok(&Plus) => WithPos::new(Operator::Plus, eat!(self, Plus)),
                    _ => break,
                };
            let right = Box::new(self.multiplicative_expr()?);
            let pos = expr.pos;
            expr = WithPos::new(Expr::Oper {
                left: Box::new(expr),
                oper,
                right,
            }, pos);
        }
        Ok(expr)
    }

    fn arr_ty(&mut self) -> Result<TyWithPos> {
        let pos = eat!(self, Array);
        eat!(self, Of);
        let type_name;
        let type_pos = eat!(self, Ident, type_name);
        let ident = self.symbols.symbol(&type_name);
        Ok(WithPos::new(Ty::Array {
            ident: WithPos::new(ident, type_pos),
        }, pos))
    }

    fn boolean(&mut self, value: bool) -> Result<ExprWithPos> {
        let pos = self.token()?.start;
        Ok(WithPos::new(Expr::Bool(value), pos))
    }

    fn break_(&mut self) -> Result<ExprWithPos> {
        let pos = eat!(self, Break);
        Ok(WithPos::new(Expr::Break, pos))
    }

    fn call_expr_or_other(&mut self) -> Result<ExprWithPos> {
        let name;
        let pos = eat!(self, Ident, name);
        let symbol = self.symbols.symbol(&name);
        if let OpenParen = self.peek()?.token {
            eat!(self, OpenParen);
            let mut args = vec![];
            loop {
                if let CloseParen = self.peek()?.token {
                    break;
                }
                let arg = self.expr()?;
                args.push(arg);
                match self.peek()?.token {
                    Comma => { self.token()?; },
                    _ => break,
                }
            }
            eat!(self, CloseParen);
            Ok(WithPos::new(Expr::Call {
                args,
                function: symbol,
            }, pos))
        }
        else {
            match self.peek()?.token {
                OpenCurly => self.rec_create(WithPos::new(symbol, pos), pos),
                _ => {
                    let var = WithPos::new(Var::Simple {
                        ident: WithPos::new(symbol, pos),
                    }, pos);
                    self.lvalue_or_assign(var)
                }
            }
        }
    }

    fn expr(&mut self) -> Result<ExprWithPos> {
        self.logical_or_expr()
    }

    fn field_dec(&mut self) -> Result<FieldWithPos> {
        let field_name;
        let pos = eat!(self, Ident, field_name);
        let name = self.symbols.symbol(&field_name);
        eat!(self, Colon);
        let type_name;
        let type_pos = eat!(self, Ident, type_name);
        let typ = self.symbols.symbol(&type_name);
        Ok(WithPos::new(Field {
            escape: false,
            name,
            typ: WithPos::new(typ, type_pos),
        }, pos))
    }

    fn field_exp(&mut self, var: VarWithPos) -> Result<VarWithPos> {
        eat!(self, Dot);
        let field_name;
        let pos = eat!(self, Ident, field_name);
        let var = WithPos::new(Var::Field {
            ident: WithPos::new(self.symbols.symbol(&field_name), pos),
            this: Box::new(var),
        }, pos);
        self.lvalue(var)
    }

    fn fields(&mut self) -> Result<Vec<FieldWithPos>> {
        let field = self.field_dec()?;
        let mut fields = vec![field];
        while let Comma = self.peek()?.token {
            eat!(self, Comma);
            fields.push(self.field_dec()?)
        }
        Ok(fields)
    }

    fn field_create(&mut self) -> Result<RecordFieldWithPos> {
        let field_name;
        let pos = eat!(self, Ident, field_name);
        let ident = self.symbols.symbol(&field_name);
        eat!(self, Equal);
        let expr = self.expr()?;
        Ok(WithPos::new(RecordField {
            expr,
            ident,
        }, pos))
    }

    fn for_loop(&mut self) -> Result<ExprWithPos> {
        let pos = eat!(self, For);
        let var_name;
        let var_pos = eat!(self, Ident, var_name);
        let var = self.symbols.symbol(&var_name);
        let iter_variable = WithPos::new(Var::Simple { ident: WithPos::new(var, var_pos) }, var_pos);
        let iter_variable_expr = WithPos::new(Expr::Variable(iter_variable.clone()), var_pos);
        eat!(self, ColonEqual);
        let start = self.expr()?;
        eat!(self, To);
        let end = self.expr()?;
        eat!(self, Do);
        let body = self.expr()?;
        // Convert for loop into while loop.
        let start_symbol = self.symbols.symbol(&var_name);
        let end_symbol = self.symbols.symbol(&format!("__{}_limit", var_name));
        let body =
            Expr::If {
                else_: None,
                condition: Box::new(
                    WithPos::dummy(Expr::Oper {
                        left: Box::new(iter_variable_expr.clone()),
                        oper: WithPos::dummy(Operator::Le),
                        right: Box::new(dummy_var_expr(end_symbol)),
                    })
                ),
                then:
                    Box::new(WithPos::dummy(Expr::While {
                        body: Box::new(WithPos::dummy(Expr::Sequence(vec![
                            body,
                            WithPos::dummy(Expr::If {
                                else_: Some(Box::new(WithPos::dummy(Expr::Break))),
                                condition:
                                    Box::new(WithPos::dummy(Expr::Oper {
                                        left: Box::new(iter_variable_expr.clone()),
                                        oper: WithPos::dummy(Operator::Lt),
                                        right: Box::new(dummy_var_expr(end_symbol)),
                                    })),
                                then:
                                    Box::new(WithPos::dummy(Expr::Assign {
                                        expr: Box::new(WithPos::dummy(Expr::Oper {
                                            left: Box::new(iter_variable_expr),
                                            oper: WithPos::dummy(Operator::Plus),
                                            right: Box::new(WithPos::dummy(Expr::Int { value: 1 })),
                                        })),
                                        var: iter_variable,
                                    })),
                            }),
                        ]))),
                        condition: Box::new(WithPos::dummy(Expr::Bool(true))),
                    })),
            };

        let declarations = Expr::Sequence(vec![
            WithPos::dummy(Expr::Decl(
                Box::new(WithPos::dummy(Variable {
                    escape: false,
                    init: start,
                    name: start_symbol,
                    typ: None,
                }))
            )),
            WithPos::dummy(Expr::Decl(
                Box::new(WithPos::dummy(Variable {
                    escape: false,
                    init: end,
                    name: end_symbol,
                    typ: None,
                }))
            )),
            WithPos::dummy(body)
        ]);

        Ok(WithPos::new(declarations, pos))
    }

    fn fun_dec(&mut self) -> Result<FuncDeclarationWithPos> {
        let pos = eat!(self, Fun);
        let func_name;
        eat!(self, Ident, func_name);
        let name = self.symbols.symbol(&func_name);
        eat!(self, OpenParen);
        let params = fields!(self, CloseParen);
        eat!(self, CloseParen);
        let result = self.optional_type()?;
        eat!(self, Equal);
        let body = self.expr()?;
        Ok(WithPos::new(FuncDeclaration {
            body,
            name,
            params,
            result,
        }, pos))
    }

    fn fun_expr(&mut self) -> Result<ExprWithPos> {
        let declaration = self.fun_dec()?;
        let pos = declaration.pos;
        Ok(WithPos::new(Expr::Decl(Box::new(WithPos::new(Declaration::Function(declaration), pos))), pos))
    }

    fn if_then_else(&mut self) -> Result<ExprWithPos> {
        let pos = eat!(self, If);
        let condition = Box::new(self.expr()?);
        eat!(self, Then);
        let then = Box::new(self.expr()?);
        let else_=
            if let Else = self.peek()?.token {
                eat!(self, Else);
                Some(Box::new(self.expr()?))
            }
            else {
                None
            };
        Ok(WithPos::new(Expr::If {
            else_,
            condition,
            then,
        }, pos))
    }

    fn int_lit(&mut self) -> Result<ExprWithPos> {
        let value;
        let pos = eat!(self, Int, value);
        Ok(WithPos::new(Expr::Int {
            value,
        }, pos))
    }

    // FIXME: var is not an expression.
    fn var_expr(&mut self) -> Result<ExprWithPos> {
        let declaration = self.var_dec(Local)?;
        let pos = declaration.pos;
        Ok(WithPos::new(Expr::Decl(Box::new(declaration)), pos))
    }

    fn logical_and_expr(&mut self) -> Result<ExprWithPos> {
        let mut expr = self.relational_expr()?;
        while let Ok(&Ampersand) = self.peek_token() {
            let oper_pos = eat!(self, Ampersand);
            let right = Box::new(self.relational_expr()?);
            let pos = expr.pos;
            expr = WithPos::new(Expr::Oper {
                left: Box::new(expr),
                oper: WithPos::new(Operator::And, oper_pos),
                right,
            }, pos);
        }
        Ok(expr)
    }

    fn logical_or_expr(&mut self) -> Result<ExprWithPos> {
        let mut expr = self.logical_and_expr()?;
        while let Ok(&Pipe) = self.peek_token() {
            let oper_pos = eat!(self, Pipe);
            let right = Box::new(self.logical_and_expr()?);
            let pos = expr.pos;
            expr = WithPos::new(Expr::Oper {
                left: Box::new(expr),
                oper: WithPos::new(Operator::Or, oper_pos),
                right,
            }, pos);
        }
        Ok(expr)
    }

    fn lvalue(&mut self, var: VarWithPos) -> Result<VarWithPos> {
        match self.peek()?.token {
            OpenSquare => self.subscript(var),
            Dot => self.field_exp(var),
            _ => Ok(var),
        }
    }

    fn lvalue_or_assign(&mut self, var: VarWithPos) -> Result<ExprWithPos> {
        let var = self.lvalue(var)?;
        let value =
            if let Of = self.peek()?.token {
                match var.node {
                    Var::Subscript { expr, this } => {
                        let pos = this.pos;
                        if let Var::Simple { ident } = this.node {
                            eat!(self, Of);
                            let init = Box::new(self.expr()?);
                            return Ok(WithPos::new(Expr::Array {
                                init,
                                size: expr,
                                typ: WithPos::new(ident.node, pos),
                            }, pos));
                        }
                        else {
                            return Err(self.unexpected_token("neither dot nor subscript")?);
                        }
                    },
                    _ => return Err(self.unexpected_token(":= or (nothing)")?),
                }
            }
            else {
                var
            };
        if let ColonEqual = self.peek()?.token {
            eat!(self, ColonEqual);
            let expr = Box::new(self.expr()?);
            let pos = expr.pos;
            Ok(WithPos::new(Expr::Assign {
                expr,
                var: value,
            }, pos))
        }
        else {
            let pos = value.pos;
            Ok(WithPos::new(Expr::Variable(value), pos))
        }
    }

    fn multiplicative_expr(&mut self) -> Result<ExprWithPos> {
        let mut expr = self.unary_expr()?;
        loop {
            let oper =
                match self.peek_token() {
                    Ok(&Slash) => WithPos::new(Operator::Divide, eat!(self, Slash)),
                    Ok(&Star) => WithPos::new(Operator::Times, eat!(self, Star)),
                    _ => break,
                };
            let right = Box::new(self.unary_expr()?);
            let pos = expr.pos;
            expr = WithPos::new(Expr::Oper {
                left: Box::new(expr),
                oper,
                right,
            }, pos);
        }
        Ok(expr)
    }

    fn nil(&mut self) -> Result<ExprWithPos> {
        let pos = eat!(self, Nil);
        Ok(WithPos::new(Expr::Nil, pos))
    }

    fn optional_type(&mut self) -> Result<Option<SymbolWithPos>> {
        let mut typ = None;
        if let Colon = self.peek()?.token {
            eat!(self, Colon);
            let type_name;
            let pos = eat!(self, Ident, type_name);
            let ident = self.symbols.symbol(&type_name);
            typ = Some(WithPos::new(ident, pos));
        }
        Ok(typ)
    }

    fn primary_expr(&mut self) -> Result<ExprWithPos> {
        match self.peek()?.token {
            Break => self.break_(),
            False => self.boolean(false),
            For => self.for_loop(),
            Fun => self.fun_expr(),
            If => self.if_then_else(),
            Ident(_) => self.call_expr_or_other(),
            Int(_) => self.int_lit(),
            Nil => self.nil(),
            OpenParen => self.seq_exp(),
            Str(_) => self.string_lit(),
            True => self.boolean(true),
            Var => self.var_expr(),
            While => self.while_loop(),
            _ => Err(self.unexpected_token("break, for, if, identifier, integer literal, let, nil, (, string literal, while")?),
        }
    }

    fn rec_create(&mut self, typ: SymbolWithPos, pos: Pos) -> Result<ExprWithPos> {
        eat!(self, OpenCurly);
        let field = self.field_create()?;
        let mut fields = vec![field];
        while let Comma = self.peek()?.token {
            eat!(self, Comma);
            fields.push(self.field_create()?)
        }
        eat!(self, CloseCurly);
        Ok(WithPos::new(Expr::Record {
            fields,
            typ,
        }, pos))
    }

    fn rec_ty(&mut self) -> Result<TyWithPos> {
        let pos = eat!(self, OpenCurly);
        let fields = fields!(self, CloseCurly);
        eat!(self, CloseCurly);
        Ok(WithPos::new(Ty::Record {
            fields,
        }, pos))
    }

    fn relational_expr(&mut self) -> Result<ExprWithPos> {
        let mut expr = self.additive_expr()?;
        loop {
            let oper =
                match self.peek_token() {
                    Ok(&Equal) => WithPos::new(Operator::Equal, eat!(self, Equal)),
                    Ok(&Greater) => WithPos::new(Operator::Gt, eat!(self, Greater)),
                    Ok(&GreaterOrEqual) => WithPos::new(Operator::Ge, eat!(self, GreaterOrEqual)),
                    Ok(&Lesser) => WithPos::new(Operator::Lt, eat!(self, Lesser)),
                    Ok(&LesserOrEqual) => WithPos::new(Operator::Le, eat!(self, LesserOrEqual)),
                    Ok(&NotEqual) => WithPos::new(Operator::Neq, eat!(self, NotEqual)),
                    _ => break,
                };
            let right = Box::new(self.additive_expr()?);
            let pos = expr.pos;
            expr = WithPos::new(Expr::Oper {
                left: Box::new(expr),
                oper,
                right,
            }, pos);
        }
        Ok(expr)
    }

    fn seq_exp(&mut self) -> Result<ExprWithPos> {
        eat!(self, OpenParen);
        let mut exprs = vec![self.expr()?];
        while let Semicolon = self.peek()?.token {
            eat!(self, Semicolon);
            exprs.push(self.expr()?);
        }
        eat!(self, CloseParen);
        let pos = exprs[0].pos;
        Ok(WithPos::new(Expr::Sequence(exprs), pos))
    }

    fn string_lit(&mut self) -> Result<ExprWithPos> {
        let value;
        let pos = eat!(self, Str, value);
        Ok(WithPos::new(Expr::Str {
            value,
        }, pos))
    }

    fn subscript(&mut self, var: VarWithPos) -> Result<VarWithPos> {
        eat!(self, OpenSquare);
        let expr = Box::new(self.expr()?);
        eat!(self, CloseSquare);
        let pos = var.pos;
        let var = WithPos::new(Var::Subscript {
            expr,
            this: Box::new(var),
        }, pos);
        self.lvalue(var)
    }

    fn ty(&mut self) -> Result<TyWithPos> {
        match self.peek()?.token {
            Array => self.arr_ty(),
            OpenCurly => self.rec_ty(),
            Ident(_) => {
                let type_name;
                let pos = eat!(self, Ident, type_name);
                let ident = self.symbols.symbol(&type_name);
                Ok(WithPos::new(Ty::Name {
                    ident: WithPos::new(ident, pos),
                }, pos))
            },
            _ => Err(self.unexpected_token("array, { or identifier")?),
        }
    }

    fn ty_decs(&mut self) -> Result<DeclarationWithPos> {
        let dec = self.ty_dec()?;
        let pos = dec.pos;
        Ok(WithPos::new(Declaration::Type(dec), pos))
    }

    fn ty_dec(&mut self) -> Result<TypeDecWithPos> {
        let pos = eat!(self, Type);
        let type_name;
        let name_pos = eat!(self, Ident, type_name);
        let name = self.symbols.symbol(&type_name);
        eat!(self, Equal);
        let ty = self.ty()?;
        Ok(WithPos::new(TypeDec {
            name: WithPos::new(name, name_pos),
            ty,
        }, pos))
    }

    fn unary_expr(&mut self) -> Result<ExprWithPos> {
        match self.peek()?.token {
            Minus => {
                let pos = eat!(self, Minus);
                let expr = self.unary_expr()?;
                Ok(WithPos::new(Expr::Oper {
                    left: Box::new(WithPos::new(Expr::Int {
                        value: 0,
                    }, pos)),
                    oper: WithPos::new(Operator::Minus, pos),
                    right: Box::new(expr),
                }, pos))
            },
            _ => self.primary_expr(),
        }
    }

    fn var_dec(&mut self, scope: Scope) -> Result<DeclarationWithPos> {
        let pos = eat!(self, Var);
        let var_name;
        eat!(self, Ident, var_name);
        let typ = self.optional_type()?;
        let name = self.symbols.symbol(&var_name);
        eat!(self, ColonEqual);
        let init = self.expr()?;
        match scope {
            Global => {
                Ok(WithPos::new(Declaration::Function(WithPos::new(FuncDeclaration {
                    body: init,
                    name,
                    params: vec![],
                    result: typ,
                }, pos)), pos))
            },
            Local => {
                Ok(WithPos::new(Variable {
                    escape: false,
                    init,
                    name,
                    typ,
                }, pos))
            },
        }
    }

    fn while_loop(&mut self) -> Result<ExprWithPos> {
        let pos = eat!(self, While);
        let condition = Box::new(self.expr()?);
        eat!(self, Do);
        let body = Box::new(self.expr()?);
        Ok(WithPos::new(Expr::While {
            body,
            condition,
        }, pos))
    }

    pub fn parse(&mut self) -> Result<Vec<DeclarationWithPos>> {
        let mut declarations = vec![];
        loop {
            match self.peek_token() {
                Err(Error::Eof) => break,
                Err(error) => return Err(error.clone()),
                Ok(token) => {
                    match token {
                        Fun => {
                            let function = self.fun_dec()?;
                            let pos = function.pos;
                            declarations.push(WithPos::new(Declaration::Function(function), pos))
                        },
                        Type => declarations.push(self.ty_decs()?),
                        Var => declarations.push(self.var_dec(Global)?),
                        _ => return Err(self.unexpected_token("function, type or var")?),
                    }
                },
            }
        }

        Ok(declarations)
    }

    fn peek(&mut self) -> result::Result<&Token, &Error> {
        if self.lookahead.is_none() {
            self.lookahead = Some(self.lexer.token());
        }
        // NOTE: lookahead always contain a value, hence unwrap.
        self.lookahead.as_ref().unwrap()
            .as_ref()
    }

    fn peek_token(&mut self) -> result::Result<&Tok, &Error> {
        self.peek()
            .map(|token| &token.token)
    }

    fn token(&mut self) -> Result<Token> {
        if let Some(token) = self.lookahead.take() {
            return token;
        }
        self.lexer.token()
    }

    fn unexpected_token(&mut self, expected: &str) -> Result<Error> {
        let token = self.token()?;
        Err(UnexpectedToken {
            expected: expected.to_string(),
            pos: token.start,
            unexpected: token.token,
        })
    }
}
