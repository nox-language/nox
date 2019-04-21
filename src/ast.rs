use crate::symbols::Symbol;

#[derive(Debug)]
pub enum Expr {
    FunctionCall {
        name: Symbol,
        arguments: Vec<Expr>,
    },
    Str(String),
}

#[derive(Debug)]
pub struct Parameter {
    name: Symbol,
    typ: Type,
}

#[derive(Debug)]
pub enum Declaration {
    FunctionDeclaration {
        body: Expr,
        name: Symbol,
        parameters: Vec<Parameter>,
    },
}

pub type Declarations = Vec<Declaration>;

#[derive(Debug)]
pub enum Type {
}
