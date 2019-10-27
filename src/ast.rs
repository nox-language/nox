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
    pub name: Symbol,
    pub typ: Type,
}

#[derive(Debug)]
pub enum Declaration {
    Function(FunctionDeclaration),
}

pub type Declarations = Vec<Declaration>;

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub body: Expr,
    pub name: Symbol,
    pub parameters: Vec<Parameter>,
}

#[derive(Debug)]
pub enum Type {
}
