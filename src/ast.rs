use symbols::Symbol;

pub struct Parameter {
    name: Symbol,
    typ: Type,
}

pub enum Declaration {
    FunctionDeclaration {
        name: Symbol,
        parameters: Vec<Parameter>,
    },
}

pub type Declarations = Vec<Declaration>;

pub enum Type {
}
