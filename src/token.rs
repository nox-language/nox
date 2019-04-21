use std::fmt::{
    Display,
    Formatter,
    Result,
};

use crate::symbols::Symbol;
use self::Token::*;

#[derive(Debug, PartialEq)]
pub enum Token {
    Equal,
    Fun,
    Ident(Symbol),
    OpenParen,
    CloseParen,
    Str(String),
}

impl Display for Token {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
        let string =
            match *self {
                Equal => "=",
                Fun => "fun",
                Ident(_symbol) => "TODO",
                OpenParen => "(",
                CloseParen => ")",
                Str(ref string) => string,
            };
        write!(formatter, "{}", string)
    }
}
