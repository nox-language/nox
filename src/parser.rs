use crate::ast::{
    Declaration,
    Declaration::FunctionDeclaration,
    Declarations,
    Expr,
};
use crate::error::{
    Error,
    ErrorKind,
    Result,
};
use crate::lexer::Lexer;
use crate::token::Token;

pub fn parse(filename: &str) -> Result<Declarations> {
    let mut parser = Parser::new(filename)?;
    parser.declarations()
}

struct Parser {
    lexer: Lexer,
}

impl Parser {
    fn new(filename: &str) -> Result<Self> {
        Ok(Self {
            lexer: Lexer::new(filename)?,
        })
    }

    fn declaration(&mut self) -> Result<Declaration> {
        self.eat(Token::Fun)?;
        let name =
            match self.lexer.next_token()? {
                Token::Ident(ident) => {
                    ident
                }
                token =>
                    return Err(Error::new(ErrorKind::Unexpected {
                            actual: token.to_string(),
                            expected: "identifier".to_string(),
                        },
                        self.lexer.position()
                    )),
            };
        self.eat(Token::OpenParen)?;
        // TODO
        self.eat(Token::CloseParen)?;
        self.eat(Token::Equal)?;
        let body = self.expression()?;
        Ok(FunctionDeclaration {
            body,
            name,
            parameters: vec![], // TODO
        })
    }

    fn declarations(&mut self) -> Result<Declarations> {
        let mut declarations = vec![];
        loop {
            match self.lexer.peek_token() {
                Ok(Token::Fun) => declarations.push(self.declaration()?),
                Ok(_) => {
                    let token = self.lexer.next_token()?;
                    return self.unexpected(token, "fun");
                },
                Err(error) => {
                    if let ErrorKind::Eof = error.kind {
                        break;
                    }
                    else {
                        return Err(error);
                    }
                },
            }
        }
        Ok(declarations)
    }

    fn eat(&mut self, token: Token) -> Result<()> {
        match self.lexer.next_token() {
            Ok(current_token) => {
                if token != current_token {
                    return Err(Error::new(ErrorKind::Unexpected {
                            actual: current_token.to_string(),
                            expected: token.to_string(),
                        },
                        self.lexer.position()));
                }
                Ok(())
            },
            Err(error) => return Err(error),
        }
    }

    fn expression(&mut self) -> Result<Expr> {
        let expr =
            match self.lexer.next_token()? {
                Token::Ident(name) => {
                    self.eat(Token::OpenParen)?;
                    let mut arguments = vec![];
                    loop {
                        if let Token::CloseParen = self.lexer.peek_token()? {
                            break;
                        }
                        arguments.push(self.expression()?);
                    }
                    self.eat(Token::CloseParen)?;
                    Expr::FunctionCall {
                        arguments,
                        name,
                    }
                },
                Token::Str(string) => Expr::Str(string),
                token => return self.unexpected(token, "function call"), // TODO: error message.
            };
        Ok(expr)
    }

    fn unexpected<A, S: ToString, T: ToString>(&self, actual: S, expected: T) -> Result<A> {
        Err(Error::new(ErrorKind::Unexpected {
                actual: actual.to_string(),
                expected: expected.to_string(),
            },
            self.lexer.position(),
        ))
    }
}
