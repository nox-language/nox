use std::fs::File;
use std::io::{self, Read};

use crate::error::{Error, ErrorKind, Result};
use crate::position::Position;
use crate::token::Token;

struct Buffer {
    buffer: Vec<u8>,
    file: File,
    index: usize,
    len: usize,
    next_byte: Option<u8>,
    position: Position,
}

impl Buffer {
    fn new(file: File) -> Self {
        Self {
            buffer: vec![0; 4096],
            file,
            index: 0,
            len: 0,
            next_byte: None,
            position: Position::new(),
        }
    }

    fn error(&self, error_kind: ErrorKind) -> Error {
        Error::new(error_kind, self.position)
    }

    fn is_empty(&self) -> bool {
        self.len == 0
    }

    fn advance(&mut self) -> Result<u8> {
        match self.next_byte.take() {
            Some(next_byte) => Ok(next_byte),
            None => {
                if self.is_empty() {
                    match self.file.read(&mut self.buffer) {
                        Ok(size) => self.len = size,
                        Err(error) => return self.result(ErrorKind::Io(error)),
                    }
                }
                if self.index >= self.len {
                    return self.result(ErrorKind::Eof);
                }
                let byte = self.buffer.get(self.index).map(|&byte| byte);
                self.index += 1;
                if self.index >= self.len {
                    self.len = 0;
                    self.index = 0;
                }
                byte.map(|byte| {
                    if byte == b'\n' {
                        self.position.line += 1;
                        self.position.column = 1;
                    }
                    byte
                }).ok_or_else(|| self.error(ErrorKind::Eof))
            }
        }
    }

    fn peek(&mut self) -> Option<u8> {
        match self.next_byte {
            Some(next_byte) => Some(next_byte),
            None => {
                self.next_byte = self.advance().ok();
                self.next_byte
            },
        }
    }

    fn result<A>(&self, error_kind: ErrorKind) -> Result<A> {
        Err(self.error(error_kind))
    }
}

pub struct Lexer {
    buffer: Buffer,
    next_token: Option<Token>,
}

impl Lexer {
    pub fn new(filename: &str) -> io::Result<Self> {
        Ok(Self {
            buffer: Buffer::new(File::open(filename)?),
            next_token: None,
        })
    }

    fn error(&self, error_kind: ErrorKind) -> Error {
        Error::new(error_kind, self.buffer.position)
    }

    fn ident(&mut self) -> Result<Token> {
        let mut ident = vec![];
        loop {
            match self.peek() {
                Err(error) =>
                    if let ErrorKind::Eof = error.kind {
                        break;
                    }
                    else {
                        return Err(error);
                    },
                Ok(byte) =>
                    match byte {
                        b'a' ..= b'z' | b'A' ..= b'Z' | b'0' ..= b'9' => {
                            ident.push(byte);
                            if let Err(error) = self.buffer.advance() {
                                if let ErrorKind::Eof = error.kind {
                                    break;
                                }
                                return Err(error);
                            }
                        },
                        _ => break,
                    }
            }
        }
        if ident.is_empty() { // TODO: might not be necessary.
            self.result(ErrorKind::Eof)
        }
        else {
            if ident == b"fun" {
                Ok(Token::Fun)
            }
            else {
                Ok(Token::Ident(0)) // TODO
            }
        }
    }

    pub fn next_token(&mut self) -> Result<Token> {
        match self.next_token.take() {
            Some(token) => Ok(token),
            None => self.token(),
        }
    }

    fn peek(&mut self) -> Result<u8> {
        self.buffer.peek().ok_or_else(|| self.error(ErrorKind::Eof))
    }

    pub fn peek_token(&mut self) -> Result<&Token> {
        match self.next_token {
            Some(ref token) => Ok(token),
            None => {
                self.next_token = Some(self.token()?);
                self.peek_token()
            },
        }
    }

    pub fn position(&self) -> Position {
        self.buffer.position
    }

    fn result<A>(&self, error_kind: ErrorKind) -> Result<A> {
        Err(self.error(error_kind))
    }

    fn string(&mut self) -> Result<Token> {
        let mut string = String::new();
        assert_eq!(self.buffer.advance()?, b'"');
        loop {
            match self.buffer.advance()? {
                b'"' => break,
                byte => string.push(byte as char),
            }
        }
        Ok(Token::Str(string))
    }

    fn token(&mut self) -> Result<Token> {
        let token =
            match self.peek()? {
                b'=' => {
                    self.buffer.advance()?;
                    Token::Equal
                },
                b'(' => {
                    self.buffer.advance()?;
                    Token::OpenParen
                },
                b')' => {
                    self.buffer.advance()?;
                    Token::CloseParen
                },
                b'a' ..= b'z' | b'A' ..= b'Z' => return self.ident(),
                b'"' => return self.string(),
                b' ' | b'\n' | b'\r' => {
                    self.buffer.advance()?;
                    return self.token();
                },
                byte => return self.result(ErrorKind::UnexpectedChar(byte)),
            };
        Ok(token)
    }
}
