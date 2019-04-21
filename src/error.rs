use std::io;

use crate::position::Position;

pub enum ErrorKind {
    Eof,
    Io(io::Error),
    Unexpected {
        actual: String,
        expected: String,
    },
    UnexpectedChar(u8),
}

pub struct Error {
    pub kind: ErrorKind,
    pub position: Position,
}

impl Error {
    pub fn new(kind: ErrorKind, position: Position) -> Self {
        Self {
            kind,
            position,
        }
    }

    pub fn to_string(&self) -> String {
        match self.kind {
            ErrorKind::Eof => "end of file".to_string(), // TODO: change this to Unexpected { actual: "end of file" }.
            ErrorKind::Io(ref error) => error.to_string(),
            ErrorKind::Unexpected { ref actual, ref expected } =>
                format!("unexpected {}, expecting {}", actual, expected),
            ErrorKind::UnexpectedChar(byte) => format!("unexpected character: {}", Into::<char>::into(byte)),
        }
    }
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Self {
        Self {
            kind: ErrorKind::Io(error),
            position: Position::new(),
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;
