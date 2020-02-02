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

use std::cmp::{max, min};
use std::fs::File;
use std::io::{self, Read, Seek, SeekFrom};
use std::result;

use position::Pos;
use self::Error::*;
use symbol::Symbols;
use terminal::{
    BLUE,
    BOLD,
    END_BOLD,
    RED,
    RESET_COLOR,
};
use token::Tok;
use types::Type;

pub type Result<T> = result::Result<T, Error>;

#[derive(Clone, Debug)]
pub enum Error {
    BreakOutsideLoop {
        pos: Pos,
    },
    CannotIndex {
        pos: Pos,
        typ: String,
    },
    Cycle {
        pos: Pos,
    },
    DuplicateParam {
        ident: String,
        pos: Pos,
    },
    Eof,
    ExtraField {
        ident: String,
        pos: Pos,
        struct_name: String,
    },
    InvalidEscape {
        escape: String,
        pos: Pos,
    },
    MissingField {
        ident: String,
        pos: Pos,
        struct_name: String,
    },
    Msg(String),
    Multi(Vec<Error>),
    NotAStruct {
        pos: Pos,
        typ: String,
    },
    StructType {
        pos: Pos,
    },
    Type {
        expected: Type,
        pos: Pos,
        unexpected: Type,
    },
    Unclosed {
        pos: Pos,
        token: &'static str,
    },
    Undefined {
        ident: String,
        item: String,
        pos: Pos,
    },
    UnexpectedField {
        ident: String,
        pos: Pos,
        struct_name: String,
    },
    UnexpectedToken {
        expected: String,
        pos: Pos,
        unexpected: Tok,
    },
    UnexpectedType {
        kind: String,
        pos: Pos,
    },
    UnknownToken {
        pos: Pos,
        start: char,
    },
}

impl Error {
    pub fn show(&self, symbols: &Symbols<()>) -> io::Result<()> {
        if let Multi(ref errors) = *self {
            for error in errors {
                error.show(symbols)?;
            }
            return Ok(());
        }
        eprint!("{}{}error: {}", BOLD, RED, RESET_COLOR);
        match *self {
            BreakOutsideLoop { pos } => {
                eprintln!("Break statement used outside of loop{}", END_BOLD);
                pos.show(symbols);
            },
            CannotIndex { pos, ref typ } => {
                eprintln!("Cannot index value of type `{}`{}", typ, END_BOLD);
                pos.show(symbols)
            },
            Cycle { pos } => {
                eprintln!("Type cycle detected:{}", END_BOLD);
                pos.show(symbols);
            },
            DuplicateParam { ref ident, pos } => {
                eprintln!("Duplicate parameter name `{}`{}", ident, END_BOLD);
                pos.show(symbols);
            },
            Eof => eprintln!("end of file"),
            ExtraField { ref ident, pos, ref struct_name } => {
                eprintln!("Extra field `{}` in struct of type `{}`{}", ident, struct_name, END_BOLD);
                pos.show(symbols);
            },
            InvalidEscape { ref escape, pos } => {
                eprintln!("Invalid escape \\{}{}", escape, END_BOLD);
                pos.show(symbols);
            },
            MissingField { ref ident, pos, ref struct_name } => {
                eprintln!("Missing field `{}` in struct of type `{}`{}", ident, struct_name, END_BOLD);
                pos.show(symbols);
            },
            Msg(ref string) => eprintln!("{}", string),
            Multi(_) => unreachable!(),
            NotAStruct { pos, ref typ } => {
                eprintln!("Type `{}` is not a struct type{}", typ, END_BOLD);
                pos.show(symbols);
                highlight_line(pos, symbols)?;
            },
            Error::StructType { pos } => {
                eprintln!("Expecting type when value is nil{}", END_BOLD);
                pos.show(symbols);
            },
            Error::Type { ref expected, pos, ref unexpected } => {
                eprintln!("Unexpected type {}, expecting {}{}", unexpected, expected, END_BOLD);
                pos.show(symbols);
                highlight_line(pos, symbols)?;
            },
            Unclosed { pos, token } => {
                eprintln!("Unclosed {} starting{}", token, END_BOLD);
                pos.show(symbols);
            },
            Undefined { ref ident, ref item, pos } => {
                eprintln!("Undefined {} `{}`{}", item, ident, END_BOLD);
                pos.show(symbols);
                highlight_line(pos, symbols)?;
            },
            UnexpectedField { ref ident, pos, ref struct_name } => {
                eprintln!("Unexpected field `{}` in struct of type `{}`{}", ident, struct_name, END_BOLD);
                pos.show(symbols);
            },
            UnexpectedToken { ref expected, pos, ref unexpected } => {
                eprintln!("Unexpected token {}, expecting {}{}", unexpected, expected, END_BOLD);
                pos.show(symbols);
                highlight_line(pos, symbols)?;
            },
            UnexpectedType { ref kind, pos } => {
                eprintln!("Expecting {} type{}", kind, END_BOLD);
                pos.show(symbols);
            },
            UnknownToken { pos, ref start } => {
                eprintln!("Unexpected start of token `{}`{}", start, END_BOLD);
                pos.show(symbols);
            },
        }
        eprintln!("");

        Ok(())
    }
}

fn highlight_line(pos: Pos, symbols: &Symbols<()>) -> io::Result<()> {
    let filename = symbols.name(pos.file);
    let mut file = File::open(filename)?;
    // TODO: support longer lines.
    const LENGTH: i64 = 4096;
    let mut buffer = [0; LENGTH as usize];
    let start = max(0, pos.byte as i64 - LENGTH / 2);
    file.seek(SeekFrom::Start(start as u64))?;
    let size_read = file.read(&mut buffer)?;
    let buffer = &buffer[..size_read];
    let current_pos = min(pos.byte as usize - start as usize, buffer.len());
    let start_of_line = buffer[..current_pos].iter().rposition(|byte| *byte == b'\n')
        .map(|pos| pos + 1)
        .unwrap_or(0);
    let end_of_line = buffer[current_pos..].iter().position(|byte| *byte == b'\n')
        .map(|pos| pos + current_pos)
        .unwrap_or_else(|| buffer.len());
    let line = &buffer[start_of_line..end_of_line];
    let num_spaces = num_text_size(pos.line as i64);
    let spaces = " ".repeat(num_spaces);
    eprintln!("{}{}{} |", BOLD, BLUE, spaces);
    eprintln!("{} |{}{} {}", pos.line, END_BOLD, RESET_COLOR, String::from_utf8_lossy(line));
    let count = min(pos.column as usize, line.len());
    let spaces_before_hint = " ".repeat(count);
    let hint = "^".repeat(pos.length);
    eprintln!("{}{}{} |{}{}{}{}", BOLD, BLUE, spaces, RED, spaces_before_hint, hint, RESET_COLOR);
    Ok(())
}

pub fn num_text_size(num: i64) -> usize {
    if num == 0 {
        return 1;
    }
    1 + (num as f64).log10().floor() as usize
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Self {
        Msg(error.to_string())
    }
}

impl<'a> From<&'a Error> for Error {
    fn from(error: &'a Error) -> Self {
        error.clone()
    }
}
