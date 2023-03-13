/*
 * Copyright (C) 2017-2023  Boucher, Antoni <bouanto@zoho.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

use std::fmt::{self, Display, Formatter};

use position::Pos;
use self::Tok::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    AmpAmp,
    Ampersand,
    Array,
    Break,
    CloseCurly,
    CloseParen,
    CloseSquare,
    Colon,
    Comma,
    Do,
    Dot,
    Else,
    EndOfFile,
    Equal,
    EqualEqual,
    Extern,
    False,
    For,
    Fun,
    Greater,
    GreaterOrEqual,
    Ident(String),
    If,
    In,
    Int(i64),
    Lesser,
    LesserOrEqual,
    Let,
    Minus,
    Nil,
    Not,
    NotEqual,
    Of,
    OpenCurly,
    OpenParen,
    OpenSquare,
    Pipe,
    PipePipe,
    Plus,
    Semicolon,
    Slash,
    Star,
    Str(String),
    Struct,
    Then,
    To,
    True,
    Typealias,
    Var,
    While,
}

#[derive(Debug)]
pub struct Token {
    pub pos: Pos,
    pub token: Tok,
}

impl Display for Tok {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        let string = (|| {
            let string = match *self {
                AmpAmp => "&&",
                Ampersand => "&",
                Array => "array",
                Break => "break",
                CloseCurly => "}",
                CloseParen => ")",
                CloseSquare => "]",
                Colon => ":",
                Comma => ",",
                Do => "do",
                Dot => ".",
                Else => "else",
                EndOfFile => "<eof>",
                Equal => "=",
                EqualEqual => "==",
                Extern => "extern",
                False => "false",
                For => "for",
                Fun => "fun",
                Greater => ">",
                GreaterOrEqual => ">=",
                Ident(ref ident) => ident,
                If => "if",
                In => "in",
                Int(num) => return num.to_string(),
                Lesser => "<",
                LesserOrEqual => "<=",
                Let => "let",
                Minus => "-",
                Nil => "nil",
                Not => "!",
                NotEqual => "!=",
                Of => "of",
                OpenCurly => "{",
                OpenParen => "(",
                OpenSquare => "[",
                Pipe => "|",
                PipePipe => "||",
                Plus => "+",
                Semicolon => ";",
                Slash => "/",
                Star => "*",
                Str(ref string) => return format!("{:?}", string),
                Struct => "struct",
                Then => "then",
                To => "to",
                True => "true",
                Typealias => "typealias",
                Var => "var",
                While => "while",
            };
            string.to_string()
        })();
        write!(formatter, "{}", string)
    }
}
