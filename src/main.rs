mod ast;
mod error;
mod gen;
mod lexer;
mod llvm;
mod parser;
mod position;
mod symbols;
mod token;

use std::mem;

use gen::generate;

fn main() {
    match parser::parse("tests/1_main.nx") {
        Ok(declarations) => {
            let code = generate(declarations).expect("generate");
            let main_function: fn() = unsafe { mem::transmute(code) };
            main_function();
        },
        Err(error) => {
            eprintln!("{line}:{column} Error at line {line}, column {column}: {msg}", line=error.position.line,
                column=error.position.column, msg=error.to_string());
        },
    }
}
