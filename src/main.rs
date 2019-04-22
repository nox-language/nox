mod ast;
mod error;
mod gen;
mod lexer;
mod llvm;
mod parser;
mod position;
mod symbols;
mod token;

use std::process::Command;

use gen::generate;

fn main() {
    match parser::parse("tests/1_main.nx") {
        Ok(declarations) => {
            let object_filename = generate(declarations).expect("generate");
            link(&object_filename, "main");
        },
        Err(error) => {
            eprintln!("{line}:{column} Error at line {line}, column {column}: {msg}", line=error.position.line,
                column=error.position.column, msg=error.to_string());
        },
    }
}

fn link(object_filename: &str, executable_name: &str) {
    Command::new("ld")
        .args(&["-e", "main", "-dynamic-linker", "/lib64/ld-linux-x86-64.so.2", "-lc", object_filename, "-o",
              executable_name])
        .status()
        .expect("ld");
}
