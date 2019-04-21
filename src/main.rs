mod ast;
mod error;
mod lexer;
mod llvm;
mod parser;
mod position;
mod symbols;
mod token;

fn main() {
    match parser::parse("tests/1_main.nx") {
        Ok(ast) => {
            println!("{:#?}", ast);
        },
        Err(error) => {
            eprintln!("{line}:{column} Error at line {line}, column {column}: {msg}", line=error.position.line,
                column=error.position.column, msg=error.to_string());
        },
    }
}
