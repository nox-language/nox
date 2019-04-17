mod ast;
mod llvm;
mod parser;
mod symbols;

fn main() {
    let ast = parser::parse("tests/main.nox");
}
