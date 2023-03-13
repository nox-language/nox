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

/*
 * FIXME: global array does not compile (change global variables from functions to (un)initialized
 * data with the assignment done at the beginning of the main function).
 * TODO: rename int and int32 to i32.
 * FIXME: function call without parentheses (it works because global variables are functions).
 * TODO: highlight multiple lines in error message.
 * FIXME: do not show type errors with invalid types.
 * FIXME: do not show type errors for undefined variable.
 * TODO: error when having a variable named main.
 * TODO: call exit in main (that requires writing an alternative runtime, because nox currently use the gcc runtime).
 */

#![allow(unknown_lints)]
#![feature(box_patterns)]

extern crate rlvm;

mod ast;
mod env;
mod error;
mod gen;
mod lexer;
mod parser;
mod position;
mod semant;
mod symbol;
mod tast;
mod terminal;
mod token;
mod types;

use std::env::args;
use std::fs::{File, read_dir};
use std::io::{self, BufReader};
use std::path::PathBuf;
use std::process::Command;
use std::rc::Rc;

use rlvm::{
    Module,
    initialize_all_target_infos,
    initialize_all_targets,
    initialize_all_target_mcs,
    initialize_all_asm_parsers,
    initialize_all_asm_printers,
    initialize_native_target,
    llvm_init,
};

use env::Env;
use error::Error;
use gen::Gen;
use lexer::Lexer;
use parser::Parser;
use semant::SemanticAnalyzer;
use symbol::{Strings, Symbols};
use terminal::Terminal;

fn main() {
    let _llvm = llvm_init();

    initialize_all_target_infos();
    initialize_all_targets();
    initialize_all_target_mcs();
    initialize_all_asm_parsers();
    initialize_all_asm_printers();
    initialize_native_target();

    let strings = Rc::new(Strings::new());
    let mut symbols = Symbols::new(Rc::clone(&strings));
    if let Err(error) = drive(strings, &mut symbols) {
        let terminal = Terminal::new();
        if let Err(error) = error.show(&symbols, &terminal) {
            eprintln!("Error printing errors: {}", error);
        }
    }
}

fn drive(strings: Rc<Strings>, symbols: &mut Symbols<()>) -> Result<(), Error> {
    let mut args = args();
    args.next();
    if let Some(filename) = args.next() {
        let file = BufReader::new(File::open(&filename)?);
        let file_symbol = symbols.symbol(&filename);
        let lexer = Lexer::new(file, file_symbol);
        let mut parser = Parser::new(lexer, symbols);
        let ast = parser.parse()?;
        let module = Module::new_with_name("module");
        let mut env = Env::new(&strings, &module);
        {
            let semantic_analyzer = SemanticAnalyzer::new(&mut env, Rc::clone(&strings), &module);
            let functions = semantic_analyzer.analyze(ast, file_symbol)?;
            env.end_scope(); // TODO: move after the semantic analysis?

            let mut gen = Gen::new(module);
            let object_output_path = gen.generate(functions, &filename);

            let mut executable_output_path = PathBuf::from(&filename);
            executable_output_path.set_extension("");
            Command::new("ld")
                .args(&[
                      "-dynamic-linker", "/lib64/ld-linux-x86-64.so.2", "-o",
                      executable_output_path.to_str().expect("executable output path"),
                      "/usr/lib/Scrt1.o", "/usr/lib/crti.o", &format!("-L{}", get_gcc_lib_dir()?),
                      "-L/usr/lib64/",
                      object_output_path.to_str().expect("object output path"),
                      "target/debug/libruntime.a", "-lpthread", "-ldl", "--no-as-needed", "-lc", "-lgcc", "--as-needed",
                      "-lgcc_s", "--no-as-needed", "/usr/lib/crtn.o"
                ])
                .status()
                .expect("link");
        }
    }
    Ok(())
}

fn get_gcc_lib_dir() -> io::Result<String> {
    let directory = "/usr/lib64/gcc/x86_64-pc-linux-gnu/";
    let files = read_dir(directory)?;
    for file in files {
        let file = file?;
        if file.metadata()?.is_dir() {
            return file.file_name().to_str()
                .map(|str| format!("{}{}", directory, str))
                .ok_or(io::ErrorKind::InvalidData.into());
        }
    }
    Err(io::ErrorKind::NotFound.into())
}
