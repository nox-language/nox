/*
 * Copyright (C) 2019-2023  Boucher, Antoni <bouanto@zoho.com>
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

use std::fs::{self, remove_file};
use std::io::{Read, Write};
use std::path::Path;
use std::process::{Command, Stdio};

#[test]
fn test_execution() {
    // TODO: take all files ending with .nx?
    let files = [
        "array",
        "array_assignment",
        "comments",
        "conditions",
        "escapes",
        "functions",
        "hello",
        "hello1",
        "hello2",
        "hello3",
        "hello5",
        "integers",
        "lib",
        "loops",
        //"merge",
        "nested",
        //"prettyprint",
        //"queens",
        "spill",
        "strings",
        "struct",
        "vars",
    ];

    for file in &files {
        let _ = remove_file(format!("./tests/{}", file));
        Command::new("./target/debug/nox")
            .arg(&format!("tests/{}.nx", file))
            .status()
            .expect("compile");
        let child = Command::new(format!("./tests/{}", file))
            .stdout(Stdio::piped())
            .stdin(Stdio::piped())
            .spawn().expect("spawn");
        if Path::new(&format!("./tests/{}.stdin", file)).exists() {
            let input = fs::read(format!("./tests/{}.stdin", file)).expect("read");
            child.stdin.expect("stdin").write_all(&input).expect("write stdin");
        }
        let mut buffer = vec![];
        let read_size = child.stdout.expect("stdout").read_to_end(&mut buffer).expect("output");
        let output = String::from_utf8_lossy(&buffer[..read_size]);
        let expected_output = String::from_utf8(fs::read(format!("./tests/{}.stdout", file)).expect("read")).expect("String::from_utf8");
        assert_eq!(output, &*expected_output, "{}.nx", file);
    }
}

#[test]
fn test_errors() {
    // TODO: take all files ending with .nx?
    let files = [
        "array",
        "function",
        "multiple_main",
        "no_main",
        "types",
        "unclosed_comment",
        "unclosed_string",
    ];

    for file in &files {
        let _ = remove_file(format!("./tests/{}", file));
        let child = Command::new("./target/debug/nox")
            .arg(&format!("tests/error/{}.nx", file))
            .stderr(Stdio::piped())
            .spawn()
            .expect("spawn");
        let mut buffer = vec![];
        let read_size = child.stderr.expect("stderr").read_to_end(&mut buffer).expect("output");
        let output = String::from_utf8_lossy(&buffer[..read_size]);
        let expected_output = String::from_utf8(fs::read(format!("./tests/error/{}.stderr", file)).expect("read")).expect("String::from_utf8");
        assert_eq!(output, &*expected_output, "{}.nx", file);
    }
}
