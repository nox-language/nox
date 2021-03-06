/*
 * Copyright (c) 2019-2020 Boucher, Antoni <bouanto@zoho.com>
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
