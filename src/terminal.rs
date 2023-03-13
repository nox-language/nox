/*
 * Copyright (C) 2020-2023  Boucher, Antoni <bouanto@zoho.com>
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

use std::io::stderr;
use std::os::raw::c_int;
use std::os::unix::io::AsRawFd;

const BOLD: &str = "\x1b[1m";
const BLUE: &str = "\x1b[34m";
const END_BOLD: &str = "\x1b[22m";
const RED: &str = "\x1b[31m";
const RESET_COLOR: &str = "\x1b[39;49m";

pub struct Terminal {
    is_a_tty: bool,
}

impl Terminal {
    pub fn new() -> Self {
        Self {
            is_a_tty: stderr_is_a_tty(),
        }
    }

    pub fn bold(&self) -> &str {
        if self.is_a_tty {
            BOLD
        }
        else {
            ""
        }
    }

    pub fn blue(&self) -> &str {
        if self.is_a_tty {
            BLUE
        }
        else {
            ""
        }
    }

    pub fn end_bold(&self) -> &str {
        if self.is_a_tty {
            END_BOLD
        }
        else {
            ""
        }
    }

    pub fn red(&self) -> &str {
        if self.is_a_tty {
            RED
        }
        else {
            ""
        }
    }

    pub fn reset_color(&self) -> &str {
        if self.is_a_tty {
            RESET_COLOR
        }
        else {
            ""
        }
    }
}

fn stderr_is_a_tty() -> bool {
    unsafe {
        isatty(stderr().as_raw_fd()) != 0
    }
}

extern "C" {
    fn isatty(fd: c_int) -> c_int;
}
