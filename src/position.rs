#[derive(Clone, Copy)]
pub struct Position {
    pub column: u32,
    pub line: u32,
}

impl Position {
    pub fn new() -> Self {
        Self {
            column: 1,
            line: 1,
        }
    }
}
