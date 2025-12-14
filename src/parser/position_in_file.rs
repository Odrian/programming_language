use std::fmt;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct PositionInFile {
    start: usize,
    end: usize,
}
impl PositionInFile {
    pub const fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
    pub const fn new_sized(start: usize, size: usize) -> Self {
        let end = start + size;
        Self { start, end }
    }
}

impl fmt::Display for PositionInFile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO: implement parsing from index to column:line
        write!(f, "{}-{}", self.start, self.end)
    }
}
