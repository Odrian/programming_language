use std::fmt::Display;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct PositionInFile {
    start: usize,
    end: usize,
}
impl PositionInFile {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
    pub fn default() -> Self {
        Self::new(0, 0)
    }
}
impl Display for PositionInFile {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // TODO: implement parsing from index to column:line
        write!(f, "{}-{}", self.start, self.end)
    }
}
