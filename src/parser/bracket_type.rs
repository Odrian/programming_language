#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum BracketType {
    Curly,
    Round,
    None, // used for parsing whole file like other brackets
    // Square,
    // Quotes
    // DoubleQuotes
}