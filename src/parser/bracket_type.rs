#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum BracketType {
    Curly,
    Round,
    // Square,
    // Quotes
    // DoubleQuotes
}

impl BracketType {
    pub fn to_string(self) -> &'static str {
        match self {
            BracketType::Round => "round",
            BracketType::Curly => "curly",
        }
    }
    pub fn to_open_string(self) -> &'static str {
        match self {
            BracketType::Round => "(",
            BracketType::Curly => "{",
        }
    }
    pub fn to_close_string(self) -> &'static str {
        match self {
            BracketType::Round => ")",
            BracketType::Curly => "}",
        }
    }
}
