#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum BracketType {
    Curly,
    Round,
    Square,
}

impl BracketType {
    pub const fn to_string(self) -> &'static str {
        match self {
            Self::Round => "round bracket",
            Self::Curly => "curly bracket",
            Self::Square => "square bracket",
        }
    }
    pub const fn to_open_string(self) -> &'static str {
        match self {
            Self::Round => "(",
            Self::Curly => "{",
            Self::Square => "[",
        }
    }
    pub const fn to_close_string(self) -> &'static str {
        match self {
            Self::Round => ")",
            Self::Curly => "}",
            Self::Square => "]",
        }
    }
}
