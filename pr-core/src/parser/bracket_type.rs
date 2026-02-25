#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum BracketType {
    Curly,
    Round,
    // Square,
}

impl BracketType {
    pub const fn to_string(self) -> &'static str {
        match self {
            Self::Round => "round",
            Self::Curly => "curly",
        }
    }
    pub const fn to_open_string(self) -> &'static str {
        match self {
            Self::Round => "(",
            Self::Curly => "{",
        }
    }
    pub const fn to_close_string(self) -> &'static str {
        match self {
            Self::Round => ")",
            Self::Curly => "}",
        }
    }
}
