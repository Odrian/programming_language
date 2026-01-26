use std::fmt::{Display, Formatter};
use crate::error::{print_error, ErrKind};
use crate::parser::BracketType;

pub enum TokenizeError {
    QuotesNotClosed,
    UnexpectedChar,
    BracketNotOpened(BracketType),
    BracketNotClosed(BracketType),
    WrongBracketClosed {
        expected_bracket: BracketType,
        actual_bracket: BracketType,
    },
    IncorrectEscape(char)
}

impl TokenizeError {
    pub fn print(self, position: usize) {
        print_error(ErrKind::Error, &self.to_string());
    }
}

impl Display for TokenizeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::QuotesNotClosed => {
                write!(f, "quotes not closed")
            }
            Self::UnexpectedChar => {
                write!(f, "unexpected char")
            }
            Self::BracketNotClosed(bracket) => {
                write!(f, "{} bracket not closed", bracket.to_string())
            }
            Self::BracketNotOpened(bracket) => {
                write!(f, "{} bracket not opened", bracket.to_string())
            }
            Self::WrongBracketClosed { expected_bracket, actual_bracket } => {
                write!(f, "expected '{}', got '{}' ",
                       expected_bracket.to_close_string(),
                       actual_bracket.to_close_string())
            }
            Self::IncorrectEscape(ch) => {
                write!(f, "unexpected {ch} after \\")
            }
        }
    }
}