use std::fmt::{Display, Formatter};
use lsp_types::{Position, Range};
use crate::error::Diagnostic;
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
    IncorrectEscape(Option<char>)
}

impl TokenizeError {
    pub fn diagnostic(self, position: Position) -> Diagnostic {
        Diagnostic::new_error(
            Range::new(position, position),
            self.to_string(),
        )
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
                if let Some(ch) = ch {
                    write!(f, "unexpected {ch} after \\")
                } else {
                    write!(f, "unexpected EOF after \\")
                }
            }
        }
    }
}