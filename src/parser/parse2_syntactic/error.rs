use std::fmt::{Display, Formatter};
use std::ops::BitOr;
use crate::error::{print_error, ErrKind};
use crate::parser::PositionInFile;

pub enum SyntacticError {
    Syntactic(PositionInFile, String),
    LiteralParseError { what: String, error: String },
    LocalGlobalStatement { name: String },
    Expected(ExpectedError),
    Unexpected(String),
}

impl SyntacticError {
    pub fn print(self) {
        print_error(ErrKind::Error, &self.to_string());
    }
    
    pub fn new_local_global(str: &str) -> Self {
        Self::LocalGlobalStatement { name: str.to_string() }
    }
    pub fn new_unexpected(str: &str) -> Self {
        Self::Unexpected(str.to_string())
    }
}

pub struct ExpectedError {
    variants: Vec<ExpectedEnum>
}

pub enum ExpectedEnum {
    Name,
    String(String),

    Comma,
    Colon,
    DoubleColon,
    Semicolon,
    Arrow,
    CurlyBracket,
    RoundBracket,
    Equal,
    As,
}

impl ExpectedError {
    fn from(expected_enum: ExpectedEnum) -> Self {
        Self { variants: vec![expected_enum] }
    }
    pub fn err(self) -> SyntacticError {
        SyntacticError::Expected(self)
    }
}
impl ExpectedEnum {
    pub fn err(self) -> SyntacticError {
        SyntacticError::Expected(ExpectedError::from(self))
    }
    pub fn new_string(str: &str) -> Self {
        Self::String(str.to_string())
    }
}

impl BitOr<ExpectedEnum> for ExpectedError {
    type Output = ExpectedError;

    fn bitor(mut self, rhs: ExpectedEnum) -> Self::Output {
        self.variants.push(rhs);
        self
    }
}

impl BitOr for ExpectedEnum {
    type Output = ExpectedError;

    fn bitor(self, rhs: Self) -> Self::Output {
        ExpectedError { variants: vec![self, rhs] }
    }
}

impl Display for ExpectedEnum {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Name => write!(f, "string"),
            Self::String(str) => write!(f, "{str}"),

            Self::Comma => write!(f, ","),
            Self::Colon => write!(f, ":"),
            Self::DoubleColon => write!(f, "::"),
            Self::Semicolon => write!(f, ";"),
            Self::Arrow => write!(f, "->"),
            Self::CurlyBracket => write!(f, "{{"),
            Self::RoundBracket => write!(f, "("),
            Self::Equal => write!(f, "="),
            Self::As => write!(f, "'as'"),
        }
    }
}

impl Display for SyntacticError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Syntactic(place_info, description) => {
                write!(f, "{description} at {place_info}")
            }

            Self::LiteralParseError { what, error } => {
                write!(f, "{error} in literal {what}")
            }

            Self::LocalGlobalStatement { name } => {
                write!(f, "local {name} not supported")
            }

            Self::Expected(expected) => {
                let variants = expected.variants.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>().join(" or ");
                write!(f, "expected {variants}")
            },
            Self::Unexpected(str) => {
                write!(f, "unexpected {str}")
            },
        }
    }
}
