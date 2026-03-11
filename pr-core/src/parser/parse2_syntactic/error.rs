use std::fmt::{Display, Formatter};
use std::ops::BitOr;
use lsp_types::Range;
use crate::error::Diagnostic;

pub struct SyntacticError {}

impl SyntacticError {
    pub fn from_text(text: &str, range: Range) -> Diagnostic {
        Diagnostic::new_error(range, text.to_string())
    }
    pub fn new_local_global(name: &str, range: Range) -> Diagnostic {
        Diagnostic::new_error(range, format!("local {name} not supported"))
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
    pub fn diagnostic(self, range: Range) -> Diagnostic {
        Diagnostic::new_error(range, self.to_string())
    }
    pub fn diagnostic_after(self, range: Range) -> Diagnostic {
        Diagnostic::new_error(range, self.to_string() + " after that")
    }
    fn to_string(self) -> String {
        let variants = self.variants.iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>().join(" or ");
        format!("expected {variants}")
    }
}
impl ExpectedEnum {
    pub fn diagnostic(self, range: Range) -> Diagnostic {
        ExpectedError::from(self).diagnostic(range)
    }
    pub fn diagnostic_after(self, range: Range) -> Diagnostic {
        ExpectedError::from(self).diagnostic_after(range)
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
