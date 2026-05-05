use pr_common::error::DiagnosticString;
use pr_common::BracketType;

pub struct TokenizeError;

impl TokenizeError {
    pub fn quotes_not_closed() -> DiagnosticString {
        DiagnosticString::from_text(
            "quotes not closed"
        )
    }
    pub fn unexpected_char() -> DiagnosticString {
        DiagnosticString::from_text(
            "unexpected char"
        )
    }
    pub fn bracket_not_closed(bracket: BracketType) -> DiagnosticString {
        DiagnosticString::new(format!(
            "{} not closed", bracket.to_string()
        ))
    }
    pub fn bracket_not_opened(bracket: BracketType) -> DiagnosticString {
        DiagnosticString::new(format!(
            "{} not opened", bracket.to_string()
        ))
    }
    pub fn wrong_bracket_closed(expected_bracket: BracketType, actual_bracket: BracketType) -> DiagnosticString {
        DiagnosticString::new(format!(
            "expected '{}', got '{}' ",
            expected_bracket.to_close_string(),
            actual_bracket.to_close_string()
        ))
    }
    pub fn incorrect_escape(escape: String) -> DiagnosticString {
        DiagnosticString::new(format!(
            "incorrect escape: \\{escape}"
        ))
    }
}
