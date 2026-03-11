use lsp_types::{DiagnosticSeverity, Position, Range};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Diagnostic {
    pub range: Option<Range>,
    pub severity: DiagnosticSeverity,
    pub message: String,
}

impl Diagnostic {
    fn new(range: Option<Range>, message: String, severity: DiagnosticSeverity) -> Self {
        Self { range, severity, message }
    }
    pub fn new_error(range: Range, message: String) -> Self {
        Self::new(Some(range), message, DiagnosticSeverity::ERROR)
    }
    pub fn new_error_unranged(message: String) -> Self {
        Self::new(None, message, DiagnosticSeverity::ERROR)
    }
    pub fn print(&self) {
        println!("error: {}", self.message)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ErrorQueue {
    vec: Vec<Diagnostic>,
}

impl Default for ErrorQueue {
    fn default() -> Self {
        Self { vec: Vec::new() }
    }
}

impl ErrorQueue {
    pub fn new_single_error(text: &str) -> Self {
        let mut errors = Self::default();
        errors.add_diag(Diagnostic::new_error_unranged(text.to_string()));
        errors
    }
    pub fn add_diag(&mut self, diagnostic: Diagnostic) {
        self.vec.push(diagnostic)
    }
    pub fn print(&self) {
        self.vec.iter().for_each(Diagnostic::print)
    }
    pub fn has_errors(&self) -> bool {
        self.vec.iter().any(|diag| diag.severity == DiagnosticSeverity::ERROR)
    }
}

pub fn pos_to_str(position: Position) -> String {
    format!("{}:{}", position.line, position.character)
}
pub fn range_to_str(range: Range) -> String {
    let start = pos_to_str(range.start);
    let end = pos_to_str(range.end);
    format!("{start} - {end}")
}

pub fn print_error(kind: ErrKind, error_string: &str) {
    let kind = kind.to_string();
    println!("{kind}: {error_string}");
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum ErrKind {
    Error,
    Warning,
}

impl ErrKind {
    const fn to_string(self) -> &'static str {
        match self {
            Self::Error => "error",
            Self::Warning => "warning",
        }
    }
}
