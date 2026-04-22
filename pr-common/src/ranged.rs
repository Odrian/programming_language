use std::fmt;
use lsp_types::Range;

#[derive(Debug, Clone)]
pub struct Ranged<T> {
    pub value: T,
    pub range: Range,
}

pub type RString = Ranged<String>;

impl RString {
    pub fn new(value: String, range: Range) -> Self {
        Self { value, range }
    }
    pub fn new_no_range(value: String) -> Self {
        Self { value, range: Range::default() }
    }
}

impl<T> PartialEq<Self> for Ranged<T> {
    fn eq(&self, _other: &Self) -> bool { true }
}
impl<T> Eq for Ranged<T> {}

impl<T : fmt::Display> fmt::Display for Ranged<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
