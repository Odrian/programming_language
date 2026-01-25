// enum FilePath {
//     ReadFile(String),
//     TestFile,
// }
// impl Display for FilePath {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
//         match self {
//             Self::ReadFile(file) => write!(f, "{file}"),
//             Self::TestFile => write!(f, "TEST_FILE"),
//         }
//     }
// }

pub type CResult<T> = Result<T, ()>;

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
