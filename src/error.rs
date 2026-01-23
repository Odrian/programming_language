use std::fmt;
use crate::parser::PositionInFile;

use inkwell::builder::BuilderError;

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

#[derive(Debug, Eq, PartialEq)]
pub enum CompilationError {
    CantDeleteObjectFile { filepath: String, io_error: String },

    SyntacticsError(PositionInFile, String),

    LiteralParseError { what: String, error: String },

    IncorrectUseStatement(PositionInFile),
    LocalFunctionNotSupported,
    LocalStructNotSupported,
    NoMainFunction,
    IncorrectMainSignature,


    LLVMError(BuilderError),
    LLVMVerifyModuleError { llvm_error: String },
    LLVMVerifyFunctionError { name: String },
    LLVMFailedToCreateAssembly { llvm_error: String },
    FailedToRunLinker { description: String },
    Placeholder,
}

impl From<BuilderError> for CompilationError {
    fn from(value: BuilderError) -> Self {
        Self::LLVMError(value)
    }
}

impl fmt::Display for CompilationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::CantDeleteObjectFile { filepath, io_error } => {
                write!(f, "while deleting object file {filepath}. Error: {io_error}")
            }

            Self::SyntacticsError(place_info, description) => {
                write!(f, "Error: {description} at {place_info}")
            }

            Self::LiteralParseError { what, error } => {
                write!(f, "Error: {error} in literal {what}")
            }

            Self::IncorrectUseStatement(position) => {
                write!(f, "Error: incorrect use statement at {position}")
            }
            Self::LocalFunctionNotSupported => {
                write!(f, "Error: local functions not supported")
            }
            Self::LocalStructNotSupported => {
                write!(f, "Error: local struct not supported")
            }
            Self::NoMainFunction => {
                write!(f, "Error: No 'main' function")
            }
            Self::IncorrectMainSignature => {
                write!(f, "Error: incorrect main signature, only () -> i32 allowed")
            }

            Self::LLVMError(build_error) => {
                write!(f, "LLVM Error: {build_error}")
            }
            Self::LLVMVerifyFunctionError { name } => {
                write!(f, "Error: function {name} probably doesn't return anything, read llvm error below")
            }
            Self::LLVMVerifyModuleError { llvm_error } => {
                write!(f, "Error: module verify error: {llvm_error}")
            }
            Self::LLVMFailedToCreateAssembly { llvm_error } => {
                write!(f, "Error: failed to create assembly file: {llvm_error}")
            }
            Self::FailedToRunLinker { description } => {
                write!(f, "Error: failed run linked 'cc': {description}")
            }
            Self::Placeholder => {
                write!(f, "Error: placeholder error")
            }
        }
    }
}
