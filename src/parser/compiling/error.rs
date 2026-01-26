use std::fmt::{Display, Formatter};
use inkwell::builder::BuilderError;
use crate::error::{print_error, ErrKind};

pub enum LLVMError {
    LLVMError(BuilderError),

    CantDeleteObjectFile { filepath: String, io_error: String },
    NoMainFunction,
    IncorrectMainSignature,
    GlobalWithValue { name: String },

    LLVMVerifyModuleError { llvm_error: String },
    LLVMVerifyFunctionError { name: String },
    LLVMFailedToCreateAssembly { llvm_error: String },
    FailedToRunLinker { description: String },
}

impl From<BuilderError> for LLVMError {
    fn from(value: BuilderError) -> Self {
        Self::LLVMError(value)
    }
}

impl LLVMError {
    pub fn print(self) {
        print_error(ErrKind::Error, &self.to_string());
    }
}

impl Display for LLVMError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LLVMError(build_error) => {
                write!(f, "LLVM Error: {build_error}")
            }

            Self::CantDeleteObjectFile { filepath, io_error } => {
                write!(f, "while deleting object file {filepath}. Error: {io_error}")
            }
            Self::NoMainFunction => {
                write!(f, "Error: No main function")
            }
            Self::IncorrectMainSignature => {
                write!(f, "Error: incorrect main signature, only () -> i32 allowed")
            }
            Self::GlobalWithValue { name } => {
                write!(f, "Error: global '{name}' has value, use ---")
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
                write!(f, "Error: failed to run 'gcc': {description}")
            }
        }
    }
}
