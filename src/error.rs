use std::fmt;
use crate::parser::PositionInFile;
use crate::parser::operations::{TwoSidedOperation, OneSidedOperation};
use crate::parser::parse3_linking::object::ObjType;

use inkwell::builder::BuilderError;

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

    FunctionOverloading { function_name: String },
    IncorrectArgumentCount { function_name: String, argument_need: usize, argument_got: usize },
    LinkingError { name: String, context: String },
    LinkingErrorFunctionUsage { name: String },

    UnexpectedGlobalStatement { statement: String },
    IncorrectUseStatement(PositionInFile),
    LocalFunctionNotSupported,
    FunctionMustReturn { function_name: String },
    NoMainFunction,
    IncorrectMainSignature,

    IncorrectType { got: ObjType, expected: ObjType },
    IncorrectOneOper { object_type: ObjType, op: OneSidedOperation },
    IncorrectTwoOper { object_type1: ObjType, object_type2: ObjType, op: TwoSidedOperation },
    IncorrectAs { what: String, from: ObjType, to: ObjType },
    IncorrectDeref { what: String, from: ObjType },
    UnexpectedVoidUse,

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

            Self::FunctionOverloading { function_name } => {
                write!(f, "Error: function overloading is not allowed, you overload {function_name}")
            }
            Self::IncorrectArgumentCount { function_name, argument_need, argument_got } => {
                write!(f, "Error: incorrect argument count for function {function_name}, need {argument_need}, got {argument_got}")
            }
            Self::LinkingError { name, context } => {
                write!(f, "Linking Error: can't find {name} in {context}")
            }
            Self::LinkingErrorFunctionUsage { name } => {
                write!(f, "Linking Error: can't use function {name} as variable value")
            }

            Self::UnexpectedGlobalStatement { statement } => {
                write!(f, "Error: unexpected global statement: {statement}")
            }
            Self::IncorrectUseStatement(position) => {
                write!(f, "Error: incorrect use statement at {position}")
            }
            Self::LocalFunctionNotSupported => {
                write!(f, "Error: local functions not supported")
            }
            Self::FunctionMustReturn { function_name } => {
                write!(f, "Error: function {function_name} may not return")
            }
            Self::NoMainFunction => {
                write!(f, "Error: No 'main' function")
            }
            Self::IncorrectMainSignature => {
                write!(f, "Error: incorrect main signature, only () -> i32 allowed")
            }

            Self::IncorrectType { got, expected } => {
                write!(f, "Error: incorrect type, got {got}, expected {expected}")
            }
            Self::IncorrectOneOper { object_type, op } => {
                write!(f, "Error: can't use '{op}' to '{object_type}'")
            }
            Self::IncorrectTwoOper { object_type1, object_type2, op } => {
                write!(f, "Error: can't use '{op}' between '{object_type1}' and '{object_type2}'")
            }
            Self::IncorrectAs { what, from, to } => {
                write!(f, "Error: can't cast {what}, which has type {from} to {to}")
            }
            Self::IncorrectDeref { what, from } => {
                write!(f, "Error: can't deref {what}, which has type {from}")
            }
            Self::UnexpectedVoidUse => {
                write!(f, "Error: `void` can't be used as actual type")
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
