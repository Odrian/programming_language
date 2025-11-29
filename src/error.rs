use std::fmt;
use crate::parser::{PositionInFile, BracketType};
use crate::parser::operations::{TwoSidedOperation, OneSidedOperation};
use crate::parser::parse3_linking::object::ObjType;

use inkwell::builder::BuilderError;

#[derive(Debug, Eq, PartialEq)]
pub enum CompilationError {
    // IO
    CantReadSourceFile { filepath: String, io_error: String },
    CantWriteToFile { filepath: String, what: String, io_error: String },
    CantDeleteObjectFile { filepath: String, io_error: String },

    // tokenize step
    QuotesNotClosed(PositionInFile),
    UnexpectedChar(char),

    // syntactic step
    SyntacticsError(PositionInFile, String),
    BracketNotOpened(PositionInFile, BracketType),
    BracketNotClosed(PositionInFile, BracketType),
    WrongBracketClosed {
        start: PositionInFile,
        end: PositionInFile,
        start_bracket_type: BracketType,
        end_bracket_type: BracketType,
    },

    LiteralParseError { what: String, error: String },

    FunctionOverloading { function_name: String },
    IncorrectArgumentCount { function_name: String, argument_need: usize, argument_got: usize },
    LinkingError { name: String, context: String },
    LinkingErrorFunctionUsage { name: String },

    UnexpectedGlobalStatement { statement: String },
    IncorrectUseStatement(PositionInFile),
    LocalFunctionNotSupported,
    UnexpectedReturn,
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
            Self::CantReadSourceFile { filepath, io_error } => {
                write!(f, "Error: '{io_error}' while reading file {filepath}: {io_error}")
            }
            Self::CantWriteToFile { filepath, what, io_error } => {
                write!(f, "Error: '{io_error}' while writing {what} to file {filepath}: {io_error}")
            }
            Self::CantDeleteObjectFile { filepath, io_error } => {
                write!(f, "Error: '{io_error}' while deleting object file {filepath}. Error: {io_error}")
            }

            Self::QuotesNotClosed(position) => {
                write!(f, "Error: quotes not closed at {position}")
            }
            Self::UnexpectedChar(char) => {
                write!(f, "Error: unexpected char '{char}'")
            }

            Self::SyntacticsError(place_info, description) => {
                write!(f, "Error: {description} at {place_info}")
            }
            Self::BracketNotClosed(place_info, bracket_type) => {
                let bracket_name = bracket_type_to_string(bracket_type);
                write!(f, "Error: {bracket_name} bracket at {place_info} not closed")
            }
            Self::BracketNotOpened(place_info, bracket_type) => {
                let bracket_name = bracket_type_to_string(bracket_type);
                write!(f, "Error: {bracket_name} bracket at {place_info} not opened")
            }
            Self::WrongBracketClosed { start, end, start_bracket_type, end_bracket_type } => {
                let start_name = bracket_type_to_string(start_bracket_type);
                let end_name = bracket_type_to_string(end_bracket_type);
                write!(f, "Error: at {end} expected {start_name} bracket, but have {end_name} bracket. Open bracket at {start}")
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
            Self::UnexpectedReturn => {
                write!(f, "Error: unexpected return")
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

fn bracket_type_to_string(bracket_type: &BracketType) -> &'static str {
    match bracket_type {
        BracketType::Round => "round",
        BracketType::Curly => "curly",
    }
}
