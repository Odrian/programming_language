use std::fmt;
use inkwell::builder::BuilderError;
use crate::parser::{PositionInFile, BracketType};
use crate::parser::operations::{TwoSidedOperation, OneSidedOperation};
use crate::parser::parse3_linking::linked_statement::LinkedExpression;
use crate::parser::parse3_linking::object::ObjType;

// TODO: Box it
#[derive(Debug, Eq, PartialEq)]
pub enum CompilationError {
    WrongArguments(String),

    CantReadSourceFile { filepath: String, io_error: String },
    CantWriteToFile { filepath: String, what: String, io_error: String },
    CantDeleteObjectFile { filepath: String, io_error: String },

    SyntacticsError(PositionInFile, String),
    BracketNotOpened(PositionInFile, BracketType),
    BracketNotClosed(PositionInFile, BracketType),
    WrongBracketClosed {
        start: PositionInFile,
        end: PositionInFile,
        start_bracket_type: BracketType,
        end_bracket_type: BracketType,
    },
    
    LiteralParseError, // FIXME

    FunctionOverloading { function_name: String },
    IncorrectArgumentCount { function_name: String, argument_need: usize, argument_got: usize },
    LinkingError { name: String, context: String },
    LinkingErrorFunctionUsage { name: String },

    UnexpectedGlobalVariable,
    UnexpectedReturn,
    FunctionMustReturn { function_name: String },
    NoMainFunction,

    IncorrectType { got: ObjType, expected: ObjType },
    IncorrectOneOper { typee: ObjType, op: OneSidedOperation },
    IncorrectTwoOper { type1: ObjType, type2: ObjType, op: TwoSidedOperation },
    IncorrectAs { what: LinkedExpression, from: ObjType, to: ObjType },

    LLVMError(BuilderError),
    LLVMVerifyModuleError { llvm_error: String },
    LLVMVerifyFunctionError { name: String },
    LLVMFailedToCreateAssembly { llvm_error: String },
    FailedToRunLinker { description: String },
}

impl From<BuilderError> for CompilationError {
    fn from(value: BuilderError) -> Self {
        Self::LLVMError(value)
    }
}

impl fmt::Display for CompilationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::WrongArguments(string) => write!(f, "{string}"),

            Self::CantReadSourceFile { filepath, io_error } => {
                write!(f, "Error reading file {filepath}: {io_error}")
            }
            Self::CantWriteToFile { filepath, what, io_error } => {
                write!(f, "Error writing {what} to file {filepath}: {io_error}")
            }
            Self::CantDeleteObjectFile { filepath, io_error } => {
                write!(f, "failed to delete object file {filepath}. Error: {io_error}")
            }

            Self::SyntacticsError(place_info, description) => {
                write!(f, "Error at {place_info}: {description}")
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

            Self::LiteralParseError => {
                write!(f, "Error: incorrect literal")
            }

            Self::FunctionOverloading { function_name } => {
                write!(f, "Error: function overloading is not allowed, you overload {function_name}")
            }
            Self::IncorrectArgumentCount { function_name, argument_need, argument_got } => {
                write!(f, "Erorr: incorrect argument count for function {function_name}, need {argument_need}, got {argument_got}")
            }
            Self::LinkingError { name, context } => {
                write!(f, "Linking Error: can't find {name} in {context}")
            }
            Self::LinkingErrorFunctionUsage { name } => {
                write!(f, "Linking Error: can't use function {name} as variable value")
            }

            Self::UnexpectedGlobalVariable => {
                write!(f, "Error: Global variables are not supported")
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

            Self::IncorrectType { got, expected } => {
                write!(f, "Error: incorrect type, got {got}, expected {expected}")
            }
            Self::IncorrectOneOper { typee, op } => {
                write!(f, "Error: can't use '{op}' to '{typee}'")
            }
            Self::IncorrectTwoOper { type1, type2, op } => {
                write!(f, "Error: can't use '{op}' between '{type1}' and '{type2}'")
            }
            Self::IncorrectAs { what, from, to } => {
                write!(f, "Error: can't cast {what}, which has type {from} to {to}")
            }

            Self::LLVMError(build_error) => {
                write!(f, "LLVM Error: {build_error}")
            }
            Self::LLVMVerifyFunctionError { name } => {
                write!(f, "Function {name} probably doesn't return anything, read text below")
            }
            Self::LLVMVerifyModuleError { llvm_error } => {
                write!(f, "Module verify error: {llvm_error}")
            }
            Self::LLVMFailedToCreateAssembly { llvm_error } => {
                write!(f, "failed to create assembly file: {llvm_error}")
            }
            Self::FailedToRunLinker { description } => {
                write!(f, "failed run linked 'cc': {description}")
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
