use std::{fmt, io};
use inkwell::builder::BuilderError;
use crate::parser::{PositionInFile, BracketType};

#[derive(Debug, Eq, PartialEq)]
pub enum CompilationError {
    FileIO { filepath: String, error: io::ErrorKind },
    SyntacticsError(PositionInFile, String),

    BracketNotOpened(PositionInFile, BracketType),
    BracketNotClosed(PositionInFile, BracketType),
    WrongBracketClosed {
        start: PositionInFile,
        end: PositionInFile,
        start_bracket_type: BracketType,
        end_bracket_type: BracketType,
    },

    IncorrectArgumentCount { function_name: String, argument_need: usize, argument_got: usize },
    LinkingError { name: String, context: String },
    LinkingErrorFunctionUsage { name: String },

    LLVMError(BuilderError),
}

impl fmt::Display for CompilationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::FileIO { filepath, error } => {
                write!(f, "Error reading file {filepath}: {error}")
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

            Self::IncorrectArgumentCount { function_name, argument_need, argument_got } => {
                write!(f, "Incorrect argument count for function {function_name}, need {argument_need}, got {argument_got}")
            }
            Self::LinkingError { name, context } => {
                write!(f, "Linking Error: can't find {name} in {context}")
            }
            Self::LinkingErrorFunctionUsage { name } => {
                write!(f, "Linking Error: can't use function {name} as variable value")
            }

            Self::LLVMError(build_error) => {
                write!(f, "LLVM Error: {build_error}")
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
