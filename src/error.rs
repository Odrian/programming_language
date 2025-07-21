use std::{error::Error, fmt, io};
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

    LinkingError { name: String, context: String },
    LinkingErrorFunctionUsage { name: String },
}

impl fmt::Display for CompilationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CompilationError::FileIO { filepath, error } => {
                write!(f, "Error reading file {filepath}: {error}")
            }
            CompilationError::SyntacticsError(place_info, description) => {
                write!(f, "Error at {place_info}: {description}")
            }
            CompilationError::BracketNotClosed(place_info, bracket_type) => {
                let bracket_name = bracket_type_to_string(bracket_type);
                write!(f, "Error: {bracket_name} bracket at {place_info} not closed")
            }
            CompilationError::BracketNotOpened(place_info, bracket_type) => {
                let bracket_name = bracket_type_to_string(bracket_type);
                write!(f, "Error: {bracket_name} bracket at {place_info} not opened")
            }
            CompilationError::WrongBracketClosed { start, end, start_bracket_type, end_bracket_type } => {
                let start_name = bracket_type_to_string(start_bracket_type);
                let end_name = bracket_type_to_string(end_bracket_type);
                write!(f, "Error: at {end} expected {start_name} bracket, but have {end_name} bracket. Open bracket at {start}")
            }
            CompilationError::LinkingError { name, context } => {
                write!(f, "Linking Error: can't find {name} in {context}")
            }
            CompilationError::LinkingErrorFunctionUsage { name } => {
                write!(f, "Linking Error: can't use function {name} as variable value")
            }
        }
    }
}

fn bracket_type_to_string(bracket_type: &BracketType) -> String {
    match bracket_type {
        BracketType::Round => String::from("round"),
        BracketType::Curly => String::from("curly"),
        BracketType::None => panic!("BracketType::None should not be here"),
    }
}

impl Error for CompilationError {}
