use std::{error::Error, fmt, io};
use crate::parser::PositionInFile;

#[derive(Debug)]
pub enum CompilationError {
    FileIO { filepath: String, error: io::Error },
    SyntacticsError(PositionInFile, String),
}

impl fmt::Display for CompilationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CompilationError::FileIO { filepath, error } => {
                write!(f, "Error reading file {}: {}", filepath, error)
            }
            CompilationError::SyntacticsError(place_info, description) => {
                write!(f, "Error at {}: {}", place_info, description)
            }
        }
    }
}

impl Error for CompilationError {}
