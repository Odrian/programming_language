use std::fmt::{Display, Formatter};
use std::path::PathBuf;
use crate::error::{print_error, ErrKind};

pub enum FileError {
    CantReadSourceFile { filepath: PathBuf, io_error: String },
    CantWriteToFile { filepath: String, what: String, io_error: String },
}

impl FileError {
    pub fn print(self) {
        print_error(ErrKind::Error, &self.to_string())
    }
}

impl Display for FileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CantReadSourceFile { filepath, io_error } => {
                write!(f, "while reading file {filepath:?}: {io_error}")
            }
            Self::CantWriteToFile { filepath, what, io_error } => {
                write!(f, "while writing {what} to file {filepath}: {io_error}")
            }
        }
    }
}
