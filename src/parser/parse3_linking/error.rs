use std::fmt::{Display, Formatter};
use crate::error::{print_error, ErrKind};

pub enum LinkingError {
    DependencyCycle,
    Overloading { name: String },
}

impl LinkingError {
    pub fn print(self) {
        print_error(ErrKind::Error, &self.to_string());
    }
}

impl Display for LinkingError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DependencyCycle => {
                write!(f, "dependency cycle")
            }
            Self::Overloading { name } => {
                write!(f, "overloaded '{name}'")
            }
        }
    }
}