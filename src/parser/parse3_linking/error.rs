use std::fmt::{Display, Formatter};
use crate::error::{print_error, ErrKind};
use crate::parser::operations::{OneSidedOperation, TwoSidedOperation};
use crate::parser::parse3_linking::object::ObjType;

pub enum LinkingError {
    DependencyCycle,
    Overloading { name: String },
    NameNotFound { name: String, context: String },

    DotNotOnStruct,
    StructFieldNameCollision { field_name: String },
    StructFieldNameNotFound { struct_name: String, field_name: String },
    IncorrectType { got: ObjType, expected: ObjType },
    CantDetermineType,
    IncorrectOneOper { object_type: ObjType, op: OneSidedOperation },
    IncorrectTwoOper { object_type1: ObjType, object_type2: ObjType, op: TwoSidedOperation },
    IncorrectAs { what: String, from: ObjType, to: ObjType },
    GlobalVariableWithoutType { name: String },
    UnexpectedVoidUse,

    FunctionMustReturn { function_name: String },

    LiteralParseError { what: String, error: String },
    IncorrectArgumentCount { function_name: String, argument_need: usize, argument_got: usize },
    FunctionAsValue { name: String },
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
            Self::NameNotFound { name, context } => {
                write!(f, "can't find {name} in {context}")
            }

            Self::DotNotOnStruct => {
                write!(f, "dot operator can be used only on struct type")
            }
            Self::StructFieldNameCollision { field_name } => {
                write!(f, "struct has two fields with name {field_name}")
            }
            Self::StructFieldNameNotFound { struct_name, field_name } => {
                write!(f, "struct '{struct_name}' hasn't field '{field_name}'")
            }
            Self::IncorrectType { got, expected } => {
                write!(f, "incorrect type, got {got}, expected {expected}")
            }
            Self::CantDetermineType => {
                write!(f, "can't determine type")
            }
            Self::IncorrectOneOper { object_type, op } => {
                write!(f, "can't use '{op}' to '{object_type}'")
            }
            Self::IncorrectTwoOper { object_type1, object_type2, op } => {
                write!(f, "can't use '{op}' between '{object_type1}' and '{object_type2}'")
            }
            Self::IncorrectAs { what, from, to } => {
                write!(f, "can't cast {what}, which has type {from} to {to}")
            }
            Self::GlobalVariableWithoutType { name } => {
                write!(f, "global variable '{name}' is declared without type annotation")
            }
            Self::UnexpectedVoidUse => {
                write!(f, "`void` can't be used as actual type")
            }

            Self::FunctionMustReturn { function_name } => {
                write!(f, "function {function_name} may not return")
            }

            Self::LiteralParseError { what, error } => {
                write!(f, "{error} in literal {what}")
            }
            Self::IncorrectArgumentCount { function_name, argument_need, argument_got } => {
                write!(f, "incorrect argument count for function {function_name}, need {argument_need}, got {argument_got}")
            }
            Self::FunctionAsValue { name } => {
                write!(f, "can't use function {name} as variable value")
            }
        }
    }
}
