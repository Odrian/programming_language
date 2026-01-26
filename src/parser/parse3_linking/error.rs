use crate::error::{print_error, ErrKind};
use crate::parser::operations::{OneSidedOperation, TwoSidedOperation};
use crate::parser::parse3_linking::linked_statement::LinkedExpression;
use crate::parser::parse3_linking::object::{ObjType, ObjectFactory};

pub enum LinkingError {
    DependencyCycle,
    Overloading { name: String },
    NameNotFound { name: String, context: String },
    CallNotFunction { name: String },

    DotNotOnStruct { got: ObjType },
    StructFieldNameCollision { struct_name: String, field_name: String, in_construction: bool },
    StructFieldNameNotFound { struct_name: String, field_name: String },
    IncorrectType { got: ObjType, expected: ObjType },
    CantDetermineType,
    IncorrectOneOper { object_type: ObjType, op: OneSidedOperation },
    IncorrectTwoOper { object_type1: ObjType, object_type2: ObjType, op: TwoSidedOperation },
    IncorrectAs { what: Box<LinkedExpression>, from: ObjType, to: ObjType },
    GlobalVariableWithoutType { name: String },
    UnexpectedVoidUse,

    FunctionMustReturn { function_name: String },

    LiteralParseError { what: String, error: String },
    IncorrectArgumentCount { function_name: String, is_vararg: bool, argument_need: usize, argument_got: usize },
    FunctionAsValue { name: String },
}

pub fn collect_errors(factory: &ObjectFactory, iter: impl IntoIterator<Item=Result<(), LinkingError>>) -> Result<(), ()> {
    let mut ans = Ok(());
    for result in iter {
        if let Err(err) = result {
            err.print(factory);
            ans = Err(());
        }
    }
    ans
}

impl LinkingError {
    pub fn print(&self, factory: &ObjectFactory) {
        print_error(ErrKind::Error, &self.to_string::<false>(factory));
    }

    pub fn to_string<const WITH_ID: bool>(&self, factory: &ObjectFactory) -> String {
        match self {
            Self::DependencyCycle => {
                "dependency cycle".to_string()
            }
            Self::Overloading { name } => {
                format!("overloaded '{name}'")
            }
            Self::NameNotFound { name, context } => {
                format!("can't find {name} in {context}")
            }
            Self::CallNotFunction { name } => {
                format!("can't call {name}, it's not a function")
            }

            Self::DotNotOnStruct { got } => {
                let got = got.to_string::<WITH_ID>(factory);
                format!("dot operator can't be used on {got}")
            }
            Self::StructFieldNameCollision { struct_name, field_name, in_construction } => {
                if *in_construction {
                    format!("struct '{struct_name}' has two fields with name {field_name}")
                } else {
                    format!("construction of struct '{struct_name}' has two fields with name {field_name}")
                }
            }
            Self::StructFieldNameNotFound { struct_name, field_name } => {
                format!("struct '{struct_name}' hasn't field '{field_name}'")
            }
            Self::IncorrectType { got, expected } => {
                let got = got.to_string::<WITH_ID>(factory);
                let expected = expected.to_string::<WITH_ID>(factory);
                format!("incorrect type, got {got}, expected {expected}")
            }
            Self::CantDetermineType => {
                "can't determine type".to_string()
            }
            Self::IncorrectOneOper { object_type, op } => {
                let object_type = object_type.to_string::<WITH_ID>(factory);
                format!("can't use '{op}' to '{object_type}'")
            }
            Self::IncorrectTwoOper { object_type1, object_type2, op } => {
                let object_type1 = object_type1.to_string::<WITH_ID>(factory);
                let object_type2 = object_type2.to_string::<WITH_ID>(factory);
                format!("can't use '{op}' between '{object_type1}' and '{object_type2}'")
            }
            Self::IncorrectAs { what, from, to } => {
                let what = what.to_string::<WITH_ID>(factory);
                let from = from.to_string::<WITH_ID>(factory);
                let to = to.to_string::<WITH_ID>(factory);
                format!("can't cast {what}, which has type {from} to {to}")
            }
            Self::GlobalVariableWithoutType { name } => {
                format!("global variable '{name}' is declared without type annotation")
            }
            Self::UnexpectedVoidUse => {
                "'void' can't be used as actual type".to_string()
            }

            Self::FunctionMustReturn { function_name } => {
                format!("function {function_name} may not return")
            }

            Self::LiteralParseError { what, error } => {
                format!("{error} in literal {what}")
            }
            Self::IncorrectArgumentCount { function_name, is_vararg, argument_need, argument_got } => {
                let at_least = if *is_vararg { " at least" } else { "" };
                format!("incorrect argument count for function {function_name}, need{at_least} {argument_need}, got {argument_got}")
            }
            Self::FunctionAsValue { name } => {
                format!("can't use function {name} as variable value")
            }
        }
    }
}
