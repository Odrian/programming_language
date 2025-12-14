use std::fmt;
use std::fmt::Debug;
use crate::parser::operations::{OneSidedOperation, TwoSidedOperation};
use super::object::{Object, ObjType, FloatObjType, IntObjType};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypedExpression {
    pub object_type: ObjType,
    pub expr: LinkedExpression,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum GlobalLinkedStatement {
    Function { object: Object, args: Vec<Object>, returns: ObjType, body: Vec<LinkedStatement> },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum LinkedStatement {
    VariableDeclaration { object: Object, value: TypedExpression },
    SetVariable { what: TypedExpression, value: TypedExpression, op: Option<TwoSidedOperation> },

    Expression(TypedExpression),
    If { condition: TypedExpression, body: Vec<Self> },
    While { condition: TypedExpression, body: Vec<Self> },
    Return(Option<TypedExpression>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum LinkedExpression {
    Operation(Box<TypedExpression>, Box<TypedExpression>, TwoSidedOperation),
    UnaryOperation(Box<TypedExpression>, OneSidedOperation),
    As(Box<TypedExpression>, ObjType),

    /// `ObjType` always `IntObjType`
    IntLiteral(String, ObjType),
    /// `ObjType` always `FloatObjType`
    FloatLiteral(String, ObjType),
    BoolLiteral(bool),
    CharLiteral(u8),

    Variable(Object),
    RoundBracket(Box<TypedExpression>),
    FunctionCall { object: Object, args: Vec<TypedExpression> },
}

impl TypedExpression {
    pub fn new(object_type: ObjType, expr: LinkedExpression) -> Self {
        Self { expr, object_type }
    }
}

impl GlobalLinkedStatement {
    pub fn new_function(name: Object, args: Vec<Object>, returns: ObjType, body: Vec<LinkedStatement>) -> Self {
        Self::Function { object: name, args, returns, body }
    }
}

impl LinkedStatement {
    pub fn new_variable(object: Object, value: TypedExpression) -> Self {
        Self::VariableDeclaration { object, value }
    }
    pub fn new_set(what: TypedExpression, value: TypedExpression, op: Option<TwoSidedOperation>) -> Self {
        Self::SetVariable { what, value, op }
    }
    pub fn new_if(condition: TypedExpression, body: Vec<Self>) -> Self {
        Self::If { condition, body }
    }
    pub fn new_while(condition: TypedExpression, body: Vec<Self>) -> Self {
        Self::While { condition, body }
    }
}

impl LinkedExpression {
    pub fn new_operation(expression1: TypedExpression, expression2: TypedExpression, op: TwoSidedOperation) -> Self {
        Self::Operation(Box::new(expression1), Box::new(expression2), op)
    }
    pub fn new_unary_operation(expression: TypedExpression, op: OneSidedOperation) -> Self {
        Self::UnaryOperation(Box::new(expression), op)
    }
    pub fn new_as(expression: TypedExpression, object_type: ObjType) -> Self {
        Self::As(Box::new(expression), object_type)
    }
    pub fn new_round_bracket(expression: TypedExpression) -> Self {
        Self::RoundBracket(Box::new(expression))
    }
    pub fn new_function_call(object: Object, args: Vec<TypedExpression>) -> Self {
        Self::FunctionCall { object, args }
    }
}

// ----- Display implementation -----

fn to_string_with_tabs<T: ToString>(statements: &[T]) -> String {
    let string = statements.iter().map(|s| s.to_string()).collect::<Vec<_>>().join("\n");
    "    ".to_owned() + string.replace('\n', "\n    ").as_str()
}

impl fmt::Display for GlobalLinkedStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Function { object, args, returns, body } => {
                let inside = to_string_with_tabs(body);
                let args = args.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", ");
                write!(f, "{object} :: ({args}) -> {returns} {{\n{inside}\n}}")
            }
        }
    }
}

impl fmt::Display for LinkedStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::VariableDeclaration { object, value } => {
                write!(f, "{object} := {value}")
            }
            Self::SetVariable { what, value, op } => {
                match op {
                    Some(op) => write!(f, "{what} {op}= {value}"),
                    None => write!(f, "{what} = {value}"),
                }
            }
            Self::Expression(expression) => {
                write!(f, "{expression}")
            }
            Self::If { condition, body } => {
                let inside = to_string_with_tabs(body);
                write!(f, "if {condition} {{\n{inside}\n}}")
            }
            Self::While { condition, body } => {
                let inside = to_string_with_tabs(body);
                write!(f, "while {condition} {{\n{inside}\n}}")
            }
            Self::Return(exp) => {
                match exp {
                    Some(exp) => write!(f, "return {exp}"),
                    None => write!(f, "return")
                }
            }
        }
    }
}

impl fmt::Display for LinkedExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Operation(a, b, op) => {
                write!(f, "({a} {op} {b})")
            },
            Self::UnaryOperation(ex, op) => {
                write!(f, "{op}{ex}")
            }
            Self::As(expression, object_type) => {
                write!(f, "({expression} as {object_type})")
            }
            Self::IntLiteral(number, object_type) => write!(f, "{number}_{object_type}"),
            Self::FloatLiteral(number, object_type) => write!(f, "{number}_{object_type}"),
            Self::BoolLiteral(value) => match value {
                true => write!(f, "true"),
                false => write!(f, "false"),
            }
            Self::CharLiteral(char) => write!(f, "'{}'", *char as char),
            Self::Variable(object) => write!(f, "{object}"),
            Self::RoundBracket(expression) => write!(f, "({expression})"),
            Self::FunctionCall { object, args } => {
                let args = args.iter().map(|x| x.to_string()).collect::<Vec<_>>();
                write!(f, "{} ({})", object, args.join(", "))
            },
        }
    }
}

impl fmt::Display for ObjType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            Self::Reference(object_type) => &format!("*{object_type}"),
            Self::Void => "void",
            Self::Char => "char",
            Self::Integer(int) => match int {
                IntObjType::Bool => "bool",
                IntObjType::I8 => "i8",
                IntObjType::I16 => "i16",
                IntObjType::I32 => "i32",
                IntObjType::I64 => "i64",
                IntObjType::I128 => "i128",
                IntObjType::ISize => "isize",
                IntObjType::U8 => "u8",
                IntObjType::U16 => "u16",
                IntObjType::U32 => "u32",
                IntObjType::U64 => "u64",
                IntObjType::U128 => "u128",
                IntObjType::USize => "usize",
            }
            Self::Float(float) => match float {
                FloatObjType::F32 => "f32",
                FloatObjType::F64 => "f64",
            }
            Self::Function { .. } => {
                unimplemented!()
            }
        };
        write!(f, "{name}")
    }
}

impl fmt::Display for TypedExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expr)
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "${}", self.id)
    }
}
