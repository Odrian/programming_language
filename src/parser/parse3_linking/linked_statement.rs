use std::collections::HashMap;
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
    VariableDeclaration { value: TypedExpression },
    Function { args: Vec<Object>, returns: ObjType, body: Vec<LinkedStatement> },
    Struct { fields: Vec<ObjType>, field_names: HashMap<String, u32> },

    ExternStatement { statement: ExternLinkedStatement },
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
    StructField { left: Box<TypedExpression>, field_index: u32 },

    Literal(LinkedLiteralExpression),

    Variable(Object),
    RoundBracket(Box<TypedExpression>),
    FunctionCall { object: Object, args: Vec<TypedExpression> },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum LinkedLiteralExpression {
    Undefined(ObjType),
    /// `ObjType` always `IntObjType`
    IntLiteral(String, ObjType),
    /// `ObjType` always `FloatObjType`
    FloatLiteral(String, ObjType),
    BoolLiteral(bool),
    CharLiteral(u8),
    StringLiteral(String),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ExternLinkedStatement {
    Variable { name: String, typee: ObjType },
    Function { name: String, typee: ObjType },
}

impl TypedExpression {
    pub const fn new(object_type: ObjType, expr: LinkedExpression) -> Self {
        Self { object_type, expr }
    }
}

impl GlobalLinkedStatement {
    pub const fn new_struct(fields: Vec<ObjType>, field_names: HashMap<String, u32>) -> Self {
        Self::Struct { fields, field_names }
    }
    pub const fn new_function(args: Vec<Object>, returns: ObjType, body: Vec<LinkedStatement>) -> Self {
        Self::Function { args, returns, body }
    }
    pub const fn new_variable(value: TypedExpression) -> Self {
        Self::VariableDeclaration { value }
    }
}

impl LinkedStatement {
    pub const fn new_variable(object: Object, value: TypedExpression) -> Self {
        Self::VariableDeclaration { object, value }
    }
    pub const fn new_set(what: TypedExpression, value: TypedExpression, op: Option<TwoSidedOperation>) -> Self {
        Self::SetVariable { what, value, op }
    }
    pub const fn new_if(condition: TypedExpression, body: Vec<Self>) -> Self {
        Self::If { condition, body }
    }
    pub const fn new_while(condition: TypedExpression, body: Vec<Self>) -> Self {
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
    pub const fn new_function_call(object: Object, args: Vec<TypedExpression>) -> Self {
        Self::FunctionCall { object, args }
    }
    pub fn new_struct_field(left: TypedExpression, field_index: u32) -> Self {
        let left = Box::new(left);
        Self::StructField { left, field_index }
    }
}

impl From<LinkedLiteralExpression> for LinkedExpression {
    fn from(value: LinkedLiteralExpression) -> Self {
        Self::Literal(value)
    }
}

// ----- Display implementation -----

fn to_string_with_tabs<T: ToString>(statements: &[T]) -> String {
    let string = statements.iter().map(ToString::to_string).collect::<Vec<_>>().join("\n");
    "    ".to_owned() + string.replace('\n', "\n    ").as_str()
}

impl GlobalLinkedStatement {
    pub fn to_string(&self, object: Object) -> String {
        match self {
            Self::Struct { fields, field_names } => {
                let fields = fields.iter().enumerate().map(|(index, typee)| {
                    let (name, _) = field_names.iter().find(|(_, index1)| index as u32 == **index1).unwrap();
                    format!("{name}: {typee}")
                }).collect::<Vec<_>>().join(", ");

                format!("{object} :: struct {{ ({fields}) }}")
            }
            Self::Function { args, returns, body } => {
                let inside = to_string_with_tabs(body);
                let args = args.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ");
                format!("{object} :: ({args}) -> {returns} {{\n{inside}\n}}")
            }
            Self::VariableDeclaration { value } => {
                format!("{object} := {value}")
            }
            Self::ExternStatement { statement } => statement.to_string()
        }
    }
}

impl fmt::Display for ExternLinkedStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExternLinkedStatement::Variable { name, typee } => {
                write!(f, "{name}: {typee};")
            }
            ExternLinkedStatement::Function { name, typee } => {
                write!(f, "{name} :: {typee};")
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
            Self::Literal(literal) => write!(f, "{literal}"),
            Self::Variable(object) => write!(f, "{object}"),
            Self::RoundBracket(expression) => write!(f, "({expression})"),
            Self::FunctionCall { object, args } => {
                let args = args.iter().map(ToString::to_string).collect::<Vec<_>>();
                write!(f, "{} ({})", object, args.join(", "))
            },
            Self::StructField { left, field_index } => {
                write!(f, "{left}.[{field_index}]")
            },
        }
    }
}

impl fmt::Display for LinkedLiteralExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Undefined(_) => write!(f, "---"),
            Self::IntLiteral(number, object_type) => write!(f, "{number}_{object_type}"),
            Self::FloatLiteral(number, object_type) => write!(f, "{number}_{object_type}"),
            Self::BoolLiteral(value) => match value {
                true => write!(f, "true"),
                false => write!(f, "false"),
            }
            Self::CharLiteral(char) => write!(f, "'{}'", *char as char),
            Self::StringLiteral(str) => write!(f, "\"{str}\""),
        }
    }
}

impl fmt::Display for ObjType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Pointer(object_type) => write!(f, "*{object_type}"),
            Self::Reference(object_type) => write!(f, "&{object_type}"),
            Self::Unknown => write!(f, "unknown"),
            Self::Void => write!(f, "void"),
            Self::Char => write!(f, "char"),
            Self::Integer(int) => {
                let string = match int {
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
                };
                write!(f, "{string}")
            }
            Self::Float(float) => match float {
                FloatObjType::F32 => write!(f, "f32"),
                FloatObjType::F64 => write!(f, "f64"),
            }
            Self::Struct(object) => {
                write!(f, "struct{object}")
            }
            Self::Function { arguments, returns, is_vararg } => {
                if arguments.is_empty() {
                    if *is_vararg { unreachable!() }
                    write!(f, "() -> {returns}")
                } else {
                    let arguments = arguments.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ");
                    if *is_vararg {
                        write!(f, "({arguments}, ...) -> {returns}")
                    } else {
                        write!(f, "({arguments}) -> {returns}")
                    }
                }
            }
        }
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
