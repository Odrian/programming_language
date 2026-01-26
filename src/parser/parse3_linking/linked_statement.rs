use std::collections::HashMap;
use std::fmt::Debug;
use crate::parser::operations::{OneSidedOperation, TwoSidedOperation};
use super::object::{Object, ObjType, FloatObjType, IntObjType, ObjectFactory};

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
    StructConstruct { object: Object, fields: Vec<TypedExpression> },

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
    Variable { typee: ObjType },
    Function { typee: ObjType },
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
    pub fn new_struct_construction(object: Object, fields: Vec<TypedExpression>) -> Self {
        Self::StructConstruct { object, fields }
    }
}

impl From<LinkedLiteralExpression> for LinkedExpression {
    fn from(value: LinkedLiteralExpression) -> Self {
        Self::Literal(value)
    }
}

// ----- Display implementation -----

fn to_string_with_tabs<T>(statements: &[T], to_str: impl Fn(&T) -> String) -> String {
    let string = statements.iter().map(to_str).collect::<Vec<_>>().join("\n");
    "    ".to_owned() + string.replace('\n', "\n    ").as_str()
}

impl ObjectFactory {
    fn get_out_name<const WITH_ID: bool>(&self, object: Object) -> String {
        if WITH_ID {
            format!("{}#{}", self.get_name(object), object.id)
        } else {
            self.get_name(object).to_string()
        }
    }
}

impl GlobalLinkedStatement {
    pub fn to_string<const WITH_ID: bool>(&self, factory: &ObjectFactory, object: Object) -> String {
        match self {
            Self::Struct { fields, field_names } => {
                let fields = fields.iter().enumerate().map(|(index, typee)| {
                    let (name, _) = field_names.iter().find(|(_, index1)| index as u32 == **index1).unwrap();
                    let typee = typee.to_string::<WITH_ID>(factory);
                    format!("{name}: {typee}")
                }).collect::<Vec<_>>().join(", ");

                let name = factory.get_out_name::<WITH_ID>(object);
                format!("{name} :: struct {{ ({fields}) }}")
            }
            Self::Function { args, returns, body } => {
                let inside = to_string_with_tabs(body, |x| x.to_string::<WITH_ID>(factory));
                let args = args.iter().map(|x| factory.get_out_name::<WITH_ID>(*x)).collect::<Vec<_>>().join(", ");
                let name = factory.get_out_name::<WITH_ID>(object);
                let returns = returns.to_string::<WITH_ID>(factory);
                format!("{name} :: ({args}) -> {returns} {{\n{inside}\n}}")
            }
            Self::VariableDeclaration { value } => {
                let name = factory.get_out_name::<WITH_ID>(object);
                let obj_type = value.object_type.to_string::<WITH_ID>(factory);
                let expr = value.expr.to_string::<WITH_ID>(factory);
                format!("{name} : {obj_type} = {expr}")
            }
            Self::ExternStatement { statement } => statement.to_string::<WITH_ID>(factory, object)
        }
    }
}

impl ExternLinkedStatement {
    pub fn to_string<const WITH_ID: bool>(&self, factory: &ObjectFactory, object: Object) -> String {
        match self {
            ExternLinkedStatement::Variable { typee } => {
                let name = factory.get_out_name::<WITH_ID>(object);
                let typee = typee.to_string::<WITH_ID>(factory);
                format!("{name}: {typee};")
            }
            ExternLinkedStatement::Function { typee } => {
                let name = factory.get_out_name::<WITH_ID>(object);
                let typee = typee.to_string::<WITH_ID>(factory);
                format!("{name} :: {typee};")
            }
        }
    }
}

impl LinkedStatement {
    pub fn to_string<const WITH_ID: bool>(&self, factory: &ObjectFactory) -> String {
        match self {
            Self::VariableDeclaration { object, value } => {
                let name = factory.get_out_name::<WITH_ID>(*object);
                let obj_type = value.object_type.to_string::<WITH_ID>(factory);
                let expr = value.expr.to_string::<WITH_ID>(factory);
                format!("{name} : {obj_type} = {expr}")
            }
            Self::SetVariable { what, value, op } => {
                let what = what.to_string::<WITH_ID>(factory);
                let value = value.to_string::<WITH_ID>(factory);
                match op {
                    Some(op) => format!("{what} {op}= {value}"),
                    None => format!("{what} = {value}"),
                }
            }
            Self::Expression(expression) => {
                expression.to_string::<WITH_ID>(factory)
            }
            Self::If { condition, body } => {
                let inside = to_string_with_tabs(body, |x| x.to_string::<WITH_ID>(factory));
                let condition = condition.to_string::<WITH_ID>(factory);
                format!("if {condition} {{\n{inside}\n}}")
            }
            Self::While { condition, body } => {
                let inside = to_string_with_tabs(body, |x| x.to_string::<WITH_ID>(factory));
                let condition = condition.to_string::<WITH_ID>(factory);
                format!("while {condition} {{\n{inside}\n}}")
            }
            Self::Return(exp) => {
                match exp {
                    Some(exp) => format!("return {}", exp.to_string::<WITH_ID>(factory)),
                    None => "return".to_string()
                }
            }
        }
    }
}

impl TypedExpression {
    pub fn to_string<const WITH_ID: bool>(&self, factory: &ObjectFactory) -> String {
        self.expr.to_string::<WITH_ID>(factory)
    }
}

impl LinkedExpression {
    pub fn to_string<const WITH_ID: bool>(&self, factory: &ObjectFactory) -> String {
        match self {
            Self::Operation(a, b, op) => {
                let a = a.to_string::<WITH_ID>(factory);
                let b = b.to_string::<WITH_ID>(factory);
                format!("({a} {op} {b})")
            },
            Self::UnaryOperation(ex, op) => {
                let ex = ex.to_string::<WITH_ID>(factory);
                format!("{op}({ex})")
            }
            Self::As(expression, object_type) => {
                let expression = expression.to_string::<WITH_ID>(factory);
                let object_type = object_type.to_string::<WITH_ID>(factory);
                format!("({expression} as {object_type})")
            }
            Self::Literal(literal) => literal.to_string::<WITH_ID>(factory),
            Self::Variable(object) => factory.get_out_name::<WITH_ID>(*object),
            Self::RoundBracket(expression) => {
                let expression = expression.to_string::<WITH_ID>(factory);
                format!("({expression})")
            },
            Self::FunctionCall { object, args } => {
                let args = args.iter().map(|x| x.to_string::<WITH_ID>(factory)).collect::<Vec<_>>();
                let name = factory.get_out_name::<WITH_ID>(*object);
                format!("{name} ({})", args.join(", "))
            },
            Self::StructField { left, field_index } => {
                let left = left.to_string::<WITH_ID>(factory);
                format!("{left}.[{field_index}]")
            },
            Self::StructConstruct { object, fields } => {
                let name = factory.get_out_name::<WITH_ID>(*object);
                let fields = to_string_with_tabs(fields, |x| x.to_string::<WITH_ID>(factory));
                format!("{name} {{\n{fields}\n}}")
            }
        }
    }
}

impl LinkedLiteralExpression {
    pub fn to_string<const WITH_ID: bool>(&self, factory: &ObjectFactory) -> String {
        match self {
            Self::Undefined(_) => "---".to_string(),
            Self::IntLiteral(number, object_type) => {
                let object_type = object_type.to_string::<WITH_ID>(factory);
                format!("{number}_{object_type}")
            },
            Self::FloatLiteral(number, object_type) => {
                let object_type = object_type.to_string::<WITH_ID>(factory);
                format!("{number}_{object_type}")
            },
            Self::BoolLiteral(value) => match value {
                true => "true".to_string(),
                false => "false".to_string(),
            }
            Self::CharLiteral(char) => format!("'{}'", *char as char),
            Self::StringLiteral(str) => format!("\"{str}\""),
        }
    }
}

impl ObjType {
    pub fn to_string<const WITH_ID: bool>(&self, factory: &ObjectFactory) -> String {
        match self {
            Self::Pointer(obj_type) => {
                let obj_type = obj_type.to_string::<WITH_ID>(factory);
                format!("*{obj_type}")
            }
            Self::Reference { obj_type, is_weak: false } => {
                let obj_type = obj_type.to_string::<WITH_ID>(factory);
                format!("&{obj_type}")
            },
            Self::Reference { obj_type, is_weak: true } => {
                let obj_type = obj_type.to_string::<WITH_ID>(factory);
                format!("&weak {obj_type}")
            },
            Self::Unknown => "unknown".to_string(),
            Self::Void => "void".to_string(),
            Self::Char => "char".to_string(),
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
                string.to_string()
            }
            Self::Float(float) => match float {
                FloatObjType::F32 => "f32".to_string(),
                FloatObjType::F64 => "f64".to_string(),
            }
            Self::Struct(object) => {
                let name = factory.get_out_name::<WITH_ID>(*object);
                format!("struct::{name}")
            }
            Self::Function { arguments, returns, is_vararg } => {
                let returns = returns.to_string::<WITH_ID>(factory);
                if arguments.is_empty() {
                    if *is_vararg { unreachable!() }
                    format!("() -> {returns}")
                } else {
                    let arguments = arguments.iter().map(|x| x.to_string::<WITH_ID>(factory)).collect::<Vec<_>>().join(", ");
                    if *is_vararg {
                        format!("({arguments}, ...) -> {returns}")
                    } else {
                        format!("({arguments}) -> {returns}")
                    }
                }
            }
        }
    }
}
