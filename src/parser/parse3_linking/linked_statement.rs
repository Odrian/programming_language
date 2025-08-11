use std::fmt;
use crate::parser::operations::{OneSidedOperation, TwoSidedOperation};

use super::object::{Object, ObjType};

#[derive(Debug, Clone)]
pub struct TypedExpression<'text> {
    pub typee: ObjType,
    pub expr: LinkedExpression<'text >,
}

#[derive(Debug, Clone)]
pub enum LinkedStatement<'text> {
    VariableDeclaration { object: Object<'text>, value: TypedExpression<'text> },
    SetVariable { object: Object<'text>, value: TypedExpression<'text> },
    Expression(TypedExpression<'text>),
    If { condition: TypedExpression<'text>, body: Vec<Self> },
    While { condition: TypedExpression<'text>, body: Vec<Self> },
    Function { object: Object<'text>, args: Vec<Object<'text>>, returns: ObjType, body: Vec<Self> },
    Return(Option<TypedExpression<'text>>)
}

#[derive(Debug, Clone)]
pub enum LinkedExpression<'text> {
    Operation(Box<TypedExpression<'text>>, Box<TypedExpression<'text>>, TwoSidedOperation),
    UnaryOperation(Box<TypedExpression<'text>>, OneSidedOperation),
    NumberLiteral(&'text [char]),
    Variable(Object<'text>),
    RoundBracket(Box<TypedExpression<'text>>),
    FunctionCall { object: Object<'text>, args: Vec<TypedExpression<'text>> },
}

impl<'text> TypedExpression<'text> {
    pub fn new(typee: ObjType, expr: LinkedExpression<'text >) -> Self {
        Self { expr, typee }
    }
}

impl<'text> LinkedStatement<'text> {
    pub fn new_variable(object: Object<'text>, value: TypedExpression<'text>) -> Self {
        Self::VariableDeclaration { object, value }
    }
    pub fn new_set(object: Object<'text>, value: TypedExpression<'text>) -> Self {
        Self::SetVariable { object, value }
    }
    pub fn new_if(condition: TypedExpression<'text>, body: Vec<Self>) -> Self {
        Self::If { condition, body }
    }
    pub fn new_while(condition: TypedExpression<'text>, body: Vec<Self>) -> Self {
        Self::While { condition, body }
    }
    pub fn new_function(name: Object<'text>, args: Vec<Object<'text>>, returns: ObjType, body: Vec<Self>) -> Self {
        Self::Function { object: name, args, returns, body }
    }
}

impl<'text> LinkedExpression<'text> {
    pub fn new_operation(expression1: TypedExpression<'text>, expression2: TypedExpression<'text>, op: TwoSidedOperation) -> Self {
        Self::Operation(Box::new(expression1), Box::new(expression2), op)
    }
    pub fn new_unary_operation(expression: TypedExpression<'text>, op: OneSidedOperation) -> Self {
        Self::UnaryOperation(Box::new(expression), op)
    }
    pub fn new_round_bracket(expression: TypedExpression<'text>) -> Self {
        Self::RoundBracket(Box::new(expression))
    }
    pub fn new_function_call(object: Object<'text>, args: Vec<TypedExpression<'text>>) -> Self {
        Self::FunctionCall { object, args }
    }
}

// ----- Display implementation -----

impl fmt::Display for LinkedStatement<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn statements_to_string_with_tabs(statements: &[LinkedStatement]) -> String {
            let string = statements.iter().map(|s| s.to_string()).collect::<Vec<_>>().join("\n");
            "    ".to_owned() + string.replace('\n', "\n    ").as_str()
        }
        match self {
            Self::VariableDeclaration { object, value } => {
                write!(f, "{object} := {value}")
            }
            Self::SetVariable { object, value } => {
                write!(f, "{object} = {value}")
            }
            Self::Expression(expression) => {
                write!(f, "{expression}")
            }
            Self::If { condition, body } => {
                let inside = statements_to_string_with_tabs(body);
                write!(f, "if {condition} {{\n{inside}\n}}")
            }
            Self::While { condition, body } => {
                let inside = statements_to_string_with_tabs(body);
                write!(f, "while {condition} {{\n{inside}\n}}")
            }
            Self::Function { object, args, returns, body } => {
                let inside = statements_to_string_with_tabs(body);
                let args = args.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", ");
                write!(f, "{object} :: ({args}) -> {returns} {{\n{inside}\n}}")
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

impl fmt::Display for LinkedExpression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Operation(a, b, op) => {
                write!(f, "({a} {op} {b})")
            },
            Self::UnaryOperation(ex, op) => {
                write!(f, "{op}{ex}")
            }
            Self::NumberLiteral(n) => write!(f, "{}", n.iter().collect::<String>()),
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
        match self {
            Self::Unit => {
                write!(f, "()")
            }
            Self::Bool => {
                write!(f, "bool")
            }
            Self::Number => {
                write!(f, "i32")
            }
            Self::Function { .. } => {
                unimplemented!()
            }
        }
    }
}

impl fmt::Display for TypedExpression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expr)
    }
}

impl fmt::Display for Object<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = self.name.iter().collect::<String>();
        write!(f, "{}.{}", name, self.id)
    }
}
