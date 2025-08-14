use std::fmt;
use crate::parser::operations::{OneSidedOperation, TwoSidedOperation};

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Statement {
    VariableDeclaration { object: String, typee: Option<Typee>, value: Expression },
    SetVariable { object: String, value: Expression },
    EqualSetVariable { object: String, value: Expression, op: TwoSidedOperation },
    SetDereference { pointer: Expression, value: Expression },
    EqualSetDereference { pointer: Expression, value: Expression, op: TwoSidedOperation },

    Expression(Expression),
    If { condition: Expression, body: Vec<Self> },
    While { condition: Expression, body: Vec<Self> },
    Function { object: String, args: Vec<(String, Typee)>, returns: Option<Typee>, body: Vec<Self> },
    Return(Option<Expression>)
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression {
    Operation(Box<Self>, Box<Self>, TwoSidedOperation),
    UnaryOperation(Box<Self>, OneSidedOperation),
    As(Box<Self>, Typee),

    NumberLiteral(String),
    BoolLiteral(bool),
    CharLiteral(u8),

    Variable(String),
    RoundBracket(Box<Self>),
    FunctionCall { object: String, args: Vec<Self> },
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Typee {
    String(String),
    Reference(Box<Typee>),
}

impl Statement {
    pub fn new_variable(obj: String, typee: Option<Typee>, value: Expression) -> Self {
        Self::VariableDeclaration { object: obj, typee, value }
    }
    pub fn new_set(object: String, value: Expression) -> Self {
        Self::SetVariable { object, value }
    }
    pub fn new_equal_set(object: String, value: Expression, op: TwoSidedOperation) -> Self {
        Self::EqualSetVariable { object, value, op }
    }
    pub fn new_set_deref(pointer: Expression, value: Expression) -> Self {
        Self::SetDereference { pointer, value }
    }
    pub fn new_equal_set_deref(pointer: Expression, value: Expression, op: TwoSidedOperation) -> Self {
        Self::EqualSetDereference { pointer, value, op }
    }
    pub fn new_if(condition: Expression, body: Vec<Self>) -> Self {
        Self::If { condition, body }
    }
    pub fn new_while(condition: Expression, body: Vec<Self>) -> Self {
        Self::While { condition, body }
    }
    pub fn new_function(name: String, args: Vec<(String, Typee)>, returns: Option<Typee>, body: Vec<Self>) -> Self {
        Self::Function { object: name, args, returns, body }
    }
}
impl Expression {
    pub fn new_operation(expression1: Self, expression2: Self, op: TwoSidedOperation) -> Self {
        Self::Operation(Box::new(expression1), Box::new(expression2), op)
    }
    pub fn new_unary_operation(expression: Self, op: OneSidedOperation) -> Self {
        Self::UnaryOperation(Box::new(expression), op)
    }
    pub fn new_as(expression: Expression, typee: Typee) -> Self {
        Self::As(Box::new(expression), typee)
    }
    pub fn new_round_bracket(expression: Self) -> Self {
        Self::RoundBracket(Box::new(expression))
    }
    pub fn new_function_call(object: String, args: Vec<Self>) -> Self {
        Self::FunctionCall { object, args }
    }
}

// ----- Display implementation -----

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn statements_to_string_with_tabs(statements: &[Statement]) -> String {
            let string = statements.iter().map(|s| s.to_string()).collect::<Vec<_>>().join("\n");
            "    ".to_owned() + string.replace('\n', "\n    ").as_str()
        }
        match self {
            Self::VariableDeclaration { object: name, typee, value } => {
                match typee {
                    Some(typee) => write!(f, "{name} : {typee} = {value}"),
                    None => write!(f, "{name} := {value}"),
                }
            }
            Self::SetVariable { object: name, value } => {
                write!(f, "{name} = {value}")
            }
            Self::EqualSetVariable { object: name, value, op } => {
                write!(f, "{name} {op}= {value}")
            }
            Self::SetDereference { pointer, value } => {
                write!(f, "*{pointer} = {value}")
            }
            Self::EqualSetDereference { pointer, value, op } => {
                write!(f, "*{pointer} {op}= {value}")
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
            Self::Function { object: name, args, returns, body } => {
                let args: Vec<String> = args.iter().map(|s| format!("{}: {}", s.0, s.1)).collect();
                let args = args.join(", ");
                let returns = returns.as_ref().map_or(String::from("()"), |x| x.to_string());
                let inside = statements_to_string_with_tabs(body);
                write!(f, "{name} :: ({args}) -> {returns} {{\n{inside}\n}}")
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

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Operation(a, b, op) => {
                write!(f, "({a} {op} {b})")
            },
            Self::UnaryOperation(ex, op) => {
                write!(f, "{op}{ex}")
            }
            Self::As(expression, typee) => {
                write!(f, "({expression} as {typee})")
            }
            Self::NumberLiteral(number) => write!(f, "{number}"),
            Self::BoolLiteral(value) => match value {
                true => write!(f, "true"),
                false => write!(f, "false"),
            }
            Self::CharLiteral(char) => write!(f, "'{}'", *char as char),
            Self::Variable(name) => write!(f, "{name}"),
            Self::RoundBracket(expression) => write!(f, "({expression})"),
            Self::FunctionCall { object: name, args } => {
                let args = args.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", ");
                write!(f, "{name} ({args})")
            }
        }
    }
}

impl fmt::Display for Typee {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(string) => write!(f, "{string}"),
            Self::Reference(typee) => write!(f, "*{typee}"),
        }
    }
}

