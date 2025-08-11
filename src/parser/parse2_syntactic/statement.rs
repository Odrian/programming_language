use std::fmt;
use crate::parser::operations::{OneSidedOperation, TwoSidedOperation};

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Statement<'text> {
    VariableDeclaration { object: &'text [char], typee: Option<Typee<'text>>, value: Expression<'text> },
    SetVariable { object: &'text [char], value: Expression<'text> },
    EqualSetVariable { object: &'text [char], value: Expression<'text>, op: TwoSidedOperation },

    Expression(Expression<'text>),
    If { condition: Expression<'text>, body: Vec<Self> },
    While { condition: Expression<'text>, body: Vec<Self> },
    Function { object: &'text [char], args: Vec<(&'text [char], Typee<'text>)>, returns: Option<Typee<'text>>, body: Vec<Self> },
    Return(Option<Expression<'text>>)
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression<'text> {
    Operation(Box<Self>, Box<Self>, TwoSidedOperation),
    UnaryOperation(Box<Self>, OneSidedOperation),

    NumberLiteral(&'text [char]),
    BoolLiteral(bool),

    Variable(&'text [char]),
    RoundBracket(Box<Self>),
    FunctionCall { object: &'text [char], args: Vec<Self> },
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Typee<'text> {
    String(&'text [char])
}

impl<'text> Statement<'text> {
    pub fn new_variable(obj: &'text [char], typee: Option<Typee<'text>>, value: Expression<'text>) -> Self {
        Self::VariableDeclaration { object: obj, typee, value }
    }
    pub fn new_set(object: &'text [char], value: Expression<'text>) -> Self {
        Self::SetVariable { object, value }
    }
    pub fn new_equal_set(object: &'text [char], value: Expression<'text>, op: TwoSidedOperation) -> Self {
        Self::EqualSetVariable { object, value, op }
    }
    pub fn new_if(condition: Expression<'text>, body: Vec<Self>) -> Self {
        Self::If { condition, body }
    }
    pub fn new_while(condition: Expression<'text>, body: Vec<Self>) -> Self {
        Self::While { condition, body }
    }
    pub fn new_function(name: &'text [char], args: Vec<(&'text [char], Typee<'text>)>, returns: Option<Typee<'text>>, body: Vec<Self>) -> Self {
        Self::Function { object: name, args, returns, body }
    }
}
impl<'text> Expression<'text> {
    pub fn new_operation(expression1: Self, expression2: Self, op: TwoSidedOperation) -> Self {
        Self::Operation(Box::new(expression1), Box::new(expression2), op)
    }
    pub fn new_unary_operation(expression: Self, op: OneSidedOperation) -> Self {
        Self::UnaryOperation(Box::new(expression), op)
    }
    pub fn new_round_bracket(expression: Self) -> Self {
        Self::RoundBracket(Box::new(expression))
    }
    pub fn new_function_call(object: &'text [char], args: Vec<Self>) -> Self {
        Self::FunctionCall { object, args }
    }
}

// ----- Display implementation -----

fn name_to_str(name: &[char]) -> String {
    name.iter().collect()
}

impl fmt::Display for Statement<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn statements_to_string_with_tabs(statements: &[Statement]) -> String {
            let string = statements.iter().map(|s| s.to_string()).collect::<Vec<_>>().join("\n");
            "    ".to_owned() + string.replace('\n', "\n    ").as_str()
        }
        match self {
            Self::VariableDeclaration { object: name, typee, value } => {
                let name = name_to_str(name);
                match typee {
                    Some(typee) => write!(f, "{name} : {typee} = {value}"),
                    None => write!(f, "{name} := {value}"),
                }
            }
            Self::SetVariable { object: name, value } => {
                write!(f, "{} = {value}", name_to_str(name))
            }
            Self::EqualSetVariable { object: name, value, op } => {
                write!(f, "{} {op}= {value}", name_to_str(name))
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
                let name = name_to_str(name);
                let args: Vec<String> = args.iter().map(|s| format!("{}: {}", name_to_str(s.0), s.1)).collect();
                let args = args.join(", ");
                let returns = returns.map_or(String::from("()"), |x| x.to_string());
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

impl fmt::Display for Expression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Operation(a, b, op) => {
                write!(f, "({a} {op} {b})")
            },
            Self::UnaryOperation(ex, op) => {
                write!(f, "{op}{ex}")
            }
            Self::NumberLiteral(n) => write!(f, "{}", n.iter().collect::<String>()),
            Self::BoolLiteral(value) => match value {
                true => write!(f, "true"),
                false => write!(f, "false"),
            }
            Self::Variable(name) => write!(f, "{}", name.iter().collect::<String>()),
            Self::RoundBracket(expression) => write!(f, "({expression})"),
            Self::FunctionCall { object: name, args } => {
                let name = name.iter().collect::<String>();
                let args = args.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", ");
                write!(f, "{name} ({args})")
            }
        }
    }
}

impl fmt::Display for Typee<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(string) => write!(f, "{}", string.iter().collect::<String>())
        }
    }
}

