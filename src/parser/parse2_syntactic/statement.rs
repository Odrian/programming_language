use std::fmt;
use crate::parser::operations::{OneSidedOperation, TwoSidedOperation};

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Statement {
    ComptimeStatement(ComptimeStatement),
    DeclarationStatement { name: String, statement: DeclarationStatement },
    ExternStatement { statement: ExternStatement },

    SetVariable { what: Expression, value: Expression, op: Option<TwoSidedOperation> },

    Brackets(Vec<Self>),
    Expression(Expression),
    If { condition: Expression, body: Vec<Self> },
    While { condition: Expression, body: Vec<Self> },
    Return(Option<Expression>),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum DeclarationStatement {
    VariableDeclaration { typee: Option<Typee>, value: Expression },
    Function { args: Vec<(String, Typee)>, returns: Option<Typee>, body: Vec<Statement> },
    Struct { fields: Vec<(String, Typee)> },
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ComptimeStatement {
    Import { from: Vec<String>, what: Vec<(String, Option<String>)> },
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression {
    Operation(Box<Self>, Box<Self>, TwoSidedOperation),
    UnaryOperation(Box<Self>, OneSidedOperation),
    As(Box<Self>, Typee),
    StructField { left: Box<Self>, field: String },

    Literal(LiteralExpression),
    StructConstruct { struct_name: String, fields: Vec<(String, Expression)> },

    Variable(String),
    RoundBracket(Box<Self>),
    FunctionCall { object: String, args: Vec<Self> },
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum LiteralExpression {
    Undefined,
    NumberLiteral(String),
    BoolLiteral(bool),
    CharLiteral(u8),
    StringLiteral(String),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Typee {
    String(String),
    Pointer(Box<Typee>),
    Reference(Box<Typee>),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ExternStatement {
    Variable { name: String, typee: Typee },
    Function { name: String, args: Vec<Typee>, is_vararg: bool, returns: Option<Typee> },
}

impl Typee {
    pub fn new_string(string: String) -> Self {
        Self::String(string)
    }
    pub fn new_pointer(typee: Typee) -> Self {
        Self::Pointer(Box::new(typee))
    }
    pub fn new_reference(typee: Typee) -> Self {
        Self::Reference(Box::new(typee))
    }
}

impl Statement {
    pub const fn new_variable(name: String, typee: Option<Typee>, value: Expression) -> Self {
        Self::DeclarationStatement { name, statement: DeclarationStatement::VariableDeclaration { typee, value } }
    }
    pub const fn new_set(what: Expression, value: Expression, op: Option<TwoSidedOperation>) -> Self {
        Self::SetVariable { what, value, op }
    }
    pub const fn new_if(condition: Expression, body: Vec<Self>) -> Self {
        Self::If { condition, body }
    }
    pub const fn new_while(condition: Expression, body: Vec<Self>) -> Self {
        Self::While { condition, body }
    }
    pub const fn new_function(name: String, args: Vec<(String, Typee)>, returns: Option<Typee>, body: Vec<Self>) -> Self {
        Self::DeclarationStatement { name, statement: DeclarationStatement::Function { args, returns, body } }
    }
    pub const fn new_import(from: Vec<String>, what: Vec<(String, Option<String>)>) -> Self {
        Self::ComptimeStatement(ComptimeStatement::Import { from, what })
    }
    pub const fn new_struct(name: String, fields: Vec<(String, Typee)>) -> Self {
        Self::DeclarationStatement { name, statement: DeclarationStatement::Struct { fields } }
    }
    pub const fn new_brackets(body: Vec<Statement>) -> Self {
        Self::Brackets(body)
    }
    pub fn new_for(
        before: Statement,
        cond: Expression,
        mut body: Vec<Statement>,
        inc: Statement,
    ) -> Self {
        body.push(inc);
        Statement::new_brackets(vec![
            before,
            Statement::new_while(
                cond,
                body,
            )
        ])
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
    pub const fn new_function_call(object: String, args: Vec<Self>) -> Self {
        Self::FunctionCall { object, args }
    }
    pub fn new_dot(left: Self, field: String) -> Self {
        Self::StructField { left: Box::new(left), field }
    }
}

impl From<LiteralExpression> for Expression {
    fn from(value: LiteralExpression) -> Self {
        Self::Literal(value)
    }
}

// ----- Display implementation -----

fn to_string_with_tabs<T: ToString>(statements: &[T]) -> String {
    let string = statements.iter().map(ToString::to_string).collect::<Vec<_>>().join("\n");
    "    ".to_owned() + string.replace('\n', "\n    ").as_str()
}


impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::SetVariable { what, value, op } => {
                match op {
                    Some(op) => write!(f, "{what} {op}= {value}"),
                    None => write!(f, "{what} = {value}"),
                }
            }
            Self::Brackets(body) => {
                let inside = to_string_with_tabs(body);
                write!(f, "{{\n{inside}\n}}")
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
            Self::DeclarationStatement { name, statement} => match statement {
                DeclarationStatement::VariableDeclaration { typee, value } => {
                    match typee {
                        Some(typee) => write!(f, "{name} : {typee} = {value}"),
                        None => write!(f, "{name} := {value}"),
                    }
                }
                DeclarationStatement::Function { args, returns, body } => {
                    let args: Vec<String> = args.iter().map(|s| format!("{}: {}", s.0, s.1)).collect();
                    let args = args.join(", ");
                    let returns = returns.as_ref().map_or("()".to_string(), ToString::to_string);
                    let inside = to_string_with_tabs(body);
                    write!(f, "{name} :: ({args}) -> {returns} {{\n{inside}\n}}")
                }
                DeclarationStatement::Struct { fields } => {
                    let fields = fields.iter().map(|(field_name, field_typee)|
                        format!("  {field_name}: {field_typee}\n")
                    ).collect::<String>();
                    write!(f, "{name} :: struct {{\n{fields}}}")
                }
            }
            Self::ExternStatement { statement } => write!(f, "{statement}"),
            Self::ComptimeStatement(statement) => match statement {
                ComptimeStatement::Import { from, what } => {
                    let from = from.join("::");
                    let what = what.iter().map(|(name, as_name)| {
                        format!("{name}{}", as_name.as_ref().map_or("".to_string(), |s| format!(" as {s}")))
                    }).collect::<Vec<_>>();
                    let what = {
                        if what.len() == 1 {
                            what[0].clone()
                        } else {
                            format!("{{{}}}", what.join(", "))
                        }
                    };
                    write!(f, "use {from}::{what}")
                }
            }
        }
    }
}

impl fmt::Display for ExternStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExternStatement::Variable { name, typee } => {
                write!(f, "{name}: {typee};")
            }
            ExternStatement::Function { name, args, is_vararg, returns } => {
                let mut args_str = args.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ");
                if *is_vararg {
                    if args.len() == 0 { unreachable!() }
                    args_str += ", ...";
                }
                match returns {
                    Some(returns) => write!(f, "{name} : ({args_str}) -> {returns};"),
                    None => write!(f, "{name} : ({args_str});"),
                }
            }
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::StructField { left, field } => {
                write!(f, "{left}.{field}")
            }
            Self::Operation(a, b, op) => {
                write!(f, "({a} {op} {b})")
            },
            Self::UnaryOperation(ex, op) => {
                write!(f, "{op}{ex}")
            }
            Self::As(expression, typee) => {
                write!(f, "({expression} as {typee})")
            }
            Self::Literal(literal) => write!(f, "{literal}"),
            Self::Variable(name) => write!(f, "{name}"),
            Self::RoundBracket(expression) => write!(f, "({expression})"),
            Self::FunctionCall { object: name, args } => {
                let args = args.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ");
                write!(f, "{name} ({args})")
            }
            Self::StructConstruct { struct_name: name, fields } => {
                let fields = fields.iter().map(|(field_name, field_value)| {
                    format!("    {field_name}: {field_value},\n")
                }).collect::<String>();
                write!(f, "{name} {{\n{fields}}}")
            }
        }
    }
}

impl fmt::Display for LiteralExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Undefined => write!(f, "---"),
            Self::NumberLiteral(number) => write!(f, "{number}"),
            Self::BoolLiteral(value) => match value {
                true => write!(f, "true"),
                false => write!(f, "false"),
            }
            Self::CharLiteral(char) => write!(f, "'{}'", *char as char),
            Self::StringLiteral(str) => write!(f, "\"{str}\""),
        }
    }
}

impl fmt::Display for Typee {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(string) => write!(f, "{string}"),
            Self::Pointer(typee) => write!(f, "*{typee}"),
            Self::Reference(typee) => write!(f, "&{typee}"),
        }
    }
}

