use std::fmt::Display;
use crate::parser::parse4_linking::linked_statement::Object;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TStatement<'x, Obj> {
    VariableDeclaration { object: Obj, value: TExpression<'x, Obj> },
    SetVariable { object: Obj, value: TExpression<'x, Obj> },
    Expression(TExpression<'x, Obj>),
    // Bracket(Box<Vec<Statement>>, BracketType),
    If { condition: TExpression<'x, Obj>, body: Vec<Self> },
    While { condition: TExpression<'x, Obj>, body: Vec<Self> },
    Function { object: Obj, args: Vec<Obj>, body: Vec<Self> },
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TExpression<'x, Obj> {
    Plus(Box<TExpression<'x, Obj>>, Box<TExpression<'x, Obj>>),
    NumberLiteral(&'x [char]),
    Variable(Obj),
    RoundBracket(Box<TExpression<'x, Obj>>),
    FunctionCall { object: Obj, args: Vec<TExpression<'x, Obj>> },
}

pub type Statement<'x> = TStatement<'x, &'x [char]>;
pub type Expression<'x> = TExpression<'x, &'x [char]>;

pub type LinkedStatement<'x> = TStatement<'x, Object<'x>>;
pub type LinkedExpression<'x> = TExpression<'x, Object<'x>>;


impl<'x, Obj> TStatement<'x, Obj> {
    pub fn new_variable(obj: Obj, value: TExpression<'x, Obj>) -> Self {
        Self::VariableDeclaration { object: obj, value }
    }
    pub fn new_set(obj: Obj, value: TExpression<'x, Obj>) -> Self {
        Self::SetVariable { object: obj, value }
    }
    pub fn new_if(condition: TExpression<'x, Obj>, body: Vec<Self>) -> Self {
        Self::If { condition, body }
    }
    pub fn new_while(condition: TExpression<'x, Obj>, body: Vec<Self>) -> Self {
        Self::While { condition, body }
    }
    pub fn new_function(name: Obj, args: Vec<Obj>, body: Vec<Self>) -> Self {
        Self::Function { object: name, args, body }
    }
}
impl<'x, Obj> TExpression<'x, Obj> {
    pub fn plus(expression1: Self, expression2: Self) -> Self {
        Self::Plus(Box::new(expression1), Box::new(expression2))
    }
    pub fn round_bracket(expression: Self) -> Self {
        Self::RoundBracket(Box::new(expression))
    }
    pub fn function_call(object: Obj, args: Vec<Self>) -> Self {
        Self::FunctionCall { object, args }
    }
}

// ----- Display implementation -----

fn name_to_str(name: &[char]) -> String {
    name.iter().collect()
}

impl<'x> Display for Statement<'x> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn statements_to_string_with_tabs(statements: &[Statement]) -> String {
            let string = statements.iter().map(|s| s.to_string()).collect::<Vec<_>>().join("\n");
            "    ".to_string() + string.replace("\n", "\n    ").as_str()
        }
        match self {
            Self::VariableDeclaration { object: name, value } => {
                write!(f, "{} := {}", name_to_str(name), value)
            }
            Self::SetVariable { object: name, value } => {
                write!(f, "{} = {}", name_to_str(name), value)
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
            Self::Function { object: name, args, body } => {
                let name = name_to_str(name);
                let args: Vec<String> = args.iter().map(|s| name_to_str(s)).collect();
                let args = args.join(", ");
                let inside = statements_to_string_with_tabs(body);
                write!(f, "{name} :: ({args}) {{\n{inside}\n}}")
            }
        }
    }
}
impl<'x> Display for Expression<'x> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Plus(a, b) => write!(f, "({a} + {b})"),
            Expression::NumberLiteral(n) => write!(f, "{}", n.iter().collect::<String>()),
            Expression::Variable(name) => write!(f, "{}", name.iter().collect::<String>()),
            Expression::RoundBracket(expression) => write!(f, "({expression})"),
            Expression::FunctionCall { object: name, args } => {
                let name = name.iter().collect::<String>();
                let args = args.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", ");
                write!(f, "{name} ({args})")
            }
        }
    }
}
