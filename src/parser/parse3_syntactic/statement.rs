use std::fmt::Display;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Statement<'x> {
    VariableDeclaration { name: &'x [char], value: Expression<'x> },
    SetVariable { name: &'x [char], value: Expression<'x> },
    Expression(Expression<'x>),
    // Bracket(Box<Vec<Statement>>, BracketType),
    If { condition: Expression<'x>, body: Vec<Statement<'x>> },
    While { condition: Expression<'x>, body: Vec<Statement<'x>> },
    Function { name: &'x [char], args: Vec<&'x [char]>, body: Vec<Statement<'x>> },
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression<'x> {
    Plus(Box<Expression<'x>>, Box<Expression<'x>>),
    NumberLiteral(&'x [char]),
    Variable(&'x [char]),
    RoundBracket(Box<Expression<'x>>),
    FunctionCall { name: &'x [char], args: Vec<Expression<'x>> },
}

impl<'x> Statement<'x> {
    pub fn new_variable(name: &'x [char], value: Expression<'x>) -> Self {
        Statement::VariableDeclaration { name, value }
    }
    pub fn new_set(name: &'x [char], value: Expression<'x>) -> Self {
        Statement::SetVariable { name, value }
    }
    pub fn new_if(condition: Expression<'x>, body: Vec<Statement<'x>>) -> Self {
        Statement::If { condition, body }
    }
    pub fn new_while(condition: Expression<'x>, body: Vec<Statement<'x>>) -> Self {
        Statement::While { condition, body }
    }
    pub fn new_function(name: &'x [char], args: Vec<&'x [char]>, body: Vec<Statement<'x>>) -> Self {
        Statement::Function { name, args, body }
    }
}
impl<'x> Expression<'x> {
    pub fn plus(expression1: Expression<'x>, expression2: Expression<'x>) -> Self {
        Expression::Plus(Box::new(expression1), Box::new(expression2))
    }
    pub fn round_bracket(expression: Expression<'x>) -> Self {
        Expression::RoundBracket(Box::new(expression))
    }
}

// ----- Display implementation -----

impl<'x> Display for Statement<'x> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn statements_to_string_with_tabs(statements: &[Statement]) -> String {
            let string = statements.iter().map(|s| s.to_string()).collect::<Vec<_>>().join("\n");
            "    ".to_string() + string.replace("\n", "\n    ").as_str()
        }
        match self {
            Self::VariableDeclaration { name, value } => {
                write!(f, "{} := {}", name.iter().collect::<String>(), value)
            }
            Self::SetVariable { name, value } => {
                write!(f, "{} = {}", name.iter().collect::<String>(), value)
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
            Self::Function { name, args, body } => {
                let name = name.iter().collect::<String>();
                let args = args.iter().map(|s| s.iter().collect()).collect::<Vec<String>>().join(", ");
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
            Expression::FunctionCall { name, args } => {
                let name = name.iter().collect::<String>();
                let args = args.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", ");
                write!(f, "{name} ({args})")
            }
        }
    }
}
