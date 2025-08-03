use std::fmt;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TStatement<'text, Obj> {
    VariableDeclaration { object: Obj, value: TExpression<'text, Obj> },
    SetVariable { object: Obj, value: TExpression<'text, Obj> },
    Expression(TExpression<'text, Obj>),
    // Bracket(Box<Vec<Statement>>, BracketType),
    If { condition: TExpression<'text, Obj>, body: Vec<Self> },
    While { condition: TExpression<'text, Obj>, body: Vec<Self> },
    Function { object: Obj, args: Vec<Obj>, body: Vec<Self> },
    Return(TExpression<'text, Obj>)
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TExpression<'text, Obj> {
    Plus(Box<Self>, Box<Self>),
    NumberLiteral(&'text [char]),
    Variable(Obj),
    RoundBracket(Box<Self>),
    FunctionCall { object: Obj, args: Vec<Self> },
}

pub type Statement<'text> = TStatement<'text, &'text [char]>;
pub type Expression<'text> = TExpression<'text, &'text [char]>;

impl<'text, Obj> TStatement<'text, Obj> {
    pub fn new_variable(obj: Obj, value: TExpression<'text, Obj>) -> Self {
        Self::VariableDeclaration { object: obj, value }
    }
    pub fn new_set(obj: Obj, value: TExpression<'text, Obj>) -> Self {
        Self::SetVariable { object: obj, value }
    }
    pub fn new_if(condition: TExpression<'text, Obj>, body: Vec<Self>) -> Self {
        Self::If { condition, body }
    }
    pub fn new_while(condition: TExpression<'text, Obj>, body: Vec<Self>) -> Self {
        Self::While { condition, body }
    }
    pub fn new_function(name: Obj, args: Vec<Obj>, body: Vec<Self>) -> Self {
        Self::Function { object: name, args, body }
    }
}
impl<'text, Obj> TExpression<'text, Obj> {
    pub fn new_plus(expression1: Self, expression2: Self) -> Self {
        Self::Plus(Box::new(expression1), Box::new(expression2))
    }
    pub fn new_round_bracket(expression: Self) -> Self {
        Self::RoundBracket(Box::new(expression))
    }
    pub fn new_function_call(object: Obj, args: Vec<Self>) -> Self {
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
            Self::Return(exp) => {
                write!(f, "return {exp}")
            }
        }
    }
}

impl fmt::Display for Expression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
