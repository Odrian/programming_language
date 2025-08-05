use std::fmt;
use std::sync::atomic;
use crate::parser::parse1_tokenize::token::TwoSidedOperation;
use crate::parser::parse2_syntactic::statement::{TExpression, TStatement};

pub type LinkedStatement<'text> = TStatement<'text, Object<'text>>;
pub type LinkedExpression<'text> = TExpression<'text, Object<'text>>;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ObjType {
    Variable,
    Function { argument_count: usize }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Object<'text> {
    pub id: u32,
    pub name: &'text [char],
    pub obj_type: ObjType,
}

impl<'text> Object<'text> {
    pub fn new(name: &'text [char], obj_type: ObjType) -> Self {
        let id = Self::get_unique_id();
        Object { id, name, obj_type }
    }
    fn get_unique_id() -> u32 {
        static COUNTER: atomic::AtomicU32 = atomic::AtomicU32::new(0);
        COUNTER.fetch_add(1, atomic::Ordering::Relaxed)
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
            Self::Function { object, args, body } => {
                let inside = statements_to_string_with_tabs(body);
                let args = args.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", ");
                write!(f, "{object} :: ({args}) {{\n{inside}\n}}")
            }
            Self::Return(exp) => {
                write!(f, "return {exp}")
            }
        }
    }
}

impl fmt::Display for LinkedExpression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TwoSidedOp(a, b, op) => match op {
                TwoSidedOperation::Plus => write!(f, "({a} + {b})"),
                TwoSidedOperation::Minus => write!(f, "({a} - {b})"),
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

impl fmt::Display for Object<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = self.name.iter().collect::<String>();
        write!(f, "{}.{}", name, self.id)
    }
}
