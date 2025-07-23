use crate::error::CompilationError as CE;
use crate::parser::parse3_syntactic::{Expression, Statement};

use std::collections::HashMap;
use std::fmt::Display;
use std::sync::atomic;

pub fn link_variables(statement: Vec<Statement>) -> Result<Vec<LinkedStatement>, CE> {
    let mut object_context_window = ObjectContextWindow::new();

    object_context_window.step_in();
    let result = link_statements_recursive(&statement, &mut object_context_window);
    object_context_window.step_out();
    result
}

#[derive(Debug, Eq, PartialEq)]
pub enum LinkedStatement<'x> {
    VariableDeclaration { object: Object, value: LinkedExpression<'x> },
    SetVariable { object: Object, value: LinkedExpression<'x> },
    Expression(LinkedExpression<'x>),
    If { condition: LinkedExpression<'x>, body: Vec<LinkedStatement<'x>> },
    While { condition: LinkedExpression<'x>, body: Vec<LinkedStatement<'x>> },
    Function { object: Object, args: Vec<Object>, body: Vec<LinkedStatement<'x>> },
}
#[derive(Debug, Eq, PartialEq)]
pub enum LinkedExpression<'x> {
    Plus(Box<LinkedExpression<'x>>, Box<LinkedExpression<'x>>),
    NumberLiteral(&'x [char]),
    Variable(Object),
    RoundBracket(Box<LinkedExpression<'x>>),
    FunctionCall { object: Object, args: Vec<LinkedExpression<'x>> },
}

impl<'x> Display for LinkedStatement<'x> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn statements_to_string_with_tabs(statements: &[LinkedStatement]) -> String {
            let string = statements.iter().map(|s| s.to_string()).collect::<Vec<_>>().join("\n");
            "    ".to_string() + string.replace("\n", "\n    ").as_str()
        }
        match self {
            Self::VariableDeclaration { object, value } => {
                write!(f, "${} := {}", object.id, value)
            }
            Self::SetVariable { object, value } => {
                write!(f, "${} = {}", object.id, value)
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
                let args: Vec<_> = args.iter().map(|x| format!("${}", x.id)).collect();
                write!(f, "${} :: ({}) {{\n{}\n}}", object.id, args.join(", "), inside)
            }
            // Self::Bracket(statements, bracket) => {
            //     match bracket {
            //         BracketType::Curly => {
            //             write!(f, "{{\n{}\n}}", statements.iter().map(|s| s.to_string()).collect::<Vec<String>>().join("\n"))
            //         }
            //         BracketType::Round => {
            //             write!(f, "({})", statements.iter().map(|s| s.to_string()).collect::<Vec<String>>().join("\n"))
            //         }
            //         BracketType::None => panic!("should not be used")
            //     }
            // }
        }
    }
}
impl<'x> Display for LinkedExpression<'x> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LinkedExpression::Plus(a, b) => write!(f, "({a} + {b})"),
            LinkedExpression::NumberLiteral(n) => write!(f, "{}", n.iter().collect::<String>()),
            LinkedExpression::Variable(object) => write!(f, "${}", object.id),
            LinkedExpression::RoundBracket(expression) => write!(f, "({expression})"),
            LinkedExpression::FunctionCall { object, args } => {
                let args = args.iter().map(|x| x.to_string()).collect::<Vec<_>>();
                write!(f, "${} ({})", object.id, args.join(", "))
            },
        }
    }
}


#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ObjType {
    Variable,
    Function
}
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Object {
    id: u32,
    obj_type: ObjType,
}
impl Object {
    pub fn new(obj_type: ObjType) -> Object {
        let id = Self::get_unique_id();
        Object { id, obj_type }
    }
    fn get_unique_id() -> u32 {
        static COUNTER: atomic::AtomicU32 = atomic::AtomicU32::new(0);
        COUNTER.fetch_add(1, atomic::Ordering::Relaxed)
    }
}

#[derive(Debug, Default)]
struct ObjectsContext(HashMap<String, Object>);

impl ObjectsContext {
    fn add(&mut self, name: String, v_type: ObjType) -> Object {
        let object = Object::new(v_type);
        self.0.insert(name, object);
        object
    }
    fn get(&self, name: &String) -> Option<&Object> {
        self.0.get(name)
    }
}

#[derive(Debug)]
struct ObjectContextWindow {
    contexts: Vec<ObjectsContext>,
}

impl ObjectContextWindow {
    fn new() -> ObjectContextWindow {
        ObjectContextWindow { contexts: vec![] }
    }
    fn step_in(&mut self) {
        self.contexts.push(ObjectsContext::default());
    }
    fn step_out(&mut self) {
        if self.contexts.is_empty() {
            panic!("No more objects to step out!");
        }
        self.contexts.pop();
    }
    fn get(&self, name: &String) -> Option<&Object> {
        for object_context in self.contexts.iter().rev() {
            let object = object_context.get(name);
            if object.is_some() {
                return object;
            }
        }
        None
    }
    fn add(&mut self, name: String, v_type: ObjType) -> Object {
        self.contexts.last_mut().unwrap().add(name, v_type)
    }
}

fn link_statements_recursive<'x>(statements: &[Statement<'x>], object_context_window: &mut ObjectContextWindow) -> Result<Vec<LinkedStatement<'x>>, CE> {
    let mut result = vec![];
    for statement in statements {
        let linked = match statement {
            Statement::VariableDeclaration { name, value } => {
                let value = parse_expression(value, object_context_window)?;
                let name: String = name.iter().collect();
                let object = object_context_window.add(name, ObjType::Variable);
                LinkedStatement::VariableDeclaration { object, value }
            }
            Statement::SetVariable { name, value } => {
                let value = parse_expression(value, object_context_window)?;
                let name: String = name.iter().collect();
                let Some(&object) = object_context_window.get(&name) else {
                    let context = format!("{object_context_window:?}");
                    return Err(CE::LinkingError { name, context });
                };
                LinkedStatement::SetVariable { object, value }
            }
            Statement::Expression(expression) => {
                let expression = parse_expression(expression, object_context_window)?;
                LinkedStatement::Expression(expression)
            }
            Statement::If { condition, body } => {
                let condition = parse_expression(condition, object_context_window)?;
                object_context_window.step_in();
                let body = link_statements_recursive(body, object_context_window)?;
                object_context_window.step_out();
                LinkedStatement::If { condition, body }
            }
            Statement::While { condition, body } => {
                let condition = parse_expression(condition, object_context_window)?;
                object_context_window.step_in();
                let body = link_statements_recursive(body, object_context_window)?;
                object_context_window.step_out();
                LinkedStatement::While { condition, body }
            }
            Statement::Function { name, args, body } => {
                let name: String = name.iter().collect();
                let object = object_context_window.add(name, ObjType::Function);
                object_context_window.step_in();
                let args: Vec<Object> = args.iter().map(|x|
                    object_context_window.add(x.iter().collect::<String>(), ObjType::Variable)
                ).collect();
                let body = link_statements_recursive(body, object_context_window)?;
                object_context_window.step_out();
                LinkedStatement::Function { object, args, body }
            }
        };
        result.push(linked);
    }
    Ok(result)
}

fn parse_expression<'x>(expression: &Expression<'x>, object_context_window: &ObjectContextWindow) -> Result<LinkedExpression<'x>, CE> {
    let linked = match expression {
        Expression::Plus(expression1, expression2) => {
            let ex1 = parse_expression(expression1, object_context_window)?;
            let ex2 = parse_expression(expression2, object_context_window)?;
            LinkedExpression::Plus(Box::new(ex1), Box::new(ex2))
        }
        Expression::NumberLiteral(string) => LinkedExpression::NumberLiteral(string),
        Expression::RoundBracket(ex1) => {
            let ex1 = parse_expression(ex1, object_context_window)?;
            LinkedExpression::RoundBracket(Box::new(ex1))
        }
        Expression::Variable(name) => {
            let name: String = name.iter().collect();
            if let Some(object) = object_context_window.get(&name) {
                if object.obj_type == ObjType::Function {
                    return Err(CE::LinkingErrorFunctionUsage { name });
                }
                LinkedExpression::Variable(*object)
            } else {
                let context = format!("{object_context_window:?}");
                return Err(CE::LinkingError { name, context });
            }
        },
        Expression::FunctionCall { name, args } => {
            let name: String = name.iter().collect();
            let Some(object) = object_context_window.get(&name) else {
                let context = format!("{object_context_window:?}");
                return Err(CE::LinkingError { name, context });
            };
            if object.obj_type != ObjType::Function {
                let context = format!("{object_context_window:?}");
                return Err(CE::LinkingError { name, context });
            }
            let args = args.iter().map(|x| parse_expression(x, object_context_window)).collect::<Result<Vec<_>, _>>()?;

            LinkedExpression::FunctionCall { object: *object, args }
        }
    };
    Ok(linked)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::parse3_syntactic::parse_statements;
    use crate::parser::{parse1_tokenize, parse2_brackets};

    fn parse(text: &[char]) -> Result<Vec<LinkedStatement>, CE> {
        let tokens = parse1_tokenize::tokenize(text)?;
        let tokens2 = parse2_brackets::parse_brackets(tokens)?;
        let statements = parse_statements(&tokens2)?;
        let linked_statements = link_variables(statements)?;
        Ok(linked_statements)
    }
    fn string_to_chars(s: &str) -> Vec<char> {
        s.chars().collect()
    }
    fn assert_has_error(str: &str) {
        assert_ne!(parse(&string_to_chars(str)).err(), None);
    }
    fn assert_no_error(str: &str) {
        assert_eq!(parse(&string_to_chars(str)).err(), None);
    }
    #[test]
    fn test_variables() {
        assert_no_error("cat := 0");
        assert_no_error("cat := (0 + 0)");

        assert_has_error("cat = 0");
        assert_has_error("cat = (cat + 0)");
        assert_has_error("cat := cat");
        assert_has_error("dog := cat");
        assert_has_error("cat := (cat + 0)");

        assert_no_error("cat := 0 cat = 0");
        assert_no_error("cat := 0 cat := 0");
        assert_no_error("cat := 0 dog := cat");
        assert_no_error("cat := 0 dog := (cat + cat)");
        assert_no_error("cat := 0 dog := (cat + cat)");
    }

    #[test]
    fn test_functions() {
        assert_no_error("a :: () { }");
        assert_no_error("a :: (arg1) { }");
        assert_no_error("a :: (arg1) { x := arg1 }");
        assert_no_error("a :: (arg1, arg2) { x := arg1 + arg2 }");

        assert_has_error("a :: (arg1) { x = arg1 }");
        assert_has_error("a :: () { a = x }");
        assert_has_error("a :: () { x := a }");

        assert_no_error("a :: () { a := 0 x := a }");
    }

    #[test]
    fn test_function_with_while() {
        assert_has_error("a :: ()  { if 0    { x := 0 } e := x }");
        assert_has_error("a :: (b) { if b    { x := 0 } e := x }");
        assert_has_error("a :: ()  { while 0 { x := 0 } e := x }");
        assert_has_error("a :: (b) { while b { x := 0 } e := x }");

        let text = string_to_chars("a :: (b) { c := b while c { c = b } }");
        let result = parse(&text);
        let Ok(statements) = result else {
            let err = result.err().unwrap();
            panic!("parsing error: {err}");
        };
        assert_eq!(statements.len(), 1);
        let LinkedStatement::Function { object: function_object, args, body } = &statements[0] else {
            panic!("expected function statement");
        };
        assert_eq!(function_object.obj_type, ObjType::Function);
        assert_eq!(args.len(), 1);
        let arg = &args[0];
        assert_eq!(body.len(), 2);

        let LinkedStatement::VariableDeclaration { object: var1, value: value1 } = &body[0] else {
            panic!("expected variable declaration");
        };
        let LinkedExpression::Variable(value1) = value1 else {
            panic!("expected variable declaration");
        };

        let LinkedStatement::While { condition, body: while_body } = &body[1] else {
            panic!("expected while statement");
        };
        let LinkedExpression::Variable(condition) = condition else {
            panic!("expected variable declaration");
        };
        assert_eq!(while_body.len(), 1);
        let LinkedStatement::SetVariable { object: var2, value: value2 } = &while_body[0] else {
            panic!("expected variable declaration");
        };
        let LinkedExpression::Variable(value2) = value2 else {
            panic!("expected variable declaration");
        };
        // arg = b
        // var1 = c
        // value1 = b
        // condition = c
        // var2 = c
        // value2 = b

        assert_eq!(arg, value1);
        assert_eq!(arg, value2);
        assert_eq!(var1, condition);
        assert_eq!(var1, var2);
    }
    
    #[test]
    fn test_function_call() {
        assert_no_error("a :: () {} a()");
        assert_no_error("a :: () {} b :: () { a() }");
    }
}
