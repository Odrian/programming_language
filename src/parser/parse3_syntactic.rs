use std::fmt::Display;
use crate::error::CompilationError as CE;
use crate::parser::parse2_brackets::{Token2WithPos, Token2, TwoSidedOperation, EqualOperation, BracketType};
use crate::parser::parse1_tokenize::PositionInFile;

pub fn parse_statements(tokens: &[Token2WithPos]) -> Result<Vec<Statement>, CE> {
    let mut statements = Vec::new();

    let mut tokens = tokens;
    while !tokens.is_empty() {
        let (statement, new_tokens) = parse_statement(tokens)?;
        tokens = new_tokens;
        statements.push(statement);
    }

    Ok(statements)
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Statement {
    VariableDeclaration { name: String, value: Expression },
    SetVariable { name: String, value: Expression },
    // Bracket(Box<Vec<Statement>>, BracketType),
    If { condition: Expression, body: Vec<Statement> },
    While { condition: Expression, body: Vec<Statement> },
    Function { name: String, args: Vec<String>, body: Vec<Statement> },
}

impl Statement {
    pub fn new_variable(name: String, value: Expression) -> Self {
        Statement::VariableDeclaration { name, value }
    }
    pub fn new_if(condition: Expression, body: Vec<Statement>) -> Statement {
        Statement::If { condition, body }
    }
    pub fn new_while(condition: Expression, body: Vec<Statement>) -> Statement {
        Statement::While { condition, body }
    }
    pub fn new_function(name: String, args: Vec<String>, body: Vec<Statement>) -> Self {
        Statement::Function { name, args, body }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression {
    Plus(Box<Expression>, Box<Expression>),
    NumberLiteral(String),
    Variable(String),
    RoundBracket(Box<Expression>),
}
impl Expression {
    pub fn plus(expression1: Expression, expression2: Expression) -> Self {
        Expression::Plus(Box::new(expression1), Box::new(expression2))
    }
    pub fn round_bracket(expression: Expression) -> Self {
        Expression::RoundBracket(Box::new(expression))
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn statements_to_string_with_tabs(statements: &[Statement]) -> String {
            let string = statements.iter().map(|s| s.to_string()).collect::<Vec<_>>().join("\n");
            "    ".to_string() + string.replace("\n", "\n    ").as_str()
        }
        match self {
            Self::VariableDeclaration { name, value } => {
                write!(f, "{} := {}", name, value)
            }
            Self::SetVariable { name, value } => {
                write!(f, "{} = {}", name, value)
            }
            Self::If { condition, body } => {
                let inside = statements_to_string_with_tabs(body);
                write!(f, "if {} {{\n{}\n}}", condition, inside)
            }
            Self::While { condition, body } => {
                let inside = statements_to_string_with_tabs(body);
                write!(f, "while {} {{\n{}\n}}", condition, inside)
            }
            Self::Function { name, args, body } => {
                let inside = statements_to_string_with_tabs(body);
                write!(f, "{} :: ({}) {{\n{}\n}}", name, args.join(", "), inside)
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
impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Plus(a, b) => write!(f, "({} + {})", a, b),
            Expression::NumberLiteral(n) => write!(f, "{}", n),
            Expression::Variable(name) => write!(f, "{}", name),
            Expression::RoundBracket(boxed) => write!(f, "({})", boxed),
        }
    }
}

fn parse_statement(tokens: &[Token2WithPos]) -> Result<(Statement, &[Token2WithPos]), CE> {
    if tokens.is_empty() {
        panic!("tokens should not be empty");
    }
    let token = &tokens[0];
    match &token.token {
        Token2::NumberLiteral(_) | Token2::TwoSidedOperation(_) | Token2::EqualOperation(_) | Token2::Comma | Token2::Colon | Token2::DoubleColon => {
            Err(CE::SyntacticsError(token.position, String::from("expected statement")))
        }
        Token2::String(string) => {
            match string.as_str() {
                "if" | "while" => {
                    let name = string.as_str();
                    let (condition, new_tokens) = parse_expression(&tokens[1..], token.position)?;
                    if new_tokens.is_empty() {
                        return Err(CE::SyntacticsError(tokens.last().unwrap().position, format!("expected {name} body after that")));
                    }
                    let next_token = &new_tokens[0];
                    let Token2::Bracket(vec, BracketType::Curly) = &next_token.token else {
                        return Err(CE::SyntacticsError(token.position, format!("expected {name} body")))
                    };
                    let body = parse_statements(vec)?;

                    if name == "if" {
                        Ok((Statement::new_if(condition, body), &new_tokens[1..]))
                    } else {
                        Ok((Statement::new_while(condition, body), &new_tokens[1..]))
                    }
                }
                _ => {
                    parse_statement2(&tokens[1..], string, token.position)
                }
            }
        }
        Token2::Bracket(_, _) => {
            Err(CE::SyntacticsError(token.position, format!("unexpected bracket open at {}, expected statement", token.position)))
        }
    }
}
fn parse_statement2<'a>(tokens: &'a [Token2WithPos], string: &String, previous_place_info: PositionInFile) -> Result<(Statement, &'a [Token2WithPos]), CE> {
    if tokens.is_empty() {
        return Err(CE::SyntacticsError(previous_place_info, String::from("expected statement")));
    }
    let new_token = &tokens[0].token;
    match new_token {
        Token2::DoubleColon => {
            parse_function(&tokens[1..], string.clone(), tokens[0].position)
        },
        Token2::EqualOperation(equal_operation) => {
            let (expression2, new_tokens) = parse_expression(&tokens[1..], tokens[0].position)?;
            let statement = match equal_operation {
                EqualOperation::ColonEqual => Statement::VariableDeclaration { name: string.clone(), value: expression2 },
                EqualOperation::Equal => Statement::SetVariable { name: string.clone(), value: expression2 },
            };
            Ok((statement, new_tokens))
        }
        _ => {
            Err(CE::SyntacticsError(tokens[0].position, format!("expected statement, got '{string}' '{new_token:?}'")))
        }
    }
}

fn parse_expression(tokens: &[Token2WithPos], previous_place_info: PositionInFile) -> Result<(Expression, &[Token2WithPos]), CE> {
    if tokens.is_empty() {
        return Err(CE::SyntacticsError(previous_place_info, String::from("expected expression after that")));
    }
    let token = &tokens[0];
    match &token.token {
        Token2::TwoSidedOperation(_) | Token2::EqualOperation(_) | Token2::Comma | Token2::Colon | Token2::DoubleColon => {
            Err(CE::SyntacticsError(token.position, String::from("expected expression")))
        }
        Token2::String(string) => {
            let expression1 = Expression::Variable(string.to_string());
            parse_expression2(&tokens[1..], expression1)
        }
        Token2::NumberLiteral(value) => {
            let expression1 = Expression::NumberLiteral(value.to_string());
            parse_expression2(&tokens[1..], expression1)
        }
        Token2::Bracket(vec, BracketType::Round) => {
            let (expression, new_tokens) = parse_expression(vec, token.position)?;
            if new_tokens.is_empty() {
                let expression1 = Expression::round_bracket(expression);
                parse_expression2(&tokens[1..], expression1)
            } else {
                Err(CE::SyntacticsError(new_tokens[0].position, String::from("expected ')'")))
            }
        }
        Token2::Bracket(_, _) => {
            Err(CE::SyntacticsError(token.position, String::from("expected expression, got open bracket")))
        }
    }
}
// parse "expression1 twoSidedOp expression2"
fn parse_expression2(tokens: &[Token2WithPos], expression1: Expression) -> Result<(Expression, &[Token2WithPos]), CE> {
    if tokens.is_empty() {
        return Ok((expression1, tokens));
    }
    let token = &tokens[0];
    match &token.token {
        Token2::String(_) | Token2::NumberLiteral(_) | Token2::EqualOperation(_) | Token2::Comma | Token2::Colon | Token2::DoubleColon => {
            Ok((expression1, tokens))
        }
        Token2::TwoSidedOperation(op) => {
            match op {
                TwoSidedOperation::Plus => {
                    let (expression2, new_tokens) = parse_expression(&tokens[1..], token.position)?;
                    let expression = Expression::plus(expression1, expression2);
                    Ok((expression, new_tokens))
                }
            }
        }
        Token2::Bracket(_, _) => {
            Ok((expression1, tokens))
        }
    }
}

fn parse_function(tokens: &[Token2WithPos], name: String, previous_place_info: PositionInFile) -> Result<(Statement, &[Token2WithPos]), CE> {
    if tokens.is_empty() {
        return Err(CE::SyntacticsError(previous_place_info, String::from("expected function declaration")));
    }
    let Token2::Bracket(args, BracketType::Round) = &tokens[0].token else {
        return Err(CE::SyntacticsError(tokens[0].position, String::from("expected function declaration")));
    };

    // parse arguments
    let arguments: Vec<String> = if args.is_empty() {
        Vec::new()
    } else {
        let mut arguments = Vec::with_capacity((args.len()+1) / 2);

        let Token2::String(arg1) = &args[0].token else {
            return Err(CE::SyntacticsError(args[0].position, String::from("expected argument name")));
        };
        arguments.push(arg1.clone());

        let mut index = 1;
        while index < args.len() {
            if args[index].token != Token2::Comma {
                return Err(CE::SyntacticsError(args[index].position, String::from("expected ',' or ')'")));
            }
            index += 1;
            if index == args.len() {
                // TODO: break
                return Err(CE::SyntacticsError(args[index-1].position, String::from("expected argument after comma")));
            }
            let Token2::String(arg_i) = &args[index].token else {
                return Err(CE::SyntacticsError(args[index].position, String::from("expected argument name in function declaration")));
            };
            arguments.push(arg_i.clone());
            index += 1;
        }

        arguments
    };

    if tokens.len() == 1 {
        return Err(CE::SyntacticsError(tokens[0].position, String::from("expected curly brackets after function declaration")));
    }

    // parse inside
    let Token2::Bracket(body, BracketType::Curly) = &tokens[1].token else {
        return Err(CE::SyntacticsError(tokens[1].position, String::from("expected curly brackets after function declaration")));
    };
    let body = parse_statements(body)?;

    let statement = Statement::new_function(name, arguments, body);
    Ok((statement, &tokens[2..]))
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::{parse1_tokenize, parse2_brackets};

    fn parse(text: &str) -> Result<Vec<Statement>, CE> {
        let tokens = parse1_tokenize::tokenize(text)?;
        let tokens2 = parse2_brackets::parse_brackets(tokens)?;
        let statements = parse_statements(&tokens2)?;
        Ok(statements)
    }

    #[test]
    fn test_set() {
        assert_eq!(parse("cat = cat").err(), None);
        assert_eq!(parse("cat = 4").err(), None);
        assert_eq!(parse("cat = cat + cat").err(), None);
        assert_eq!(parse("cat = cat + 4").err(), None);
        assert_eq!(parse("cat = 4 + cat").err(), None);
        assert_eq!(parse("cat = 4 + 4").err(), None);
        assert_eq!(parse("cat = cat + cat + cat").err(), None);
        assert_eq!(parse("cat = cat + cat + cat + cat").err(), None);
        assert_eq!(parse("cat := cat + cat + cat + cat").err(), None);

        assert_ne!(parse("4 = cat").err(), None);
        assert_ne!(parse("cat = cat = cat").err(), None);
        assert_ne!(parse("cat + cat = cat").err(), None);
        assert_ne!(parse("cat + 4 = cat").err(), None);
        assert_ne!(parse("cat + cat").err(), None);
        assert_ne!(parse("cat += cat").err(), None);
        assert_ne!(parse("cat =+ cat").err(), None);
        assert_ne!(parse("cat = +cat").err(), None);
        assert_ne!(parse("cat == cat").err(), None);
        assert_ne!(parse("cat := +cat").err(), None);
        assert_ne!(parse("cat :== cat").err(), None);
    }

    #[test]
    fn test_set_with_brackets() {
        assert_eq!(parse("cat = (4 + 4)").err(), None);
        assert_eq!(parse("cat = ((4 + 4))").err(), None);
        assert_eq!(parse("cat = 4 + (4 + 4)").err(), None);
        assert_eq!(parse("cat = (4 + 4) + 4").err(), None);
        assert_eq!(parse("cat = cat \n cat = cat").err(), None);

        assert_ne!(parse("(cat + cat) = cat").err(), None);
        assert_ne!(parse("(cat + cat) := cat").err(), None);
        assert_ne!(parse("(4 + 4) = 4").err(), None);
        assert_ne!(parse("(4 + 4) := 4").err(), None);

        let variable_name = String::from("cat");
        let variable = Expression::Variable(variable_name.clone());
        assert_eq!(parse("cat := cat + (cat + cat)"),
            Ok(vec![Statement::new_variable(
                variable_name.clone(),
                Expression::plus(
                    variable.clone(),
                    Expression::round_bracket(
                        Expression::plus(variable.clone(), variable.clone())
                    )
                )
            )])
        );
    }

    #[test]
    fn test_if_while() {
        let variable_name = String::from("cat");
        let variable = Expression::Variable(variable_name.clone());

        assert_eq!(parse("if cat { }"),
            Ok(vec![Statement::new_if(variable.clone(), vec![], )])
        );
        assert_eq!(parse("if cat { cat := cat }"),
            Ok(vec![Statement::new_if(variable.clone(),
                vec![Statement::new_variable(variable_name.clone(), variable.clone())],
            )])
        );
        assert_eq!(parse("if cat { cat := cat cat := cat }"),
           Ok(vec![Statement::new_if(variable.clone(),
                vec![
                    Statement::new_variable(variable_name.clone(), variable.clone()),
                    Statement::new_variable(variable_name.clone(), variable.clone())
                ],
           )])
        );
        assert_eq!(parse("while cat { }"),
           Ok(vec![Statement::new_while(variable.clone(), vec![], )])
        );
        assert_eq!(parse("while cat { cat := cat }"),
            Ok(vec![Statement::new_while(variable.clone(),
                vec![Statement::new_variable(variable_name.clone(), variable.clone())],
            )])
        );

        assert_ne!(parse("if cat = cat { cat := cat }").err(), None);
        assert_ne!(parse("if { cat = cat }").err(), None);
    }
    #[test]
    fn test_function() {
        let name = String::from("foo");
        let arg1 = String::from("arg1");
        let arg2 = String::from("arg2");
        assert_eq!(parse("foo :: () { }").err(), None);

        assert_ne!(parse(":: () { }").err(), None);
        assert_ne!(parse("foo () { }").err(), None);
        assert_ne!(parse("foo :: { }").err(), None);
        assert_ne!(parse("foo :: ()").err(), None);

        assert_eq!(parse("foo :: () { }"),
            Ok(vec![Statement::new_function(name.clone(), vec![], vec![])])
        );
        assert_eq!(parse("foo :: (arg1) { }"),
                   Ok(vec![Statement::new_function(name.clone(), vec![arg1.clone()], vec![])])
        );
        assert_eq!(parse("foo :: (arg1, arg2) { }"),
                   Ok(vec![Statement::new_function(name.clone(), vec![arg1.clone(), arg2.clone()], vec![])])
        );
        let set_expr = Statement::new_variable("cat".to_owned(), Expression::Variable("dog".to_owned()));
        assert_eq!(parse("foo :: (arg1, arg2) { cat := dog }"),
           Ok(vec![Statement::new_function(name.clone(), vec![arg1.clone(), arg2.clone()], vec![
               set_expr.clone(),
           ])])
        );

        assert_eq!(parse("foo :: (arg1, arg2) { cat := dog cat := dog }"),
           Ok(vec![Statement::new_function(name.clone(), vec![arg1.clone(), arg2.clone()], vec![
               set_expr.clone(),
               set_expr.clone(),
           ])])
        );
        let another_set_expr = Statement::new_variable("owl".to_owned(), Expression::plus(
            Expression::Variable("dog".to_owned()), Expression::Variable("kitten".to_owned())
        ));
        assert_eq!(parse("foo :: (arg1, arg2) { owl := dog + kitten cat := dog owl := dog + kitten }"),
           Ok(vec![Statement::new_function(name.clone(), vec![arg1.clone(), arg2.clone()], vec![
               another_set_expr.clone(),
               set_expr.clone(),
               another_set_expr.clone(),
           ])])
        );
    }

    #[test]
    fn test_function_with_while() {
        let name = String::from("foo");
        let arg1 = String::from("arg1");
        let arg2 = String::from("arg2");
        let set_statement = Statement::new_variable(
            arg2.clone(),
            Expression::plus(
                Expression::Variable(arg2.clone()),
                Expression::NumberLiteral("1".to_owned()),
            ),
        );
        let while_statement = Statement::new_while(
            Expression::Variable(arg1.clone()),
            vec![set_statement.clone()]
        );
        let create_function_statement = |body| {
            Statement::new_function(name.clone(), vec![arg1.clone(), arg2.clone()], body)
        };
        assert_eq!(parse("foo :: (arg1, arg2) { while arg1 { arg2 := arg2 + 1 } }"),
           Ok(vec![create_function_statement(vec![
               while_statement.clone(),
           ])])
        );
        assert_eq!(parse("foo::(arg1,arg2){while arg1{arg2:=arg2+1}}"),
           Ok(vec![create_function_statement(vec![
               while_statement.clone(),
           ])])
        );
        assert_eq!(parse("foo :: (arg1, arg2) { arg2 := arg2 + 1 while arg1 { arg2 := arg2 + 1 } }"),
           Ok(vec![create_function_statement(vec![
               set_statement.clone(),
               while_statement.clone(),
           ])])
        );
        assert_eq!(parse("foo :: (arg1, arg2) { arg2 := arg2 + 1 while arg1 { arg2 := arg2 + 1 } arg2 := arg2 + 1 }"),
           Ok(vec![create_function_statement(vec![
               set_statement.clone(),
               while_statement.clone(),
               set_statement.clone(),
           ])])
        );
    }
}
