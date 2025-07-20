use std::fmt::Display;
use crate::error::CompilationError as CE;
use crate::parser::tokenize2_brackets::{Token2WithPos, Token2, TwoSidedOperation, BracketType};
use crate::parser::tokenize1::PositionInFile;

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
    SetVariable { expression1: Expression, expression2: Expression },
    // Bracket(Box<Vec<Statement>>, BracketType),
    If { condition: Expression, body: Vec<Statement> },
    While { condition: Expression, body: Vec<Statement> },
    Expression(Expression),
}

impl Statement {
    pub fn new_set(expression1: Expression, expression2: Expression) -> Self {
        Statement::SetVariable { expression1, expression2 }
    }
    pub fn new_if(condition: Expression, body: Vec<Statement>) -> Statement {
        Statement::If { condition, body }
    }
    pub fn new_while(condition: Expression, body: Vec<Statement>) -> Statement {
        Statement::While { condition, body }
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
        fn add_tab(s: String) -> String {
            "    ".to_owned() + s.as_str()
        }
        match self {
            Self::SetVariable { expression1, expression2 } => {
                write!(f, "{} = {}", expression1, expression2)
            }
            Self::Expression(expression) => {
                write!(f, "{}", expression)
            }
            Self::If { condition, body } => {
                let inside = body.iter().map(|s| add_tab(s.to_string())).collect::<Vec<_>>().join("\n");
                write!(f, "if {} {{\n{}\n}}", condition, inside)
            }
            Self::While { condition, body } => {
                let inside = body.iter().map(|s| add_tab(s.to_string())).collect::<Vec<_>>().join("\n");
                write!(f, "while {} {{\n{}\n}}", condition, inside)
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
    let token1 = &tokens[0];
    match &token1.token {
        Token2::TwoSidedOperation(_) | Token2::NumberLiteral(_) | Token2::Comma | Token2::Colon | Token2::DoubleColon => {
            Err(CE::SyntacticsError(token1.position, String::from("expected statement")))
        }
        Token2::String(string) => {
            match string.as_str() {
                "if" | "while" => {
                    let name = string.as_str();
                    let (condition, tokens_) = parse_expression(&tokens[1..], token1.position)?;
                    if tokens_.is_empty() {
                        return Err(CE::SyntacticsError(tokens.last().unwrap().position, format!("expected {name} body after that")));
                    }
                    let next_token = &tokens_[0];
                    let Token2::Bracket(vec, BracketType::Curly) = &next_token.token else {
                        return Err(CE::SyntacticsError(token1.position, String::from("expected statement")))
                    };
                    let body = parse_statements(vec)?;

                    if name == "if" {
                        Ok((Statement::new_if(condition, body), &tokens_[1..]))
                    } else {
                        Ok((Statement::new_while(condition, body), &tokens_[1..]))
                    }
                }
                _ => {
                    let expression1 = Expression::Variable(string.clone());
                    parse_statement2(&tokens[1..], expression1, token1.position)
                }
            }
        }
        Token2::Bracket(_, _) => {
            Err(CE::SyntacticsError(token1.position, format!("unexpected bracket open at {}, expected statement", token1.position)))
        }
    }
}
fn parse_statement2(tokens: &[Token2WithPos], expression1: Expression, previous_place_info: PositionInFile) -> Result<(Statement, &[Token2WithPos]), CE> {
    if tokens.is_empty() {
        return Err(CE::SyntacticsError(previous_place_info, String::from("expected statement")));
    }
    let token2 = &tokens[0];
    match &token2.token {
        Token2::NumberLiteral(_) | Token2::String(_) | Token2::Comma | Token2::Colon | Token2::DoubleColon => {
            Ok((Statement::Expression(expression1), tokens))
        }
        Token2::TwoSidedOperation(TwoSidedOperation::Equal) => {
            let (expression2, tokens) = parse_expression(&tokens[1..], token2.position)?;
            let statement = Statement::new_set(expression1, expression2);
            Ok((statement, tokens))
        }
        Token2::TwoSidedOperation(_) => {
            Err(CE::SyntacticsError(token2.position, String::from("unexpected operation, expected '='")))
        }
        Token2::Bracket(_, _) => {
            Err(CE::SyntacticsError(token2.position, String::from("unexpected bracket open, expected '='")))
        }
    }
}

fn parse_expression(tokens: &[Token2WithPos], previous_place_info: PositionInFile) -> Result<(Expression, &[Token2WithPos]), CE> {
    if tokens.is_empty() {
        return Err(CE::SyntacticsError(previous_place_info, String::from("expected expression after that")));
    }
    let token1 = &tokens[0];
    match &token1.token {
        Token2::TwoSidedOperation(_) | Token2::Comma | Token2::Colon | Token2::DoubleColon => {
            Err(CE::SyntacticsError(token1.position, String::from("expected expression")))
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
            let (expression, tokens_) = parse_expression(vec, token1.position)?;
            if tokens_.is_empty() {
                let expression1 = Expression::round_bracket(expression);
                parse_expression2(&tokens[1..], expression1)
            } else {
                Err(CE::SyntacticsError(tokens_[0].position, String::from("expected ')'")))
            }
        }
        Token2::Bracket(_, _) => {
            Err(CE::SyntacticsError(token1.position, String::from("expected expression, got open bracket")))
        }
    }
}
// parse "expression1 twoSidedOp expression2"
fn parse_expression2(tokens: &[Token2WithPos], expression1: Expression) -> Result<(Expression, &[Token2WithPos]), CE> {
    if tokens.is_empty() {
        return Ok((expression1, tokens));
    }
    let token2 = &tokens[0];
    match &token2.token {
        Token2::String(_) | Token2::NumberLiteral(_) | Token2::Comma | Token2::Colon | Token2::DoubleColon => {
            Ok((expression1, tokens))
        }
        Token2::TwoSidedOperation(TwoSidedOperation::Equal) => {
            Err(CE::SyntacticsError(token2.position, String::from("unexpected '='")))
        }
        Token2::TwoSidedOperation(op) => {
            match op {
                TwoSidedOperation::Equal => unreachable!(),
                TwoSidedOperation::Plus => {
                    let (expression2, tokens) = parse_expression(&tokens[1..], token2.position)?;
                    let expression = Expression::plus(expression1, expression2);
                    Ok((expression, tokens))
                }
            }
        }
        Token2::Bracket(_, _) => {
            Ok((expression1, tokens))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::{tokenize1, tokenize2_brackets};

    #[test]
    fn test_statements() {
        fn parse(text: &str) -> Result<Vec<Statement>, CE> {
            let tokens = tokenize1::tokenize(text)?;
            let tokens2 = tokenize2_brackets::parse_brackets(tokens)?;
            let statements = parse_statements(&tokens2)?;
            Ok(statements)
        }
        assert_eq!(parse("cat = cat").err(), None);
        assert_eq!(parse("cat = 4").err(), None);
        assert_eq!(parse("cat = cat + cat").err(), None);
        assert_eq!(parse("cat = cat + 4").err(), None);
        assert_eq!(parse("cat = 4 + cat").err(), None);
        assert_eq!(parse("cat = 4 + 4").err(), None);
        assert_eq!(parse("cat = cat + cat + cat").err(), None);
        assert_eq!(parse("cat = cat + cat + cat + cat").err(), None);

        assert_ne!(parse("4 = cat").err(), None);
        assert_ne!(parse("cat = cat = cat").err(), None);
        assert_ne!(parse("cat + cat = cat").err(), None);
        assert_ne!(parse("cat + 4 = cat").err(), None);
        assert_ne!(parse("cat + cat").err(), None);
        assert_ne!(parse("cat += cat").err(), None);
        assert_ne!(parse("cat =+ cat").err(), None);
        assert_ne!(parse("cat = +cat").err(), None);
        assert_ne!(parse("cat == cat").err(), None);

        assert_eq!(parse("cat = (4 + 4)").err(), None);
        assert_eq!(parse("cat = ((4 + 4))").err(), None);
        assert_eq!(parse("cat = 4 + (4 + 4)").err(), None);
        assert_eq!(parse("cat = (4 + 4) + 4").err(), None);
        assert_eq!(parse("cat = cat \n cat = cat").err(), None);

        assert_ne!(parse("(cat + cat) = cat").err(), None);
        assert_ne!(parse("(4 + 4) = 4").err(), None);

        let variable = Expression::Variable(String::from("cat"));
        assert_eq!(parse("cat = cat + (cat + cat)"),
                   Ok(vec![Statement::new_set(
                       variable.clone(),
                       Expression::plus(
                           variable.clone(),
                           Expression::round_bracket(
                               Expression::plus(variable.clone(), variable.clone())
                           )
                       )
                   )])
        );

        assert_eq!(parse("if cat { }"),
            Ok(vec![Statement::new_if(variable.clone(), vec![], )])
        );
        assert_eq!(parse("if cat { cat = cat }"),
            Ok(vec![Statement::new_if(variable.clone(),
                vec![Statement::new_set(variable.clone(), variable.clone())],
            )])
        );
        assert_eq!(parse("if cat { cat = cat cat = cat }"),
           Ok(vec![Statement::new_if(variable.clone(),
                vec![
                    Statement::new_set(variable.clone(), variable.clone()),
                    Statement::new_set(variable.clone(), variable.clone())
                ],
           )])
        );
        assert_eq!(parse("while cat { }"),
           Ok(vec![Statement::new_while(variable.clone(), vec![], )])
        );
        assert_eq!(parse("while cat { cat = cat }"),
            Ok(vec![Statement::new_while(variable.clone(),
                vec![Statement::new_set(variable.clone(), variable.clone())],
            )])
        );

        assert_ne!(parse("if cat = cat { cat = cat }").err(), None);
        assert_ne!(parse("if { cat = cat }").err(), None);
    }
}
