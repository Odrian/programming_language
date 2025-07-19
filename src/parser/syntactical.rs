use std::fmt::Display;
use crate::error::CompilationError as CE;
use crate::parser::tokenizer::{TokenWithPos, Token, PositionInFile};

pub fn parse(tokens: &[TokenWithPos]) -> Result<Vec<Statement>, CE> {
    let mut statements = Vec::new();

    let mut tokens = tokens;
    while !tokens.is_empty() {
        let (statement, new_tokens) = parse_statement(tokens)?;
        tokens = new_tokens;
        statements.push(statement);
    }

    Ok(statements)
}

pub enum Statement {
    SetVariable { expression1: Expression, expression2: Expression },
    Expression(Expression),
}

pub enum Expression {
    Plus(Box<Expression>, Box<Expression>),
    NumberLiteral(String),
    Variable(String),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SetVariable { expression1, expression2 } => {
                write!(f, "{} = {}", expression1, expression2)
            }
            Self::Expression(expression) => {
                write!(f, "{}", expression)
            }
        }
    }
}
impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Plus(a, b) => write!(f, "({} + {})", a, b),
            Self::NumberLiteral(n) => write!(f, "{}", n),
            Self::Variable(name) => write!(f, "{}", name),
        }
    }
}

fn parse_statement(tokens: &[TokenWithPos]) -> Result<(Statement, &[TokenWithPos]), CE> {
    if tokens.is_empty() {
        panic!("tokens should not be empty");
    }
    let token1 = &tokens[0];
    match &token1.token {
        Token::Plus | Token::Equal | Token::NumberLiteral(_) => {
            Err(CE::SyntacticsError(token1.position, format!("expected statement, but get {:?}", &token1.token)))
        }
        Token::String(string) => {
            let expression1 = Expression::Variable(string.clone());
            parse_statement2(&tokens[1..], expression1, token1.position)
        }
    }
}
fn parse_statement2(tokens: &[TokenWithPos], expression1: Expression, previous_place_info: PositionInFile) -> Result<(Statement, &[TokenWithPos]), CE> {
    if tokens.is_empty() {
        return Err(CE::SyntacticsError(previous_place_info, String::from("expected statement")));
    }
    let token2 = &tokens[0];
    match &token2.token {
        Token::Plus | Token::NumberLiteral(_) | Token::String(_) => {
            Ok((Statement::Expression(expression1), tokens))
        }
        Token::Equal => {
            let (expression2, tokens) = parse_expression(&tokens[1..], token2.position)?;
            let statement = Statement::SetVariable { expression1, expression2 };
            Ok((statement, tokens))
        }
    }
}

fn parse_expression(tokens: &[TokenWithPos], previous_place_info: PositionInFile) -> Result<(Expression, &[TokenWithPos]), CE> {
    if tokens.is_empty() {
        return Err(CE::SyntacticsError(previous_place_info, String::from("expected expression after that")));
    }
    let token1 = &tokens[0];
    match &token1.token {
        Token::Equal | Token::Plus => {
            Err(CE::SyntacticsError(token1.position, format!("expected expression, but get {:?}", &token1.token)))
        }
        Token::String(string) => {
            let expression1 = Expression::Variable(string.to_string());
            parse_expression2(&tokens[1..], expression1)
        }
        Token::NumberLiteral(value) => {
            let expression1 = Expression::NumberLiteral(value.to_string());
            parse_expression2(&tokens[1..], expression1)
        }
    }
}
// parse "expression1 + expression2"
fn parse_expression2(tokens: &[TokenWithPos], expression1: Expression) -> Result<(Expression, &[TokenWithPos]), CE> {
    if tokens.is_empty() {
        return Ok((expression1, tokens));
    }
    let token2 = &tokens[0];
    match &token2.token {
        Token::String(_) | Token::NumberLiteral(_) | Token::Equal => {
            Ok((expression1, tokens))
        }
        Token::Plus => {
            let (expression2, tokens) = parse_expression(&tokens[1..], token2.position)?;
            let expression = Expression::Plus(Box::new(expression1), Box::new(expression2));
            Ok((expression, tokens))
        }
    }
}
