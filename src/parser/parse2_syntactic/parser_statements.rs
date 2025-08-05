use crate::error::CompilationError as CE;
use crate::parser::{BracketType, PositionInFile};

use super::statement::*;
use crate::parser::parse1_tokenize::token::*;

pub fn parse_statements<'text>(tokens: &[TokenWithPos<'text>]) -> Result<Vec<Statement<'text>>, CE> {
    let mut statements = Vec::new();

    let mut state = ParsingState::new(tokens);
    while !state.at_end() {
        statements.push(state.parse_statement()?);
    }

    Ok(statements)
}

pub struct ParsingState<'text, 'a> {
    tokens: &'a [TokenWithPos<'text>],
    index: usize,
    size: usize,
}

impl<'text, 'a> ParsingState<'text, 'a> {
    pub fn new(tokens: &'a [TokenWithPos<'text>]) -> Self {
        let size = tokens.len();
        ParsingState { tokens, index: 0, size }
    }
    pub fn at_end(&self) -> bool {
        self.index >= self.size
    }
    pub fn parse_statement(&mut self) -> Result<Statement<'text>, CE> {
        assert!(!self.at_end(), "tokens should not be empty");

        let token = &self.tokens[self.index];
        self.index += 1;
        match &token.token {
            Token::NumberLiteral(_) | Token::TwoSidedOperation(_) | Token::EqualOperation(_) | Token::Comma | Token::Colon | Token::DoubleColon => {
                Err(CE::SyntacticsError(token.position, String::from("expected statement")))
            }
            Token::String(chars) => {
                let string = chars.iter().collect::<String>();
                match string.as_str() {
                    "if" | "while" => {
                        let condition = self.parse_expression(token.position)?;
                        if self.at_end() {
                            return Err(CE::SyntacticsError(self.tokens.last().unwrap().position, format!("expected {string} body after that")));
                        }
                        let next_token = &self.tokens[self.index];
                        self.index += 1;
                        let Token::Bracket(vec, BracketType::Curly) = &next_token.token else {
                            return Err(CE::SyntacticsError(token.position, format!("expected {string} body")))
                        };
                        let body = parse_statements(vec)?;

                        if string.as_str() == "if" {
                            Ok(Statement::new_if(condition, body))
                        } else {
                            Ok(Statement::new_while(condition, body))
                        }
                    }
                    "return" => {
                        let result_expression = self.parse_expression(token.position)?;
                        Ok(Statement::Return(result_expression))
                    }
                    _ => {
                        self.parse_statement2(chars, token.position)
                    }
                }
            }
            Token::Bracket(_, _) => {
                Err(CE::SyntacticsError(token.position, format!("unexpected bracket open at {}, expected statement", token.position)))
            }
        }
    }
    fn parse_statement2(&mut self, string: &'text [char], previous_place_info: PositionInFile) -> Result<Statement<'text>, CE> {
        if self.at_end() {
            return Err(CE::SyntacticsError(previous_place_info, String::from("expected statement")));
        }
        let new_token = &self.tokens[self.index];
        self.index += 1;
        match &new_token.token {
            Token::DoubleColon => {
                self.parse_function(string, new_token.position)
            },
            Token::EqualOperation(equal_operation) => {
                let expression2 = self.parse_expression(new_token.position)?;
                let statement = match equal_operation {
                    EqualOperation::ColonEqual => Statement::new_variable(string, expression2),
                    EqualOperation::Equal => Statement::new_set(string, expression2),
                };
                Ok(statement)
            }
            Token::Bracket(vec, BracketType::Round) => {
                let name = string;
                let args = parse_function_arguments(vec, new_token.position)?;
                let statement = Statement::Expression(Expression::new_function_call(name, args));
                Ok(statement)
            }
            _ => {
                let string = string.iter().collect::<String>();
                Err(CE::SyntacticsError(new_token.position, format!("expected statement, got '{string}' '{:?}'", new_token.token)))
            }
        }
    }

    fn parse_expression(&mut self, previous_place_info: PositionInFile) -> Result<Expression<'text>, CE> {
        if self.at_end() {
            return Err(CE::SyntacticsError(previous_place_info, String::from("expected expression after that")));
        }
        let token = &self.tokens[self.index];
        self.index += 1;
        match &token.token {
            Token::TwoSidedOperation(_) | Token::EqualOperation(_) | Token::Comma | Token::Colon | Token::DoubleColon => {
                Err(CE::SyntacticsError(token.position, String::from("expected expression")))
            }
            Token::String(string) => {
                let expression1 = Expression::Variable(string);
                self.parse_expression2(expression1)
            }
            Token::NumberLiteral(value) => {
                let expression1 = Expression::NumberLiteral(value);
                self.parse_expression2(expression1)
            }
            Token::Bracket(vec, BracketType::Round) => {
                let mut new_state = ParsingState::new(vec);
                let expression = new_state.parse_expression(token.position)?;
                if new_state.at_end() {
                    let expression1 = Expression::new_round_bracket(expression);
                    self.parse_expression2(expression1)
                } else {
                    Err(CE::SyntacticsError(vec[new_state.index].position, String::from("expected ')'")))
                }
            }
            Token::Bracket(_, _) => {
                Err(CE::SyntacticsError(token.position, String::from("expected expression, got open bracket")))
            }
        }
    }
    // parse "expression1 twoSidedOp expression2"
    fn parse_expression2(&mut self, expression1: Expression<'text>) -> Result<Expression<'text>, CE> {
        if self.at_end() {
            return Ok(expression1);
        }
        let token = &self.tokens[self.index];
        match &token.token {
            Token::TwoSidedOperation(op) => {
                self.index += 1;
                let expression2 = self.parse_expression(token.position)?;
                Ok(Expression::new_two_sided_op(expression1, expression2, *op))
            }
            Token::Bracket(vec, BracketType::Round) => {
                self.index += 1;
                let Expression::Variable(name) = expression1 else {
                    return Err(CE::SyntacticsError(token.position, String::from("unexpected round brackets after expression")));
                };
                let args = parse_function_arguments(vec, token.position)?;
                Ok(Expression::new_function_call(name, args))
            }
            Token::Bracket(_, _) | Token::String(_) | Token::NumberLiteral(_) | Token::EqualOperation(_) | Token::Comma | Token::Colon | Token::DoubleColon => {
                Ok(expression1)
            }
        }
    }

    fn parse_function(&mut self, name: &'text [char], previous_place_info: PositionInFile) -> Result<Statement<'text>, CE> {
        if self.at_end() {
            return Err(CE::SyntacticsError(previous_place_info, String::from("expected function declaration")));
        }
        let token = &self.tokens[self.index];
        self.index += 1;
        let Token::Bracket(args, BracketType::Round) = &token.token else {
            return Err(CE::SyntacticsError(token.position, String::from("expected function declaration")));
        };

        // parse arguments
        let arguments: Vec<&[char]> = if args.is_empty() {
            Vec::new()
        } else {
            let mut arguments = Vec::with_capacity(args.len().div_ceil(2));

            let Token::String(arg1) = &args[0].token else {
                return Err(CE::SyntacticsError(args[0].position, String::from("expected argument name")));
            };
            arguments.push(*arg1);

            let mut index = 1;
            while index < args.len() {
                if args[index].token != Token::Comma {
                    return Err(CE::SyntacticsError(args[index].position, String::from("expected ',' or ')'")));
                }
                index += 1;
                if index == args.len() {
                    break // redundant comma
                }
                let Token::String(arg_i) = &args[index].token else {
                    return Err(CE::SyntacticsError(args[index].position, String::from("expected argument name in function declaration")));
                };
                arguments.push(*arg_i);
                index += 1;
            }

            arguments
        };

        if self.at_end() {
            return Err(CE::SyntacticsError(token.position, String::from("expected curly brackets after function declaration")));
        }

        // parse inside
        let new_token = &self.tokens[self.index];
        self.index += 1;
        let Token::Bracket(body, BracketType::Curly) = &new_token.token else {
            return Err(CE::SyntacticsError(new_token.position, String::from("expected curly brackets after function declaration")));
        };
        let body = parse_statements(body)?;

        let statement = Statement::new_function(name, arguments, body);
        Ok(statement)
    }
}

fn parse_function_arguments<'text>(tokens: &[TokenWithPos<'text>], previous_place_info: PositionInFile) -> Result<Vec<Expression<'text>>, CE> {
    let mut args = Vec::new();

    let mut state = ParsingState::new(tokens);
    while !state.at_end() {
        let expression = state.parse_expression(previous_place_info)?;
        args.push(expression);

        if !state.at_end() {
            let token = &state.tokens[state.index];
            if token.token == Token::Comma {
                state.index += 1;
            } else {
                return Err(CE::SyntacticsError(token.position, format!("expected ',' or ')', got {:?}", token.token)));
            }
        }
    }

    Ok(args)
}
