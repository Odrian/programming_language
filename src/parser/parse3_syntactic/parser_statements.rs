use crate::error::CompilationError as CE;
use crate::parser::{BracketType, PositionInFile};

use super::statement::*;
use crate::parser::parse2_brackets::token2::*;

pub fn parse_statements<'x>(tokens: &[Token2WithPos<'x>]) -> Result<Vec<Statement<'x>>, CE> {
    let mut statements = Vec::new();

    let mut state = ParsingState::new(tokens);
    while !state.at_end() {
        statements.push(state.parse_statement()?);
    }

    Ok(statements)
}

pub struct ParsingState<'x, 'a> {
    tokens: &'a [Token2WithPos<'x>],
    index: usize,
    size: usize,
}

impl<'x, 'a> ParsingState<'x, 'a> {
    pub fn new(tokens: &'a [Token2WithPos<'x>]) -> Self {
        let size = tokens.len();
        ParsingState { tokens, index: 0, size }
    }
    pub fn at_end(&self) -> bool {
        self.index >= self.size
    }
    pub fn parse_statement(&mut self) -> Result<Statement<'x>, CE> {
        if self.at_end() {
            panic!("tokens should not be empty");
        }
        let token = &self.tokens[self.index];
        self.index += 1;
        match &token.token {
            Token2::NumberLiteral(_) | Token2::TwoSidedOperation(_) | Token2::EqualOperation(_) | Token2::Comma | Token2::Colon | Token2::DoubleColon => {
                Err(CE::SyntacticsError(token.position, String::from("expected statement")))
            }
            Token2::String(chars) => {
                let string = chars.iter().collect::<String>();
                match string.as_str() {
                    "if" | "while" => {
                        let condition = self.parse_expression(token.position)?;
                        if self.at_end() {
                            return Err(CE::SyntacticsError(self.tokens.last().unwrap().position, format!("expected {string} body after that")));
                        }
                        let next_token = &self.tokens[self.index];
                        self.index += 1;
                        let Token2::Bracket(vec, BracketType::Curly) = &next_token.token else {
                            return Err(CE::SyntacticsError(token.position, format!("expected {string} body")))
                        };
                        let body = parse_statements(vec)?;

                        if string.as_str() == "if" {
                            Ok(Statement::new_if(condition, body))
                        } else {
                            Ok(Statement::new_while(condition, body))
                        }
                    }
                    _ => {
                        self.parse_statement2(chars, token.position)
                    }
                }
            }
            Token2::Bracket(_, _) => {
                Err(CE::SyntacticsError(token.position, format!("unexpected bracket open at {}, expected statement", token.position)))
            }
        }
    }
    fn parse_statement2(&mut self, string: &'x [char], previous_place_info: PositionInFile) -> Result<Statement<'x>, CE> {
        if self.at_end() {
            return Err(CE::SyntacticsError(previous_place_info, String::from("expected statement")));
        }
        let new_token = &self.tokens[self.index];
        self.index += 1;
        match &new_token.token {
            Token2::DoubleColon => {
                self.parse_function(string, new_token.position)
            },
            Token2::EqualOperation(equal_operation) => {
                let expression2 = self.parse_expression(new_token.position)?;
                let statement = match equal_operation {
                    EqualOperation::ColonEqual => Statement::new_variable(string, expression2),
                    EqualOperation::Equal => Statement::new_set(string, expression2),
                };
                Ok(statement)
            }
            Token2::Bracket(vec, BracketType::Round) => {
                let name = string;
                let args = parse_function_arguments(vec, new_token.position)?;
                let statement = Statement::Expression(Expression::FunctionCall { name, args });
                Ok(statement)
            }
            _ => {
                let string = string.iter().collect::<String>();
                Err(CE::SyntacticsError(new_token.position, format!("expected statement, got '{string}' '{:?}'", new_token.token)))
            }
        }
    }

    fn parse_expression(&mut self, previous_place_info: PositionInFile) -> Result<Expression<'x>, CE> {
        if self.at_end() {
            return Err(CE::SyntacticsError(previous_place_info, String::from("expected expression after that")));
        }
        let token = &self.tokens[self.index];
        self.index += 1;
        match &token.token {
            Token2::TwoSidedOperation(_) | Token2::EqualOperation(_) | Token2::Comma | Token2::Colon | Token2::DoubleColon => {
                Err(CE::SyntacticsError(token.position, String::from("expected expression")))
            }
            Token2::String(string) => {
                let expression1 = Expression::Variable(string);
                self.parse_expression2(expression1)
            }
            Token2::NumberLiteral(value) => {
                let expression1 = Expression::NumberLiteral(value);
                self.parse_expression2(expression1)
            }
            Token2::Bracket(vec, BracketType::Round) => {
                let mut new_state = ParsingState::new(vec);
                let expression = new_state.parse_expression(token.position)?;
                if new_state.at_end() {
                    let expression1 = Expression::round_bracket(expression);
                    self.parse_expression2(expression1)
                } else {
                    Err(CE::SyntacticsError(vec[new_state.index].position, String::from("expected ')'")))
                }
            }
            Token2::Bracket(_, _) => {
                Err(CE::SyntacticsError(token.position, String::from("expected expression, got open bracket")))
            }
        }
    }
    // parse "expression1 twoSidedOp expression2"
    fn parse_expression2(&mut self, expression1: Expression<'x>) -> Result<Expression<'x>, CE> {
        if self.at_end() {
            return Ok(expression1);
        }
        let token = &self.tokens[self.index];
        match &token.token {
            Token2::String(_) | Token2::NumberLiteral(_) | Token2::EqualOperation(_) | Token2::Comma | Token2::Colon | Token2::DoubleColon => {
                Ok(expression1)
            }
            Token2::TwoSidedOperation(op) => {
                self.index += 1;
                match op {
                    TwoSidedOperation::Plus => {
                        let expression2 = self.parse_expression(token.position)?;
                        Ok(Expression::plus(expression1, expression2))
                    }
                }
            }
            Token2::Bracket(vec, BracketType::Round) => {
                self.index += 1;
                let Expression::Variable(name) = expression1 else {
                    return Err(CE::SyntacticsError(token.position, String::from("unexpected round brackets after expression")));
                };
                let args = parse_function_arguments(vec, token.position)?;
                Ok(Expression::FunctionCall { name, args })
            }
            Token2::Bracket(_, _) => {
                Ok(expression1)
            }
        }
    }

    fn parse_function(&mut self, name: &'x [char], previous_place_info: PositionInFile) -> Result<Statement<'x>, CE> {
        if self.at_end() {
            return Err(CE::SyntacticsError(previous_place_info, String::from("expected function declaration")));
        }
        let token = &self.tokens[self.index];
        self.index += 1;
        let Token2::Bracket(args, BracketType::Round) = &token.token else {
            return Err(CE::SyntacticsError(token.position, String::from("expected function declaration")));
        };

        // parse arguments
        let arguments: Vec<&[char]> = if args.is_empty() {
            Vec::new()
        } else {
            let mut arguments = Vec::with_capacity(args.len().div_ceil(2));

            let Token2::String(arg1) = &args[0].token else {
                return Err(CE::SyntacticsError(args[0].position, String::from("expected argument name")));
            };
            arguments.push(*arg1);

            let mut index = 1;
            while index < args.len() {
                if args[index].token != Token2::Comma {
                    return Err(CE::SyntacticsError(args[index].position, String::from("expected ',' or ')'")));
                }
                index += 1;
                if index == args.len() {
                    // TODO: break
                    return Err(CE::SyntacticsError(args[index - 1].position, String::from("expected argument after comma")));
                }
                let Token2::String(arg_i) = &args[index].token else {
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
        let Token2::Bracket(body, BracketType::Curly) = &new_token.token else {
            return Err(CE::SyntacticsError(new_token.position, String::from("expected curly brackets after function declaration")));
        };
        let body = parse_statements(body)?;

        let statement = Statement::new_function(name, arguments, body);
        Ok(statement)
    }
}

fn parse_function_arguments<'x>(tokens: &[Token2WithPos<'x>], previous_place_info: PositionInFile) -> Result<Vec<Expression<'x>>, CE> {
    let mut args = Vec::new();

    let mut state = ParsingState::new(tokens);
    while !state.at_end() {
        let expression = state.parse_expression(previous_place_info)?;
        args.push(expression);

        if !state.at_end() {
            let token = &state.tokens[state.index];
            if token.token == Token2::Comma {
                state.index += 1;
            } else {
                return Err(CE::SyntacticsError(token.position, format!("expected ',' or ')', got {:?}", token.token)));
            }
        }
    }

    Ok(args)
}
