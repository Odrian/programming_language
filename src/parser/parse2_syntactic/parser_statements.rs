use crate::error::CompilationError as CE;
use crate::parser::{BracketType, PositionInFile};

use crate::parser::operations::{NumberOperation, OneSidedOperation, TwoSidedOperation};
use crate::parser::parse1_tokenize::token::*;
use super::statement::*;

use std::iter::Peekable;
use std::vec::IntoIter;

pub fn parse_statements(tokens: Vec<TokenWithPos>) -> Result<Vec<Statement>, CE> {
    ParsingState::new(tokens).parse_statements()
}

struct ParsingState {
    tokens: Peekable<IntoIter<TokenWithPos>>,
}

impl ParsingState {
    pub fn new(tokens: Vec<TokenWithPos>) -> Self {
        ParsingState { tokens: tokens.into_iter().peekable() }
    }
    fn at_end(&mut self) -> bool {
        self.tokens.peek().is_none()
    }
    pub fn parse_statements(&mut self) -> Result<Vec<Statement>, CE> {
        let mut statements = Vec::new();

        // skip all ';'
        while self.tokens.peek().map_or_else(|| false, |t| t.token == Token::Semicolon) {
            self.tokens.next();
        }

        while !self.at_end() {
            statements.push(self.parse_statement()?);

            // skip all ';'
            while self.tokens.peek().map_or_else(|| false, |t| t.token == Token::Semicolon) {
                self.tokens.next();
            }
        }

        Ok(statements)
    }
    fn parse_statement(&mut self) -> Result<Statement, CE> {
        let Some(TokenWithPos { token, position: _ }) = self.tokens.peek() else { unreachable!() };
        match token {
            Token::String(_) => {
                let Some(TokenWithPos { token, position }) = self.tokens.next() else { unreachable!() };
                let Token::String(string) = token else { unreachable!() };

                match string.as_str() {
                    "if" | "while" => {
                        let condition = self.parse_expression(position, false)?;

                        let Some(TokenWithPos { token, position }) = self.tokens.next() else {
                            return Err(CE::SyntacticsError(position, format!("expected {string} body after that")));
                        };

                        let Token::Bracket(vec, BracketType::Curly) = token else {
                            return Err(CE::SyntacticsError(position, format!("expected {string} body")))
                        };
                        let body = parse_statements(vec)?;

                        if string.as_str() == "if" {
                            Ok(Statement::new_if(condition, body))
                        } else {
                            Ok(Statement::new_while(condition, body))
                        }
                    }
                    "return" => {
                        let peek_token = self.tokens.peek();
                        if peek_token.is_none() {
                            Ok(Statement::Return(None))
                        } else if let Some(token_with_position) = peek_token && Token::Semicolon == token_with_position.token {
                            Ok(Statement::Return(None))
                        } else {
                            let expression = self.parse_expression(position, false)?;
                            Ok(Statement::Return(Some(expression)))
                        }
                    }
                    _ => {
                        self.parse_statement2(string, position)
                    }
                }
            }
            Token::Semicolon => unreachable!(),
            _ => {
                // next token exists, position is used only if next is None
                let position = PositionInFile::new(0, 0);
                Ok(Statement::Expression(self.parse_expression(position, false)?))
            }
        }
    }
    /// parse "name .."
    fn parse_statement2(&mut self, string: String, position: PositionInFile) -> Result<Statement, CE> {
        let Some(TokenWithPos { token, position }) = self.tokens.next() else {
            return Err(CE::SyntacticsError(position, "expected statement, got EOL".to_owned()));
        };
        match token {
            Token::DoubleColon => {
                // name ::
                self.parse_function(string, position)
            },
            Token::Colon => {
                // name :
                let typee = self.parse_type(position)?;

                let Some(TokenWithPos { token, position }) = self.tokens.next() else {
                    return Err(CE::SyntacticsError(position, format!("expected '=' after '{string}' : {typee}")))
                };
                if token != Token::EqualOperation(EqualOperation::Equal) {
                    return Err(CE::SyntacticsError(position, format!("expected '=' after '{string}' : {typee}")))
                }

                let expression = self.parse_expression(position, false)?;
                let statement = Statement::new_variable(string, Some(typee), expression);
                Ok(statement)
            }
            Token::EqualOperation(equal_operation) => {
                // name _=
                let expression2 = self.parse_expression(position, false)?;
                let statement = match equal_operation {
                    EqualOperation::ColonEqual => Statement::new_variable(string, None, expression2),
                    EqualOperation::Equal => Statement::new_set(string, expression2),
                    EqualOperation::OperationEqual(op) => {
                        Statement::new_equal_set(string, expression2, op)
                    }
                };
                Ok(statement)
            }
            Token::Bracket(vec, BracketType::Round) => {
                // name(..)
                let name = string;
                let args = parse_function_arguments(vec, position)?;
                let expression = self.parse_expression2(Expression::new_function_call(name, args), false)?;
                Ok(Statement::Expression(expression))
            }
            _ => {
                Err(CE::SyntacticsError(position, format!("expected statement, got '{string}' '{token:?}'")))
            }
        }
    }

    fn parse_expression(&mut self, position: PositionInFile, was_unary: bool) -> Result<Expression, CE> {
        let Some(TokenWithPos { token, position }) = self.tokens.next() else {
            return Err(CE::SyntacticsError(position, "expected expression after that".to_owned()));
        };
        match token {
            Token::String(string) => { // string
                let expression1 = match string.as_str() {
                    "true" => Expression::BoolLiteral(true),
                    "false" => Expression::BoolLiteral(false),
                    _ => Expression::Variable(string),
                };
                self.parse_expression2(expression1, was_unary)
            }
            Token::NumberLiteral(value) => { // 123
                let expression1 = Expression::NumberLiteral(value);
                self.parse_expression2(expression1, was_unary)
            }
            Token::Bracket(vec, BracketType::Round) => { // (..)
                let mut new_state = ParsingState::new(vec);
                let expression = new_state.parse_expression(position, false)?;
                if !new_state.at_end() {
                    return Err(CE::SyntacticsError(new_state.tokens.next().unwrap().position, "expected ')'".to_owned()))
                }
                let expression1 = Expression::new_round_bracket(expression);
                self.parse_expression2(expression1, was_unary)
            }
            Token::Operation(TwoSidedOperation::Number(NumberOperation::Sub)) => { // -.. 
                let op = OneSidedOperation::UnaryMinus;
                let expression = self.parse_expression(position, true)?;
                
                // if let Expression::NumberLiteral(string) = &expression {
                //     if string.chars().next() == Some('-') {
                //         return Err(CE::LiteralParseError { what: string[1..].to_owned(), error: "double unary minus".to_owned() })
                //     }
                //     let expression = Expression::NumberLiteral("-".to_owned() + &string);
                //     Ok(expression)
                // } else {
                    let unary_expression = Expression::new_unary_operation(expression, op);
                    Ok(unary_expression)
                // }
            }
            Token::UnaryOperation(op) => { // `unary`..
                let expression = self.parse_expression(position, true)?;
                let unary_expression = Expression::new_unary_operation(expression, op);
                Ok(unary_expression)
            }
            Token::Quotes(string) => { // '..'
                let char_value = parse_quotes(string)?;
                let expression = Expression::CharLiteral(char_value);
                self.parse_expression2(expression, was_unary)
            }
            Token::DoubleQuotes(_string) => { // ".."
                unimplemented!("string literal")
            }
            _ => {
                Err(CE::SyntacticsError(position, "expected expression".to_owned()))
            }
        }
    }
    // parse "expression .."
    fn parse_expression2(&mut self, expression1: Expression, was_unary: bool) -> Result<Expression, CE> {
        let Some(TokenWithPos { token, position }) = self.tokens.peek() else {
            return Ok(expression1);
        };
        match token {
            Token::Operation(_) => { // exp +
                if was_unary {
                    return Ok(expression1)
                }
                let Some(TokenWithPos { token: Token::Operation(op), position }) = self.tokens.next() else { unreachable!() };

                let expression2 = self.parse_expression(position, false)?;
                Ok(Expression::new_operation(expression1, expression2, op))
            }
            Token::Bracket(_, BracketType::Round) => { // exp(..)
                let Some(TokenWithPos { token, position }) = self.tokens.next() else { unreachable!() };
                let Token::Bracket(vec, BracketType::Round) = token else { unreachable!() };

                let Expression::Variable(name) = expression1 else {
                    return Err(CE::SyntacticsError(position, "unexpected round brackets after expression".to_owned()));
                };
                let args = parse_function_arguments(vec, position)?;
                self.parse_expression2(Expression::new_function_call(name, args), was_unary)
            }
            Token::String(string) if string == "as" => { // exp as
                if was_unary {
                    return Ok(expression1)
                }

                let position = self.tokens.next().unwrap().position;

                let typee = self.parse_type(position)?;
                let expression = Expression::new_as(expression1, typee);
                self.parse_expression2(expression, false)
            }
            Token::Semicolon | Token::Comma | Token::Bracket(_, _) => {
                Ok(expression1)
            }
            _ => {
                Err(CE::SyntacticsError(*position, format!("unexpected token {token:?}")))
            }
        }
    }

    fn parse_function(&mut self, name: String, position: PositionInFile) -> Result<Statement, CE> {
        let Some(TokenWithPos { token, position }) = self.tokens.next() else {
            return Err(CE::SyntacticsError(position, "expected function declaration".to_owned()));
        };

        let Token::Bracket(args, BracketType::Round) = token else {
            return Err(CE::SyntacticsError(position, "expected function declaration".to_owned()));
        };

        // parse arguments
        let arguments = self.parse_function_declaration_arguments(args)?;

        // parse return type
        let Some(token_with_pos) = self.tokens.next() else {
            return Err(CE::SyntacticsError(position, "expected function body or return type annotation".to_owned()));
        };
        let mut token_with_pos = token_with_pos;
        let return_type = {
            if token_with_pos.token == Token::Arrow {
                let return_type = self.parse_type(token_with_pos.position)?;
                let Some(new_token_with_pos) = self.tokens.next() else {
                    return Err(CE::SyntacticsError(token_with_pos.position, "expected function body".to_owned()));
                };
                token_with_pos = new_token_with_pos;
                Some(return_type)
            } else {
                None
            }
        };

        let TokenWithPos { token, position } = token_with_pos;
        // parse inside
        let Token::Bracket(body, BracketType::Curly) = token else {
            return Err(CE::SyntacticsError(position, "expected curly brackets after function declaration".to_owned()));
        };
        let body = parse_statements(body)?;

        let statement = Statement::new_function(name, arguments, return_type, body);
        Ok(statement)
    }
    fn parse_function_declaration_arguments(&self, args: Vec<TokenWithPos>) -> Result<Vec<(String, Typee)>, CE> {
        if args.is_empty() {
            return Ok(Vec::new())
        }
        let mut arguments = Vec::with_capacity(args.len().div_ceil(2));
        let mut state = Self::new(args);

        while let Some(TokenWithPos { token, position }) = state.tokens.next() {
            let Token::String(arg_i) = token else {
                return Err(CE::SyntacticsError(position, "expected argument name in function declaration".to_owned()));
            };

            let Some(TokenWithPos { token, position }) = state.tokens.next() else {
                return Err(CE::SyntacticsError(position, "expected argument type after name".to_owned()))
            };
            if token != Token::Colon {
                return Err(CE::SyntacticsError(position, "expected argument type after name".to_owned()))
            }

            let argument_type = state.parse_type(position)?;

            arguments.push((arg_i, argument_type));

            let Some(TokenWithPos { token, position }) = state.tokens.next() else {
                break;
            };
            if token != Token::Comma {
                return Err(CE::SyntacticsError(position, "expected ',' or ')'".to_owned()));
            }
        }

        Ok(arguments)
    }

    fn parse_type(&mut self, position: PositionInFile) -> Result<Typee, CE> {
        let Some(TokenWithPos { token, position }) = self.tokens.next() else {
            return Err(CE::SyntacticsError(position, "expected type after that".to_owned()));
        };

        match token {
            Token::String(string) => Ok(Typee::String(string)),
            _ => Err(CE::SyntacticsError(position, "expected type".to_owned()))
        }
    }
}

fn parse_function_arguments(tokens: Vec<TokenWithPos>, position: PositionInFile) -> Result<Vec<Expression>, CE> {
    let mut state = ParsingState::new(tokens);
    let mut args = Vec::new();

    while !state.at_end() {
        let expression = state.parse_expression(position, false)?;
        args.push(expression);

        if !state.at_end() {
            let TokenWithPos { token, position } = state.tokens.next().unwrap();
            if token != Token::Comma {
                return Err(CE::SyntacticsError(position, format!("expected ',' or ')', got {token:?}")));
            }
        }
    }

    Ok(args)
}

fn parse_quotes(string: String) -> Result<u8, CE> {
    let mut chars = string.chars();
    let Some(first_char) = chars.next() else {
        return Err(CE::LiteralParseError { what: format!("'{string}'"), error: "incorrect char literal".to_owned() })
    };

    if chars.next().is_some() {
        return Err(CE::LiteralParseError { what: format!("'{string}'"), error: "incorrect char literal".to_owned() })
    }
    if !first_char.is_ascii() {
        return Err(CE::LiteralParseError { what: format!("'{string}'"), error: "incorrect char literal".to_owned() })
    }
    let char_value = first_char as u8;
    Ok(char_value)
}
