use std::collections::VecDeque;
use crate::error::CompilationError as CE;
use crate::parser::{BracketType, PositionInFile};

use crate::parser::operations::{NumberOperation, OneSidedOperation, TwoSidedOperation};
use crate::parser::parse1_tokenize::token::*;
use super::statement::*;

pub fn parse_statements(tokens: Vec<TokenWithPos>) -> Result<Vec<Statement>, CE> {
    ParsingState::new(tokens).parse_statements()
}

struct ParsingState {
    tokens: VecDeque<TokenWithPos>,
}

impl ParsingState {
    pub fn new(tokens: Vec<TokenWithPos>) -> Self {
        ParsingState { tokens: tokens.into() }
    }
    fn next(&mut self) -> Option<TokenWithPos> {
        self.tokens.pop_front()
    }
    fn peek(&self) -> Option<&TokenWithPos> {
        self.tokens.front()
    }
    fn at_end(&mut self) -> bool {
        self.tokens.is_empty()
    }
    pub fn parse_statements(&mut self) -> Result<Vec<Statement>, CE> {
        let mut statements = Vec::new();

        // skip all ';'
        while self.peek().map_or_else(|| false, |t| t.token == Token::Semicolon) {
            self.next();
        }

        while !self.at_end() {
            statements.push(self.parse_statement()?);

            // skip all ';'
            while self.peek().map_or_else(|| false, |t| t.token == Token::Semicolon) {
                self.next();
            }
        }

        Ok(statements)
    }
    fn parse_statement(&mut self) -> Result<Statement, CE> {
        let Some(TokenWithPos { token, position }) = self.next() else { unreachable!() };
        match token {
            Token::String(_) => {
                let Token::String(string) = token else { unreachable!() };

                match string.as_str() {
                    "if" | "while" => {
                        let condition = self.parse_expression(position)?;

                        let Some(TokenWithPos { token, position }) = self.next() else {
                            return Err(CE::SyntacticsError(position, format!("expected {string} body after that")));
                        };

                        let Token::Bracket(vec, BracketType::Curly) = token else {
                            return Err(CE::SyntacticsError(position, format!("expected {string} body")))
                        };
                        let body = ParsingState::new(vec).parse_statements()?;

                        if string.as_str() == "if" {
                            Ok(Statement::new_if(condition, body))
                        } else {
                            Ok(Statement::new_while(condition, body))
                        }
                    }
                    "return" => {
                        let peek_token = self.peek();
                        if peek_token.is_none() {
                            Ok(Statement::Return(None))
                        } else if let Some(token_with_position) = peek_token && Token::Semicolon == token_with_position.token {
                            Ok(Statement::Return(None))
                        } else {
                            let expression = self.parse_expression(position)?;
                            Ok(Statement::Return(Some(expression)))
                        }
                    }
                    _ => {
                        self.parse_statement2(string, position)
                    }
                }
            }
            Token::Operation(TwoSidedOperation::Number(NumberOperation::Mul)) => { // *..
                let left_expression = self.parse_expression_without_ops(position, true)?;
                let Some(TokenWithPos { token, position }) = self.next() else {
                    return Err(CE::SyntacticsError(position, "unused dereference".to_owned()));
                };
                let Token::EqualOperation(equal_op) = token else {
                    return Err(CE::SyntacticsError(position, "expected =/_= after *(..)".to_owned()));
                };
                let value = self.parse_expression(position)?;
                match equal_op {
                    EqualOperation::Equal => {
                        Ok(Statement::new_set_deref(left_expression, value))
                    },
                    EqualOperation::OperationEqual(op) => {
                        Ok(Statement::new_equal_set_deref(left_expression, value, op))
                    },
                    EqualOperation::ColonEqual => {
                        Err(CE::SyntacticsError(position, "'*name := ..' is ".to_owned()))
                    },
                }
            }
            Token::Semicolon => unreachable!(),
            _ => {
                Err(CE::SyntacticsError(position, format!("unexpected token {:?}", token)))
                // // next token exists, position is used only if next is None
                // let position = PositionInFile::new(0, 0);
                // Ok(Statement::Expression(self.parse_expression(position, false)?))
            }
        }
    }
    /// parse "name .."
    fn parse_statement2(&mut self, string: String, position: PositionInFile) -> Result<Statement, CE> {
        let Some(TokenWithPos { token, position }) = self.next() else {
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

                let Some(TokenWithPos { token, position }) = self.next() else {
                    return Err(CE::SyntacticsError(position, format!("expected '=' after '{string}' : {typee}")))
                };
                if token != Token::EqualOperation(EqualOperation::Equal) {
                    return Err(CE::SyntacticsError(position, format!("expected '=' after '{string}' : {typee}")))
                }

                let expression = self.parse_expression(position)?;
                let statement = Statement::new_variable(string, Some(typee), expression);
                Ok(statement)
            }
            Token::EqualOperation(equal_operation) => {
                // name _=
                let expression2 = self.parse_expression(position)?;
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
                let expression = self.parse_expression2_without_ops(Expression::new_function_call(name, args), false)?;
                Ok(Statement::Expression(expression))
            }
            _ => {
                Err(CE::SyntacticsError(position, format!("expected statement, got '{string}' '{token:?}'")))
            }
        }
    }

    fn parse_expression(&mut self, position: PositionInFile) -> Result<Expression, CE> {
        let mut expressions = Vec::<Expression>::new();
        let mut operations = Vec::<TwoSidedOperation>::new();
        let mut position = position;

        loop {
            let next_expression = self.parse_expression_without_ops(position, false)?;
            expressions.push(next_expression);

            let Some(TokenWithPos { token, position: _ }) = self.peek() else {
                break;
            };
            if matches!(token, Token::Semicolon | Token::Comma | Token::Bracket(_, _) | Token::EqualOperation(_)) {
                break
            }

            let Some(TokenWithPos { token, position: position2 }) = self.next() else { unreachable!() };
            let Token::Operation(op) = token else {
                return Err(CE::SyntacticsError(position2, "expected expression".to_owned()))
            };
            position = position2;
            operations.push(op)
        }

        // FIXME: speed up to O(n log n)
        fn create_expression(mut exps: Vec<Expression>, mut ops: Vec<TwoSidedOperation>) -> Expression {
            if ops.is_empty() {
                return exps.pop().unwrap()
            }
            let split_index = (0..ops.len()).min_by_key(|x| ops[*x].get_prior()).unwrap();

            let exps_right = exps.split_off(split_index + 1);
            let ops_right = ops.split_off(split_index + 1);
            let op_middle = ops.pop().unwrap();

            let left_exp = create_expression(exps, ops);
            let right_exp = create_expression(exps_right, ops_right);
            Expression::new_operation(left_exp, right_exp, op_middle)
        }

        let result = create_expression(expressions, operations);

        Ok(result)
    }
    fn parse_expression_without_ops(&mut self, position: PositionInFile, was_unary: bool) -> Result<Expression, CE> {
        let Some(TokenWithPos { token, position }) = self.next() else {
            return Err(CE::SyntacticsError(position, "expected expression after that".to_owned()));
        };
        match token {
            Token::String(string) => { // string
                let expression1 = match string.as_str() {
                    "true" => Expression::BoolLiteral(true),
                    "false" => Expression::BoolLiteral(false),
                    _ => Expression::Variable(string),
                };
                self.parse_expression2_without_ops(expression1, was_unary)
            }
            Token::NumberLiteral(value) => { // 123
                let expression1 = Expression::NumberLiteral(value);
                self.parse_expression2_without_ops(expression1, was_unary)
            }
            Token::Bracket(vec, BracketType::Round) => { // (..)
                let mut new_state = ParsingState::new(vec);
                let expression = new_state.parse_expression(position)?;
                if !new_state.at_end() {
                    return Err(CE::SyntacticsError(new_state.next().unwrap().position, "expected ')'".to_owned()))
                }
                let expression1 = Expression::new_round_bracket(expression);
                self.parse_expression2_without_ops(expression1, was_unary)
            }
            Token::Operation(TwoSidedOperation::Number(NumberOperation::Sub)) => { // -.. 
                let op = OneSidedOperation::UnaryMinus;
                let expression = self.parse_expression_without_ops(position, true)?;
                
                let unary_expression = Expression::new_unary_operation(expression, op);
                Ok(self.parse_expression2_without_ops(unary_expression, false)?)
            }
            Token::Operation(TwoSidedOperation::Number(NumberOperation::Mul)) => { // *..
                let op = OneSidedOperation::Dereference;
                let expression = self.parse_expression_without_ops(position, true)?;

                let unary_expression = Expression::new_unary_operation(expression, op);
                Ok(self.parse_expression2_without_ops(unary_expression, false)?)
            }
            Token::Operation(TwoSidedOperation::Number(NumberOperation::BitAnd)) => { // &..
                let op = OneSidedOperation::GetReference;
                let expression = self.parse_expression_without_ops(position, true)?;

                let unary_expression = Expression::new_unary_operation(expression, op);
                Ok(self.parse_expression2_without_ops(unary_expression, false)?)
            }
            Token::UnaryOperation(op) => { // `unary`..
                let expression = self.parse_expression_without_ops(position, true)?;
                let unary_expression = Expression::new_unary_operation(expression, op);
                Ok(self.parse_expression2_without_ops(unary_expression, false)?)
            }
            Token::Quotes(string) => { // '..'
                let char_value = parse_quotes(string)?;
                let expression = Expression::CharLiteral(char_value);
                self.parse_expression2_without_ops(expression, was_unary)
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
    fn parse_expression2_without_ops(&mut self, expression1: Expression, was_unary: bool) -> Result<Expression, CE> {
        let Some(TokenWithPos { token, position }) = self.peek() else {
            return Ok(expression1);
        };
        match token {
            Token::Operation(_) => { // exp +
                Ok(expression1)
            }
            Token::Bracket(_, BracketType::Round) => { // exp(..)
                let Some(TokenWithPos { token, position }) = self.next() else { unreachable!() };
                let Token::Bracket(vec, BracketType::Round) = token else { unreachable!() };

                // FIXME: allow function variables
                let Expression::Variable(name) = expression1 else {
                    return Err(CE::SyntacticsError(position, "unexpected round brackets after expression".to_owned()));
                };
                let args = parse_function_arguments(vec, position)?;
                self.parse_expression2_without_ops(Expression::new_function_call(name, args), was_unary)
            }
            Token::String(string) if string == "as" => { // exp as
                if was_unary {
                    return Ok(expression1)
                }

                let position = self.next().unwrap().position;

                let typee = self.parse_type(position)?;
                let expression = Expression::new_as(expression1, typee);
                self.parse_expression2_without_ops(expression, false)
            }
            Token::Semicolon | Token::Comma | Token::Bracket(_, _) | Token::EqualOperation(_) => {
                Ok(expression1)
            }
            _ => {
                Err(CE::SyntacticsError(*position, format!("unexpected token {token:?}")))
            }
        }
    }

    fn parse_function(&mut self, name: String, position: PositionInFile) -> Result<Statement, CE> {
        let Some(TokenWithPos { token, position }) = self.next() else {
            return Err(CE::SyntacticsError(position, "expected function declaration".to_owned()));
        };

        let Token::Bracket(args, BracketType::Round) = token else {
            return Err(CE::SyntacticsError(position, "expected function declaration".to_owned()));
        };

        // parse arguments
        let arguments = self.parse_function_declaration_arguments(args)?;

        // parse return type
        let Some(token_with_pos) = self.next() else {
            return Err(CE::SyntacticsError(position, "expected function body or return type annotation".to_owned()));
        };
        let mut token_with_pos = token_with_pos;
        let return_type = {
            if token_with_pos.token == Token::Arrow {
                let return_type = self.parse_type(token_with_pos.position)?;
                let Some(new_token_with_pos) = self.next() else {
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
        let body = ParsingState::new(body).parse_statements()?;

        let statement = Statement::new_function(name, arguments, return_type, body);
        Ok(statement)
    }
    fn parse_function_declaration_arguments(&self, args: Vec<TokenWithPos>) -> Result<Vec<(String, Typee)>, CE> {
        if args.is_empty() {
            return Ok(Vec::new())
        }
        let mut arguments = Vec::with_capacity(args.len().div_ceil(2));
        let mut state = Self::new(args);

        while let Some(TokenWithPos { token, position }) = state.next() {
            let Token::String(arg_i) = token else {
                return Err(CE::SyntacticsError(position, "expected argument name in function declaration".to_owned()));
            };

            let Some(TokenWithPos { token, position }) = state.next() else {
                return Err(CE::SyntacticsError(position, "expected argument type after name".to_owned()))
            };
            if token != Token::Colon {
                return Err(CE::SyntacticsError(position, "expected argument type after name".to_owned()))
            }

            let argument_type = state.parse_type(position)?;

            arguments.push((arg_i, argument_type));

            let Some(TokenWithPos { token, position }) = state.next() else {
                break;
            };
            if token != Token::Comma {
                return Err(CE::SyntacticsError(position, "expected ',' or ')'".to_owned()));
            }
        }

        Ok(arguments)
    }

    fn parse_type(&mut self, position: PositionInFile) -> Result<Typee, CE> {
        let Some(TokenWithPos { token, position }) = self.next() else {
            return Err(CE::SyntacticsError(position, "expected type after that".to_owned()));
        };

        match token {
            Token::String(string) => Ok(Typee::String(string)),
            Token::Operation(TwoSidedOperation::Number(NumberOperation::Mul)) => {
                let typee = self.parse_type(position)?;
                Ok(Typee::Reference(Box::new(typee)))
            }
            _ => Err(CE::SyntacticsError(position, "expected type".to_owned()))
        }
    }
}

fn parse_function_arguments(tokens: Vec<TokenWithPos>, position: PositionInFile) -> Result<Vec<Expression>, CE> {
    let mut state = ParsingState::new(tokens);
    let mut args = Vec::new();

    while !state.at_end() {
        let expression = state.parse_expression(position)?;
        args.push(expression);

        if !state.at_end() {
            let TokenWithPos { token, position } = state.next().unwrap();
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
