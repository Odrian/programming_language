use crate::error::CompilationError as CE;
use crate::parser::{BracketType, PositionInFile};
use crate::parser::operations::{NumberOperation, OneSidedOperation, TwoSidedOperation};
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
                        // FIXME: require ';' at end of statement?
                        let save_index = self.index;
                        let result_expression = self.parse_expression(token.position);
                        match result_expression {
                            Ok(expression) => Ok(Statement::Return(Some(expression))),
                            Err(_) => {
                                self.index = save_index;
                                Ok(Statement::Return(None))
                            },
                        }
                    }
                    _ => {
                        self.parse_statement2(chars, token.position)
                    }
                }
            }
            Token::Bracket(_, _) => {
                Err(CE::SyntacticsError(token.position, format!("unexpected bracket open at {}, expected statement", token.position)))
            }
            _ => {
                Err(CE::SyntacticsError(token.position, String::from("expected statement")))
            }
        }
    }
    fn parse_statement2(&mut self, string: &'text [char], previous_place_info: PositionInFile) -> Result<Statement<'text>, CE> {
        if self.at_end() {
            return Err(CE::SyntacticsError(previous_place_info, String::from("expected statement, got EOL")));
        }
        let new_token = &self.tokens[self.index];
        self.index += 1;
        match &new_token.token {
            Token::DoubleColon => {
                // name ::
                self.parse_function(string, new_token.position)
            },
            Token::Colon => {
                // name :
                let (typee, index) = parse_type(self.tokens, self.index, self.tokens[self.index-1].position)?;
                self.index = index;

                if self.at_end() || self.tokens[self.index].token != Token::EqualOperation(EqualOperation::Equal) {
                    return Err(CE::SyntacticsError(self.tokens[self.index-1].position, format!("expected '=' after '{}' : {typee}", string.iter().collect::<String>())))
                }
                self.index += 1;

                let expression = self.parse_expression(self.tokens[self.index-1].position)?;
                let statement = Statement::new_variable(string, Some(typee), expression);
                Ok(statement)
            }
            Token::EqualOperation(equal_operation) => {
                // name _=
                let expression2 = self.parse_expression(new_token.position)?;
                let statement = match equal_operation {
                    EqualOperation::ColonEqual => Statement::new_variable(string, None, expression2),
                    EqualOperation::Equal => Statement::new_set(string, expression2),
                };
                Ok(statement)
            }
            Token::Bracket(vec, BracketType::Round) => {
                // name(..)
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
                if !new_state.at_end() {
                    return Err(CE::SyntacticsError(vec[new_state.index].position, String::from("expected ')'")))
                }
                let expression1 = Expression::new_round_bracket(expression);
                self.parse_expression2(expression1)
            }
            Token::Operation(TwoSidedOperation::Number(NumberOperation::Sub)) => {
                let op = OneSidedOperation::UnaryMinus;
                let expression = self.parse_expression(token.position)?;
                let unary_expression = Expression::new_unary_operation(expression, op);
                Ok(unary_expression)
            }
            Token::UnaryOperation(op) => {
                let expression = self.parse_expression(token.position)?;
                let unary_expression = Expression::new_unary_operation(expression, *op);
                Ok(unary_expression)
            }
            Token::Bracket(_, _) => {
                Err(CE::SyntacticsError(token.position, String::from("expected expression, got open bracket")))
            }
            _ => {
                Err(CE::SyntacticsError(token.position, String::from("expected expression")))
            }
        }
    }
    // parse "name .." or "(expr1) .."
    fn parse_expression2(&mut self, expression1: Expression<'text>) -> Result<Expression<'text>, CE> {
        if self.at_end() {
            return Ok(expression1);
        }
        let token = &self.tokens[self.index];
        match &token.token {
            Token::Operation(op) => {
                self.index += 1;
                let expression2 = self.parse_expression(token.position)?;
                Ok(Expression::new_operation(expression1, expression2, *op))
            }
            Token::Bracket(vec, BracketType::Round) => {
                self.index += 1;
                let Expression::Variable(name) = expression1 else {
                    return Err(CE::SyntacticsError(token.position, String::from("unexpected round brackets after expression")));
                };
                let args = parse_function_arguments(vec, token.position)?;
                self.parse_expression2(Expression::new_function_call(name, args))
            }
            _ => {
                // FIXME: bad for debugging
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
        let arguments = self.parse_function_declaration_arguments(args)?;

        // parse return type
        let return_type = {
            if !self.at_end() && self.tokens[self.index].token == Token::Arrow {
                self.index += 1;
                let (return_type, new_index) = parse_type(self.tokens, self.index, self.tokens[self.index-1].position)?;
                self.index = new_index;
                Some(return_type)
            } else {
                None
            }
        };

        // parse inside
        if self.at_end() {
            return Err(CE::SyntacticsError(self.tokens[self.index-1].position, String::from("expected curly brackets after function declaration")));
        }
        let new_token = &self.tokens[self.index];
        self.index += 1;
        let Token::Bracket(body, BracketType::Curly) = &new_token.token else {
            return Err(CE::SyntacticsError(new_token.position, String::from("expected curly brackets after function declaration")));
        };
        let body = parse_statements(body)?;

        let statement = Statement::new_function(name, arguments, return_type, body);
        Ok(statement)
    }
    fn parse_function_declaration_arguments(&self, args: &[TokenWithPos<'text>]) -> Result<Vec<(&'text [char], Typee<'text>)>, CE> {
        if args.is_empty() {
            return Ok(Vec::new())
        }
        let mut arguments = Vec::with_capacity(args.len().div_ceil(2));

        let mut index = 0;
        while index < args.len() {
            let Token::String(arg_i) = &args[index].token else {
                return Err(CE::SyntacticsError(args[index].position, String::from("expected argument name in function declaration")));
            };
            index += 1;

            if index == args.len() || args[index].token != Token::Colon {
                return Err(CE::SyntacticsError(args[index-1].position, String::from("expected argument type after name")))
            }
            index += 1;

            let (argument_type, new_index) = parse_type(args, index, args[index-1].position)?;
            index = new_index;

            arguments.push((*arg_i, argument_type));

            if index == args.len() {
                break;
            }
            if args[index].token != Token::Comma {
                return Err(CE::SyntacticsError(args[index].position, String::from("expected ',' or ')'")));
            }
            index += 1;
        }

        Ok(arguments)
    }
}

fn parse_type<'text>(tokens: &[TokenWithPos<'text>], start_index: usize, previous_place_info: PositionInFile) -> Result<(Typee<'text>, usize), CE> {
    if start_index >= tokens.len() {
        return Err(CE::SyntacticsError(previous_place_info, String::from("expected type after that")));
    }
    match tokens[start_index].token {
        Token::String(string) => Ok((Typee::String(string), start_index + 1)),
        _ => Err(CE::SyntacticsError(tokens[start_index].position, String::from("expected type")))
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
