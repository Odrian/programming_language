use std::collections::VecDeque;
use lsp_types::Range;
use crate::parser::BracketType;

use crate::parser::operations::{BoolOperation, CompareOperator, NumberOperation, OneSidedOperation, TwoSidedOperation};
use crate::parser::parse1_tokenize::token::*;
use super::statement::*;
use super::error::{ExpectedEnum, SyntacticError};

pub fn parse_statements(tokens: Vec<RangedToken>) -> Result<Vec<Statement>, SyntacticError> {
    ParsingState::new(tokens).parse_statements(true)
}

struct ParsingState {
    tokens: VecDeque<RangedToken>,
}

impl ParsingState {
    pub fn new(tokens: Vec<RangedToken>) -> Self {
        Self { tokens: tokens.into() }
    }
    fn next(&mut self) -> Option<RangedToken> {
        self.tokens.pop_front()
    }
    fn peek(&self) -> Option<&RangedToken> {
        self.tokens.front()
    }
    fn at_end(&self) -> bool {
        self.tokens.is_empty()
    }
    pub fn parse_statements(&mut self, is_global: bool) -> Result<Vec<Statement>, SyntacticError> {
        let mut statements = Vec::new();

        self.skip_semicolons();

        while !self.at_end() {
            self.parse_statement(is_global, &mut statements)?;

            self.skip_semicolons();
        }

        Ok(statements)
    }
    fn skip_semicolons(&mut self) {
        while let Some(RangedToken { token: Token::Semicolon, range: _ }) = self.peek() {
            self.next();
        }
    }
    fn parse_statement(&mut self, is_global: bool, result: &mut Vec<Statement>) -> Result<(), SyntacticError> {
        let Some(RangedToken { token, range }) = self.next() else { unreachable!() };
        match token {
            Token::Keyword(keyword) => match keyword {
                TokenKeyword::If | TokenKeyword::While => {
                    let condition = self.parse_expression(range, true)?;

                    let Some(RangedToken { token, range }) = self.next() else {
                        return Err(ExpectedEnum::CurlyBracket.err());
                    };

                    let Token::Bracket(vec, BracketType::Curly) = token else {
                        return Err(ExpectedEnum::CurlyBracket.err())
                    };
                    let body = ParsingState::new(vec).parse_statements(false)?;

                    if keyword == TokenKeyword::If {
                        result.push(Statement::new_if(condition, body))
                    } else {
                        result.push(Statement::new_while(condition, body))
                    }
                    Ok(())
                }
                TokenKeyword::For => {
                    let Some(RangedToken { token: Token::String(name), range }) = self.next() else {
                        return Err(ExpectedEnum::Name.err())
                    };
                    let Some(RangedToken { token: Token::String(in_str), range }) = self.next() else {
                        return Err(ExpectedEnum::new_string("'in'").err())
                    };
                    if in_str != "in" {
                        return Err(ExpectedEnum::new_string("'in'").err())
                    }

                    let from_value = self.parse_expression_without_ops(range, true, true)?;

                    let Some(RangedToken { token: Token::DoubleDot, range }) = self.next() else {
                        return Err(ExpectedEnum::new_string("..").err())
                    };
                    let compare_op = if let Some(RangedToken { token: Token::EqualOperation(EqualOperation::Equal), range }) = self.peek() {
                        self.next();
                        CompareOperator::LessEqual
                    } else {
                        CompareOperator::Less
                    };

                    let to_value = self.parse_expression_without_ops(range, true, true)?;

                    let Some(RangedToken { token: Token::Bracket(body, BracketType::Curly), range }) = self.next() else {
                        return Err(ExpectedEnum::CurlyBracket.err())
                    };
                    let mut state = Self::new(body);
                    let body = state.parse_statements(false)?;

                    result.push(Statement::new_for(
                        Statement::new_variable(name.clone(), None, from_value),
                        Expression::new_operation(
                            Expression::Variable(name.clone()),
                            to_value,
                            compare_op.into(),
                        ),
                        body,
                        Statement::new_set(
                            Expression::Variable(name),
                            LiteralExpression::NumberLiteral("1".to_string()).into(),
                            Some(NumberOperation::Add.into())
                        )
                    ));
                    
                    Ok(())
                }
                TokenKeyword::Return => {
                    let peek_token = self.peek();
                    if peek_token.is_none() {
                        result.push(Statement::Return(None))
                    } else if let Some(RangedToken { token: Token::Semicolon, range: _ }) = peek_token {
                        result.push(Statement::Return(None))
                    } else {
                        let expression = self.parse_expression(range, false)?;
                        result.push(Statement::Return(Some(expression)))
                    }
                    Ok(())
                }
                TokenKeyword::Import => {
                    if !is_global { return Err(SyntacticError::new_local_global("import")) }
                    result.push(self.parse_import(range)?);
                    Ok(())
                },
                TokenKeyword::Extern => {
                    if !is_global { return Err(SyntacticError::new_local_global("extern")) }
                    
                    if matches!(self.peek(), Some(RangedToken { token: Token::Bracket(_, BracketType::Curly), range: _ })) {
                        let Some(RangedToken { token: Token::Bracket(vec, _), range }) = self.next() else { unreachable!() };

                        let mut state = Self::new(vec);
                        while !state.at_end() {
                            state.skip_semicolons();
                            result.push(state.parse_extern(range)?)
                        }

                        Ok(())
                    } else {
                        result.push(self.parse_extern(range)?);
                        Ok(())
                    }
                },
            },
            Token::String(_) => {
                let Token::String(string) = token else { unreachable!() };
                result.push(self.parse_statement2(string, is_global, range)?);
                Ok(())
            }
            Token::Operation(TwoSidedOperation::Number(NumberOperation::Mul)) => { // *..
                let left_expression = self.parse_expression_without_ops(range, true, false)?;
                let left_expression = Expression::new_unary_operation(left_expression, OneSidedOperation::Dereference);
                result.push(self.parse_statement3(left_expression, range)?);
                Ok(())
            }
            Token::Bracket(body, BracketType::Curly) => {
                if !is_global { return Err(SyntacticError::new_local_global("brackets")) }

                let mut state = Self::new(body);
                let body = state.parse_statements(false)?;
                result.push(Statement::new_brackets(body));
                Ok(())
            }
            Token::Semicolon => unreachable!(),
            _ => Err((ExpectedEnum::new_string("keyword") | ExpectedEnum::Name | ExpectedEnum::new_string("*") | ExpectedEnum::Semicolon).err())
        }
    }
    /// parse "name .." as definition
    fn parse_statement2(&mut self, name: String, is_global: bool, range: Range) -> Result<Statement, SyntacticError> {
        let Some(RangedToken { token, range: _ }) = self.peek() else {
            return Err(SyntacticError::new_unexpected("EOF"));
        };
        match token {
            Token::DoubleColon => {
                // name ::
                let Some(RangedToken { token: _, range }) = self.next() else { unreachable!() };

                let Some(RangedToken { token, range: _ }) = self.peek() else {
                    return Err((ExpectedEnum::RoundBracket | ExpectedEnum::new_string("struct")).err())
                };
                match token {
                    Token::Bracket(_, BracketType::Round) => {
                        if !is_global { return Err(SyntacticError::new_local_global("function")) }

                        self.parse_function(name, range)
                    },
                    Token::String(string) if string == "struct" => {
                        if !is_global { return Err(SyntacticError::new_local_global("struct")) }

                        let Some(RangedToken { token: _, range }) = self.next() else { unreachable!() };
                        self.parse_struct(name, range)
                    }
                    _ => Err((ExpectedEnum::RoundBracket | ExpectedEnum::new_string("struct")).err())
                }
            },
            Token::Colon => {
                // name :
                self.next();
                let typee = self.parse_type(range)?;

                let Some(RangedToken { token, range }) = self.next() else {
                    return Err((ExpectedEnum::Equal | ExpectedEnum::Semicolon).err())
                };
                if token == Token::Semicolon {
                    let token = LiteralExpression::Undefined.into();
                    let statement = Statement::new_variable(name, Some(typee), token);
                    return Ok(statement)
                }
                if token != Token::EqualOperation(EqualOperation::Equal) {
                    return Err((ExpectedEnum::Equal | ExpectedEnum::Semicolon).err())
                }

                let expression = self.parse_expression(range, false)?;
                let statement = Statement::new_variable(name, Some(typee), expression);
                Ok(statement)
            },
            Token::EqualOperation(EqualOperation::ColonEqual) => {
                self.next();

                let expression2 = self.parse_expression(range, false)?;
                Ok(Statement::new_variable(name, None, expression2))
            },
            Token::Bracket(_, BracketType::Round) => {
                // name(..)
                let Some(RangedToken { token, range }) = self.next() else { unreachable!() };
                let Token::Bracket(vec, _) = token else { unreachable!() };

                let args = parse_function_arguments(vec, range)?;
                let left = Expression::new_function_call(name, args);
                let expression = self.parse_expression2_without_ops(left, false, false)?;
                Ok(Statement::Expression(expression))
            }
            _ => {
                let left = Expression::Variable(name);
                let left = self.parse_expression2_without_ops(left, true, false)?;
                self.parse_statement3(left, range)
            }
        }
    }
    /// parse left .. ;
    fn parse_statement3(&mut self, left: Expression, range: Range) -> Result<Statement, SyntacticError> {
        let Some(RangedToken { token, range }) = self.next() else {
            return Err(SyntacticError::new_unexpected("EOF"));
        };
        match token {
            Token::DoubleColon | Token::Colon | Token::EqualOperation(EqualOperation::ColonEqual) | Token::Bracket(_, BracketType::Round) => {
                unreachable!("was in parse_statement2")
            }
            Token::EqualOperation(equal_operation) => {
                // name _=
                let expression2 = self.parse_expression(range, false)?;
                let statement = match equal_operation {
                    EqualOperation::ColonEqual => unreachable!(),
                    EqualOperation::Equal => Statement::new_set(left, expression2, None),
                    EqualOperation::OperationEqual(op) => {
                        Statement::new_set(left, expression2, Some(op))
                    }
                };
                Ok(statement)
            }
            Token::Semicolon => {
                Ok(Statement::Expression(left))
            }
            _ => {
                Err((ExpectedEnum::DoubleColon | ExpectedEnum::Colon | ExpectedEnum::new_string("_=") | ExpectedEnum::RoundBracket | ExpectedEnum::Semicolon).err())
            }
        }
    }

    fn parse_expression(&mut self, range: Range, in_cond: bool) -> Result<Expression, SyntacticError> {
        let mut expressions = Vec::<Expression>::new();
        let mut operations = Vec::<TwoSidedOperation>::new();
        let mut range = range;

        loop {
            let next_expression = self.parse_expression_without_ops(range, false, in_cond)?;
            expressions.push(next_expression);

            let Some(RangedToken { token, range: _ }) = self.peek() else {
                break;
            };
            if matches!(token, Token::Semicolon | Token::Comma | Token::Bracket(_, _) | Token::EqualOperation(_)) {
                break
            }

            let Some(RangedToken { token, range: range2 }) = self.next() else { unreachable!() };
            let Token::Operation(op) = token else {
                return Err(ExpectedEnum::new_string("operation").err())
            };
            range = range2;
            operations.push(op);
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
    fn parse_expression_without_ops(
        &mut self,
        range: Range,
        was_unary: bool,
        in_cond: bool,
    ) -> Result<Expression, SyntacticError> {
        let Some(RangedToken { token, range }) = self.next() else {
            return Err(SyntacticError::new_unexpected("EOF"));
        };
        match token {
            Token::String(string) => { // string
                let expression1 = match string.as_str() {
                    "true" => LiteralExpression::BoolLiteral(true).into(),
                    "false" => LiteralExpression::BoolLiteral(false).into(),
                    _ => Expression::Variable(string),
                };
                self.parse_expression2_without_ops(expression1, was_unary, in_cond)
            }
            Token::NumberLiteral(token) => { // 123
                let expression1 = LiteralExpression::NumberLiteral(token).into();
                self.parse_expression2_without_ops(expression1, was_unary, in_cond)
            }
            Token::Bracket(vec, BracketType::Round) => { // (..)
                let mut new_state = ParsingState::new(vec);
                let expression = new_state.parse_expression(range, false)?;
                if !new_state.at_end() {
                    return Err(ExpectedEnum::new_string(")").err())
                }
                let expression1 = Expression::new_round_bracket(expression);
                self.parse_expression2_without_ops(expression1, was_unary, in_cond)
            }
            Token::Operation(TwoSidedOperation::Number(NumberOperation::Sub)) => { // -..
                if let Some(RangedToken { token: Token::Operation(TwoSidedOperation::Number(NumberOperation::Sub)), range: _ }) = self.peek() {
                    self.next();
                    let Some(RangedToken { token: Token::Operation(TwoSidedOperation::Number(NumberOperation::Sub)), range: _ }) = self.next() else {
                        return Err(ExpectedEnum::new_string("---").err())
                    };

                    let undef_expression = LiteralExpression::Undefined.into();
                    // return Ok(undef_expression); // no operators on undef token
                    return self.parse_expression2_without_ops(undef_expression, false, in_cond)
                }
                
                let op = OneSidedOperation::UnaryMinus;
                let expression = self.parse_expression_without_ops(range, true, in_cond)?;
                
                let unary_expression = Expression::new_unary_operation(expression, op);
                self.parse_expression2_without_ops(unary_expression, false, in_cond)
            }
            Token::Operation(TwoSidedOperation::Number(NumberOperation::Mul)) => { // *..
                let op = OneSidedOperation::Dereference;
                let expression = self.parse_expression_without_ops(range, true, in_cond)?;

                let unary_expression = Expression::new_unary_operation(expression, op);
                self.parse_expression2_without_ops(unary_expression, false, in_cond)
            }
            Token::Operation(TwoSidedOperation::Number(NumberOperation::BitAnd)) => { // &..
                let op = OneSidedOperation::GetReference;
                let expression = self.parse_expression_without_ops(range, true, in_cond)?;

                let unary_expression = Expression::new_unary_operation(expression, op);
                self.parse_expression2_without_ops(unary_expression, false, in_cond)
            }
            Token::Operation(TwoSidedOperation::Bool(BoolOperation::And)) => {
                let op = OneSidedOperation::GetReference;
                let expression = self.parse_expression_without_ops(range, true, in_cond)?;

                let unary_expression = Expression::new_unary_operation(expression, op);
                let unary_expression2 = Expression::new_unary_operation(unary_expression, op);
                self.parse_expression2_without_ops(unary_expression2, false, in_cond)
            }
            Token::UnaryOperation(op) => { // `unary`..
                let expression = self.parse_expression_without_ops(range, true, in_cond)?;
                let unary_expression = Expression::new_unary_operation(expression, op);
                self.parse_expression2_without_ops(unary_expression, false, in_cond)
            }
            Token::Quotes(string) => { // '..'
                let char_value = parse_quotes(&string)?;
                let expression = LiteralExpression::CharLiteral(char_value).into();
                self.parse_expression2_without_ops(expression, was_unary, in_cond)
            }
            Token::DoubleQuotes(string) => { // ".."
                let expression = LiteralExpression::StringLiteral(string).into();
                self.parse_expression2_without_ops(expression, was_unary, in_cond)
            }
            _ => {
                Err(ExpectedEnum::new_string(&format!("expression, got {token:?}")).err())
            }
        }
    }
    // parse "expression .."
    fn parse_expression2_without_ops(
        &mut self,
        expression1: Expression,
        was_unary: bool,
        in_cond: bool
    ) -> Result<Expression, SyntacticError> {
        let Some(RangedToken { token, range }) = self.peek() else {
            return Ok(expression1);
        };
        match token {
            Token::Operation(_) => { // exp +
                Ok(expression1)
            }
            Token::Bracket(_, BracketType::Round) => { // exp(..)
                let Some(RangedToken { token, range }) = self.next() else { unreachable!() };
                let Token::Bracket(vec, _) = token else { unreachable!() };

                // FIXME: allow function variables
                let Expression::Variable(name) = expression1 else {
                    return Err(SyntacticError::new_unexpected("round brackets after expression"));
                };
                let args = parse_function_arguments(vec, range)?;
                self.parse_expression2_without_ops(Expression::new_function_call(name, args), was_unary, false)
            }
            Token::String(string) if string == "as" => { // exp as
                if was_unary {
                    return Ok(expression1)
                }

                let range = self.next().unwrap().range;

                let typee = self.parse_type(range)?;
                let expression = Expression::new_as(expression1, typee);
                self.parse_expression2_without_ops(expression, false, in_cond)
            }
            Token::Dot => {
                let Some(RangedToken { token: _, range: _ }) = self.next() else { unreachable!() };
                let Some(RangedToken { token, range }) = self.next() else { unreachable!() };
                
                match token {
                    Token::String(field_name) => {
                        let expression = Expression::new_dot(expression1, field_name);
                        self.parse_expression2_without_ops(expression, was_unary, in_cond)
                    }
                    _ => Err(SyntacticError::new_unexpected("dot operator"))
                }
            }
            Token::Bracket(_, BracketType::Curly) if !in_cond && matches!(&expression1, Expression::Variable(..)) => {
                let Some(RangedToken { token, range }) = self.next() else { unreachable!() };
                let Token::Bracket(vec, _) = token else { unreachable!() };
                let Expression::Variable(name) = expression1 else { unreachable!() };

                let fields = parse_struct_construction(vec, range)?;
 
                Ok(Expression::StructConstruct {
                    struct_name: name,
                    fields
                })
            }
            Token::Semicolon | Token::Comma | Token::Bracket(_, _) | Token::EqualOperation(_) | Token::DoubleDot => {
                Ok(expression1)
            }
            _ => {
                Err(SyntacticError::Syntactic(*range, format!("unexpected token {token:?}")))
            }
        }
    }

    fn parse_function(&mut self, name: String, range: Range) -> Result<Statement, SyntacticError> {
        let Some(RangedToken { token, range }) = self.next() else {
            return Err(ExpectedEnum::RoundBracket.err());
        };

        let Token::Bracket(args, BracketType::Round) = token else {
            return Err(ExpectedEnum::RoundBracket.err());
        };

        // parse arguments
        let arguments = parse_function_declaration_arguments(args)?;

        // parse return type
        let Some(token_with_pos) = self.next() else {
            return Err((ExpectedEnum::Arrow | ExpectedEnum::CurlyBracket).err());
        };
        let mut token_with_pos = token_with_pos;
        let return_type = {
            if token_with_pos.token == Token::Arrow {
                let return_type = self.parse_type(token_with_pos.range)?;
                let Some(new_token_with_pos) = self.next() else {
                    return Err(ExpectedEnum::CurlyBracket.err());
                };
                token_with_pos = new_token_with_pos;
                Some(return_type)
            } else {
                None
            }
        };

        let RangedToken { token, range } = token_with_pos;
        // parse inside
        let Token::Bracket(body, BracketType::Curly) = token else {
            return Err(ExpectedEnum::CurlyBracket.err());
        };
        let body = ParsingState::new(body).parse_statements(false)?;

        let statement = Statement::new_function(name, arguments, return_type, body);
        Ok(statement)
    }

    fn parse_type(&mut self, range: Range) -> Result<Typee, SyntacticError> {
        let Some(RangedToken { token, range }) = self.next() else {
            return Err(ExpectedEnum::new_string("type").err());
        };

        match token {
            Token::String(string) => Ok(Typee::String(string)),
            Token::Operation(TwoSidedOperation::Number(NumberOperation::Mul)) => {
                let typee = self.parse_type(range)?;
                Ok(Typee::new_pointer(typee))
            }
            Token::UnaryOperation(OneSidedOperation::Dereference) => unreachable!(), // lexer make Mul from *
            Token::Operation(TwoSidedOperation::Number(NumberOperation::BitAnd)) => {
                let typee = self.parse_type(range)?;
                Ok(Typee::new_reference(typee))
            }
            Token::Operation(TwoSidedOperation::Bool(BoolOperation::And)) => {
                let typee = self.parse_type(range)?;
                Ok(Typee::new_reference(Typee::new_reference(typee)))
            }
            _ => Err(ExpectedEnum::new_string("type").err())
        }
    }

    fn parse_struct(&mut self, name: String, _range: Range) -> Result<Statement, SyntacticError> {
        let Some(RangedToken { token: Token::Bracket(tokens, BracketType::Curly), range: _ }) = self.next() else {
            return Err(ExpectedEnum::CurlyBracket.err())
        };
        let mut fields = Vec::new();
        let mut state = ParsingState::new(tokens);
        while !state.at_end() {
            // name
            let Some(RangedToken { token: Token::String(field_name), range: _ }) = state.next() else {
                return Err(ExpectedEnum::Name.err())
            };

            // :
            let Some(RangedToken { token: Token::Colon, range }) = state.next() else {
                return Err(ExpectedEnum::Colon.err())
            };

            // typee
            let field_typee = state.parse_type(range)?;
            fields.push((field_name, field_typee));

            if state.at_end() { break }

            // ,
            let Some(RangedToken { token: Token::Comma, range: _ }) = state.next() else {
                return Err(ExpectedEnum::Comma.err())
            };
        }

        Ok(Statement::new_struct(name, fields))
    }
    fn parse_extern(&mut self, range: Range) -> Result<Statement, SyntacticError> {
        let Some(RangedToken { token: Token::String(name), range: _ }) = self.next() else {
            return Err(ExpectedEnum::Name.err())
        };

        let Some(RangedToken { token, range }) = self.next() else {
            return Err((ExpectedEnum::Colon | ExpectedEnum::DoubleColon).err())
        };

        if token == Token::Colon {
            // name : ..
            let typee = self.parse_type(range)?;

            let Some(RangedToken { token: Token::Semicolon, range: _ }) = self.next() else {
                return Err(ExpectedEnum::Colon.err())
            };

            Ok(Statement::ExternStatement { statement: ExternStatement::Variable { name, typee }})
        } else if token == Token::DoubleColon {
            // name :: ..

            let Some(RangedToken { token: Token::Bracket(vec, BracketType::Round), range: _ }) = self.next() else {
                return Err(ExpectedEnum::RoundBracket.err())
            };

            let (args, is_vararg) = parse_extern_function_arguments(vec)?;

            let Some(RangedToken { token, range }) = self.next() else {
                return Err(ExpectedEnum::Semicolon.err())
            };

            if token == Token::Semicolon {
                Ok(Statement::ExternStatement { statement: ExternStatement::Function { name, args, is_vararg, returns: None }})
            } else if token == Token::Arrow {
                let returns = self.parse_type(range)?;

                let Some(RangedToken { token: Token::Semicolon, range: _ }) = self.next() else {
                    return Err(ExpectedEnum::Semicolon.err())
                };

                Ok(Statement::ExternStatement { statement: ExternStatement::Function { name, args, is_vararg, returns: Some(returns) }})
            } else {
                Err((ExpectedEnum::Arrow | ExpectedEnum::Semicolon).err())
            }
        } else {
            Err((ExpectedEnum::Colon | ExpectedEnum::DoubleColon).err())
        }
    }
    fn parse_import(&mut self, mut range0: Range) -> Result<Statement, SyntacticError> {
        let Some(RangedToken { token: Token::String(string), range }) = self.next() else {
            return Err(ExpectedEnum::Name.err())
        };

        let mut from = vec![string];
        loop {
            let Some(RangedToken { token, range }) = self.next() else {
                return Err((ExpectedEnum::DoubleColon | ExpectedEnum::Semicolon | ExpectedEnum::As).err())
            };
            match token {
                Token::DoubleColon => {}
                Token::Semicolon => { // ::x;
                    let what = vec![(from.pop().unwrap(), None)];
                    return Ok(Statement::new_import(from, what))
                }
                Token::String(as_string) if as_string == "as" => { // ::x as x;
                    let Some(RangedToken { token: Token::String(as_name), range }) = self.next() else {
                        return Err(ExpectedEnum::Name.err())
                    };

                    let Some(RangedToken { token: Token::Semicolon, range: _ }) = self.next() else {
                        return Err(ExpectedEnum::Semicolon.err())
                    };

                    let what = vec![(from.pop().unwrap(), Some(as_name))];
                    return Ok(Statement::new_import(from, what))
                }
                _ => return Err((ExpectedEnum::DoubleColon | ExpectedEnum::Semicolon | ExpectedEnum::As).err())
            }

            let Some(RangedToken { token: Token::String(_), range: _ }) = self.peek() else {
                break
            };

            let Some(RangedToken { token: Token::String(string), range }) = self.next() else { unreachable!() };
            from.push(string);
            range0 = range;
        }

        // ::{x, .., x as x, ..};
        let Some(RangedToken { token: Token::Bracket(whats, BracketType::Curly), range }) = self.next() else {
            return Err((ExpectedEnum::Name | ExpectedEnum::CurlyBracket).err())
        };

        let mut what = Vec::with_capacity(whats.len());
        let mut state = ParsingState::new(whats);
        while !state.at_end() {
            let Some(RangedToken { token: Token::String(name), range }) = state.next() else {
                return Err(ExpectedEnum::Name.err())
            };

            if state.peek().is_none() { break }
            let Some(RangedToken { token, range }) = state.next() else {
                return Err((ExpectedEnum::Comma | ExpectedEnum::As).err())
            };
            if token == Token::Comma {
                what.push((name, None));
                continue
            }
            if !matches!(token, Token::String(as_string) if as_string == "as") {
                return Err((ExpectedEnum::Comma | ExpectedEnum::As).err())
            }

            let Some(RangedToken { token: Token::String(as_name), range }) = state.next() else {
                return Err(ExpectedEnum::Name.err())
            };
            what.push((name, Some(as_name)));

            if state.at_end() { break }
            let Some(RangedToken { token: Token::Comma, range }) = state.next() else {
                return Err(ExpectedEnum::Comma.err())
            };
            range0 = range;
        }
        let Some(RangedToken { token: Token::Semicolon, range: _ }) = self.next() else {
            return Err(ExpectedEnum::Semicolon.err())
        };

        Ok(Statement::new_import(from, what))
    }
    fn parse_triple_dot(&mut self, mut range: Range) -> Result<(), SyntacticError> {
        let Some(RangedToken { token: Token::DoubleDot, range: _ }) = self.next() else {
            return Err(ExpectedEnum::new_string("...").err())
        };
        let Some(RangedToken { token: Token::Dot, range: _ }) = self.next() else {
            return Err(ExpectedEnum::new_string("...").err())
        };
        Ok(())
    }
}

fn parse_function_declaration_arguments(args: Vec<RangedToken>) -> Result<Vec<(String, Typee)>, SyntacticError> {
    if args.is_empty() {
        return Ok(Vec::new())
    }
    let mut arguments = Vec::with_capacity(args.len().div_ceil(2));
    let mut state = ParsingState::new(args);

    while let Some(RangedToken { token, range }) = state.next() {
        let Token::String(arg_i) = token else {
            return Err(ExpectedEnum::Name.err());
        };

        let Some(RangedToken { token: Token::Colon, range }) = state.next() else {
            return Err(ExpectedEnum::Semicolon.err())
        };

        let argument_type = state.parse_type(range)?;

        arguments.push((arg_i, argument_type));

        let Some(RangedToken { token, range }) = state.next() else {
            break;
        };
        if token != Token::Comma {
            return Err(ExpectedEnum::Comma.err());
        }
    }

    Ok(arguments)
}
fn parse_extern_function_arguments(args: Vec<RangedToken>) -> Result<(Vec<Typee>, bool), SyntacticError> {
    if args.is_empty() {
        return Ok((Vec::new(), false))
    }
    let mut arguments = Vec::with_capacity(args.len().div_ceil(2));
    let mut state = ParsingState::new(args);

    let mut range0 = Range::default();
    while !state.at_end() {
        if matches!(state.peek(), Some(RangedToken { token: Token::DoubleDot, .. })) {
            state.parse_triple_dot(range0)?;

            if !state.at_end() {
                return Err(ExpectedEnum::new_string(")").err())
            }
            return Ok((arguments, true))
        }

        let argument_type = state.parse_type(range0)?;

        arguments.push(argument_type);

        let Some(RangedToken { token, range }) = state.next() else {
            break;
        };
        if token != Token::Comma {
            return Err(ExpectedEnum::Comma.err());
        }
        range0 = range
    }

    Ok((arguments, false))
}

fn parse_function_arguments(tokens: Vec<RangedToken>, range: Range) -> Result<Vec<Expression>, SyntacticError> {
    let mut state = ParsingState::new(tokens);
    let mut args = Vec::new();

    while !state.at_end() {
        let expression = state.parse_expression(range, false)?;
        args.push(expression);

        if !state.at_end() {
            let RangedToken { token, range } = state.next().unwrap();
            if token != Token::Comma {
                return Err(ExpectedEnum::Comma.err());
            }
        }
    }

    Ok(args)
}

fn parse_struct_construction(tokens: Vec<RangedToken>, range: Range) -> Result<Vec<(String, Expression)>, SyntacticError> {
    let mut fields = Vec::new();
    let mut state = ParsingState::new(tokens);

    while !state.at_end() {
        let Some(RangedToken { token: Token::String(field_name), range }) = state.next() else {
            return Err(ExpectedEnum::Name.err())
        };
        let Some(RangedToken { token: Token::Colon, range }) = state.next() else {
            return Err(ExpectedEnum::Colon.err())
        };
        let field_value = state.parse_expression(range, false)?;
        fields.push((field_name, field_value));
        if state.at_end() {
            break
        }

        let Some(RangedToken { token: Token::Comma, range }) = state.next() else {
            return Err(ExpectedEnum::Comma.err())
        };
    }

    Ok(fields)
}

fn parse_quotes(string: &str) -> Result<u8, SyntacticError> {
    let mut chars = string.chars();
    let Some(first_char) = chars.next() else {
        return Err(SyntacticError::LiteralParseError { what: format!("'{string}'"), error: "incorrect char literal".to_owned() })
    };

    if chars.next().is_some() {
        return Err(SyntacticError::LiteralParseError { what: format!("'{string}'"), error: "incorrect char".to_owned() })
    }
    if !first_char.is_ascii() {
        return Err(SyntacticError::LiteralParseError { what: format!("'{string}'"), error: "incorrect char".to_owned() })
    }
    let char_value = first_char as u8;
    Ok(char_value)
}
