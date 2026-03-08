use std::collections::VecDeque;
use lsp_types::Range;

use crate::parser::BracketType;
use crate::parser::operations::{BoolOperation, CompareOperator, NumberOperation, OneSidedOperation, RTwoSidedOperation, TwoSidedOperation};
use crate::parser::parse1_tokenize::token::*;
use crate::RString;
use super::statement::*;
use super::error::{ExpectedEnum, SyntacticError};

pub fn parse_statements(tokens: Vec<RangedToken>) -> Result<Vec<RStatement>, SyntacticError> {
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
    pub fn parse_statements(&mut self, is_global: bool) -> Result<Vec<RStatement>, SyntacticError> {
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
    fn parse_statement(&mut self, is_global: bool, result: &mut Vec<RStatement>) -> Result<(), SyntacticError> {
        let Some(RangedToken { token, range: range0 }) = self.next() else { unreachable!() };
        match token {
            Token::Keyword(keyword) => match keyword {
                TokenKeyword::If | TokenKeyword::While => {
                    let condition = self.parse_expression(true)?;

                    let Some(RangedToken { token, range: range_br }) = self.next() else {
                        return Err(ExpectedEnum::CurlyBracket.err());
                    };

                    let Token::Bracket(vec, BracketType::Curly) = token else {
                        return Err(ExpectedEnum::CurlyBracket.err())
                    };
                    let body = ParsingState::new(vec).parse_statements(false)?;

                    let statement = if keyword == TokenKeyword::If {
                        Statement::new_if(condition, body)
                    } else {
                        Statement::new_while(condition, body)
                    };
                    result.push(statement.add_range(Range::new(range0.start, range_br.end)));
                    Ok(())
                }
                TokenKeyword::For => {
                    let Some(RangedToken { token: Token::String(name), range }) = self.next() else {
                        return Err(ExpectedEnum::Name.err())
                    };
                    let name = RString::new(name, range);
                    let Some(RangedToken { token: Token::String(in_str), range: _ }) = self.next() else {
                        return Err(ExpectedEnum::new_string("'in'").err())
                    };
                    if in_str != "in" {
                        return Err(ExpectedEnum::new_string("'in'").err())
                    }

                    let from_value = self.parse_expression_without_ops(true, true)?;

                    let Some(RangedToken { token: Token::DoubleDot, range: _ }) = self.next() else {
                        return Err(ExpectedEnum::new_string("..").err())
                    };

                    let compare_op: TwoSidedOperation = if let Some(
                        RangedToken { token: Token::EqualOperation(EqualOperation::Equal), range: _ }
                    ) = self.peek()
                    {

                        self.next();
                        CompareOperator::LessEqual.into()
                    } else {
                        CompareOperator::Less.into()
                    };

                    let to_value = self.parse_expression_without_ops(true, true)?;

                    let Some(RangedToken { token: Token::Bracket(body, BracketType::Curly), range: _ }) = self.next() else {
                        return Err(ExpectedEnum::CurlyBracket.err())
                    };
                    let mut state = Self::new(body);
                    let body = state.parse_statements(false)?;

                    result.push(Statement::new_for(
                        Statement::new_variable(name.clone(), None, from_value).add_no_range(),
                        Expression::new_operation(
                            Expression::Variable(name.clone()).add_no_range(),
                            to_value,
                            compare_op.add_no_range(),
                        ).add_no_range(),
                        body,
                        Statement::new_set(
                            Expression::Variable(name).add_no_range(),
                            Expression::from(LiteralExpression::NumberLiteral("1".to_string())).add_no_range(),
                            Some(TwoSidedOperation::from(NumberOperation::Add).add_no_range())
                        ).add_no_range()
                    ).add_no_range());

                    Ok(())
                }
                TokenKeyword::Return => {
                    let peek_token = self.peek();
                    if peek_token.is_none() {
                        result.push(Statement::Return(None).add_range(range0))
                    } else if let Some(RangedToken { token: Token::Semicolon, range: _ }) = peek_token {
                        result.push(Statement::Return(None).add_range(range0))
                    } else {
                        let expression = self.parse_expression(false)?;
                        result.push(Statement::Return(Some(expression)).add_range(range0))
                    }
                    Ok(())
                }
                TokenKeyword::Import => {
                    if !is_global { return Err(SyntacticError::new_local_global("import")) }
                    result.push(self.parse_import(range0)?);
                    Ok(())
                },
                TokenKeyword::Extern => {
                    if !is_global { return Err(SyntacticError::new_local_global("extern")) }
                    
                    if matches!(self.peek(), Some(RangedToken { token: Token::Bracket(_, BracketType::Curly), range: _ })) {
                        let Some(RangedToken { token: Token::Bracket(vec, _), range: _ }) = self.next() else { unreachable!() };

                        let mut state = Self::new(vec);
                        while !state.at_end() {
                            state.skip_semicolons();
                            result.push(state.parse_extern(range0)?)
                        }

                        Ok(())
                    } else {
                        result.push(self.parse_extern(range0)?);
                        Ok(())
                    }
                },
            },
            Token::String(_) => {
                let Token::String(string) = token else { unreachable!() };
                let string = RString::new(string, range0);
                result.push(self.parse_statement2(string, is_global)?);
                Ok(())
            }
            Token::Operation(TwoSidedOperation::Number(NumberOperation::Mul)) => { // *..
                let left_expression = self.parse_expression_without_ops(true, false)?;
                let op = OneSidedOperation::Dereference.add_range(range0);
                let left_expression = new_unary_expr(left_expression, op);
                result.push(self.parse_statement3(left_expression)?);
                Ok(())
            }
            Token::Bracket(body, BracketType::Curly) => {
                if !is_global { return Err(SyntacticError::new_local_global("brackets")) }

                let mut state = Self::new(body);
                let body = state.parse_statements(false)?;
                result.push(Statement::new_brackets(body).add_range(range0));
                Ok(())
            }
            Token::Semicolon => unreachable!(),
            _ => Err((ExpectedEnum::new_string("keyword") | ExpectedEnum::Name | ExpectedEnum::new_string("*") | ExpectedEnum::Semicolon).err())
        }
    }
    /// parse "name .." as definition
    fn parse_statement2(&mut self, name: RString, is_global: bool) -> Result<RStatement, SyntacticError> {
        let Some(RangedToken { token, range: _ }) = self.peek() else {
            return Err(SyntacticError::new_unexpected("EOF"));
        };
        match token {
            Token::DoubleColon => {
                // name ::
                let Some(RangedToken { token: _, range: _ }) = self.next() else { unreachable!() };

                let Some(RangedToken { token, range: _ }) = self.peek() else {
                    return Err((ExpectedEnum::RoundBracket | ExpectedEnum::new_string("struct")).err())
                };
                match token {
                    Token::Bracket(_, BracketType::Round) => {
                        if !is_global { return Err(SyntacticError::new_local_global("function")) }

                        self.parse_function(name)
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
                let Some(RangedToken { token: _, range }) = self.next() else { unreachable!() };
                let typee = self.parse_type(range)?;

                let Some(RangedToken { token, range: _ }) = self.next() else {
                    return Err((ExpectedEnum::Equal | ExpectedEnum::Semicolon).err())
                };
                if token == Token::Semicolon {
                    let expression: Expression = LiteralExpression::Undefined.into();
                    let expression = expression.add_no_range();
                    let range_st = Range::new(name.range.start, expression.range.end);
                    let statement = Statement::new_variable(name, Some(typee), expression);
                    return Ok(statement.add_range(range_st));
                }
                if token != Token::EqualOperation(EqualOperation::Equal) {
                    return Err((ExpectedEnum::Equal | ExpectedEnum::Semicolon).err())
                }

                let expression = self.parse_expression(false)?;
                let range_st = Range::new(name.range.start, expression.range.end);
                let statement = Statement::new_variable(name, Some(typee), expression);
                Ok(statement.add_range(range_st))
            },
            Token::EqualOperation(EqualOperation::ColonEqual) => {
                self.next();

                let expression2 = self.parse_expression(false)?;
                let range_st = Range::new(name.range.start, expression2.range.end);
                let statement = Statement::new_variable(name, None, expression2);
                Ok(statement.add_range(range_st))
            },
            Token::Bracket(_, BracketType::Round) => {
                // name(..)
                let Some(RangedToken { token, range: range_bracket }) = self.next() else { unreachable!() };
                let Token::Bracket(vec, _) = token else { unreachable!() };

                let range_call = Range::new(name.range.start, range_bracket.end);

                let args = parse_function_arguments(vec)?;
                let left = Expression::new_function_call(name, args).add_range(range_call);
                let expression = self.parse_expression2_without_ops(left, false, false)?;
                let statement = Statement::Expression(expression.value).add_range(expression.range);
                Ok(statement)
            }
            _ => {
                let range = name.range;
                let left = Expression::Variable(name).add_range(range);
                let left = self.parse_expression2_without_ops(left, true, false)?;
                self.parse_statement3(left)
            }
        }
    }
    /// parse left .. ;
    fn parse_statement3(&mut self, left: RExpression) -> Result<RStatement, SyntacticError> {
        let Some(RangedToken { token, range: range0 }) = self.next() else {
            return Err(SyntacticError::new_unexpected("EOF"));
        };
        match token {
            Token::DoubleColon | Token::Colon | Token::EqualOperation(EqualOperation::ColonEqual) | Token::Bracket(_, BracketType::Round) => {
                unreachable!("was in parse_statement2")
            }
            Token::EqualOperation(equal_operation) => {
                // name _=
                let expression2 = self.parse_expression(false)?;
                let range_st = Range::new(left.range.start, expression2.range.end);
                let statement = match equal_operation {
                    EqualOperation::ColonEqual => unreachable!(),
                    EqualOperation::Equal => Statement::new_set(left, expression2, None),
                    EqualOperation::OperationEqual(op) => {
                        Statement::new_set(left, expression2, Some(op.add_range(range0)))
                    }
                };
                Ok(statement.add_range(range_st))
            }
            Token::Semicolon => {
                Ok(Statement::Expression(left.value).add_range(left.range))
            }
            _ => {
                Err((ExpectedEnum::DoubleColon | ExpectedEnum::Colon | ExpectedEnum::new_string("_=") | ExpectedEnum::RoundBracket | ExpectedEnum::Semicolon).err())
            }
        }
    }

    fn parse_expression(&mut self, in_cond: bool) -> Result<RExpression, SyntacticError> {
        let mut expressions = Vec::<RExpression>::new();
        let mut operations = Vec::<RTwoSidedOperation>::new();
        // let mut range = Range::default();

        loop {
            let next_expression = self.parse_expression_without_ops(false, in_cond)?;
            expressions.push(next_expression);

            let Some(RangedToken { token, range: _ }) = self.peek() else {
                break;
            };
            if matches!(token, Token::Semicolon | Token::Comma | Token::Bracket(_, _) | Token::EqualOperation(_)) {
                break
            }

            let Some(RangedToken { token, range }) = self.next() else { unreachable!() };
            let Token::Operation(op) = token else {
                return Err(ExpectedEnum::new_string("operation").err())
            };
            // range = range2;
            operations.push(op.add_range(range));
        }

        // FIXME: speed up to O(n log n)
        fn create_expression(mut exps: Vec<RExpression>, mut ops: Vec<RTwoSidedOperation>) -> RExpression {
            if ops.is_empty() {
                return exps.pop().unwrap()
            }
            let split_index = (0..ops.len()).min_by_key(|x| ops[*x].value.get_prior()).unwrap();

            let exps_right = exps.split_off(split_index + 1);
            let ops_right = ops.split_off(split_index + 1);
            let op_middle = ops.pop().unwrap();

            let left_exp = create_expression(exps, ops);
            let right_exp = create_expression(exps_right, ops_right);
            let range = Range::new(left_exp.range.start, right_exp.range.end);
            Expression::new_operation(left_exp, right_exp, op_middle).add_range(range)
        }

        let result = create_expression(expressions, operations);

        Ok(result)
    }
    fn parse_expression_without_ops(
        &mut self,
        was_unary: bool,
        in_cond: bool,
    ) -> Result<RExpression, SyntacticError> {
        let Some(RangedToken { token, range }) = self.next() else {
            return Err(SyntacticError::new_unexpected("EOF"));
        };
        match token {
            Token::String(string) => { // string
                let string = RString::new(string, range);
                let expression1 = match string.value.as_str() {
                    "true" => LiteralExpression::BoolLiteral(true).into(),
                    "false" => LiteralExpression::BoolLiteral(false).into(),
                    _ => Expression::Variable(string),
                }.add_range(range);
                self.parse_expression2_without_ops(expression1, was_unary, in_cond)
            }
            Token::NumberLiteral(token) => { // 123
                let expression1 = Expression::from(LiteralExpression::NumberLiteral(token)).add_range(range);
                self.parse_expression2_without_ops(expression1, was_unary, in_cond)
            }
            Token::Bracket(vec, BracketType::Round) => { // (..)
                let mut new_state = ParsingState::new(vec);
                let expression = new_state.parse_expression(false)?;
                if !new_state.at_end() {
                    return Err(ExpectedEnum::new_string(")").err())
                }
                let expression1 = Expression::new_round_bracket(expression).add_range(range);
                self.parse_expression2_without_ops(expression1, was_unary, in_cond)
            }
            Token::Operation(TwoSidedOperation::Number(NumberOperation::Sub)) => { // -..
                if let Some(RangedToken { token: Token::Operation(TwoSidedOperation::Number(NumberOperation::Sub)), range: _ }) = self.peek() {
                    self.next();
                    let Some(RangedToken { token: Token::Operation(TwoSidedOperation::Number(NumberOperation::Sub)), range: range2 }) = self.next() else {
                        return Err(ExpectedEnum::new_string("---").err())
                    };

                    let range = Range::new(range.start, range2.end);
                    let undef_expression = Expression::from(LiteralExpression::Undefined).add_range(range);
                    // return Ok(undef_expression); // no operators on undef token
                    return self.parse_expression2_without_ops(undef_expression, false, in_cond)
                }

                let op = OneSidedOperation::UnaryMinus.add_range(range);
                let expression = self.parse_expression_without_ops(true, in_cond)?;

                let range1 = Range::new(range.start, expression.range.end);
                let unary_expression = Expression::new_unary_operation(expression, op).add_range(range1);
                self.parse_expression2_without_ops(unary_expression, false, in_cond)
            }
            Token::Operation(TwoSidedOperation::Number(NumberOperation::Mul)) => { // *..
                let op = OneSidedOperation::Dereference.add_range(range);
                let expression = self.parse_expression_without_ops(true, in_cond)?;

                let unary_expression = new_unary_expr(expression, op);
                self.parse_expression2_without_ops(unary_expression, false, in_cond)
            }
            Token::Operation(TwoSidedOperation::Number(NumberOperation::BitAnd)) => { // &..
                let op = OneSidedOperation::GetReference.add_range(range);
                let expression = self.parse_expression_without_ops(true, in_cond)?;

                let unary_expression = new_unary_expr(expression, op);
                self.parse_expression2_without_ops(unary_expression, false, in_cond)
            }
            Token::Operation(TwoSidedOperation::Bool(BoolOperation::And)) => {
                let op = OneSidedOperation::GetReference.add_range(range);
                let expression = self.parse_expression_without_ops(true, in_cond)?;

                let unary_expression = new_unary_expr(expression, op.clone());
                let unary_expression2 = new_unary_expr(unary_expression, op);
                self.parse_expression2_without_ops(unary_expression2, false, in_cond)
            }
            Token::UnaryOperation(op) => { // `unary`..
                let op = op.add_range(range);
                let expression = self.parse_expression_without_ops(true, in_cond)?;
                let unary_expression = new_unary_expr(expression, op);
                self.parse_expression2_without_ops(unary_expression, false, in_cond)
            }
            Token::Quotes(string) => { // '..'
                let char_value = parse_quotes(&string)?;
                let expression = Expression::from(LiteralExpression::CharLiteral(char_value)).add_range(range);
                self.parse_expression2_without_ops(expression, was_unary, in_cond)
            }
            Token::DoubleQuotes(string) => { // ".."
                let expression = Expression::from(LiteralExpression::StringLiteral(string)).add_range(range);
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
        expression1: RExpression,
        was_unary: bool,
        in_cond: bool
    ) -> Result<RExpression, SyntacticError> {
        let Some(RangedToken { token, range: _ }) = self.peek() else {
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
                let Expression::Variable(name) = expression1.value else {
                    return Err(SyntacticError::new_unexpected("round brackets after expression"));
                };
                let args = parse_function_arguments(vec)?;
                let range = Range::new(expression1.range.start, range.end);
                let expression = Expression::new_function_call(name, args).add_range(range);
                self.parse_expression2_without_ops(expression, was_unary, false)
            }
            Token::String(string) if string == "as" => { // exp as
                if was_unary {
                    return Ok(expression1)
                }

                let range = self.next().unwrap().range;

                let typee = self.parse_type(range)?;
                let range = Range::new(expression1.range.start, typee.range.end);
                let expression = Expression::new_as(expression1, typee).add_range(range);
                self.parse_expression2_without_ops(expression, false, in_cond)
            }
            Token::Dot => {
                let Some(RangedToken { token: _, range: _ }) = self.next() else { unreachable!() };
                let Some(RangedToken { token, range }) = self.next() else { unreachable!() };

                match token {
                    Token::String(field_name) => {
                        let field_name = RString::new(field_name, range);

                        let range = Range::new(expression1.range.start, field_name.range.end);
                        let expression = Expression::new_dot(expression1, field_name).add_range(range);
                        self.parse_expression2_without_ops(expression, was_unary, in_cond)
                    }
                    _ => Err(SyntacticError::new_unexpected("dot operator"))
                }
            }
            Token::Bracket(_, BracketType::Curly) if !in_cond && matches!(&expression1.value, Expression::Variable(..)) => {
                // name { ... }
                let Some(RangedToken { token, range: range0 }) = self.next() else { unreachable!() };
                let Token::Bracket(vec, _) = token else { unreachable!() };
                let Expression::Variable(name) = expression1.value else { unreachable!() };

                let fields = parse_struct_construction(vec)?;

                let range_st = Range::new(name.range.start, range0.end);
                let expression = Expression::StructConstruct {
                    struct_name: name,
                    fields,
                };
                Ok(expression.add_range(range_st))
            }
            Token::Semicolon | Token::Comma | Token::Bracket(_, _) | Token::EqualOperation(_) | Token::DoubleDot => {
                Ok(expression1)
            }
            _ => {
                let Some(RangedToken { token, range }) = self.next() else { unreachable!() };
                Err(SyntacticError::Syntactic(range, format!("unexpected token {token:?}")))
            }
        }
    }

    fn parse_function(&mut self, name: RString) -> Result<RStatement, SyntacticError> {
        let Some(RangedToken { token, range: _ }) = self.next() else {
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

        let RangedToken { token, range: _ } = token_with_pos;
        // parse inside
        let Token::Bracket(body, BracketType::Curly) = token else {
            return Err(ExpectedEnum::CurlyBracket.err());
        };
        let body = ParsingState::new(body).parse_statements(false)?;

        let range = name.range;
        Ok(Statement::new_function(name, arguments, return_type, body)
            .add_range(range))
    }

    fn parse_type(&mut self, _range: Range) -> Result<RTypee, SyntacticError> {
        let Some(RangedToken { token, range }) = self.next() else {
            return Err(ExpectedEnum::new_string("type").err());
        };

        match token {
            Token::String(string) => Ok(Typee::String(string).add_range(range)),
            Token::Operation(TwoSidedOperation::Number(NumberOperation::Mul)) => {
                let typee = self.parse_type(range)?;
                let range1 = Range::new(range.start, typee.range.end);
                Ok(Typee::new_pointer(typee).add_range(range1))
            }
            Token::UnaryOperation(OneSidedOperation::Dereference) => unreachable!(), // lexer make Mul from *
            Token::Operation(TwoSidedOperation::Number(NumberOperation::BitAnd)) => {
                let typee = self.parse_type(range)?;
                let range1 = Range::new(range.start, typee.range.end);
                Ok(Typee::new_reference(typee).add_range(range1))
            }
            Token::Operation(TwoSidedOperation::Bool(BoolOperation::And)) => {
                let typee = self.parse_type(range)?;
                let range1 = Range::new(range.start, typee.range.end);
                let mut range2 = range1;
                range2.start.character += 1; // FIXME: may be incorrect
                Ok(Typee::new_reference(
                    Typee::new_reference(typee).add_range(range2)
                ).add_range(range1))
            }
            _ => Err(ExpectedEnum::new_string("type").err())
        }
    }

    fn parse_struct(&mut self, name: RString, _range: Range) -> Result<RStatement, SyntacticError> {
        let Some(RangedToken { token: Token::Bracket(tokens, BracketType::Curly), range: _ }) = self.next() else {
            return Err(ExpectedEnum::CurlyBracket.err())
        };
        let mut fields = Vec::new();
        let mut state = ParsingState::new(tokens);
        while !state.at_end() {
            // name
            let Some(RangedToken { token: Token::String(field_name), range}) = state.next() else {
                return Err(ExpectedEnum::Name.err())
            };
            let field_name = RString::new(field_name, range);

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

        let range_name = name.range;
        Ok(Statement::new_struct(name, fields).add_range(range_name))
    }
    fn parse_extern(&mut self, _range0: Range) -> Result<RStatement, SyntacticError> {
        let Some(RangedToken { token: Token::String(name), range }) = self.next() else {
            return Err(ExpectedEnum::Name.err())
        };
        let name = RString::new(name, range);

        let Some(RangedToken { token, range }) = self.next() else {
            return Err((ExpectedEnum::Colon | ExpectedEnum::DoubleColon).err())
        };

        if token == Token::Colon {
            // name : ..
            let typee = self.parse_type(range)?;

            let Some(RangedToken { token: Token::Semicolon, range: _ }) = self.next() else {
                return Err(ExpectedEnum::Colon.err())
            };

            let range_st = name.range;
            let statement = Statement::new_extern(ExternStatement::Variable { name, typee });
            Ok(statement.add_range(range_st))
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
                let range_st = name.range;
                let statement = Statement::new_extern(ExternStatement::Function { name, args, is_vararg, returns: None });
                Ok(statement.add_range(range_st))
            } else if token == Token::Arrow {
                let returns = self.parse_type(range)?;

                let Some(RangedToken { token: Token::Semicolon, range: _ }) = self.next() else {
                    return Err(ExpectedEnum::Semicolon.err())
                };

                let range_st = name.range;
                let statement = Statement::new_extern(ExternStatement::Function { name, args, is_vararg, returns: Some(returns) });
                Ok(statement.add_range(range_st))
            } else {
                Err((ExpectedEnum::Arrow | ExpectedEnum::Semicolon).err())
            }
        } else {
            Err((ExpectedEnum::Colon | ExpectedEnum::DoubleColon).err())
        }
    }
    fn parse_import(&mut self, range_st: Range) -> Result<RStatement, SyntacticError> {
        let Some(RangedToken { token: Token::String(string), range }) = self.next() else {
            return Err(ExpectedEnum::Name.err())
        };

        let mut from: Vec<RString> = vec![RString::new(string, range)];
        loop {
            let Some(RangedToken { token, range: _ }) = self.next() else {
                return Err((ExpectedEnum::DoubleColon | ExpectedEnum::Semicolon | ExpectedEnum::As).err())
            };
            match token {
                Token::DoubleColon => {}
                Token::Semicolon => { // ::x;
                    let what = vec![(from.pop().unwrap(), None)];
                    return Ok(Statement::new_import(from, what).add_range(range_st))
                }
                Token::String(as_string) if as_string == "as" => { // ::x as x;
                    let Some(RangedToken { token: Token::String(as_name), range }) = self.next() else {
                        return Err(ExpectedEnum::Name.err())
                    };
                    let as_name = RString::new(as_name, range);

                    let Some(RangedToken { token: Token::Semicolon, range: _ }) = self.next() else {
                        return Err(ExpectedEnum::Semicolon.err())
                    };

                    let what = vec![(from.pop().unwrap(), Some(as_name))];
                    return Ok(Statement::new_import(from, what).add_range(range_st))
                }
                _ => return Err((ExpectedEnum::DoubleColon | ExpectedEnum::Semicolon | ExpectedEnum::As).err())
            }

            let Some(RangedToken { token: Token::String(_), range: _ }) = self.peek() else {
                break
            };

            let Some(RangedToken { token: Token::String(string), range }) = self.next() else { unreachable!() };
            let string = RString::new(string, range);
            from.push(string);
        }

        // ::{x, .., x as x, ..};
        let Some(RangedToken { token: Token::Bracket(whats, BracketType::Curly), range: _ }) = self.next() else {
            return Err((ExpectedEnum::Name | ExpectedEnum::CurlyBracket).err())
        };

        let mut what = Vec::with_capacity(whats.len());
        let mut state = ParsingState::new(whats);
        while !state.at_end() {
            let Some(RangedToken { token: Token::String(name), range }) = state.next() else {
                return Err(ExpectedEnum::Name.err())
            };
            let name = RString::new(name, range);

            if state.peek().is_none() { break }
            let Some(RangedToken { token, range: _ }) = state.next() else {
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
            let as_name = RString::new(as_name, range);
            what.push((name, Some(as_name)));

            if state.at_end() { break }
            let Some(RangedToken { token: Token::Comma, range: _ }) = state.next() else {
                return Err(ExpectedEnum::Comma.err())
            };
        }
        let Some(RangedToken { token: Token::Semicolon, range: _ }) = self.next() else {
            return Err(ExpectedEnum::Semicolon.err())
        };

        Ok(Statement::new_import(from, what).add_range(range_st))
    }
    fn parse_triple_dot(&mut self) -> Result<(), SyntacticError> {
        let Some(RangedToken { token: Token::DoubleDot, range: _ }) = self.next() else {
            unreachable!()
        };
        let Some(RangedToken { token: Token::Dot, range: _ }) = self.next() else {
            return Err(ExpectedEnum::new_string("...").err())
        };
        Ok(())
    }
}

fn parse_function_declaration_arguments(args: Vec<RangedToken>) -> Result<Vec<(RString, RTypee)>, SyntacticError> {
    if args.is_empty() {
        return Ok(Vec::new())
    }
    let mut arguments = Vec::with_capacity(args.len().div_ceil(2));
    let mut state = ParsingState::new(args);

    while let Some(RangedToken { token, range }) = state.next() {
        let Token::String(arg_i) = token else {
            return Err(ExpectedEnum::Name.err());
        };
        let arg_i = RString::new(arg_i, range);

        let Some(RangedToken { token: Token::Colon, range }) = state.next() else {
            return Err(ExpectedEnum::Semicolon.err())
        };

        let argument_type = state.parse_type(range)?;

        arguments.push((arg_i, argument_type));

        let Some(RangedToken { token, range: _ }) = state.next() else {
            break;
        };
        if token != Token::Comma {
            return Err(ExpectedEnum::Comma.err());
        }
    }

    Ok(arguments)
}
fn parse_extern_function_arguments(args: Vec<RangedToken>) -> Result<(Vec<RTypee>, bool), SyntacticError> {
    if args.is_empty() {
        return Ok((Vec::new(), false))
    }
    let mut arguments = Vec::with_capacity(args.len().div_ceil(2));
    let mut state = ParsingState::new(args);

    let mut range0 = Range::default();
    while !state.at_end() {
        if matches!(state.peek(), Some(RangedToken { token: Token::DoubleDot, .. })) {
            state.parse_triple_dot()?;

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

fn parse_function_arguments(tokens: Vec<RangedToken>) -> Result<Vec<RExpression>, SyntacticError> {
    let mut state = ParsingState::new(tokens);
    let mut args = Vec::new();

    while !state.at_end() {
        let expression = state.parse_expression(false)?;
        args.push(expression);

        if !state.at_end() {
            let RangedToken { token, range: _ } = state.next().unwrap();
            if token != Token::Comma {
                return Err(ExpectedEnum::Comma.err());
            }
        }
    }

    Ok(args)
}

fn parse_struct_construction(tokens: Vec<RangedToken>) -> Result<Vec<(RString, RExpression)>, SyntacticError> {
    let mut fields = Vec::new();
    let mut state = ParsingState::new(tokens);

    while !state.at_end() {
        let Some(RangedToken { token: Token::String(field_name), range }) = state.next() else {
            return Err(ExpectedEnum::Name.err())
        };
        let field_name = RString::new(field_name, range);
        let Some(RangedToken { token: Token::Colon, range: _ }) = state.next() else {
            return Err(ExpectedEnum::Colon.err())
        };
        let field_value = state.parse_expression(false)?;
        fields.push((field_name, field_value));
        if state.at_end() {
            break
        }

        let Some(RangedToken { token: Token::Comma, range: _ }) = state.next() else {
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
