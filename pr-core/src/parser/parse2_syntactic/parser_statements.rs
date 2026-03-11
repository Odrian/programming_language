use std::collections::VecDeque;
use lsp_types::Range;
use crate::error::{Diagnostic, ErrorQueue};
use crate::parser::BracketType;
use crate::parser::operations::{BoolOperation, CompareOperator, NumberOperation, OneSidedOperation, RTwoSidedOperation, TwoSidedOperation};
use crate::parser::parse1_tokenize::token::*;
use crate::RString;
use super::statement::*;
use super::error::{ExpectedEnum, SyntacticError};

pub fn parse_statements(errors: &mut ErrorQueue, tokens: Vec<RangedToken>) -> Vec<RStatement> {
    let unused_range = Range::default();
    ParsingState::new(errors, tokens, unused_range).parse_statements_inplace(true)
}

struct ParsingState<'a> {
    errors: &'a mut ErrorQueue,
    tokens: VecDeque<RangedToken>,
    all_range: Range,
}

impl ParsingState<'_> {
    fn new(errors: &mut ErrorQueue, tokens: Vec<RangedToken>, all_range: Range) -> ParsingState<'_> {
        ParsingState { errors, tokens: tokens.into(), all_range }
    }
    fn new_state(&mut self, tokens: Vec<RangedToken>, all_range: Range) -> ParsingState<'_> {
        ParsingState { errors: self.errors, tokens: tokens.into(), all_range }
    }
    fn add_diag(&mut self, diagnostic: Diagnostic) {
        self.errors.add_diag(diagnostic)
    }
    #[must_use]
    fn next(&mut self) -> Option<RangedToken> {
        self.tokens.pop_front()
    }
    fn next_unwrap(&mut self, gen_diag: impl Fn(&mut ParsingState) -> Diagnostic) -> Result<(Token, Range), ()> {
        let Some(ranged_token) = self.next() else {
            let diag = gen_diag(self);
            self.errors.add_diag(diag);
            return Err(())
        };
        let RangedToken { token, range } = ranged_token;
        Ok((token, range))
    }

    fn peek(&self) -> Option<&RangedToken> {
        self.tokens.front()
    }
    fn at_end(&self) -> bool {
        self.tokens.is_empty()
    }
    fn parse_statements(&mut self, vec: Vec<RangedToken>, range: Range, is_global: bool) -> Vec<RStatement> {
        let mut state = self.new_state(vec, range);
        state.parse_statements_inplace(is_global)
    }
    fn parse_statements_inplace(&mut self, is_global: bool) -> Vec<RStatement> {
        let mut statements = Vec::new();

        self.skip_semicolons();

        while !self.at_end() {
            let _ = self.parse_statement(is_global, &mut statements);

            self.skip_semicolons();
        }

        statements
    }
    fn skip_semicolons(&mut self) {
        while let Some(RangedToken { token: Token::Semicolon, range: _ }) = self.peek() {
            let _ = self.next();
        }
    }
    fn parse_statement(&mut self, is_global: bool, result: &mut Vec<RStatement>) -> Result<(), ()> {
        let (token, range0) = self.next_unwrap(|_| unreachable!())?;
        match token {
            Token::Keyword(keyword) => match keyword {
                TokenKeyword::If | TokenKeyword::While => {
                    let condition = self.parse_expression(true)?;

                    let (token, range_br) = self.next_unwrap(|s|
                        ExpectedEnum::CurlyBracket.diagnostic(s.all_range))?;
                    let Token::Bracket(vec, BracketType::Curly) = token else {
                        self.add_diag(ExpectedEnum::CurlyBracket.diagnostic(range_br));
                        return Err(())
                    };
                    let body = self.parse_statements(vec, range_br, false);

                    let statement = if keyword == TokenKeyword::If {
                        Statement::new_if(condition, body)
                    } else {
                        Statement::new_while(condition, body)
                    };
                    result.push(statement.add_range(Range::new(range0.start, range_br.end)));
                    Ok(())
                }
                TokenKeyword::For => {
                    let (token, range) = self.next_unwrap(|s|
                        ExpectedEnum::Name.diagnostic_after(s.all_range))?;
                    let Token::String(name) = token else {
                        self.add_diag(ExpectedEnum::Name.diagnostic(range));
                        return Err(());
                    };
                    let name = RString::new(name, range);

                    let (token, range) = self.next_unwrap(|s|
                        ExpectedEnum::new_string("'in'").diagnostic_after(s.all_range))?;
                    if !matches!(token, Token::String(s) if s == "in") {
                        self.add_diag(ExpectedEnum::new_string("'in'").diagnostic(range));
                        return Err(());
                    }

                    let from_value = self.parse_expression_without_ops(true, true)?;

                    let (token, range) = self.next_unwrap(|s|
                        ExpectedEnum::new_string("..").diagnostic_after(s.all_range))?;
                    if token != Token::DoubleDot {
                        self.add_diag(ExpectedEnum::new_string("..").diagnostic(range));
                        return Err(());
                    };

                    let compare_op: TwoSidedOperation = if let Some(
                        RangedToken { token: Token::EqualOperation(EqualOperation::Equal), range: _ }
                    ) = self.peek()
                    {
                        let _ = self.next();
                        CompareOperator::LessEqual.into()
                    } else {
                        CompareOperator::Less.into()
                    };

                    let to_value = self.parse_expression_without_ops(true, true)?;

                    let (token, range) = self.next_unwrap(|s|
                        ExpectedEnum::CurlyBracket.diagnostic_after(s.all_range))?;
                    let Token::Bracket(body, BracketType::Curly) = token else {
                        self.add_diag(ExpectedEnum::CurlyBracket.diagnostic(range));
                        return Err(());
                    };

                    let body = self.parse_statements(body, range, false);

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
                    if !is_global {
                        self.add_diag(SyntacticError::new_local_global("import", range0));
                        let _  = self.parse_import(range0, &mut Vec::new());
                        return Err(())
                    }
                    self.parse_import(range0, result)?;
                    Ok(())
                },
                TokenKeyword::Extern => {
                    if !is_global {
                        self.add_diag(SyntacticError::new_local_global("extern", range0));
                        return Err(())
                    }

                    if matches!(self.peek(), Some(RangedToken { token: Token::Bracket(_, BracketType::Curly), range: _ })) {
                        let Some(RangedToken { token: Token::Bracket(vec, _), range }) = self.next() else { unreachable!() };

                        let mut state = self.new_state(vec, range);
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
                self.parse_statement2(string, result, is_global)
            }
            Token::Operation(TwoSidedOperation::Number(NumberOperation::Mul)) => { // *..
                let left_expression = self.parse_expression_without_ops(true, false)?;
                let op = OneSidedOperation::Dereference.add_range(range0);
                let left_expression = new_unary_expr(left_expression, op);
                self.parse_statement3(result, left_expression)
            }
            Token::Bracket(body, BracketType::Curly) => {
                if !is_global {
                    self.add_diag(SyntacticError::from_text("unexpected global brackets", range0));
                    let _ = self.parse_statements(body, range0, false);
                    return Err(())
                }

                let body = self.parse_statements(body, range0, false);
                result.push(Statement::new_brackets(body).add_range(range0));
                Ok(())
            }
            Token::Semicolon => unreachable!(),
            _ => {
                self.add_diag((ExpectedEnum::new_string("keyword") | ExpectedEnum::Name |
                    ExpectedEnum::new_string("*") | ExpectedEnum::Semicolon)
                    .diagnostic(range0));
                Err(())
            }
        }
    }
    /// parse "name .." as definition
    fn parse_statement2(&mut self, name: RString, result: &mut Vec<RStatement>, is_global: bool) -> Result<(), ()> {
        let Some(RangedToken { token, range: _ }) = self.peek() else {
            self.add_diag(SyntacticError::from_text("unexpected EOF", self.all_range));
            return Err(());
        };
        match token {
            Token::DoubleColon => {
                // name ::
                let (_, range) = self.next_unwrap(|_| unreachable!())?;

                let Some(RangedToken { token, range }) = self.peek() else {
                    self.add_diag((ExpectedEnum::RoundBracket | ExpectedEnum::new_string("struct"))
                        .diagnostic(range));
                    return Err(())
                };
                match token {
                    Token::Bracket(_, BracketType::Round) => {
                        if !is_global {
                            self.add_diag(SyntacticError::new_local_global("function", name.range));
                            let _ = self.parse_function(name, &mut Vec::new());
                            return Err(())
                        }

                        self.parse_function(name, result)
                    },
                    Token::String(string) if string == "struct" => {
                        if !is_global {
                            self.add_diag(SyntacticError::new_local_global("struct", name.range));
                            return Err(());
                        }

                        let _ = self.next();
                        self.parse_struct(name, result)
                    }
                    _ => {
                        self.add_diag((ExpectedEnum::RoundBracket | ExpectedEnum::new_string("struct"))
                            .diagnostic(*range));
                        Err(())
                    }
                }
            },
            Token::Colon => {
                // name :
                let (_, range) = self.next_unwrap(|_| unreachable!())?;
                let typee = self.parse_type(range)?;

                let (token, _) = self.next_unwrap(|s|
                    (ExpectedEnum::Equal | ExpectedEnum::Semicolon).diagnostic(s.all_range))?;
                if token == Token::Semicolon {
                    let expression: Expression = LiteralExpression::Undefined.into();
                    let expression = expression.add_no_range();
                    let range_st = Range::new(name.range.start, expression.range.end);
                    let statement = Statement::new_variable(name, Some(typee), expression);
                    result.push(statement.add_range(range_st));
                    return Ok(());
                }
                if token != Token::EqualOperation(EqualOperation::Equal) {
                    self.add_diag((ExpectedEnum::Equal | ExpectedEnum::Semicolon).diagnostic(range));
                    return Err(());
                }

                let expression = self.parse_expression(false)?;
                let range_st = Range::new(name.range.start, expression.range.end);
                let statement = Statement::new_variable(name, Some(typee), expression);
                result.push(statement.add_range(range_st));
                Ok(())
            },
            Token::EqualOperation(EqualOperation::ColonEqual) => {
                let _ = self.next();

                let expression2 = self.parse_expression(false)?;
                let range_st = Range::new(name.range.start, expression2.range.end);
                let statement = Statement::new_variable(name, None, expression2);
                result.push(statement.add_range(range_st));
                Ok(())
            },
            Token::Bracket(_, BracketType::Round) => {
                // name(..)
                let Some(RangedToken { token: _, range: range_bracket }) = self.peek() else { unreachable!() };

                let range_call = Range::new(name.range.start, range_bracket.end);

                let args = self.parse_function_arguments()?;
                let left = Expression::new_function_call(name, args).add_range(range_call);
                let expression = self.parse_expression2_without_ops(left, false, false)?;
                let statement = Statement::Expression(expression.value).add_range(expression.range);
                result.push(statement);
                Ok(())
            }
            _ => {
                let range = name.range;
                let left = Expression::Variable(name).add_range(range);
                let left = self.parse_expression2_without_ops(left, true, false)?;
                self.parse_statement3(result, left)
            }
        }
    }
    /// parse left .. ;
    fn parse_statement3(&mut self, result: &mut Vec<RStatement>, left: RExpression) -> Result<(), ()> {
        let (token, range0) = self.next_unwrap(|s|
            SyntacticError::from_text("unexpected EOF", s.all_range))?;

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
                result.push(statement.add_range(range_st));
                Ok(())
            }
            Token::Semicolon => {
                result.push(Statement::Expression(left.value).add_range(left.range));
                Ok(())
            }
            _ => {
                self.add_diag((ExpectedEnum::DoubleColon | ExpectedEnum::Colon |
                    ExpectedEnum::Equal | ExpectedEnum::RoundBracket | ExpectedEnum::Semicolon)
                    .diagnostic(range0));
                Err(())
            }
        }
    }

    fn parse_expression(&mut self, in_cond: bool) -> Result<RExpression, ()> {
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

            let (token, range) = self.next_unwrap(|_| unreachable!())?;
            let Token::Operation(op) = token else {
                self.add_diag((
                    ExpectedEnum::new_string("operation") | ExpectedEnum::Equal | ExpectedEnum::Semicolon |
                        ExpectedEnum::Comma | ExpectedEnum::RoundBracket)
                    .diagnostic(range));
                return Err(())
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
    ) -> Result<RExpression, ()> {
        let (token, range) = self.next_unwrap(|s|
            SyntacticError::from_text("unexpected EOF", s.all_range))?;

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
                let mut new_state = self.new_state(vec, range);
                let expression = new_state.parse_expression(false)?;
                if !new_state.at_end() {
                    let (_, range) = new_state.next_unwrap(|_| unreachable!())?;
                    new_state.add_diag(ExpectedEnum::new_string(")").diagnostic(range))
                }
                let expression1 = Expression::new_round_bracket(expression).add_range(range);
                self.parse_expression2_without_ops(expression1, was_unary, in_cond)
            }
            Token::Operation(TwoSidedOperation::Number(NumberOperation::Sub)) => { // -..
                if let Some(RangedToken { token: Token::Operation(TwoSidedOperation::Number(NumberOperation::Sub)), range: _ }) = self.peek() {
                    let _ = self.next();
                    let Some(RangedToken { token: Token::Operation(TwoSidedOperation::Number(NumberOperation::Sub)), range: range2 }) = self.next() else {
                        self.add_diag(ExpectedEnum::new_string("---").diagnostic(range));
                        return Err(())
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
                let char_value = self.parse_quotes(&string, range);
                let expression = Expression::from(LiteralExpression::CharLiteral(char_value)).add_range(range);
                self.parse_expression2_without_ops(expression, was_unary, in_cond)
            }
            Token::DoubleQuotes(string) => { // ".."
                let expression = Expression::from(LiteralExpression::StringLiteral(string)).add_range(range);
                self.parse_expression2_without_ops(expression, was_unary, in_cond)
            }
            _ => {
                self.add_diag(ExpectedEnum::new_string(&format!("expression, got {token:?}"))
                    .diagnostic(range));
                Err(())
            }
        }
    }
    // parse "expression .."
    fn parse_expression2_without_ops(
        &mut self,
        expression1: RExpression,
        was_unary: bool,
        in_cond: bool
    ) -> Result<RExpression, ()> {
        let Some(RangedToken { token, range }) = self.peek() else {
            return Ok(expression1);
        };
        match token {
            Token::Operation(_) => { // exp +
                Ok(expression1)
            }
            Token::Bracket(_, BracketType::Round) => { // exp(..)
                let range = Range::new(expression1.range.start, range.end);

                // FIXME: allow function variables
                let Expression::Variable(name) = expression1.value else {
                    self.add_diag(SyntacticError::from_text(
                        "unexpected round brackets after expression", expression1.range));
                    return Err(())
                };

                let args = self.parse_function_arguments()?;
                let expression = Expression::new_function_call(name, args).add_range(range);
                self.parse_expression2_without_ops(expression, was_unary, false)
            }
            Token::String(string) if string == "as" => { // exp as
                if was_unary {
                    return Ok(expression1)
                }

                let (_, range) = self.next_unwrap(|_| unreachable!())?;

                let typee = self.parse_type(range)?;
                let range = Range::new(expression1.range.start, typee.range.end);
                let expression = Expression::new_as(expression1, typee).add_range(range);
                self.parse_expression2_without_ops(expression, false, in_cond)
            }
            Token::Dot => {
                let dot_range = *range;
                let _ = self.next();
                let (token, range) = self.next_unwrap(|s|
                    SyntacticError::from_text("unexpected dot operator", s.all_range))?;

                match token {
                    Token::String(field_name) => {
                        let field_name = RString::new(field_name, range);

                        let range = Range::new(expression1.range.start, field_name.range.end);
                        let expression = Expression::new_dot(expression1, field_name).add_range(range);
                        self.parse_expression2_without_ops(expression, was_unary, in_cond)
                    }
                    _ => {
                        self.add_diag(SyntacticError::from_text("unexpected dot operator", dot_range));
                        Err(())
                    }
                }
            }
            Token::Bracket(_, BracketType::Curly) if !in_cond && matches!(&expression1.value, Expression::Variable(..)) => {
                // name { ... }
                let Expression::Variable(name) = expression1.value else { unreachable!() };
                let range_st = Range::new(name.range.start, range.end);

                let fields = self.parse_struct_construction()?;

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
                let (token, range) = self.next_unwrap(|_| unreachable!())?;
                self.add_diag(SyntacticError::from_text(&format!("unexpected token {token:?}"), range));
                Err(())
            }
        }
    }

    fn parse_function(&mut self, name: RString, result: &mut Vec<RStatement>) -> Result<(), ()> {
        // parse arguments
        let arguments = self.parse_function_declaration_arguments()?;

        // parse return type
        let (mut token, mut range) = self.next_unwrap(|s|
            (ExpectedEnum::Arrow | ExpectedEnum::CurlyBracket).diagnostic_after(s.all_range))?;

        let return_type = {
            if token == Token::Arrow {
                let return_type = self.parse_type(range)?;

                (token, range) = self.next_unwrap(|s|
                    ExpectedEnum::CurlyBracket.diagnostic_after(s.all_range))?;
                Some(return_type)
            } else {
                None
            }
        };

        // parse inside
        let Token::Bracket(body, BracketType::Curly) = token else {
            self.add_diag(ExpectedEnum::CurlyBracket.diagnostic(range));
            return Err(())
        };
        let body = self.parse_statements(body, range, false);

        let range = name.range;
        result.push(Statement::new_function(name, arguments, return_type, body)
            .add_range(range));
        Ok(())
    }

    fn parse_type(&mut self, range0: Range) -> Result<RTypee, ()> {
        let (token, range) = self.next_unwrap(|_|
            ExpectedEnum::new_string("type").diagnostic_after(range0))?;

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
            _ => {
                self.add_diag(ExpectedEnum::new_string("type").diagnostic(range));
                Err(())
            }
        }
    }

    fn parse_struct(&mut self, name: RString, result: &mut Vec<RStatement>) -> Result<(), ()> {
        let (token, range) = self.next_unwrap(|s|
            ExpectedEnum::CurlyBracket.diagnostic_after(s.all_range))?;
        let Token::Bracket(tokens, BracketType::Curly) = token else {
            self.add_diag(ExpectedEnum::CurlyBracket.diagnostic(range));
            return Err(())
        };

        let mut fields = Vec::new();
        let mut state = self.new_state(tokens, range);
        while !state.at_end() {
            // name
            let (token, range) = state.next_unwrap(|_| unreachable!())?;
            let Token::String(field_name) = token else {
                state.add_diag(ExpectedEnum::Name.diagnostic(range));
                return Err(())
            };
            let field_name = RString::new(field_name, range);

            // :
            let (token, range) = state.next_unwrap(|s|
                ExpectedEnum::Colon.diagnostic_after(s.all_range))?;
            if token != Token::Colon {
                state.add_diag(ExpectedEnum::Colon.diagnostic(range));
                return Err(())
            }

            // typee
            let field_typee = state.parse_type(range)?;
            fields.push((field_name, field_typee));

            if state.at_end() { break }

            // ,
            let (token, range) = state.next_unwrap(|s|
                ExpectedEnum::Comma.diagnostic_after(s.all_range))?;
            if token != Token::Comma {
                state.add_diag(ExpectedEnum::Comma.diagnostic(range));
                return Err(())
            }
        }

        let range_name = name.range;
        result.push(Statement::new_struct(name, fields).add_range(range_name));
        Ok(())
    }
    fn parse_extern(&mut self, range0: Range) -> Result<RStatement, ()> {
        let (token, range) = self.next_unwrap(|_|
            ExpectedEnum::Name.diagnostic_after(range0))?;
        let Token::String(name) = token else {
            self.add_diag(ExpectedEnum::Name.diagnostic(range));
            return Err(());
        };
        let name = RString::new(name, range);

        let (token, range) = self.next_unwrap(|s|
            (ExpectedEnum::Colon | ExpectedEnum::DoubleColon).diagnostic_after(s.all_range))?;

        if token == Token::Colon {
            // name : ..
            let typee = self.parse_type(range)?;

            let (token, range) = self.next_unwrap(|s|
                ExpectedEnum::Colon.diagnostic_after(s.all_range))?;
            if token != Token::Semicolon {
                self.add_diag(ExpectedEnum::Colon.diagnostic(range));
                return Err(())
            };

            let range_st = name.range;
            let statement = Statement::new_extern(ExternStatement::Variable { name, typee });
            Ok(statement.add_range(range_st))
        } else if token == Token::DoubleColon {
            // name :: ..
            let (args, is_vararg) = self.parse_extern_function_arguments()?;

            let (token, range) = self.next_unwrap(|s|
                ExpectedEnum::Semicolon.diagnostic_after(s.all_range))?;

            if token == Token::Semicolon {
                let range_st = name.range;
                let statement = Statement::new_extern(ExternStatement::Function { name, args, is_vararg, returns: None });
                Ok(statement.add_range(range_st))
            } else if token == Token::Arrow {
                let returns = self.parse_type(range)?;

                let (token, range) = self.next_unwrap(|s|
                    ExpectedEnum::Semicolon.diagnostic_after(s.all_range))?;
                if token != Token::Semicolon {
                    self.add_diag(ExpectedEnum::Semicolon.diagnostic(range));
                    return Err(())
                }

                let range_st = name.range;
                let statement = Statement::new_extern(ExternStatement::Function { name, args, is_vararg, returns: Some(returns) });
                Ok(statement.add_range(range_st))
            } else {
                self.add_diag((ExpectedEnum::Arrow | ExpectedEnum::Semicolon).diagnostic(range));
                Err(())
            }
        } else {
            self.add_diag((ExpectedEnum::Colon | ExpectedEnum::DoubleColon).diagnostic(range));
            Err(())
        }
    }
    fn parse_import(&mut self, range_st: Range, result: &mut Vec<RStatement>) -> Result<(), ()> {
        let (token, range) = self.next_unwrap(|s|
            ExpectedEnum::Name.diagnostic(s.all_range))?;
        let Token::String(string) = token else {
            self.add_diag(ExpectedEnum::Name.diagnostic(range));
            return Err(())
        };

        let mut from: Vec<RString> = vec![RString::new(string, range)];
        loop {
            let (token, range) = self.next_unwrap(|s|
                (ExpectedEnum::DoubleColon | ExpectedEnum::Semicolon | ExpectedEnum::As)
                    .diagnostic(s.all_range))?;

            match token {
                Token::DoubleColon => {}
                Token::Semicolon => { // ::x;
                    let what = vec![(from.pop().unwrap(), None)];
                    result.push(Statement::new_import(from, what).add_range(range_st));
                    return Ok(())
                }
                Token::String(as_string) if as_string == "as" => { // ::x as x;
                    let (token, range) = self.next_unwrap(|s|
                        ExpectedEnum::Name.diagnostic(s.all_range))?;
                    let Token::String(as_name) = token else {
                        self.add_diag(ExpectedEnum::Name.diagnostic(range));
                        return Err(())
                    };
                    let as_name = RString::new(as_name, range);

                    let (token, range) = self.next_unwrap(|s|
                        ExpectedEnum::Semicolon.diagnostic(s.all_range))?;
                    if token != Token::Semicolon {
                        self.add_diag(ExpectedEnum::Semicolon.diagnostic(range));
                        return Err(())
                    };

                    let what = vec![(from.pop().unwrap(), Some(as_name))];
                    result.push(Statement::new_import(from, what).add_range(range_st));
                    return Ok(())
                }
                _ => {
                    self.add_diag((ExpectedEnum::DoubleColon | ExpectedEnum::Semicolon | ExpectedEnum::As)
                        .diagnostic(range));
                    return Err(())
                }
            }

            let Some(RangedToken { token: Token::String(_), range: _ }) = self.peek() else {
                break
            };

            let Some(RangedToken { token: Token::String(string), range }) = self.next() else { unreachable!() };
            let string = RString::new(string, range);
            from.push(string);
        }

        // ::{x, .., x as x, ..};
        let (token, range) = self.next_unwrap(|s|
            (ExpectedEnum::Name | ExpectedEnum::CurlyBracket).diagnostic(s.all_range))?;
        let Token::Bracket(whats, BracketType::Curly) = token else {
            self.add_diag((ExpectedEnum::Name | ExpectedEnum::CurlyBracket).diagnostic(range));
            return Err(())
        };

        let mut what = Vec::with_capacity(whats.len());
        let mut state = self.new_state(whats, range);
        while !state.at_end() {
            let (token, range) = state.next_unwrap(|_| unreachable!())?;
            let Token::String(name) = token else {
                state.add_diag(ExpectedEnum::Name.diagnostic(range));
                return Err(())
            };
            let name = RString::new(name, range);

            if state.peek().is_none() { break }
            let (token, range) = state.next_unwrap(|_| unreachable!())?;
            if token == Token::Comma {
                what.push((name, None));
                continue
            }
            if !matches!(token, Token::String(as_string) if as_string == "as") {
                state.add_diag((ExpectedEnum::Comma | ExpectedEnum::As).diagnostic(range));
                return Err(())
            }

            let (token, range) = state.next_unwrap(|s|
                ExpectedEnum::Name.diagnostic(s.all_range))?;
            let Token::String(as_name) = token else {
                state.add_diag(ExpectedEnum::Name.diagnostic(range));
                return Err(())
            };
            let as_name = RString::new(as_name, range);
            what.push((name, Some(as_name)));

            if state.at_end() { break }
            let (token, range) = state.next_unwrap(|_| unreachable!())?;
            if token != Token::Comma {
                state.add_diag(ExpectedEnum::Comma.diagnostic(range));
            }
        }
        let (token, range) = self.next_unwrap(|s|
            ExpectedEnum::Semicolon.diagnostic(s.all_range))?;
        if token != Token::Semicolon {
            self.add_diag(ExpectedEnum::Semicolon.diagnostic(range));
        };

        result.push(Statement::new_import(from, what).add_range(range_st));
        Ok(())
    }
    /// expects `..` and parse next `.` so have `...`
    fn parse_triple_dot(&mut self) -> Result<(), ()> {
        let Some(RangedToken { token: Token::DoubleDot, range: _ }) = self.next() else {
            unreachable!()
        };
        let (token, range) = self.next_unwrap(|s|
            ExpectedEnum::new_string("...").diagnostic_after(s.all_range))?;
        if token != Token::Dot {
            self.add_diag(ExpectedEnum::new_string("...").diagnostic(range));
        }

        Ok(())
    }
    /// parse `(...)`
    fn parse_function_declaration_arguments(&mut self) -> Result<Vec<(RString, RTypee)>, ()> {
        let (token, range) = self.next_unwrap(|s|
            ExpectedEnum::RoundBracket.diagnostic_after(s.all_range))?;
        let Token::Bracket(args, BracketType::Round) = token else {
            self.add_diag(ExpectedEnum::RoundBracket.diagnostic(range));
            return Err(())
        };

        if args.is_empty() {
            return Ok(Vec::new())
        }
        let mut arguments = Vec::with_capacity(args.len().div_ceil(2));
        let mut state = self.new_state(args, range);

        while !state.at_end() {
            let (token, range) = state.next_unwrap(|_| unreachable!())?;
            let Token::String(arg_i) = token else {
                state.add_diag(ExpectedEnum::Name.diagnostic(range));
                return Err(());
            };
            let arg_i = RString::new(arg_i, range);

            let (token, range) = state.next_unwrap(|s|
                ExpectedEnum::Colon.diagnostic_after(s.all_range))?;
            if token != Token::Colon {
                ExpectedEnum::Colon.diagnostic(range);
                return Err(())
            }

            let argument_type = state.parse_type(range)?;

            arguments.push((arg_i, argument_type));

            let Some(RangedToken { token, range }) = state.next() else {
                break;
            };
            if token != Token::Comma {
                state.add_diag(ExpectedEnum::Comma.diagnostic(range));
                return Err(());
            }
        }

        Ok(arguments)
    }
    /// parse `(...)`
    fn parse_extern_function_arguments(&mut self) -> Result<(Vec<RTypee>, bool), ()> {
        let (token, range) = self.next_unwrap(|s|
            ExpectedEnum::RoundBracket.diagnostic_after(s.all_range))?;
        let Token::Bracket(args, BracketType::Round) = token else {
            self.add_diag(ExpectedEnum::RoundBracket.diagnostic(range));
            return Err(())
        };

        if args.is_empty() {
            return Ok((Vec::new(), false))
        }
        let mut arguments = Vec::with_capacity(args.len().div_ceil(2));
        let mut state = self.new_state(args, range);

        let mut range0 = Range::default();
        while !state.at_end() {
            if matches!(state.peek(), Some(RangedToken { token: Token::DoubleDot, .. })) {
                state.parse_triple_dot()?;

                if !state.at_end() {
                    let (_, range) = state.next_unwrap(|_| unreachable!())?;
                    state.add_diag(ExpectedEnum::new_string(")").diagnostic(range));
                    return Err(());
                }
                return Ok((arguments, true))
            }

            let argument_type = state.parse_type(range0)?;

            arguments.push(argument_type);

            let Some(RangedToken { token, range }) = state.next() else {
                break;
            };
            if token != Token::Comma {
                state.add_diag(ExpectedEnum::Comma.diagnostic(range));
                return Err(())
            }
            range0 = range
        }

        Ok((arguments, false))
    }

    /// expect token `(...)`
    fn parse_function_arguments(&mut self) -> Result<Vec<RExpression>, ()> {
        let (token, range_bracket) = self.next_unwrap(|_| unreachable!())?;
        let Token::Bracket(vec, _) = token else { unreachable!() };

        let mut state = self.new_state(vec, range_bracket);
        let mut args = Vec::new();

        while !state.at_end() {
            let expression = state.parse_expression(false)?;
            args.push(expression);

            if !state.at_end() {
                let (token, range) = state.next_unwrap(|_| unreachable!())?;
                if token != Token::Comma {
                    state.add_diag(ExpectedEnum::Comma.diagnostic(range));
                    return Err(())
                }
            }
        }

        Ok(args)
    }

    fn parse_struct_construction(&mut self) -> Result<Vec<(RString, RExpression)>, ()> {
        let (token, range0) = self.next_unwrap(|_| unreachable!())?;
        let Token::Bracket(vec, _) = token else { unreachable!() };

        let mut fields = Vec::new();
        let mut state = self.new_state(vec, range0);

        while !state.at_end() {
            let (token, range) = state.next_unwrap(|_| unreachable!())?;

            let Token::String(field_name) = token else {
                state.add_diag(ExpectedEnum::Name.diagnostic(range));
                return Err(())
            };
            let field_name = RString::new(field_name, range);

            let (token, range) = state.next_unwrap(|s|
                ExpectedEnum::Colon.diagnostic_after(s.all_range))?;
            if token != Token::Colon {
                state.add_diag(ExpectedEnum::Colon.diagnostic(range));
                return Err(())
            }

            let field_value = state.parse_expression(false)?;
            fields.push((field_name, field_value));
            if state.at_end() {
                break
            }

            let (token, range) = state.next_unwrap(|_| unreachable!())?;
            if token != Token::Comma {
                state.add_diag(ExpectedEnum::Comma.diagnostic(range));
                return Err(())
            }
        }

        Ok(fields)
    }

    fn parse_quotes(&mut self, string: &str, range: Range) -> u8 {
        let mut chars = string.chars();
        let Some(first_char) = chars.next() else {
            self.add_diag(SyntacticError::from_text("empty char literal", range));
            return '?' as u8;
        };

        if chars.next().is_some() {
            self.add_diag(SyntacticError::from_text(">1 char in literal", range));
            return '?' as u8;
        }
        if !first_char.is_ascii() {
            self.add_diag(SyntacticError::from_text("not ascii char in literal", range));
            return '?' as u8;
        }
        let char_value = first_char as u8;
        char_value
    }
}
