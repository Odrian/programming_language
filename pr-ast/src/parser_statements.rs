use lsp_types::Range;
use pr_common::error::{Diagnostic, ErrorQueue};
use pr_common::BracketType;
use pr_common::operations::*;
use pr_common::ranged::RString;
use pr_tokenize::token::*;
use pr_tokenize::{TokenLinearTree, TokenIntoIter};
use crate::statement::*;
use crate::error::{ExpectedEnum, SyntacticError};

pub fn parse_statements(errors: &mut ErrorQueue, tokens: TokenLinearTree) -> Vec<RStatement> {
    let unused_range = Range::default();
    let mut iter = tokens.into_iter();
    ParsingState::new(errors, iter.iterator(), unused_range).parse_statements_inplace(true)
}

type Node<'a> = pr_common::ranged_tree::Node<'a, Token, TokenBlock>;
type RefNode<'a> = pr_common::ranged_tree::RefNode<'a, Token, TokenBlock>;
type RangedToken<'a> = (Node<'a>, Range);
type RefRangedToken<'a> = (RefNode<'a>, &'a Range);

struct ParsingState<'e, 'a, 'b> {
    errors: &'e mut ErrorQueue,
    tokens: &'a mut TokenIntoIter<'b>,
    all_range: Range,
}

impl<'e, 'a, 'b> ParsingState<'e, 'a, 'b> {
    fn new(errors: &'e mut ErrorQueue, tokens: &'a mut TokenIntoIter<'b>, all_range: Range) -> ParsingState<'e, 'a, 'b> {
        ParsingState { errors, tokens, all_range }
    }
    fn new_state<'e2, 'a2, 'b2>(errors: &'e2 mut ErrorQueue, tokens: &'a2 mut TokenIntoIter<'b2>, all_range: Range) -> ParsingState<'e2, 'a2, 'b2> {
        ParsingState { errors, tokens, all_range }
    }
    fn add_diag(&mut self, diagnostic: Diagnostic) {
        self.errors.add_diag(diagnostic)
    }
    #[must_use]
    fn next(&mut self) -> Option<RangedToken<'b>> {
        self.tokens.next()
    }
    fn next_unwrap(&mut self, gen_diag: impl Fn(&mut ParsingState<'e, 'a, 'b>) -> Diagnostic) -> Result<RangedToken<'a>, ()> {
        let Some((token, range)) = self.tokens.next() else {
            let diag = gen_diag(self);
            self.errors.add_diag(diag);
            return Err(())
        };
        Ok((token, range))
    }

    fn peek(&self) -> Option<RefRangedToken<'b>> {
        self.tokens.peek()
    }
    fn at_end(&self) -> bool {
        self.tokens.is_empty()
    }
    fn parse_statements(&mut self, mut tokens: TokenIntoIter<'_>, range: Range, is_global: bool) -> Vec<RStatement> {
        let mut state = Self::new_state(self.errors, &mut tokens, range);
        state.parse_statements_inplace(is_global)
    }
    fn parse_statements_inplace(&mut self, is_global: bool) -> Vec<RStatement> {
        let mut statements = Vec::new();

        while !self.at_end() {
            let result = self.parse_statement(is_global, &mut statements);

            if result.is_err() {
                while let Some((token, _range)) = self.peek() && !matches!(token, RefNode::Elem(Token::Semicolon)) {
                    let _ = self.next();
                }
                let _ = self.next();
            }
        }

        statements
    }
    fn parse_statement(&mut self, is_global: bool, result: &mut Vec<RStatement>) -> Result<(), ()> {
        let (token, range0) = self.next_unwrap(|_| unreachable!())?;
        match token {
            Node::Elem(Token::Keyword(keyword)) => match keyword {
                TokenKeyword::If | TokenKeyword::While => {
                    let condition = self.parse_expression(true)?;

                    let (token, range_br) = self.next_unwrap(|s|
                        ExpectedEnum::CurlyBracket.diagnostic(s.all_range))?;
                    let Node::Block(BracketType::Curly, vec) = token else {
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
                    let Node::Elem(Token::String(name)) = token else {
                        self.add_diag(ExpectedEnum::Name.diagnostic(range));
                        return Err(());
                    };
                    let name = RString::new(name, range);

                    let (token, range) = self.next_unwrap(|s|
                        ExpectedEnum::In_KW.diagnostic_after(s.all_range))?;
                    if !matches!(token, Node::Elem(Token::String(s)) if s == "in") {
                        self.add_diag(ExpectedEnum::In_KW.diagnostic(range));
                        return Err(());
                    }

                    let from_value = self.parse_expression_without_ops(true, true)?;

                    let (token, range) = self.next_unwrap(|s|
                        ExpectedEnum::DoubleDot.diagnostic_after(s.all_range))?;
                    if !matches!(token, Node::Elem(Token::DoubleDot)) {
                        self.add_diag(ExpectedEnum::DoubleDot.diagnostic(range));
                        return Err(());
                    };

                    let compare_op: TwoSidedOperation = if let Some(
                        (RefNode::Elem(Token::EqualOperation(EqualOperation::Equal)), _range)
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
                    let Node::Block(BracketType::Curly, body) = token else {
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
                    } else if let Some((RefNode::Elem(Token::Semicolon), _)) = peek_token {
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
                        let _ = self.parse_import(range0, &mut Vec::new());
                        return Err(())
                    }
                    self.parse_import(range0, result)?;
                    Ok(())
                }
                TokenKeyword::Extern => {
                    if !is_global {
                        self.add_diag(SyntacticError::new_local_global("extern", range0));
                        return Err(())
                    }

                    if matches!(self.peek(), Some((RefNode::Block(BracketType::Curly, _), _))) {
                        let Some((Node::Block(BracketType::Curly, mut vec), range)) = self.next() else { unreachable!() };

                        let mut state = Self::new_state(self.errors, &mut vec, range);
                        while !state.at_end() {
                            if let Ok(ext) = state.parse_extern(range0) {
                                result.push(ext)
                            }
                        }

                        Ok(())
                    } else {
                        if let Ok(ext) = self.parse_extern(range0) {
                            result.push(ext)
                        }
                        Ok(())
                    }
                }
            },
            Node::Elem(Token::String(_)) => {
                let Node::Elem(Token::String(string)) = token else { unreachable!() };
                let string = RString::new(string, range0);
                self.parse_statement2(string, result, is_global)
            }
            Node::Elem(Token::Operation(TwoSidedOperation::Number(NumberOperation::Mul))) => {
                let left_expression = self.parse_expression_without_ops(true, false)?;
                let op = OneSidedOperation::Dereference.add_range(range0);
                let left_expression = new_unary_expr(left_expression, op);
                self.parse_statement3(result, left_expression)
            }
            Node::Block(BracketType::Curly, body) => {
                if !is_global {
                    self.add_diag(SyntacticError::from_text("unexpected global brackets", range0));
                    let _ = self.parse_statements(body, range0, false);
                    return Err(())
                }

                let body = self.parse_statements(body, range0, false);
                result.push(Statement::new_brackets(body).add_range(range0));
                Ok(())
            }
            Node::Elem(Token::Semicolon) => {
                self.add_diag(SyntacticError::unnecessary_semicolon(range0));
                Ok(())
            }
            Node::Elem(Token::NumberLiteral(token)) => {
                let expression1 = Expression::from(LiteralExpression::NumberLiteral(token)).add_range(range0);
                self.parse_statement3(result, expression1)
            }
            _ => {
                self.add_diag(ExpectedEnum::new_string("statement").diagnostic(range0));
                Err(())
            }
        }
    }
    /// parse `name ..` as definition
    fn parse_statement2(&mut self, name: RString, result: &mut Vec<RStatement>, is_global: bool) -> Result<(), ()> {
        let Some((token, _range)) = self.peek() else {
            self.add_diag(SyntacticError::from_text("unexpected EOF", self.all_range));
            return Err(());
        };
        match token {
            RefNode::Elem(Token::DoubleColon) => {
                // name ::
                let (_, range) = self.next_unwrap(|_| unreachable!())?;

                let Some((token, range)) = self.peek() else {
                    self.add_diag((ExpectedEnum::RoundBracket | ExpectedEnum::Struct_KW)
                        .diagnostic(range));
                    return Err(())
                };
                match token {
                    RefNode::Block(BracketType::Round, _) => {
                        if !is_global {
                            self.add_diag(SyntacticError::new_local_global("function", name.range));
                            let _ = self.parse_function(name, &mut Vec::new());
                            return Err(())
                        }

                        self.parse_function(name, result)
                    },
                    RefNode::Elem(Token::String(string)) if string == "struct" => {
                        if !is_global {
                            self.add_diag(SyntacticError::new_local_global("struct", name.range));
                            return Err(());
                        }

                        let _ = self.next();
                        self.parse_struct(name, result)
                    }
                    _ => {
                        self.add_diag((ExpectedEnum::RoundBracket | ExpectedEnum::Struct_KW)
                            .diagnostic(*range));
                        Err(())
                    }
                }
            },
            RefNode::Elem(Token::Colon) => {
                // name :
                let (_, range) = self.next_unwrap(|_| unreachable!())?;
                let typee = self.parse_type(range)?;

                let (token, _) = self.next_unwrap(|s|
                    (ExpectedEnum::Equal | ExpectedEnum::Semicolon).diagnostic(s.all_range))?;
                if matches!(token, Node::Elem(Token::Semicolon)) {
                    let expression: Expression = LiteralExpression::Undefined.into();
                    let expression = expression.add_no_range();
                    let range_st = Range::new(name.range.start, expression.range.end);
                    let statement = Statement::new_variable(name, Some(typee), expression);
                    result.push(statement.add_range(range_st));
                    return Ok(());
                }
                if !matches!(token, Node::Elem(Token::EqualOperation(EqualOperation::Equal))) {
                    self.add_diag((ExpectedEnum::Equal | ExpectedEnum::Semicolon).diagnostic(range));
                    return Err(());
                }

                let expression = self.parse_expression(false)?;
                self.consume_semicolon(expression.range);

                let range_st = Range::new(name.range.start, expression.range.end);
                let statement = Statement::new_variable(name, Some(typee), expression);
                result.push(statement.add_range(range_st));
                Ok(())
            },
            RefNode::Elem(Token::EqualOperation(EqualOperation::ColonEqual)) => {
                let _ = self.next();

                let expression2 = self.parse_expression(false)?;
                self.consume_semicolon(expression2.range);

                let range_st = Range::new(name.range.start, expression2.range.end);
                let statement = Statement::new_variable(name, None, expression2);
                result.push(statement.add_range(range_st));
                Ok(())
            },
            _ => {
                if is_global {
                    self.add_diag(SyntacticError::from_text("global expression", name.range));
                    return Err(());
                }

                let range = name.range;
                let left = Expression::Variable(name).add_range(range);
                let left = self.parse_expression2_without_ops(left, true, false)?;
                self.parse_statement3(result, left)
            }
        }
    }
    /// parse `left .. ;`
    fn parse_statement3(&mut self, result: &mut Vec<RStatement>, left: RExpression) -> Result<(), ()> {
        let Some((token, _range)) = self.peek() else {
            result.push(Statement::Expression(left.value).add_range(left.range));
            return Ok(())
        };

        match token {
            RefNode::Elem(Token::EqualOperation(..)) => {
                let Some((Node::Elem(Token::EqualOperation(equal_operation)), range0)) = self.next() else { unreachable!() };

                // name _=
                let expression2 = self.parse_expression(false)?;
                self.consume_semicolon(expression2.range);

                let range_st = Range::new(left.range.start, expression2.range.end);
                let statement = match equal_operation {
                    EqualOperation::ColonEqual => {
                        self.add_diag(SyntacticError::incorrect_variable_definition(range0));
                        return Err(())
                    }
                    EqualOperation::Equal => Statement::new_set(left, expression2, None),
                    EqualOperation::OperationEqual(op) => {
                        Statement::new_set(left, expression2, Some(op.add_range(range0)))
                    }
                };
                result.push(statement.add_range(range_st));
                Ok(())
            }

            RefNode::Block(BracketType::Round, _) => {
                // name(..)
                let Some((_, range_bracket)) = self.peek() else { unreachable!() };

                let Expression::Variable(name) = left.value else {
                    self.add_diag(SyntacticError::incorrect_call(left.range));
                    return Err(())
                };

                let range_call = Range::new(name.range.start, range_bracket.end);

                let args = self.parse_function_arguments()?;
                let left = Expression::new_function_call(name, args).add_range(range_call);
                let expression = self.parse_expression2_without_ops(left, false, false)?;
                self.consume_semicolon(expression.range);

                let statement = Statement::Expression(expression.value).add_range(expression.range);
                result.push(statement);
                Ok(())
            }
            _ => {
                self.consume_semicolon(left.range);
                let expression = Statement::Expression(left.value).add_range(left.range);
                result.push(expression);
                Ok(())
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

            let Some((token, _range)) = self.peek() else {
                break;
            };
            if !matches!(token, RefNode::Elem(Token::Operation(..))) {
                break
            }
            let (token, range) = self.next_unwrap(|_| unreachable!())?;
            let Node::Elem(Token::Operation(op)) = token else { unreachable!() };
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
            Node::Elem(Token::String(string)) => { // string
                let string = RString::new(string, range);
                let expression1 = match string.value.as_str() {
                    "true" => LiteralExpression::BoolLiteral(true).into(),
                    "false" => LiteralExpression::BoolLiteral(false).into(),
                    _ => Expression::Variable(string),
                }.add_range(range);
                self.parse_expression2_without_ops(expression1, was_unary, in_cond)
            }
            Node::Elem(Token::NumberLiteral(token)) => { // 123
                let expression1 = Expression::from(LiteralExpression::NumberLiteral(token)).add_range(range);
                self.parse_expression2_without_ops(expression1, was_unary, in_cond)
            }
            Node::Block(BracketType::Round, mut vec) => { // (..)
                let mut new_state = Self::new_state(self.errors, &mut vec, range);
                let expression = new_state.parse_expression(false)?;
                if !new_state.at_end() {
                    let (_, range) = new_state.next_unwrap(|_| unreachable!())?;
                    new_state.add_diag(ExpectedEnum::CloseRoundBracket.diagnostic(range))
                }
                let expression1 = Expression::new_round_bracket(expression).add_range(range);
                self.parse_expression2_without_ops(expression1, was_unary, in_cond)
            }
            Node::Elem(Token::Operation(TwoSidedOperation::Number(NumberOperation::Sub))) => { // -..
                if let Some((RefNode::Elem(Token::Operation(TwoSidedOperation::Number(NumberOperation::Sub))), _)) = self.peek() {
                    let _ = self.next();
                    let Some((Node::Elem(Token::Operation(TwoSidedOperation::Number(NumberOperation::Sub))), range2)) = self.next() else {
                        self.add_diag(ExpectedEnum::Undefined.diagnostic(range));
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
            Node::Elem(Token::Operation(TwoSidedOperation::Number(NumberOperation::Mul))) => { // *..
                let op = OneSidedOperation::Dereference.add_range(range);
                let expression = self.parse_expression_without_ops(true, in_cond)?;

                let unary_expression = new_unary_expr(expression, op);
                self.parse_expression2_without_ops(unary_expression, false, in_cond)
            }
            Node::Elem(Token::Operation(TwoSidedOperation::Number(NumberOperation::BitAnd))) => { // &..
                let op = OneSidedOperation::GetReference.add_range(range);
                let expression = self.parse_expression_without_ops(true, in_cond)?;

                let unary_expression = new_unary_expr(expression, op);
                self.parse_expression2_without_ops(unary_expression, false, in_cond)
            }
            Node::Elem(Token::Operation(TwoSidedOperation::Bool(BoolOperation::And))) => {
                let op = OneSidedOperation::GetReference.add_range(range);
                let expression = self.parse_expression_without_ops(true, in_cond)?;

                let unary_expression = new_unary_expr(expression, op.clone());
                let unary_expression2 = new_unary_expr(unary_expression, op);
                self.parse_expression2_without_ops(unary_expression2, false, in_cond)
            }
            Node::Elem(Token::UnaryOperation(op)) => { // `unary`..
                let op = op.add_range(range);
                let expression = self.parse_expression_without_ops(true, in_cond)?;
                let unary_expression = new_unary_expr(expression, op);
                self.parse_expression2_without_ops(unary_expression, false, in_cond)
            }
            Node::Elem(Token::Quotes(string)) => { // '..'
                let char_value = self.parse_quotes(&string, range);
                let expression = Expression::from(LiteralExpression::CharLiteral(char_value)).add_range(range);
                self.parse_expression2_without_ops(expression, was_unary, in_cond)
            }
            Node::Elem(Token::DoubleQuotes(string)) => { // ".."
                let expression = Expression::from(LiteralExpression::StringLiteral(string)).add_range(range);
                self.parse_expression2_without_ops(expression, was_unary, in_cond)
            }
            _ => {
                self.add_diag(ExpectedEnum::new_string("expression")
                    .diagnostic(range));
                Err(())
            }
        }
    }
    // parse `expression ..`
    fn parse_expression2_without_ops(
        &mut self,
        expression1: RExpression,
        was_unary: bool,
        in_cond: bool
    ) -> Result<RExpression, ()> {
        let Some((token, range)) = self.peek() else {
            return Ok(expression1);
        };
        match token {
            RefNode::Elem(Token::Operation(_)) => { // exp +
                Ok(expression1)
            }
            RefNode::Block(BracketType::Round, _) => { // exp(..)
                let range = Range::new(expression1.range.start, range.end);

                let Expression::Variable(name) = expression1.value else {
                    self.add_diag(SyntacticError::from_text(
                        "unexpected round brackets after expression", expression1.range));
                    return Err(())
                };

                let args = self.parse_function_arguments()?;
                let expression = Expression::new_function_call(name, args).add_range(range);
                self.parse_expression2_without_ops(expression, was_unary, false)
            }
            RefNode::Elem(Token::String(string)) if string == "as" => { // exp as
                if was_unary {
                    return Ok(expression1)
                }

                let (_, range) = self.next_unwrap(|_| unreachable!())?;

                let typee = self.parse_type(range)?;
                let range = Range::new(expression1.range.start, typee.range.end);
                let expression = Expression::new_as(expression1, typee).add_range(range);
                self.parse_expression2_without_ops(expression, false, in_cond)
            }
            RefNode::Elem(Token::Dot) => {
                let dot_range = *range;
                let _ = self.next();
                let (token, range) = self.next_unwrap(|s|
                    SyntacticError::from_text("unexpected dot operator", s.all_range))?;

                match token {
                    Node::Elem(Token::String(field_name)) => {
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
            RefNode::Block(BracketType::Curly, _) if !in_cond && matches!(&expression1.value, Expression::Variable(..)) => {
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
            _ => {
                Ok(expression1)
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
            if matches!(token, Node::Elem(Token::Arrow)) {
                let return_type = self.parse_type(range)?;

                (token, range) = self.next_unwrap(|s|
                    ExpectedEnum::CurlyBracket.diagnostic_after(s.all_range))?;
                Some(return_type)
            } else {
                None
            }
        };

        // parse body
        let Node::Block(BracketType::Curly, body) = token else {
            self.add_diag(ExpectedEnum::CurlyBracket.diagnostic(range));
            return Err(())
        };
        let body = self.parse_statements(body, range, false);

        let range_fn = name.range;
        result.push(Statement::new_function(name, arguments, return_type, body)
            .add_range(range_fn));
        Ok(())
    }

    fn parse_type(&mut self, range0: Range) -> Result<RTypee, ()> {
        let (token, range) = self.next_unwrap(|_|
            ExpectedEnum::new_string("type").diagnostic_after(range0))?;

        match token {
            Node::Elem(Token::String(string)) => {
                Ok(Typee::String(string).add_range(range))
            }
            Node::Elem(Token::Operation(TwoSidedOperation::Number(NumberOperation::Mul))) => {
                let typee = self.parse_type(range)?;
                let range1 = Range::new(range.start, typee.range.end);
                Ok(Typee::new_pointer(typee).add_range(range1))
            }
            Node::Elem(Token::UnaryOperation(OneSidedOperation::Dereference)) => {
                unreachable!() // lexer parse * as Mul
            }
            Node::Elem(Token::Operation(TwoSidedOperation::Number(NumberOperation::BitAnd))) => {
                let typee = self.parse_type(range)?;
                let range1 = Range::new(range.start, typee.range.end);
                Ok(Typee::new_reference(typee).add_range(range1))
            }
            Node::Elem(Token::Operation(TwoSidedOperation::Bool(BoolOperation::And))) => {
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
        let Node::Block(BracketType::Curly, mut tokens) = token else {
            self.add_diag(ExpectedEnum::CurlyBracket.diagnostic(range));
            return Err(())
        };

        let mut fields = Vec::new();
        let mut state = Self::new_state(self.errors, &mut tokens, range);
        while !state.at_end() {
            // name
            let (token, range) = state.next_unwrap(|_| unreachable!())?;
            let Node::Elem(Token::String(field_name)) = token else {
                state.add_diag(ExpectedEnum::Name.diagnostic(range));
                return Err(())
            };
            let field_name = RString::new(field_name, range);

            // :
            let (token, range) = state.next_unwrap(|s|
                ExpectedEnum::Colon.diagnostic_after(s.all_range))?;
            if !matches!(token, Node::Elem(Token::Colon)) {
                state.add_diag(ExpectedEnum::Colon.diagnostic(range));
                return Err(())
            }

            // typee
            let field_typee = state.parse_type(range)?;
            fields.push((field_name, field_typee));

            if state.at_end() { break }

            // ,
            let (token, range) = state.next_unwrap(|_| unreachable!())?;
            if !matches!(token, Node::Elem(Token::Comma)) {
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
        if matches!(token, Node::Elem(Token::Semicolon)) {
            self.add_diag(SyntacticError::unnecessary_semicolon(range0));
            return Err(());
        }
        let Node::Elem(Token::String(name)) = token else {
            self.add_diag(ExpectedEnum::Name.diagnostic(range));
            return Err(());
        };
        let name = RString::new(name, range);

        let (token, range) = self.next_unwrap(|s|
            (ExpectedEnum::Colon | ExpectedEnum::DoubleColon).diagnostic_after(s.all_range))?;

        if matches!(token, Node::Elem(Token::Colon)) {
            // name : ..
            let typee = self.parse_type(range)?;

            let (token, range) = self.next_unwrap(|s|
                ExpectedEnum::Semicolon.diagnostic_after(s.all_range))?;
            if !matches!(token, Node::Elem(Token::Semicolon)) {
                self.add_diag(ExpectedEnum::Semicolon.diagnostic(range));
                return Err(())
            }

            let range_st = name.range;
            let statement = Statement::new_extern(ExternStatement::Variable { name, typee });
            Ok(statement.add_range(range_st))
        } else if matches!(token, Node::Elem(Token::DoubleColon)) {
            // name :: ..
            let (args, is_vararg) = self.parse_extern_function_arguments()?;

            let (token, range) = self.next_unwrap(|s|
                ExpectedEnum::Semicolon.diagnostic_after(s.all_range))?;

            if matches!(token, Node::Elem(Token::Semicolon)) {
                let range_st = name.range;
                let statement = Statement::new_extern(ExternStatement::Function { name, args, is_vararg, returns: None });
                Ok(statement.add_range(range_st))
            } else if matches!(token, Node::Elem(Token::Arrow)) {
                let returns = self.parse_type(range)?;

                self.consume_semicolon(returns.range);

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
        let Node::Elem(Token::String(string)) = token else {
            self.add_diag(ExpectedEnum::Name.diagnostic(range));
            return Err(())
        };

        let mut from: Vec<RString> = vec![RString::new(string, range)];
        loop {
            let Some((token, _)) = self.peek() else {
                self.add_diag(SyntacticError::expected_semicolon(from.last().unwrap().range));

                let what = vec![(from.pop().unwrap(), None)];
                result.push(Statement::new_import(from, what).add_range(range_st));
                return Ok(())
            };

            match token {
                RefNode::Elem(Token::DoubleColon) => {
                    let _ = self.next();
                }
                RefNode::Elem(Token::String(as_string)) if as_string == "as" => {
                    let _ = self.next();

                    let (token, range) = self.next_unwrap(|s|
                        ExpectedEnum::Name.diagnostic(s.all_range))?;
                    let Node::Elem(Token::String(as_name)) = token else {
                        self.add_diag(ExpectedEnum::Name.diagnostic(range));
                        return Err(())
                    };
                    let as_name = RString::new(as_name, range);

                    self.consume_semicolon(as_name.range);

                    let what = vec![(from.pop().unwrap(), Some(as_name))];
                    result.push(Statement::new_import(from, what).add_range(range_st));
                    return Ok(())
                }
                _ => { // ::x
                    self.consume_semicolon(from.last().unwrap().range);

                    let what = vec![(from.pop().unwrap(), None)];
                    result.push(Statement::new_import(from, what).add_range(range_st));
                    return Ok(())
                }
            }

            let Some((RefNode::Elem(Token::String(_)), _)) = self.peek() else {
                break
            };

            let Some((Node::Elem(Token::String(string)), range)) = self.next() else { unreachable!() };
            let string = RString::new(string, range);
            from.push(string);
        }

        // ::{x, .., x as x, ..};
        let (token, range_br) = self.next_unwrap(|s|
            (ExpectedEnum::Name | ExpectedEnum::CurlyBracket).diagnostic(s.all_range))?;
        let Node::Block(BracketType::Curly, mut whats) = token else {
            self.add_diag((ExpectedEnum::Name | ExpectedEnum::CurlyBracket).diagnostic(range_br));
            return Err(())
        };

        let mut what = Vec::with_capacity(whats.len());
        let mut state = Self::new_state(self.errors, &mut whats, range_br);
        while !state.at_end() {
            let (token, range) = state.next_unwrap(|_| unreachable!())?;
            let Node::Elem(Token::String(name)) = token else {
                state.add_diag(ExpectedEnum::Name.diagnostic(range));
                return Err(())
            };
            let name = RString::new(name, range);

            if state.peek().is_none() {
                what.push((name, None));
                break
            }
            let (token, range) = state.next_unwrap(|_| unreachable!())?;
            if matches!(token, Node::Elem(Token::Comma)) {
                what.push((name, None));
                continue
            }
            if !matches!(token, Node::Elem(Token::String(as_string)) if as_string == "as") {
                state.add_diag((ExpectedEnum::Comma | ExpectedEnum::As).diagnostic(range));
                return Err(())
            }

            let (token, range) = state.next_unwrap(|s|
                ExpectedEnum::Name.diagnostic(s.all_range))?;
            let Node::Elem(Token::String(as_name)) = token else {
                state.add_diag(ExpectedEnum::Name.diagnostic(range));
                return Err(())
            };
            let as_name = RString::new(as_name, range);
            what.push((name, Some(as_name)));

            if state.at_end() { break }

            let (token, range) = state.next_unwrap(|_| unreachable!())?;
            if !matches!(token, Node::Elem(Token::Comma)) {
                state.add_diag(ExpectedEnum::Comma.diagnostic(range));
            }
        }

        self.consume_semicolon(range_br);

        result.push(Statement::new_import(from, what).add_range(range_st));
        Ok(())
    }
    /// expects `..` and parse next `.` so have `...`
    fn parse_triple_dot(&mut self) -> Result<(), ()> {
        let Some((Node::Elem(Token::DoubleDot), _range)) = self.next() else {
            unreachable!()
        };
        let (token, range) = self.next_unwrap(|s|
            ExpectedEnum::TripleDot.diagnostic_after(s.all_range))?;
        if !matches!(token, Node::Elem(Token::Dot)) {
            self.add_diag(ExpectedEnum::TripleDot.diagnostic(range));
        }

        Ok(())
    }
    /// parse `(...)`
    fn parse_function_declaration_arguments(&mut self) -> Result<Vec<(RString, RTypee)>, ()> {
        let (token, range) = self.next_unwrap(|s|
            ExpectedEnum::RoundBracket.diagnostic_after(s.all_range))?;
        let Node::Block(BracketType::Round, mut args) = token else {
            self.add_diag(ExpectedEnum::RoundBracket.diagnostic(range));
            return Err(())
        };

        if args.is_empty() {
            return Ok(Vec::new())
        }
        let mut arguments = Vec::with_capacity(args.len().div_ceil(2));
        let mut state = Self::new_state(self.errors, &mut args, range);

        while !state.at_end() {
            // name
            let (token, range) = state.next_unwrap(|_| unreachable!())?;
            let Node::Elem(Token::String(arg_i)) = token else {
                state.add_diag(ExpectedEnum::Name.diagnostic(range));
                return Err(());
            };
            let arg_i = RString::new(arg_i, range);

            // :
            let (token, range) = state.next_unwrap(|s|
                ExpectedEnum::Colon.diagnostic_after(s.all_range))?;
            if !matches!(token, Node::Elem(Token::Colon)) {
                state.add_diag(ExpectedEnum::Colon.diagnostic(range));
                return Err(())
            }

            // type
            let argument_type = state.parse_type(range)?;
            arguments.push((arg_i, argument_type));

            // optional comma
            let Some((token, range)) = state.next() else {
                break;
            };

            if !matches!(token, Node::Elem(Token::Comma)) {
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
        let Node::Block(BracketType::Round, mut args) = token else {
            self.add_diag(ExpectedEnum::RoundBracket.diagnostic(range));
            return Err(())
        };

        if args.is_empty() {
            return Ok((Vec::new(), false))
        }
        let mut arguments = Vec::with_capacity(args.len().div_ceil(2));
        let mut state = Self::new_state(self.errors, &mut args, range);

        let mut range0 = Range::default();
        while !state.at_end() {
            // check for ...
            if matches!(state.peek(), Some((RefNode::Elem(Token::DoubleDot), _))) {
                state.parse_triple_dot()?;

                if !state.at_end() {
                    let (_, range) = state.next_unwrap(|_| unreachable!())?;
                    state.add_diag(ExpectedEnum::CloseRoundBracket.diagnostic(range));
                    return Err(());
                }

                return Ok((arguments, true))
            }

            // type
            let argument_type = state.parse_type(range0)?;
            arguments.push(argument_type);

            // optional comma
            let Some((token, range)) = state.next() else {
                break;
            };

            if !matches!(token, Node::Elem(Token::Comma)) {
                state.add_diag(ExpectedEnum::Comma.diagnostic(range));
                return Err(())
            }

            range0 = range;
        }

        Ok((arguments, false))
    }

    /// expect token `(...)`
    fn parse_function_arguments(&mut self) -> Result<Vec<RExpression>, ()> {
        let (token, range_bracket) = self.next_unwrap(|_| unreachable!())?;
        let Node::Block(_, mut vec) = token else { unreachable!() };

        let mut state = Self::new_state(self.errors, &mut vec, range_bracket);
        let mut args = Vec::new();

        while !state.at_end() {
            let expression = state.parse_expression(false)?;
            args.push(expression);

            if !state.at_end() {
                let (token, range) = state.next_unwrap(|_| unreachable!())?;
                if !matches!(token, Node::Elem(Token::Comma)) {
                    state.add_diag(ExpectedEnum::Comma.diagnostic(range));
                    return Err(())
                }
            }
        }

        Ok(args)
    }

    fn parse_struct_construction(&mut self) -> Result<Vec<(RString, RExpression)>, ()> {
        let (token, range0) = self.next_unwrap(|_| unreachable!())?;
        let Node::Block(_, mut vec) = token else { unreachable!() };

        let mut fields = Vec::new();
        let mut state = Self::new_state(self.errors, &mut vec, range0);

        while !state.at_end() {
            let (token, range) = state.next_unwrap(|_| unreachable!())?;

            let Node::Elem(Token::String(field_name)) = token else {
                state.add_diag(ExpectedEnum::Name.diagnostic(range));
                return Err(())
            };
            let field_name = RString::new(field_name, range);

            let (token, range) = state.next_unwrap(|s|
                ExpectedEnum::Colon.diagnostic_after(s.all_range))?;
            if !matches!(token, Node::Elem(Token::Colon)) {
                state.add_diag(ExpectedEnum::Colon.diagnostic(range));
                return Err(())
            }

            let field_value = state.parse_expression(false)?;
            fields.push((field_name, field_value));
            if state.at_end() {
                break
            }

            let (token, range) = state.next_unwrap(|_| unreachable!())?;
            if !matches!(token, Node::Elem(Token::Comma)) {
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
            return b'?';
        };

        if chars.next().is_some() {
            self.add_diag(SyntacticError::from_text(">1 char in literal", range));
            return b'?';
        }
        if !first_char.is_ascii() {
            self.add_diag(SyntacticError::from_text("not ascii char in literal", range));
            return b'?';
        }
        first_char as u8
    }

    fn consume_semicolon(&mut self, range_before: Range) {
        let Some((token, _range)) = self.peek() else {
            return;
        };

        if !matches!(token, RefNode::Elem(Token::Semicolon)) {
            self.add_diag(SyntacticError::expected_semicolon(range_before));
            return;
        }

        let _ = self.next();
    }
}
