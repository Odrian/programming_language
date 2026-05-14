use crate::error::{ExpectedEnum, SyntacticError};
use crate::statement::*;
use crate::{Node, ParsingState, RefNode};
use lsp_types::Range;
use pr_common::operations::*;
use pr_common::ranged::RString;
use pr_common::BracketType;
use pr_lexer::token::*;
use pr_lexer::TokenIntoIter;

impl ParsingState<'_, '_, '_> {
    pub fn parse_statements(&mut self, mut tokens: TokenIntoIter<'_>, range: Range, is_global: bool) -> Vec<RStatement> {
        let mut state = self.new_state(&mut tokens, range);
        state.parse_statements_inplace(is_global)
    }
    pub fn parse_statements_inplace(&mut self, is_global: bool) -> Vec<RStatement> {
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
        let attributes = self.parse_attributes();

        // skip on #cfg(false)
        let result = if attributes.is_cgf_skip {
            &mut vec![]
        } else {
            result
        };

        if attributes.is_extern {
            return self.parse_extern(is_global, result);
        }

        if self.at_end() {
            self.add_diag(SyntacticError::unexpected_end(self.all_range));
            return Err(())
        }

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
                        result.push(Statement::Return(None).add_range(range0));
                        self.consume_semicolon(range0);
                    } else {
                        let expression = self.parse_expression(false)?;
                        result.push(Statement::Return(Some(expression)).add_range(range0));
                        self.consume_semicolon(range0);
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
            }
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
                if is_global {
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
            self.add_diag(SyntacticError::unexpected_end(self.all_range));
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
                    let expression: Expression = LiteralExpression::Undefined { is_zeroed: true }.into();
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
}
