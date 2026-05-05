use crate::error::{ExpectedEnum, SyntacticError};
use crate::statement::{new_unary_expr, Expression, LiteralExpression, RExpression};
use crate::{Node, ParsingState, RefNode};
use lsp_types::Range;
use pr_common::operations::{BoolOperation, NumberOperation, OneSidedOperation, RTwoSidedOperation, TwoSidedOperation};
use pr_common::ranged::RString;
use pr_common::BracketType;
use pr_lexer::token::Token;

impl ParsingState<'_, '_, '_> {
    pub fn parse_expression(&mut self, in_cond: bool) -> Result<RExpression, ()> {
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
            let split_index = (0..ops.len()).max_by_key(|x| ops[*x].value.get_prior()).unwrap();

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
    pub fn parse_expression_without_ops(
        &mut self,
        was_unary: bool,
        in_cond: bool,
    ) -> Result<RExpression, ()> {
        let (token, range) = self.next_unwrap(|s|
            SyntacticError::unexpected_end(s.all_range))?;

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
                let mut new_state = self.new_state(&mut vec, range);
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
    pub fn parse_expression2_without_ops(
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

}