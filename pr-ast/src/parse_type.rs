use crate::error::ExpectedEnum;
use crate::statement::{RTypee, Typee};
use crate::{Node, ParsingState};
use lsp_types::Range;
use pr_common::operations::{BoolOperation, NumberOperation, OneSidedOperation, TwoSidedOperation};
use pr_lexer::token::Token;

impl ParsingState<'_, '_, '_> {
    pub fn parse_type(&mut self, range0: Range) -> Result<RTypee, ()> {
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
}