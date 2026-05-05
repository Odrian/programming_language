use crate::error::ExpectedEnum;
use crate::statement::{RExpression, RStatement, Statement};
use crate::{Node, ParsingState};
use pr_common::ranged::RString;
use pr_common::BracketType;
use pr_lexer::token::Token;

impl ParsingState<'_, '_, '_> {
    pub fn parse_struct_construction(&mut self) -> Result<Vec<(RString, RExpression)>, ()> {
        let (token, range0) = self.next_unwrap(|_| unreachable!())?;
        let Node::Block(_, mut vec) = token else { unreachable!() };

        let mut fields = Vec::new();
        let mut state = self.new_state(&mut vec, range0);

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
    pub fn parse_struct(&mut self, name: RString, result: &mut Vec<RStatement>) -> Result<(), ()> {
        let (token, range) = self.next_unwrap(|s|
            ExpectedEnum::CurlyBracket.diagnostic_after(s.all_range))?;
        let Node::Block(BracketType::Curly, mut tokens) = token else {
            self.add_diag(ExpectedEnum::CurlyBracket.diagnostic(range));
            return Err(())
        };

        let mut fields = Vec::new();
        let mut state = self.new_state(&mut tokens, range);
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
}
