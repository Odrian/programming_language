use crate::error::ExpectedEnum;
use crate::statement::{RExpression, RStatement, RTypee, Statement};
use crate::{Node, ParsingState};
use pr_common::ranged::RString;
use pr_common::BracketType;
use pr_lexer::token::Token;

impl ParsingState<'_, '_, '_> {
    pub fn parse_function(&mut self, name: RString, result: &mut Vec<RStatement>) -> Result<(), ()> {
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
    /// parse `(argument1: type1, ...)`
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
        let mut state = self.new_state(&mut args, range);

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
    /// parses `(argument1, ...)`
    pub fn parse_function_arguments(&mut self) -> Result<Vec<RExpression>, ()> {
        let (token, range_bracket) = self.next_unwrap(|_| unreachable!())?;
        let Node::Block(_, mut vec) = token else { unreachable!() };

        let mut state = self.new_state(&mut vec, range_bracket);
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
}
