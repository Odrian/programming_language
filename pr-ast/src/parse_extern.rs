use crate::error::{ExpectedEnum, SyntacticError};
use crate::statement::{ExternStatement, RStatement, RTypee, Statement};
use crate::{Node, ParsingState, RefNode};
use lsp_types::Range;
use pr_common::ranged::RString;
use pr_common::BracketType;
use pr_lexer::token::Token;

impl ParsingState<'_, '_, '_> {
    pub fn parse_extern(&mut self, is_global: bool, result: &mut Vec<RStatement>) -> Result<(), ()> {
        if !is_global {
            self.add_diag(SyntacticError::new_local_global("extern", self.all_range));
            return Err(())
        }

        let range0 = self.all_range;
        if matches!(self.peek(), Some((RefNode::Block(BracketType::Curly, _), _))) {
            let Some((Node::Block(BracketType::Curly, mut vec), range)) = self.next() else { unreachable!() };

            let mut state = self.new_state(&mut vec, range);
            while !state.at_end() {
                if let Ok(ext) = state.parse_single_extern(range0) {
                    result.push(ext)
                }
            }

            Ok(())
        } else {
            if let Ok(ext) = self.parse_single_extern(range0) {
                result.push(ext)
            }
            Ok(())
        }
    }
    fn parse_single_extern(&mut self, range0: Range) -> Result<RStatement, ()> {
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
            let (args, is_vararg) = self.parse_extern_function_declaration_arguments()?;

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
    /// parse `(type1, ...)`
    fn parse_extern_function_declaration_arguments(&mut self) -> Result<(Vec<RTypee>, bool), ()> {
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
        let mut state = self.new_state(&mut args, range);

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
}
