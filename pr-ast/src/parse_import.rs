use crate::error::{ExpectedEnum, SyntacticError};
use crate::statement::{RStatement, Statement};
use crate::{Node, ParsingState, RefNode};
use lsp_types::Range;
use pr_common::ranged::RString;
use pr_common::BracketType;
use pr_lexer::token::Token;

impl ParsingState<'_, '_, '_> {
    pub fn parse_import(&mut self, range_st: Range, result: &mut Vec<RStatement>) -> Result<(), ()> {
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
        let mut state = self.new_state(&mut whats, range_br);
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
}