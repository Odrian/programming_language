use crate::error::{ExpectedEnum, SyntacticError};
use crate::{Node, ParsingState, RefNode};
use pr_common::BracketType;
use pr_lexer::token::{EqualOperation, Token, TokenBlock};

#[derive(Default)]
pub struct Attributes {
    pub is_cgf_skip: bool,
    pub is_extern: bool,
}

impl ParsingState<'_, '_, '_> {
    pub fn parse_attributes(&mut self) -> Attributes {
        let mut attributes = Attributes::default();

        // loop for multiple #...
        while let Some((token, &range)) = self.peek() {
            let token = match token {
                RefNode::Elem(elem) => elem,
                RefNode::Block(..) => break,
            };
            if !matches!(token, Token::Hashtag) {
                break;
            }
            let _ = self.next();

            let Some((token, &range)) = self.peek() else {
                self.add_diag(SyntacticError::unknown_attribute(range, "EOF"));
                continue
            };

            match token {
                RefNode::Elem(_elem) => {
                    // #...
                    self.parse_attribute(&mut attributes)
                }
                RefNode::Block(TokenBlock::Square, _iter) => {
                    // #[...]
                    let Some((Node::Block(_token, mut iter), all_range)) = self.next() else {
                        unreachable!()
                    };

                    let mut state = self.new_state(&mut iter, all_range);

                    while !state.at_end() {
                        state.parse_attribute(&mut attributes);

                        if state.at_end() { break }

                        let Some((token_comma, &range)) = state.peek() else { unreachable!() };

                        if matches!(token_comma, RefNode::Elem(Token::Comma)) {
                            let _ = state.next();
                        } else {
                            state.add_diag(ExpectedEnum::Comma.diagnostic(range));
                            continue
                        }
                    }
                }
                RefNode::Block(block, _iter) => {
                    self.add_diag(SyntacticError::unknown_attribute(range, block.to_string()));
                    continue
                }
            }
        }

        attributes
    }
    fn parse_attribute(&mut self, attributes: &mut Attributes) {
        let Some((Node::Elem(Token::String(string)), range)) = self.next() else {
            self.add_diag(SyntacticError::unknown_attribute(self.all_range, "EOF"));
            return;
        };

        match string.as_str() {
            "extern" => {
                attributes.is_extern = true;
            }
            "cfg" => {
                let result = self.parse_cfg();
                if matches!(result, Ok(false)) {
                    attributes.is_cgf_skip = true;
                }
            }
            string => {
                self.add_diag(SyntacticError::unknown_attribute(range, string));
                return;
            }
        }
    }
}


enum CfgParseType {
    Any, All, None
}
impl ParsingState<'_, '_, '_> {
    fn parse_cfg(&mut self) -> Result<bool, ()> {
        self.parse_cfg_recursive(CfgParseType::None)
    }

    fn parse_cfg_recursive(&mut self, parse_type: CfgParseType) -> Result<bool, ()> {
        let (token, range_round) = self.next_unwrap(|s|
            ExpectedEnum::RoundBracket.diagnostic_after(s.all_range))?;
        let Node::Block(BracketType::Round, mut vec) = token else {
            self.add_diag(SyntacticError::from_text("expected cfg: (...)", range_round));
            return Err(());
        };
        let mut state = self.new_state(&mut vec, range_round);

        let mut vec = Vec::new();
        loop {
            let (token, range_str) = state.next_unwrap(|s|
                ExpectedEnum::Name.diagnostic_after(s.all_range))?;
            let Node::Elem(Token::String(str)) = token else {
                self.add_diag(ExpectedEnum::Name.diagnostic(range_str));
                return Err(());
            };

            let result = match str.as_str() {
                "true" => true,
                "false" => false,
                "not" => {
                    let result = state.parse_cfg_recursive(CfgParseType::None)?;
                    !result
                }
                "any" => {
                    state.parse_cfg_recursive(CfgParseType::Any)?
                }
                "all" => {
                    state.parse_cfg_recursive(CfgParseType::All)?
                }
                feature => {
                    let token = state.peek().map(|x| x.0);

                    match token {
                        Some(RefNode::Elem(Token::Comma)) | None => {
                            // #cfg(name, ...) | #cfg(name)
                            let _ = state.next();

                            let Some(result) = state.target.check_cfg_name(feature) else {
                                state.add_diag(SyntacticError::from_text("", range_str));
                                return Err(());
                            };

                            result
                        }
                        Some(RefNode::Elem(Token::EqualOperation(EqualOperation::Equal))) => {
                            // #cfg(name = value, ...)
                            let (token, range_value) = state.next_unwrap(|s|
                                ExpectedEnum::Name.diagnostic_after(s.all_range))?;
                            let Node::Elem(Token::String(value)) = token else {
                                state.add_diag(ExpectedEnum::Name.diagnostic(range_value));
                                return Err(());
                            };

                            let Some(result) = state.target.check_cfg(feature, &value) else {
                                state.add_diag(SyntacticError::from_text("", range_str));
                                return Err(());
                            };

                            result
                        }
                        _ => {
                            let (_token, range) = state.next_unwrap(|_| unreachable!())?;
                            state.add_diag(
                                (ExpectedEnum::Comma | ExpectedEnum::Equal | ExpectedEnum::CloseRoundBracket)
                                    .diagnostic(range));
                            return Err(());
                        }
                    }
                }
            };

            if matches!(parse_type, CfgParseType::None) {
                if !state.at_end() {
                    state.add_diag(ExpectedEnum::End.diagnostic(state.all_range));
                    return Err(());
                }
                return Ok(result)
            }

            vec.push(result);

            if state.at_end() { break } // (...)

            let (token, range_comma) = state.next_unwrap(|s|
                ExpectedEnum::Comma.diagnostic_after(s.all_range))?;
            let Node::Elem(Token::Comma) = token else {
                state.add_diag(ExpectedEnum::Comma.diagnostic(range_comma));
                return Err(());
            };

            if state.at_end() { break } // (..., )
        }

        match parse_type {
            CfgParseType::Any => Ok(vec.iter().any(|x| *x)),
            CfgParseType::All => Ok(vec.iter().all(|x| *x)),
            CfgParseType::None => unreachable!(),
        }
    }
}
