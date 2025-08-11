use crate::error::CompilationError as CE;
use crate::parser::{BracketType, PositionInFile, operations::*};

use super::token::*;

use std::str::Chars;

pub fn parse_tokens(text: &str) -> Result<Vec<TokenWithPos>, CE> {
    let (token, _) = parse_inside_brackets(&mut text.chars(), 0, None)?;
    let Token::Bracket(vec, _) = token.token else { unreachable!() };
    Ok(vec)
}

fn parse_inside_brackets(
    text: &mut Chars,
    start_index: usize,
    open_bracket_type: Option<BracketType>,
) -> Result<(TokenWithPos, usize), CE> {
    let mut result_tokens = Vec::new();

    let mut buffer = String::new();
    let mut start_buffer_index = start_index;
    let mut index = start_index;
    while let Some(char) = text.next() {
        if let Some(bracket_type) = is_open_bracket(char) {
            // open bracket
            if index != start_buffer_index {
                let mut tokens = split_text_without_brackets(buffer, start_buffer_index);
                buffer = String::new();
                result_tokens.append(&mut tokens);
            }

            let (new_token, new_index) =
                parse_inside_brackets(text, index + 1, Some(bracket_type))?;
            result_tokens.push(new_token);
            index = new_index;
            start_buffer_index = new_index;
        } else if let Some(bracket_type) = is_close_bracket(char) {
            // close bracket
            let Some(open_bracket_type) = open_bracket_type else {
                let position = PositionInFile::new_sized(index, 1);
                return Err(CE::BracketNotOpened(position, bracket_type));
            };

            if bracket_type != open_bracket_type {
                return Err(CE::WrongBracketClosed {
                    start: PositionInFile::new_sized(start_index, 1),
                    start_bracket_type: open_bracket_type,
                    end: PositionInFile::new_sized(index, 1),
                    end_bracket_type: bracket_type,
                });
            }

            if index != start_buffer_index {
                let mut tokens = split_text_without_brackets(buffer, start_buffer_index);
                result_tokens.append(&mut tokens);
            }

            let result_token = Token::Bracket(result_tokens, open_bracket_type);
            return Ok((TokenWithPos::new(result_token, PositionInFile::new(start_index, index)), index + 1));
        } else {
            buffer.push(char);
            index += 1;
        }
    }
    if index != start_buffer_index {
        let mut tokens = split_text_without_brackets(buffer, start_buffer_index);
        result_tokens.append(&mut tokens);
    }

    if let Some(open_bracket_type) = open_bracket_type {
        Err(CE::BracketNotClosed(
            PositionInFile::new_sized(start_index, 1),
            open_bracket_type,
        ))
    } else {
        let unused_bracket_type = BracketType::Round;
        let unused_number = 0;
        let result_token = Token::Bracket(result_tokens, unused_bracket_type);
        Ok((TokenWithPos::new(result_token, PositionInFile::new(start_index, index)), unused_number))
    }
}

fn is_open_bracket(char: char) -> Option<BracketType> {
    match char {
        '{' => Some(BracketType::Curly),
        '(' => Some(BracketType::Round),
        _ => None
    }
}

fn is_close_bracket(char: char) -> Option<BracketType> {
    match char {
        '}' => Some(BracketType::Curly),
        ')' => Some(BracketType::Round),
        _ => None
    }
}

pub fn split_text_without_brackets(text: String, offset_index: usize) -> Vec<TokenWithPos> {
    let mut iter = text.chars().peekable();
    let mut state = TokenizeState::new(offset_index);

    // let mut line = 1;
    // let mut column = 1;

    while let Some(char) = iter.next() {
        // split_state.update_place_info(PositionInFile::new(line, column));
        // column += 1;
        match char {
            '\n' => {
                state.add(1, None);
                // line += 1;
                // column = 1;
            }
            '=' => {
                if iter.peek() == Some(&'=') {
                    iter.next();
                    state.add(2, Some(CompareOperator::Equal.into())) // ==
                } else {
                    state.add(1, Some(EqualOperation::Equal.into())) // =
                }
            }
            '!' => {
                match iter.peek() {
                    Some('=') => {
                        iter.next();
                        state.add(2, Some(CompareOperator::NotEqual.into())) // !=
                    },
                    _ => state.add(1, Some(OneSidedOperation::BoolNot.into())), // !
                }
            }
            '>' => {
                match iter.peek() {
                    Some('=') => {
                        iter.next();
                        state.add(2, Some(CompareOperator::GreaterEqual.into())) // >=
                    }
                    _ => state.add(1, Some(CompareOperator::Greater.into())), // >
                }
            }
            '<' => {
                match iter.peek() {
                    Some('=') => {
                        iter.next();
                        state.add(2, Some(CompareOperator::LessEqual.into())) // <=
                    }
                    _ => state.add(1, Some(CompareOperator::Less.into())), // <
                }
            }
            ':' => {
                match iter.peek() {
                    Some('=') => {
                        iter.next();
                        state.add(2, Some(EqualOperation::ColonEqual.into())) // :=
                    }
                    Some(':') => {
                        iter.next();
                        state.add(2, Some(Token::DoubleColon)) // ::
                    }
                    _ => state.add(1, Some(Token::Colon)), // :
                }
            }
            '-' => {
                match iter.peek() {
                    Some('=') => {
                        iter.next();
                        state.add(2, Some(EqualOperation::OperationEqual(NumberOperation::Sub.into()).into())) // -=
                    }
                    Some('>') => {
                        iter.next();
                        state.add(2, Some(Token::Arrow)) // ->
                    }
                    _ => state.add(1, Some(NumberOperation::Sub.into())), // -
                }
            }
            '+' | '*' | '/' | '%' => {
                let token: TwoSidedOperation = match char {
                    '+' => NumberOperation::Add.into(),
                    '*' => NumberOperation::Mul.into(),
                    '/' => NumberOperation::Div.into(),
                    '%' => NumberOperation::Rem.into(),
                    _ => unreachable!()
                };
                match iter.peek() {
                    Some('=') => {
                        iter.next();
                        state.add(2, Some(EqualOperation::OperationEqual(token).into())) // +=
                    }
                    _ => state.add(1, Some(token.into())), // +
                }
            }
            '&' => {
                match iter.peek() {
                    Some('&') => {
                        iter.next();
                        state.add(2, Some(BoolOperation::And.into())) // &&
                    }
                    _ => state.add(1, Some(NumberOperation::BitAnd.into())), // &
                }
            }
            '|' => {
                match iter.peek() {
                    Some('|') => {
                        iter.next();
                        state.add(2, Some(BoolOperation::Or.into())) // ||
                    }
                    _ => state.add(1, Some(NumberOperation::BitOr.into())), // |
                }
            }
            ',' => {
                let token = Token::Comma;
                state.add(1, Some(token));
            }
            ';' => {
                let token = Token::Semicolon;
                state.add(1, Some(token));
            }
            '{' | '}' | '(' | ')' => {
                unreachable!()
            }
            _ if char.is_ascii_whitespace() => {
                state.add(1, None);
            }
            _ => {
                state.use_char_in_string(char);
            }
        }
    }
    state.flush_buffer();
    state.tokens
}

/// guarantees that `buffer_start <= buffer_end <= text.len()`
struct TokenizeState {
    tokens: Vec<TokenWithPos>,
    buffer: String,
    buffer_start: usize,
    buffer_end: usize,
    offset_index: usize,
}
impl TokenizeState {
    fn new(offset_index: usize) -> Self {
        Self {
            tokens: Vec::new(),
            buffer: String::new(),
            buffer_start: 0,
            buffer_end: 0,
            offset_index,
        }
    }

    fn use_char_in_string(&mut self, char: char) {
        self.buffer_end += 1;
        self.buffer.push(char);
    }
    #[inline]
    fn add(&mut self, skip_chars: usize, token: Option<Token>) {
        self.flush_buffer();
        if let Some(token) = token {
            let place_info = PositionInFile::new(self.offset_index + self.buffer_end, self.offset_index + self.buffer_end + skip_chars);
            self.tokens.push(TokenWithPos::new(token, place_info));
        }
        self.buffer_start += skip_chars;
        self.buffer_end += skip_chars;
    }

    fn flush_buffer(&mut self) {
        if self.buffer_start != self.buffer_end {
            let first_char = self.buffer.chars().next().unwrap();

            let token_text = self.buffer.split_off(0);
            let token = if first_char.is_ascii_digit() {
                Token::NumberLiteral(token_text)
            } else {
                Token::String(token_text)
            };
            let place_info = PositionInFile::new(self.offset_index + self.buffer_start, self.offset_index + self.buffer_end);
            self.tokens
                .push(TokenWithPos::new(token, place_info));
        }
        self.buffer_start = self.buffer_end;
    }
}
