use crate::error::CompilationError as CE;
use crate::parser::{BracketType, PositionInFile};

use super::token::*;

pub fn parse_tokens(text: &[char]) -> Result<Vec<TokenWithPos>, CE> {
    let (token, _) = parse_inside_brackets(text, 0, None)?;
    let Token::Bracket(vec, _) = token.token else { unreachable!() };
    Ok(vec)
}

fn parse_inside_brackets(
    text: &[char],
    start_index: usize,
    open_bracket_type: Option<BracketType>,
) -> Result<(TokenWithPos, usize), CE> {
    let mut result_tokens = Vec::new();

    let mut start_buffer_index = start_index;
    let mut index = start_index;
    while index < text.len() {
        let char = text[index];
        if let Some(bracket_type) = is_open_bracket(char) {
            // open bracket
            if index != start_buffer_index {
                let mut tokens = split_text_without_brackets(&text[start_buffer_index..index]);
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
                let mut tokens = split_text_without_brackets(&text[start_buffer_index..index]);
                result_tokens.append(&mut tokens);
            }

            let result_token = Token::Bracket(result_tokens, open_bracket_type);
            return Ok((TokenWithPos::new(result_token, PositionInFile::new(start_index, index)), index + 1));
        } else {
            index += 1;
        }
    }
    if index != start_buffer_index {
        let mut tokens = split_text_without_brackets(&text[start_buffer_index..index]);
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

pub fn split_text_without_brackets(text: &[char]) -> Vec<TokenWithPos> {
    let mut state = TokenizeState::new(text);

    // let mut line = 1;
    // let mut column = 1;

    while let Some(char) = state.peek_char() {
        // split_state.update_place_info(PositionInFile::new(line, column));
        // column += 1;
        match char {
            '\n' => {
                state.add(1, None);
                // line += 1;
                // column = 1;
            }
            '=' => {
                state.add(1, Some(Token::EqualOperation(EqualOperation::Equal)));
            }
            ':' => {
                let char_2th = state.peek_nth_char(2);
                if char_2th == Some(':') {
                    state.add(2, Some(Token::DoubleColon));
                } else if char_2th == Some('=') {
                    state.add(2, Some(Token::EqualOperation(EqualOperation::ColonEqual)));
                } else {
                    state.add(1, Some(Token::Colon));
                }
            }
            '-' => {
                let token = Token::TwoSidedOperation(TwoSidedOperation::Minus);
                state.add(1, Some(token));
            }
            '+' => {
                let token = Token::TwoSidedOperation(TwoSidedOperation::Plus);
                state.add(1, Some(token));
            }
            ',' => {
                let token = Token::Comma;
                state.add(1, Some(token));
            }
            '{' | '}' | '(' | ')' => {
                unreachable!()
            }
            _ if char.is_ascii_whitespace() => {
                state.add(1, None);
            }
            _ => {
                state.use_char_in_string();
            }
        }
    }
    state.flush_buffer();
    state.tokens
}

/// guarantees that `buffer_start <= buffer_end <= text.len()`
struct TokenizeState<'text> {
    text: &'text [char],
    tokens: Vec<TokenWithPos<'text>>,
    buffer_start: usize,
    buffer_end: usize,
}
impl<'text> TokenizeState<'text> {
    fn new(text: &'text [char]) -> Self {
        Self {
            text,
            tokens: Vec::new(),
            buffer_start: 0,
            buffer_end: 0,
        }
    }
    fn peek_char(&self) -> Option<char> {
        self.text.get(self.buffer_end).copied()
    }
    /// counting from 1
    fn peek_nth_char(&self, n: usize) -> Option<char> {
        self.text.get(self.buffer_end + n - 1).copied()
    }

    fn use_char_in_string(&mut self) {
        self.buffer_end += 1;
        self.check_indexes();
    }
    fn add(&mut self, skip_chars: usize, token: Option<Token<'text>>) {
        self.flush_buffer();
        if let Some(token) = token {
            let place_info = PositionInFile::new(self.buffer_end, self.buffer_end + skip_chars);
            self.tokens.push(TokenWithPos::new(token, place_info));
        }
        self.buffer_start += skip_chars;
        self.buffer_end += skip_chars;
        self.check_indexes();
    }

    fn flush_buffer(&mut self) {
        if self.buffer_start != self.buffer_end {
            let first_char = self.text[self.buffer_start];

            let token_text = &self.text[self.buffer_start..self.buffer_end];
            let token = if first_char.is_ascii_digit() {
                Token::NumberLiteral(token_text)
            } else {
                Token::String(token_text)
            };
            let place_info = PositionInFile::new(self.buffer_start, self.buffer_end);
            self.tokens
                .push(TokenWithPos::new(token, place_info));
        }
        self.buffer_start = self.buffer_end;
    }
    fn check_indexes(&self) {
        assert!(self.buffer_end <= self.text.len(), "Unexpected end of string");
    }
}
