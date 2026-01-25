use crate::error::CResult;
use crate::parser::{operations::*, BracketType, PositionInFile};

use super::error::TokenizeError;
use super::token::*;

use std::str::Chars;

pub fn parse_tokens(text: &str) -> CResult<Vec<TokenWithPos>> {
    let (token, _) = parse_inside_brackets(&mut text.chars(), 0, None)?;
    let Token::Bracket(vec, _) = token.token else { unreachable!() };
    Ok(vec)
}

fn parse_inside_brackets(
    text: &mut Chars,
    start_index: usize,
    open_bracket_type: Option<BracketType>,
) -> CResult<(TokenWithPos, usize)> {
    let mut result_tokens = Vec::new();

    let mut buffer = String::new();
    let mut start_buffer_index = start_index;
    let mut index = start_index;

    while let Some(char) = text.next() {
        if char == '"' || char == '\'' || char == '`' {
            if index != start_buffer_index {
                let mut tokens = split_text_without_brackets(&buffer, start_buffer_index)?;
                buffer = String::new();
                result_tokens.append(&mut tokens);
            }

            let mut inside_quotes = String::new();
            loop {
                let Some(next_char) = text.next() else {
                    TokenizeError::QuotesNotClosed
                        .print(index);
                    return Err(());
                };
                if next_char == char {
                    break;
                }
                inside_quotes.push(next_char);
            }

            let new_index = index + inside_quotes.len() + 2;
            let token = match char {
                '\'' => Token::Quotes(inside_quotes),
                '"' => Token::DoubleQuotes(inside_quotes),
                '`' => Token::String(inside_quotes),
                _ => unreachable!()
            };
            let position = PositionInFile::new(index, new_index);
            result_tokens.push(TokenWithPos::new(token, position));
            index += new_index;
            start_buffer_index = index;
        } else if let Some(bracket_type) = is_open_bracket(char) {
            // open bracket
            if index != start_buffer_index {
                let mut tokens = split_text_without_brackets(&buffer, start_buffer_index)?;
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
                TokenizeError::BracketNotOpened(bracket_type)
                    .print(index);
                return Err(());
            };

            if bracket_type != open_bracket_type {
                TokenizeError::WrongBracketClosed { expected_bracket: open_bracket_type, actual_bracket: bracket_type }
                    .print(index);
                return Err(());
            }

            if index != start_buffer_index {
                let mut tokens = split_text_without_brackets(&buffer, start_buffer_index)?;
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
        let mut tokens = split_text_without_brackets(&buffer, start_buffer_index)?;
        result_tokens.append(&mut tokens);
    }

    if let Some(open_bracket_type) = open_bracket_type {
        TokenizeError::BracketNotClosed(open_bracket_type)
            .print(start_index);
        Err(())
    } else {
        let unused_bracket_type = BracketType::Round;
        let unused_number = 0;
        let result_token = Token::Bracket(result_tokens, unused_bracket_type);
        Ok((TokenWithPos::new(result_token, PositionInFile::new(start_index, index)), unused_number))
    }
}

const fn is_open_bracket(char: char) -> Option<BracketType> {
    match char {
        '{' => Some(BracketType::Curly),
        '(' => Some(BracketType::Round),
        _ => None
    }
}

const fn is_close_bracket(char: char) -> Option<BracketType> {
    match char {
        '}' => Some(BracketType::Curly),
        ')' => Some(BracketType::Round),
        _ => None
    }
}

pub fn split_text_without_brackets(text: &str, offset_index: usize) -> CResult<Vec<TokenWithPos>> {
    let mut iter = text.chars().peekable();
    let mut state = TokenizeState::new(offset_index);

    while let Some(char) = iter.next() {
        match char {
            '=' => {
                if iter.peek() == Some(&'=') {
                    iter.next();
                    state.add(2, Some(CompareOperator::Equal.into())); // ==
                } else {
                    state.add(1, Some(EqualOperation::Equal.into())); // =
                }
            }
            '!' => {
                match iter.peek() {
                    Some('=') => {
                        iter.next();
                        state.add(2, Some(CompareOperator::NotEqual.into())); // !=
                    },
                    _ => state.add(1, Some(OneSidedOperation::BoolNot.into())), // !
                }
            }
            '>' => {
                match iter.peek() {
                    Some('=') => {
                        iter.next();
                        state.add(2, Some(CompareOperator::GreaterEqual.into())); // >=
                    }
                    _ => state.add(1, Some(CompareOperator::Greater.into())), // >
                }
            }
            '<' => {
                match iter.peek() {
                    Some('=') => {
                        iter.next();
                        state.add(2, Some(CompareOperator::LessEqual.into())); // <=
                    }
                    _ => state.add(1, Some(CompareOperator::Less.into())), // <
                }
            }
            ':' => {
                match iter.peek() {
                    Some('=') => {
                        iter.next();
                        state.add(2, Some(EqualOperation::ColonEqual.into())); // :=
                    }
                    Some(':') => {
                        iter.next();
                        state.add(2, Some(Token::DoubleColon)); // ::
                    }
                    _ => state.add(1, Some(Token::Colon)), // :
                }
            }
            '-' => {
                match iter.peek() {
                    Some('=') => {
                        iter.next();
                        state.add(2, Some(EqualOperation::OperationEqual(NumberOperation::Sub.into()).into())); // -=
                    }
                    Some('>') => {
                        iter.next();
                        state.add(2, Some(Token::Arrow)); // ->
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
                        state.add(2, Some(EqualOperation::OperationEqual(token).into())); // +=
                    }
                    _ => state.add(1, Some(token.into())), // +
                }
            }
            '&' => {
                match iter.peek() {
                    Some('&') => {
                        iter.next();
                        state.add(2, Some(BoolOperation::And.into())); // &&
                    }
                    _ => state.add(1, Some(NumberOperation::BitAnd.into())), // &
                }
            }
            '|' => {
                match iter.peek() {
                    Some('|') => {
                        iter.next();
                        state.add(2, Some(BoolOperation::Or.into())); // ||
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
            '{' | '}' | '(' | ')' | '\'' | '"' => {
                unreachable!()
            }
            _ if char.is_ascii_whitespace() => {
                state.add(1, None);
            }
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '#' => {
                state.use_char_in_string(char);
            }
            '.' if state.is_buffer_number => {
                state.use_char_in_string(char);
            }
            '.' => {
                let token = Token::Dot;
                state.add(1, Some(token));
            }
            _ => {
                TokenizeError::UnexpectedChar
                    .print(state.offset_index + state.buffer_end);
                return Err(())
            }
        }
    }
    state.flush_buffer();
    Ok(state.tokens)
}

/// guarantees that `buffer_start <= buffer_end <= text.len()`
struct TokenizeState {
    tokens: Vec<TokenWithPos>,
    buffer: String,
    is_buffer_number: bool,
    buffer_start: usize,
    buffer_end: usize,
    offset_index: usize,
}
impl TokenizeState {
    const fn new(offset_index: usize) -> Self {
        Self {
            tokens: Vec::new(),
            buffer: String::new(),
            is_buffer_number: false,
            buffer_start: 0,
            buffer_end: 0,
            offset_index,
        }
    }

    fn use_char_in_string(&mut self, char: char) {
        if self.buffer.is_empty() {
            self.is_buffer_number = char.is_ascii_digit();
        }
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
                match token_text.as_str() {
                    "return" => TokenKeyword::Return.into(),
                    "if" => TokenKeyword::If.into(),
                    "while" => TokenKeyword::While.into(),
                    "import" => TokenKeyword::Import.into(),
                    "#extern" => TokenKeyword::Extern.into(),
                    _ => Token::String(token_text)
                }
            };
            let place_info = PositionInFile::new(self.offset_index + self.buffer_start, self.offset_index + self.buffer_end);
            self.tokens
                .push(TokenWithPos::new(token, place_info));
        }
        self.is_buffer_number = false;
        self.buffer_start = self.buffer_end;
    }
}
