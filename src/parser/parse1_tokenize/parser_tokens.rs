use crate::parser::PositionInFile;

use super::token::*;

pub fn split_text(text: &[char]) -> Vec<TokenWithPos> {
    let mut state = TokenizeState::new(text);

    // let mut line = 1;
    // let mut column = 1;

    while let Some(c) = state.peek_char() {
        // split_state.update_place_info(PositionInFile::new(line, column));
        // column += 1;
        match c {
            '\n' => {
                state.add(1, None);
                // line += 1;
                // column = 1;
            }
            '=' => {
                state.add(1, Some(Token::Equal));
            }
            ':' => {
                let char_2th = state.peek_nth_char(2);
                if char_2th == Some(':') {
                    state.add(2, Some(Token::DoubleColon));
                } else if char_2th == Some('=') {
                    state.add(2, Some(Token::ColonEqual));
                } else {
                    state.add(1, Some(Token::Colon));
                };
            }
            ',' | '+' | '{' | '}' | '(' | ')' => {
                let token = match c {
                    ',' => Token::Comma,
                    '+' => Token::Plus,
                    '{' => Token::CurlyBracketOpen,
                    '}' => Token::CurlyBracketClose,
                    '(' => Token::RoundBracketOpen,
                    ')' => Token::RoundBracketClose,
                    _ => unreachable!()
                };
                state.add(1, Some(token));
            }
            _ if c.is_ascii_whitespace() => {
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

/// guarantees that buffer_start <= buffer_end <= text.len()
struct TokenizeState<'x> {
    text: &'x [char],
    tokens: Vec<TokenWithPos<'x>>,
    buffer_start: usize,
    buffer_end: usize,
}
impl<'x> TokenizeState<'x> {
    fn new(text: &'x [char]) -> Self {
        Self {
            text,
            tokens: Vec::new(),
            buffer_start: 0,
            buffer_end: 0,
        }
    }
    fn peek_char(&self) -> Option<char> {
        self.text.get(self.buffer_end).cloned()
    }
    /// counting from 1
    fn peek_nth_char(&self, n: usize) -> Option<char> {
        self.text.get(self.buffer_end + n - 1).cloned()
    }

    fn use_char_in_string(&mut self) {
        self.buffer_end += 1;
        self.check_indexes();
    }
    fn add(&mut self, skip_chars: usize, token: Option<Token<'x>>) {
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
        if self.buffer_end > self.text.len() {
            panic!("Unexpected end of string");
        }
    }
}
