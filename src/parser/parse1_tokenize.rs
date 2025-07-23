use std::fmt::Display;
use crate::error::CompilationError as CE;

pub fn tokenize(text: &[char]) -> Result<Vec<TokenWithPos>, CE> {
    let tokens = split_text(text);

    Ok(tokens)
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token<'x> {
    String(&'x [char]),         // any String
    NumberLiteral(&'x [char]),  // any String starting with a digit
    Comma,                  // ,
    Colon,                  // :
    DoubleColon,            // ::
    Equal,                  // =
    ColonEqual,             // :=
    Plus,                   // +
    CurlyBracketOpen,       // {
    CurlyBracketClose,      // }
    RoundBracketOpen,       // (
    RoundBracketClose,      // )
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct PositionInFile {
    start: usize,
    end: usize,
}
impl PositionInFile {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
    pub fn default() -> Self {
        Self::new(0, 0)
    }
}
impl Display for PositionInFile {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // TODO: implement parsing from index to column:line
        write!(f, "{}-{}", self.start, self.end)
    }
}

pub struct TokenWithPos<'x> {
    pub token: Token<'x>,
    pub position: PositionInFile,
}
impl<'x> TokenWithPos<'x> {
    pub fn new(token: Token<'x>, place: PositionInFile) -> Self {
        Self { token, position: place }
    }
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

fn split_text(text: &[char]) -> Vec<TokenWithPos> {
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

#[cfg(test)]
mod tests {
    use super::*;

    fn string_to_chars(s: &str) -> Vec<char> {
        s.chars().collect()
    }
    fn tokenize_text(text: &[char]) -> Result<Vec<Token>, CE> {
        tokenize(text).map(|v| v.into_iter().map(|t| t.token).collect())
    }

    #[test]
    fn test_tokens() {
        let v_cat = string_to_chars("cat");
        let v_323 = string_to_chars("323");
        let v_3d = string_to_chars("3d");

        let text = string_to_chars("cat+323,cat=3d{({),}");
        let expected = Ok(vec![
            Token::String(&v_cat),
            Token::Plus,
            Token::NumberLiteral(&v_323),
            Token::Comma,
            Token::String(&v_cat),
            Token::Equal,
            Token::NumberLiteral(&v_3d),
            Token::CurlyBracketOpen,
            Token::RoundBracketOpen,
            Token::CurlyBracketOpen,
            Token::RoundBracketClose,
            Token::Comma,
            Token::CurlyBracketClose,
        ]);
        let actual = tokenize_text(&text);
        assert_eq!(actual, expected);
    }
    #[test]
    fn test_token_colon() {
        let v_cat = &string_to_chars("cat");
        let v_dog = &string_to_chars("dog");

        let text = string_to_chars(":::dog::cat: :dog::::");
        let expected = Ok(vec![
            Token::DoubleColon,
            Token::Colon,
            Token::String(v_dog),
            Token::DoubleColon,
            Token::String(v_cat),
            Token::Colon,
            Token::Colon,
            Token::String(v_dog),
            Token::DoubleColon,
            Token::DoubleColon,
        ]);
        let actual = tokenize_text(&text);
        assert_eq!(actual, expected);
    }
    #[test]
    fn test_token_colon_equal() {
        let v_dog = &string_to_chars("dog");

        let text = string_to_chars(":= dog ::= :==");
        let expected = Ok(vec![
            Token::ColonEqual,
            Token::String(v_dog),
            Token::DoubleColon,
            Token::Equal,
            Token::ColonEqual,
            Token::Equal,
        ]);
        let actual = tokenize_text(&text);
        assert_eq!(actual, expected);
    }

    #[test]
    fn text_empty_string() {
        assert_eq!(tokenize_text(&[]), Ok(Vec::new()));
        assert_eq!(tokenize_text(&[' ']), Ok(Vec::new()));
    }
}
