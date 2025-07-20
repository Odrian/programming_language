use std::fmt::Display;
use crate::error::CompilationError as CE;
use std::fs;

pub fn tokenize_file(filepath: &str) -> Result<Vec<TokenWithPos>, CE> {
    let text = fs::read_to_string(filepath).map_err(|error| CE::FileIO {
        filepath: filepath.to_string(),
        error: error.kind(), 
    })?;

    tokenize(&text)
}

pub fn tokenize(text: &str) -> Result<Vec<TokenWithPos>, CE> {
    let tokens = split_text(&text);

    Ok(tokens)
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token {
    String(String),         // any String
    NumberLiteral(String),  // any String starting with a digit
    Equal,                  // =
    Plus,                   // +
    CurlyBracketOpen,       // {
    CurlyBracketClose,      // }
    RoundBracketOpen,       // (
    RoundBracketClose,      // )
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct PositionInFile {
    line: u32,
    column: u32,
}
impl PositionInFile {
    pub fn new(line: u32, column: u32) -> Self {
        Self { line, column }
    }
    pub fn default() -> Self {
        Self::new(0, 0)
    }
}
impl Display for PositionInFile {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

pub struct TokenWithPos {
    pub token: Token,
    pub position: PositionInFile,
}
impl TokenWithPos {
    pub fn new(token: Token, place: PositionInFile) -> Self {
        Self { token, position: place }
    }
}

struct TokenizeState {
    tokens: Vec<TokenWithPos>,
    buffer: String,
    buffer_place_info: PositionInFile,
    now_place_info: PositionInFile,
}
impl TokenizeState {
    fn new() -> Self {
        Self {
            tokens: Vec::new(),
            buffer: String::new(),
            buffer_place_info: PositionInFile::default(),
            now_place_info: PositionInFile::default(),
        }
    }
    fn add_char(&mut self, c: char) {
        self.buffer.push(c);
    }
    fn update_place_info(&mut self, place_info: PositionInFile) {
        self.now_place_info = place_info;
    }
    fn flush_buffer(&mut self) {
        if !self.buffer.is_empty() {
            let first_char = self.buffer.chars().next().unwrap();

            let buffer = std::mem::take(&mut self.buffer); // makes self.buffer = ""
            let token = if first_char.is_ascii_digit() {
                Token::NumberLiteral(buffer)
            } else {
                Token::String(buffer)
            };
            self.tokens
                .push(TokenWithPos::new(token, self.buffer_place_info));
        }
        self.buffer_place_info = self.now_place_info;
    }
    fn add(&mut self, token: Token) {
        self.tokens
            .push(TokenWithPos::new(token, self.now_place_info));
    }
}

fn split_text(text: &str) -> Vec<TokenWithPos> {
    let mut chars = text.chars().peekable();

    let mut split_state = TokenizeState::new();

    let mut line = 1;
    let mut column = 1;

    while let Some(c) = chars.next() {
        split_state.update_place_info(PositionInFile::new(line, column));
        column += 1;
        match c {
            '\n' => {
                split_state.flush_buffer();
                line += 1;
                column = 1;
            }
            '=' => {
                split_state.flush_buffer();
                split_state.add(Token::Equal);
            }
            '+' | '{' | '}' | '(' | ')' => {
                let token = match c {
                    '+' => Token::Plus,
                    '{' => Token::CurlyBracketOpen,
                    '}' => Token::CurlyBracketClose,
                    '(' => Token::RoundBracketOpen,
                    ')' => Token::RoundBracketClose,
                    _ => unreachable!()
                };
                split_state.flush_buffer();
                split_state.add(token);
            }
            _ if c.is_ascii_whitespace() => {
                split_state.flush_buffer();
            }
            _ => {
                split_state.add_char(c);
            }
        }
    }
    split_state.flush_buffer();
    split_state.tokens
}
