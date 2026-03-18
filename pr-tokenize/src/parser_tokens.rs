use std::str::Chars;
use lsp_types::{Position, Range};
use pr_common::error::ErrorQueue;
use pr_common::operations::*;
use pr_common::BracketType;
use crate::error::TokenizeError;
use crate::token::*;

pub fn parse_tokens(errors: &mut ErrorQueue, text: &str) -> Vec<RangedToken> {
    let (token, _) = parse_inside_brackets(errors, &mut text.chars(), Position::default(), None);
    let Token::Bracket(vec, _) = token.token else { unreachable!() };
    vec
}

#[inline]
fn add_one(next: Option<char>, position: &mut Position) {
    let Some(next) = next else { return; };

    if next == '\n' {
        position.line += 1;
        position.character = 0;
    } else {
        position.character += 1;
    }
}

struct IndexedTextBuffer {
    text: String,
    start_buffer_index: Position,
    index: Position,
}

impl IndexedTextBuffer {
    fn new(start_buffer_index: Position) -> Self {
        Self {
            text: String::new(),
            start_buffer_index,
            index: start_buffer_index,
        }
    }
    fn set_new_index(&mut self, index: Position) {
        self.text.clear();
        self.index = index;
        self.start_buffer_index = index;
    }
    fn flush(&mut self, result_tokens: &mut Vec<RangedToken>, errors: &mut ErrorQueue) {
        if self.index != self.start_buffer_index {
            let mut tokens = split_text_without_brackets(errors, &self.text, self.start_buffer_index);
            self.text.clear();
            result_tokens.append(&mut tokens);
        }
    }
    fn add_char(&mut self, ch: char) {
        self.text.push(ch);
        add_one(Some(ch), &mut self.index);
    }
    fn skip_char(&mut self) {
        add_one(Some('a'), &mut self.index)
    }
    fn consume(&mut self) -> String {
        self.start_buffer_index = self.index;
        std::mem::take(&mut self.text)
    }
}

fn parse_inside_brackets(
    errors: &mut ErrorQueue,
    text: &mut Chars,
    start_position: Position,
    open_bracket_type: Option<BracketType>
) -> (RangedToken, Position) {
    let mut result_tokens = Vec::new();

    let mut buffer = {
        let mut start_buffer_index = start_position;
        if open_bracket_type.is_some() { add_one(Some('('), &mut start_buffer_index); }
        IndexedTextBuffer::new(start_buffer_index)
    };

    let mut text_was: Chars;
    loop {
        text_was = text.clone();
        let Some(char) = text.next() else { break; };

        if char == '"' || char == '\'' || char == '`' {
            buffer.flush(&mut result_tokens, errors);

            let quotes_start_position = buffer.index;
            loop { // here index map to previous char in start of loop
                let Some(next_char) = text.next() else {
                    errors.add_diag(
                        TokenizeError::QuotesNotClosed
                            .diagnostic(quotes_start_position)
                    );
                    break
                };
                if next_char == char {
                    buffer.skip_char();
                    break;
                } else if next_char == '\\' && char != '`' {
                    buffer.skip_char();
                    let escape_char = text.next();
                    match escape_char {
                        Some('\\') => buffer.add_char('\\'),
                        Some('0') => buffer.add_char('\0'),
                        Some('t') => buffer.add_char('\t'),
                        Some('n') => buffer.add_char('\n'),
                        Some('r') => buffer.add_char('\r'),
                        Some('"') => buffer.add_char('"'),
                        Some('\'') => buffer.add_char('\''),
                        Some('x') => unimplemented!("8bit escapes"),
                        Some(ch) => {
                            errors.add_diag(
                                TokenizeError::IncorrectEscape(Some(ch))
                                    .diagnostic(buffer.index)
                            );
                            buffer.add_char(ch);
                        }
                        None => {
                            errors.add_diag(
                                TokenizeError::IncorrectEscape(None)
                                    .diagnostic(buffer.index)
                            );
                        }
                    }
                    continue
                } else {
                    buffer.add_char(next_char);
                }
            }

            let quotes_text = buffer.consume();
            let token = match char {
                '\'' => Token::Quotes(quotes_text),
                '"' => Token::DoubleQuotes(quotes_text),
                '`' => Token::String(quotes_text),
                _ => unreachable!()
            };

            let position = Range::new(quotes_start_position, buffer.index);
            result_tokens.push(RangedToken::new(token, position));
        } else if let Some(bracket_type) = is_open_bracket(char) {
            // open bracket
            buffer.flush(&mut result_tokens, errors);

            let (new_token, new_index) =
                parse_inside_brackets(errors, text, buffer.index, Some(bracket_type));
            result_tokens.push(new_token);
            buffer.set_new_index(new_index);
        } else if let Some(bracket_type) = is_close_bracket(char) {
            // close bracket
            let Some(open_bracket_type) = open_bracket_type else {
                errors.add_diag(
                    TokenizeError::BracketNotOpened(bracket_type)
                        .diagnostic(buffer.index)
                );
                buffer.skip_char();
                continue;
            };

            if bracket_type != open_bracket_type {
                errors.add_diag(
                    TokenizeError::WrongBracketClosed { expected_bracket: open_bracket_type, actual_bracket: bracket_type }
                        .diagnostic(buffer.index)
                );

                // expect {  [  }, so unconsume close bracket and step out
                *text = text_was;
            }

            buffer.flush(&mut result_tokens, errors);

            let result_token = Token::Bracket(result_tokens, open_bracket_type);

            let mut index = buffer.index;
            add_one(Some(char), &mut index);
            let range = Range::new(start_position, index);

            return (RangedToken::new(result_token, range), index);
        } else {
            buffer.add_char(char);
        }
    }
    buffer.flush(&mut result_tokens, errors);

    if let Some(open_bracket_type) = open_bracket_type {
        errors.add_diag(
            TokenizeError::BracketNotClosed(open_bracket_type)
                .diagnostic(start_position)
        );
    }

    let unused_bracket_type = BracketType::Round;
    let unused_position = Position::default();

    let result_token = Token::Bracket(result_tokens, unused_bracket_type);
    let range = Range::new(start_position, buffer.index);
    (RangedToken::new(result_token, range), unused_position)
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

pub fn split_text_without_brackets(
    errors: &mut ErrorQueue,
    text: &str,
    offset_position: Position
) -> Vec<RangedToken> {
    let mut iter = text.chars().peekable();
    let mut state = TokenizeState::new(offset_position);

    while let Some(char) = iter.next() {
        match char {
            '=' => {
                if iter.peek() == Some(&'=') {
                    iter.next();
                    state.add(2, CompareOperator::Equal.into()); // ==
                } else {
                    state.add(1, EqualOperation::Equal.into()); // =
                }
            }
            '!' => {
                match iter.peek() {
                    Some('=') => {
                        iter.next();
                        state.add(2, CompareOperator::NotEqual.into()); // !=
                    },
                    _ => state.add(1, OneSidedOperation::BoolNot.into()), // !
                }
            }
            '>' => {
                match iter.peek() {
                    Some('=') => {
                        iter.next();
                        state.add(2, CompareOperator::GreaterEqual.into()); // >=
                    }
                    _ => state.add(1, CompareOperator::Greater.into()), // >
                }
            }
            '<' => {
                match iter.peek() {
                    Some('=') => {
                        iter.next();
                        state.add(2, CompareOperator::LessEqual.into()); // <=
                    }
                    _ => state.add(1, CompareOperator::Less.into()), // <
                }
            }
            ':' => {
                match iter.peek() {
                    Some('=') => {
                        iter.next();
                        state.add(2, EqualOperation::ColonEqual.into()); // :=
                    }
                    Some(':') => {
                        iter.next();
                        state.add(2, Token::DoubleColon); // ::
                    }
                    _ => state.add(1, Token::Colon), // :
                }
            }
            '-' => {
                match iter.peek() {
                    Some('=') => {
                        iter.next();
                        state.add(2, EqualOperation::OperationEqual(NumberOperation::Sub.into()).into()); // -=
                    }
                    Some('>') => {
                        iter.next();
                        state.add(2, Token::Arrow); // ->
                    }
                    _ => state.add(1, NumberOperation::Sub.into()), // -
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
                        state.add(2, EqualOperation::OperationEqual(token).into()); // +=
                    }
                    _ => state.add(1, token.into()), // +
                }
            }
            '&' => {
                match iter.peek() {
                    Some('&') => {
                        iter.next();
                        state.add(2, BoolOperation::And.into()); // &&
                    }
                    _ => state.add(1, NumberOperation::BitAnd.into()), // &
                }
            }
            '|' => {
                match iter.peek() {
                    Some('|') => {
                        iter.next();
                        state.add(2, BoolOperation::Or.into()); // ||
                    }
                    _ => state.add(1, NumberOperation::BitOr.into()), // |
                }
            }
            ',' => {
                let token = Token::Comma;
                state.add(1, token);
            }
            ';' => {
                let token = Token::Semicolon;
                state.add(1, token);
            }
            '{' | '}' | '(' | ')' | '\'' | '"' => {
                unreachable!()
            }
            _ if char.is_ascii_whitespace() => {
                state.add_whitespace(char);
            }
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '#' => {
                state.use_char_in_string(char);
            }
            '.' if state.is_buffer_number => {
                if state.buffer.ends_with('.') {
                    state.buffer.pop();
                    state.buffer_end.character -= 1; // correct because previous char actually dot
                    state.flush_buffer();
                    state.add(2, Token::DoubleDot); // correct because buffer is flushed
                } else {
                    state.use_char_in_string(char);
                }
            }
            '.' => {
                if iter.peek() == Some(&'.') {
                    iter.next();
                    let token = Token::DoubleDot;
                    state.add(2, token);
                } else {
                    let token = Token::Dot;
                    state.add(1, token);
                }
            }
            _ => {
                errors.add_diag(
                    TokenizeError::UnexpectedChar
                        .diagnostic(state.buffer_end)
                );
                state.use_char_in_string(char);
            }
        }
    }
    state.flush_buffer();
    state.tokens
}

/// guarantees that `buffer_start <= buffer_end <= text.len()`
struct TokenizeState {
    tokens: Vec<RangedToken>,
    buffer: String,
    is_buffer_number: bool,
    buffer_start: Position,
    buffer_end: Position,
}
impl TokenizeState {
    const fn new(offset_position: Position) -> Self {
        Self {
            tokens: Vec::new(),
            buffer: String::new(),
            is_buffer_number: false,
            buffer_start: offset_position,
            buffer_end: offset_position,
        }
    }

    fn use_char_in_string(&mut self, char: char) {
        if self.buffer.is_empty() {
            self.is_buffer_number = char.is_ascii_digit();
        }
        self.buffer_end.character += 1; // correct because char is not new line
        self.buffer.push(char);
    }
    #[inline]
    fn add(&mut self, skip_chars: u32, token: Token) {
        self.flush_buffer(); // after start==end

        self.buffer_end.character += skip_chars;

        let place_info = Range::new(self.buffer_start, self.buffer_end);
        self.tokens.push(RangedToken::new(token, place_info));
        self.buffer_start = self.buffer_end;
        // here start==end
    }
    fn add_whitespace(&mut self, ch: char) {
        self.flush_buffer(); // after start==end

        add_one(Some(ch), &mut self.buffer_start);
        self.buffer_end = self.buffer_start;
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
                    "for" => TokenKeyword::For.into(),
                    "while" => TokenKeyword::While.into(),
                    "import" => TokenKeyword::Import.into(),
                    "#extern" => TokenKeyword::Extern.into(),
                    _ => Token::String(token_text)
                }
            };
            let place_info = Range::new(self.buffer_start, self.buffer_end);
            self.tokens
                .push(RangedToken::new(token, place_info));
        }
        self.is_buffer_number = false;
        self.buffer_start = self.buffer_end;
    }
}
