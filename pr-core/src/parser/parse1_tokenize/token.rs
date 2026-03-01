use lsp_types::Range;
use crate::parser::BracketType;
use crate::parser::operations::*;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct RangedToken {
    pub token: Token,
    pub range: Range,
}

impl RangedToken {
    pub fn new(token: Token, range: Range) -> Self {
        Self { token, range }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token {
    Keyword(TokenKeyword),
    String(String),             // any String
    NumberLiteral(String),      // any String starting with a digit
    Semicolon,                  // ;
    Dot,                        // .
    DoubleDot,                  // ..
    Comma,                      // ,
    Colon,                      // :
    DoubleColon,                // ::
    Arrow,                      // ->

    EqualOperation(EqualOperation),
    Operation(TwoSidedOperation),
    UnaryOperation(OneSidedOperation),

    Bracket(Vec<RangedToken>, BracketType),
    Quotes(String),
    DoubleQuotes(String),
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum TokenKeyword {
    If, While, For,
    Return,
    Import,
    Extern,
}

/// `TwoSidedOperation` must not change type
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum EqualOperation {
    Equal,                      // =
    ColonEqual,                 // :=
    OperationEqual(TwoSidedOperation), // _=
}

impl From<TokenKeyword> for Token {
    fn from(value: TokenKeyword) -> Self {
        Self::Keyword(value)
    }
}

impl From<EqualOperation> for Token {
    fn from(value: EqualOperation) -> Self {
        Self::EqualOperation(value)
    }
}

impl From<OneSidedOperation> for Token {
    fn from(value: OneSidedOperation) -> Self {
        Self::UnaryOperation(value)
    }
}

impl From<TwoSidedOperation> for Token {
    fn from(value: TwoSidedOperation) -> Self {
        Self::Operation(value)
    }
}

impl From<NumberOperation> for Token {
    fn from(value: NumberOperation) -> Self {
        Self::Operation(TwoSidedOperation::Number(value))
    }
}

impl From<BoolOperation> for Token {
    fn from(value: BoolOperation) -> Self {
        Self::Operation(TwoSidedOperation::Bool(value))
    }
}

impl From<CompareOperator> for Token {
    fn from(value: CompareOperator) -> Self {
        Self::Operation(TwoSidedOperation::Compare(value))
    }
}
