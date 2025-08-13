use crate::parser::{BracketType, PositionInFile};
use crate::parser::operations::*;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TokenWithPos {
    pub token: Token,
    pub position: PositionInFile,
}
impl TokenWithPos {
    pub fn new(token: Token, position: PositionInFile) -> Self {
        Self { token, position }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token {
    String(String),             // any String
    NumberLiteral(String),      // any String starting with a digit
    Semicolon,                  // ;
    Comma,                      // ,
    Colon,                      // :
    DoubleColon,                // ::
    Arrow,                      // ->

    EqualOperation(EqualOperation),
    Operation(TwoSidedOperation),
    UnaryOperation(OneSidedOperation),

    Bracket(Vec<TokenWithPos>, BracketType),
    Quotes(String),
    DoubleQuotes(String),
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum EqualOperation {
    Equal,                      // =
    ColonEqual,                 // :=
    OperationEqual(TwoSidedOperation), // _=
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
