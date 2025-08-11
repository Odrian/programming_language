use crate::parser::{BracketType, PositionInFile};
use crate::parser::two_sided_operation::*;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TokenWithPos<'text> {
    pub token: Token<'text>,
    pub position: PositionInFile,
}
impl<'text> TokenWithPos<'text> {
    pub fn new(token: Token<'text>, position: PositionInFile) -> Self {
        Self { token, position }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token<'text> {
    String(&'text [char]),         // any String
    NumberLiteral(&'text [char]),  // any String starting with a digit
    Comma,                      // ,
    Colon,                      // :
    DoubleColon,                // ::
    Arrow,                      // ->

    EqualOperation(EqualOperation),
    TwoSidedOperation(TwoSidedOperation),

    Bracket(Vec<TokenWithPos<'text>>, BracketType),
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum EqualOperation {
    Equal,                      // =
    ColonEqual,                 // :=
}

impl From<EqualOperation> for Token<'_> {
    fn from(value: EqualOperation) -> Self {
        Self::EqualOperation(value)
    }
}

impl From<NumberOperation> for Token<'_> {
    fn from(value: NumberOperation) -> Self {
        Self::TwoSidedOperation(TwoSidedOperation::Number(value))
    }
}

impl From<BoolOperation> for Token<'_> {
    fn from(value: BoolOperation) -> Self {
        Self::TwoSidedOperation(TwoSidedOperation::Bool(value))
    }
}

impl From<CompareOperator> for Token<'_> {
    fn from(value: CompareOperator) -> Self {
        Self::TwoSidedOperation(TwoSidedOperation::Compare(value))
    }
}
