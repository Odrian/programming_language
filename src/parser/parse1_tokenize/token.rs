use crate::parser::{BracketType, PositionInFile};

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

    EqualOperation(EqualOperation),
    TwoSidedOperation(TwoSidedOperation),

    Bracket(Vec<TokenWithPos<'text>>, BracketType),
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum TwoSidedOperation {
    Plus,                       // +
}
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum EqualOperation {
    Equal,                      // =
    ColonEqual,                 // :=
}
