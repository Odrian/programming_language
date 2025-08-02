use crate::parser::{BracketType, PositionInFile};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TokenWithPos<'x> {
    pub token: Token<'x>,
    pub position: PositionInFile,
}
impl<'x> TokenWithPos<'x> {
    pub fn new(token: Token<'x>, position: PositionInFile) -> Self {
        Self { token, position }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token<'x> {
    String(&'x [char]),         // any String
    NumberLiteral(&'x [char]),  // any String starting with a digit
    Comma,                      // ,
    Colon,                      // :
    DoubleColon,                // ::

    EqualOperation(EqualOperation),
    TwoSidedOperation(TwoSidedOperation),

    Bracket(Vec<TokenWithPos<'x>>, BracketType),
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
