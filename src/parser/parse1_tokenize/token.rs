use crate::parser::PositionInFile;

pub struct TokenWithPos<'x> {
    pub token: Token<'x>,
    pub position: PositionInFile,
}
impl<'x> TokenWithPos<'x> {
    pub fn new(token: Token<'x>, place: PositionInFile) -> Self {
        Self { token, position: place }
    }
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
