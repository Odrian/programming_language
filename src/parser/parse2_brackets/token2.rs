use std::fmt::Debug;
use crate::parser::{PositionInFile, BracketType};

#[derive(Eq, PartialEq, Clone)]
pub struct Token2WithPos<'x> {
    pub token: Token2<'x>,
    pub position: PositionInFile,
}
impl<'x> Token2WithPos<'x> {
    pub fn new(token: Token2<'x>, position: PositionInFile) -> Token2WithPos<'x> {
        Self { token, position }
    }
}
impl<'x> Debug for Token2WithPos<'x> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.token)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token2<'x> {
    String(&'x [char]),
    NumberLiteral(&'x [char]),
    Comma,
    Colon,
    DoubleColon,
    EqualOperation(EqualOperation),
    TwoSidedOperation(TwoSidedOperation),
    Bracket(Vec<Token2WithPos<'x>>, BracketType),
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum TwoSidedOperation {
    Plus,
}
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum EqualOperation {
    Equal,
    ColonEqual,
}
