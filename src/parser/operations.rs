use std::fmt;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum OneSidedOperation {
    BoolNot,
    UnaryMinus,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum TwoSidedOperation {
    Number(NumberOperation),
    Bool(BoolOperation),
    Compare(CompareOperator),
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum NumberOperation {
    Add,
    Sub,
    Mul,
    Div,
    BitAnd,
    BitOr,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum BoolOperation {
    And,
    Or,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum CompareOperator {
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

impl TwoSidedOperation {
    pub fn get_prior(&self) -> u8 {
        match self {
            Self::Number(num_op) => match num_op {
                NumberOperation::Add => 4,
                NumberOperation::Sub => 4,
                NumberOperation::Mul => 5,
                NumberOperation::Div => 5,
                NumberOperation::BitAnd => 3,
                NumberOperation::BitOr => 2,
            }
            Self::Compare(_comp_op) => 6,
            Self::Bool(bool_op) => match bool_op {
                BoolOperation::And => 1,
                BoolOperation::Or => 0,
            }
        }
    }
}

impl CompareOperator {
    pub fn is_equal_op(&self) -> bool {
        matches!(self, Self::Equal | Self::NotEqual)
    }
}


impl From<NumberOperation> for TwoSidedOperation {
    fn from(value: NumberOperation) -> Self {
        Self::Number(value)
    }
}

impl From<BoolOperation> for TwoSidedOperation {
    fn from(value: BoolOperation) -> Self {
        Self::Bool(value)
    }
}

impl From<CompareOperator> for TwoSidedOperation {
    fn from(value: CompareOperator) -> Self {
        Self::Compare(value)
    }
}

impl fmt::Display for OneSidedOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op_char = match self {
            Self::BoolNot =>        "!",
            Self::UnaryMinus => "-"
        };
        write!(f, "{op_char}")
    }
}

impl fmt::Display for TwoSidedOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op_char = match self {
            Self::Number(num_op) => match num_op {
                NumberOperation::Add =>     "+",
                NumberOperation::Sub =>     "-",
                NumberOperation::Mul =>     "*",
                NumberOperation::Div =>     "/",
                NumberOperation::BitOr =>   "|",
                NumberOperation::BitAnd =>  "&",
            }
            Self::Compare(comp_op) => match comp_op {
                CompareOperator::Equal => "==",
                CompareOperator::NotEqual => "!=",
                CompareOperator::Greater => ">",
                CompareOperator::GreaterEqual => ">=",
                CompareOperator::Less => "<",
                CompareOperator::LessEqual => "<=",
            }
            Self::Bool(bool_op) => match bool_op {
                BoolOperation::And => "&&",
                BoolOperation::Or => "||",
            }
        };
        write!(f, "{op_char}")
    }
}
