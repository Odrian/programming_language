use std::fmt;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum TwoSidedOperation {
    Number(NumberOperation)
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum NumberOperation {
    Add,
    Sub,
    Mul,
    Div,
}

impl TwoSidedOperation {
    pub fn get_prior(&self) -> u8 {
        match self {
            Self::Number(num_op) => {
                match num_op {
                    NumberOperation::Add => 0,
                    NumberOperation::Sub => 0,
                    NumberOperation::Mul => 1,
                    NumberOperation::Div => 1,
                }
            }
        }
    }
}


impl From<NumberOperation> for TwoSidedOperation {
    fn from(value: NumberOperation) -> Self {
        Self::Number(value)
    }
}

impl fmt::Display for TwoSidedOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op_char = match self {
            TwoSidedOperation::Number(num_op) => match num_op {
                NumberOperation::Add =>    "+",
                NumberOperation::Sub =>   "-",
                NumberOperation::Mul =>   "*",
                NumberOperation::Div =>  "/",
            }
        };
        write!(f, "{op_char}")
    }
}
