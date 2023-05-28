use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprError {
    SyntaxError(String),
    TypeError(String),
    Overflow(String),
    NameError(String),
}

impl fmt::Display for ExprError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExprError::SyntaxError(s) => write!(f, "Syntax error: {}", s),
            ExprError::TypeError(s) => write!(f, "Type error: {}", s),
            ExprError::Overflow(s) => write!(f, "Overflow: {}", s),
            ExprError::NameError(s) => write!(f, "Name error: {}", s),
        }
    }
}

pub type Result<T> = std::result::Result<T, ExprError>;
