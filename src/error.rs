use std::fmt;

use crate::{
    expr::{BinOp, UnOp, Val},
    tokeniser::Token,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprError {
    InvalidExpression,
    UnexpectedCharacter(char),
    UnexpectedToken(Token),
    UnmatchedLeftBracket,
    UnmatchedRightBracket,
    IntegerParseError(String),
    BinopTypeError(Val, Val, BinOp),
    UnopTypeError(Val, UnOp),
    Overflow(u64, u64, BinOp),
    VarError(String),
    CallError(String),
}

impl fmt::Display for ExprError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExprError::InvalidExpression => write!(f, "Empty or invalid expression"),
            ExprError::UnexpectedCharacter(c) => write!(f, "Unexpected character: {}", c),
            ExprError::UnexpectedToken(t) => write!(f, "Unexpected token: {}", t),
            ExprError::UnmatchedLeftBracket => write!(f, "Unmatched '('"),
            ExprError::UnmatchedRightBracket => write!(f, "Unmatched ')'"),
            ExprError::IntegerParseError(s) => write!(f, "Invalid integer: {}", s),
            ExprError::BinopTypeError(a, b, op) => {
                write!(f, "Type error: cannot apply {op} to {a} and {b}",)
            }
            ExprError::UnopTypeError(a, op) => write!(f, "Type error: cannot apply {op} to {a}",),
            ExprError::Overflow(a, b, op) => write!(f, "Overflow error: {a} {op} {b}",),
            ExprError::VarError(s) => write!(f, "Variable not found: {s}"),
            ExprError::CallError(s) => write!(f, "Function not found: {s}"),
        }
    }
}

pub type Result<T> = std::result::Result<T, ExprError>;
