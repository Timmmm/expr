use std::fmt;

use crate::ast::{BinOp, Val, UnOp};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprError {
    SyntaxError(String), // TODO: use a proper error type
    BinopTypeError(Val, Val, BinOp),
    UnopTypeError(Val, UnOp),
    Overflow(u64, u64, BinOp),
    VarError(String),
    CallError(String),
}

impl fmt::Display for ExprError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExprError::SyntaxError(s) => write!(f, "Syntax error: {}", s),
            ExprError::BinopTypeError(a, b, op) => write!(
                f,
                "Type error: cannot apply {op} to {a} and {b}",
            ),
            ExprError::UnopTypeError(a, op) => write!(
                f,
                "Type error: cannot apply {op} to {a}",
            ),
            ExprError::Overflow(a, b, op) => write!(
                f,
                "Overflow error: {a} {op} {b}",
            ),
            ExprError::VarError(s) => write!(f, "Variable not found: {s}"),
            ExprError::CallError(s) => write!(f, "Function not found: {s}"),
        }
    }
}

pub type Result<T> = std::result::Result<T, ExprError>;
