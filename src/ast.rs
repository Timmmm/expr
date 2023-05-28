
use crate::error::{ExprError, Result};

#[derive(PartialEq, Eq, Debug)]
pub enum Val {
    Int(u64),
    Bool(bool),
}

pub enum Ast {
    BinOp(Box<Ast>, Box<Ast>, BinOp),
    UnOp(Box<Ast>, UnOp),
    Var(String),
    Call(String, Box<Ast>),
    Literal(Val),
}

impl Ast {
    pub fn evaluate(&self) -> Result<Val> {
        match self {
            Ast::BinOp(a, b, op) => match (a.evaluate()?, b.evaluate()?) {
                (Val::Int(a), Val::Int(b)) => {
                    match op {
                        BinOp::Add => Ok(Val::Int(a.checked_add(b).ok_or(ExprError::Overflow("add overflow".to_string()))?)),
                        BinOp::Sub => Ok(Val::Int(a.checked_sub(b).ok_or(ExprError::Overflow("subtract overflow".to_string()))?)),
                        BinOp::WrappingAdd => Ok(Val::Int(a.wrapping_add(b))),
                        BinOp::WrappingSub => Ok(Val::Int(a.wrapping_sub(b))),
                        BinOp::Mul => Ok(Val::Int(a * b)), // TODO: Maybe just make everything wrapping...?
                        BinOp::Div => Ok(Val::Int(a / b)),
                        BinOp::Mod => Ok(Val::Int(a % b)),
                        BinOp::ShiftLeft => Ok(Val::Int(a << b)),
                        BinOp::ShiftRight => Ok(Val::Int(a >> b)),
                        BinOp::Eq => Ok(Val::Bool(a == b)),
                        BinOp::NotEq => Ok(Val::Bool(a != b)),
                        BinOp::Less => Ok(Val::Bool(a < b)),
                        BinOp::LessEq => Ok(Val::Bool(a <= b)),
                        BinOp::Greater => Ok(Val::Bool(a > b)),
                        BinOp::GreaterEq => Ok(Val::Bool(a >= b)),
                        BinOp::BitAnd => Ok(Val::Int(a & b)),
                        BinOp::BitOr => Ok(Val::Int(a | b)),
                        BinOp::BitXor => Ok(Val::Int(a ^ b)),
                        _ => Err(ExprError::TypeError("invalid binary operator for ints".to_string())),
                    }
                }
                (Val::Bool(a), Val::Bool(b)) => {
                    match op {
                        BinOp::Eq => Ok(Val::Bool(a == b)),
                        BinOp::NotEq => Ok(Val::Bool(a != b)),
                        BinOp::LogicalAnd => Ok(Val::Bool(a && b)),
                        BinOp::LogicalOr => Ok(Val::Bool(a || b)),
                        _ => Err(ExprError::TypeError("invalid binary operator for bools".to_string())),
                    }
                }
                _ => Err(ExprError::TypeError("binary operand types must match".to_string())),
            }
            Ast::UnOp(a, op) => match a.evaluate()? {
                Val::Int(a) => {
                    match op {
                        UnOp::BitNot => Ok(Val::Int(!a)),
                        _ => Err(ExprError::TypeError("invalid unary operator for ints".to_string())),
                    }
                }
                Val::Bool(a) => {
                    match op {
                        UnOp::LogicalNot => Ok(Val::Bool(!a)),
                        _ => Err(ExprError::TypeError("invalid unary operator for bools".to_string())),
                    }
                }
            }

            _ => todo!()
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum BinOp {
    Add,
    Sub,
    WrappingAdd,
    WrappingSub,
    Mul,
    Div,
    Mod,
    ShiftLeft,
    ShiftRight,
    Eq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    BitAnd,
    BitOr,
    BitXor,
    LogicalAnd,
    LogicalOr,
}

#[derive(PartialEq, Eq, Debug)]
pub enum UnOp {
    LogicalNot,
    BitNot,
}