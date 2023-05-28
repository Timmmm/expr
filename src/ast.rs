
use crate::error::{ExprError, Result};

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum Val {
    Int(u64),
    Bool(bool),
}

#[derive(PartialEq, Eq, Debug)]
pub enum Ast {
    BinOp(Box<Ast>, Box<Ast>, BinOp),
    UnOp(Box<Ast>, UnOp),
    Var(String),
    Call(String, Box<Ast>),
    Literal(Val),
}

pub trait Context {
    fn var(&self, name: &str) -> Option<Val>;
    fn call(&self, name: &str, arg: Val) -> Option<Val>;
}

impl Ast {
    pub fn evaluate(&self, context: &dyn Context) -> Result<Val> {
        match self {
            Ast::BinOp(a, b, op) => match (a.evaluate(context)?, b.evaluate(context)?) {
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
            Ast::UnOp(a, op) => match a.evaluate(context)? {
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
            Ast::Literal(v) => Ok(*v),
            Ast::Call(func, param) => context.call(func, param.evaluate(context)?).ok_or(ExprError::NameError(format!("function '{}' not found", func))),
            Ast::Var(name) => context.var(name).ok_or(ExprError::NameError(format!("variable '{}' not found", name))),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
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

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum UnOp {
    LogicalNot,
    BitNot,
}

pub trait Precedence {
    fn precedence(&self) -> u8;
}

impl Precedence for BinOp {
    fn precedence(&self) -> u8 {
        // Same as Go. https://go.dev/ref/spec#Operators
        match self {
            BinOp::LogicalOr => 0,
            BinOp::LogicalAnd => 1,
            BinOp::Eq | BinOp::NotEq | BinOp::Less | BinOp::LessEq | BinOp::Greater | BinOp::GreaterEq => 2,
            BinOp::Add | BinOp::Sub | BinOp::WrappingAdd | BinOp::WrappingSub | BinOp::BitOr | BinOp::BitXor => 3,
            BinOp::Mul | BinOp::Div | BinOp::Mod | BinOp::BitAnd | BinOp::ShiftLeft | BinOp::ShiftRight => 4,
        }
    }
}

impl Precedence for UnOp {
    fn precedence(&self) -> u8 {
        match self {
            UnOp::LogicalNot | UnOp::BitNot => 5,
        }
    }
}
