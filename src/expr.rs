use std::fmt::Display;

use crate::error::{ExprError, Result};

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum Val {
    Int(u64),
    Bool(bool),
}

impl Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Int(i) => write!(f, "{}", i),
            Val::Bool(b) => write!(f, "{}", b),
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum Expr {
    BinOp(Box<Expr>, Box<Expr>, BinOp),
    UnOp(Box<Expr>, UnOp),
    Var(String),
    Call(String, Box<Expr>),
    Literal(Val),
}

pub trait Context {
    fn var(&self, name: &str) -> Option<Val>;
    fn call(&self, name: &str, arg: Val) -> Option<Val>;
}

impl Expr {
    pub fn evaluate(&self, context: &dyn Context) -> Result<Val> {
        match self {
            Expr::BinOp(a, b, op) => {
                match (a.evaluate(context)?, b.evaluate(context)?) {
                    (Val::Int(a), Val::Int(b)) => {
                        match op {
                            BinOp::Add => Ok(Val::Int(
                                a.checked_add(b)
                                    .ok_or(ExprError::Overflow(a, b, BinOp::Add))?,
                            )),
                            BinOp::Sub => Ok(Val::Int(
                                a.checked_sub(b)
                                    .ok_or(ExprError::Overflow(a, b, BinOp::Sub))?,
                            )),
                            BinOp::Mul => Ok(Val::Int(
                                a.checked_mul(b)
                                    .ok_or(ExprError::Overflow(a, b, BinOp::Mul))?,
                            )),
                            BinOp::Div => Ok(Val::Int(
                                a.checked_div(b)
                                    .ok_or(ExprError::Overflow(a, b, BinOp::Div))?,
                            )),
                            BinOp::Mod => Ok(Val::Int(
                                a.checked_rem(b)
                                    .ok_or(ExprError::Overflow(a, b, BinOp::Mod))?,
                            )),
                            BinOp::ShiftLeft => Ok(Val::Int(
                                a.checked_shl(b.try_into().unwrap_or(u32::MAX)).unwrap_or(0),
                            )),
                            BinOp::ShiftRight => Ok(Val::Int(
                                a.checked_shr(b.try_into().unwrap_or(u32::MAX)).unwrap_or(0),
                            )),
                            BinOp::Eq => Ok(Val::Bool(a == b)),
                            BinOp::NotEq => Ok(Val::Bool(a != b)),
                            BinOp::Less => Ok(Val::Bool(a < b)),
                            BinOp::LessEq => Ok(Val::Bool(a <= b)),
                            BinOp::Greater => Ok(Val::Bool(a > b)),
                            BinOp::GreaterEq => Ok(Val::Bool(a >= b)),
                            BinOp::BitAnd => Ok(Val::Int(a & b)),
                            BinOp::BitOr => Ok(Val::Int(a | b)),
                            BinOp::BitXor => Ok(Val::Int(a ^ b)),
                            _ => Err(ExprError::BinopTypeError(Val::Int(a), Val::Int(b), *op)),
                        }
                    }
                    (Val::Bool(a), Val::Bool(b)) => match op {
                        BinOp::Eq => Ok(Val::Bool(a == b)),
                        BinOp::NotEq => Ok(Val::Bool(a != b)),
                        BinOp::LogicalAnd => Ok(Val::Bool(a && b)),
                        BinOp::LogicalOr => Ok(Val::Bool(a || b)),
                        _ => Err(ExprError::BinopTypeError(Val::Bool(a), Val::Bool(b), *op)),
                    },
                    (a, b) => Err(ExprError::BinopTypeError(a, b, *op)),
                }
            }
            Expr::UnOp(a, op) => match a.evaluate(context)? {
                Val::Int(a) => match op {
                    UnOp::BitNot => Ok(Val::Int(!a)),
                    _ => Err(ExprError::UnopTypeError(Val::Int(a), *op)),
                },
                Val::Bool(a) => match op {
                    UnOp::LogicalNot => Ok(Val::Bool(!a)),
                    _ => Err(ExprError::UnopTypeError(Val::Bool(a), *op)),
                },
            },
            Expr::Literal(v) => Ok(*v),
            Expr::Call(func, param) => context
                .call(func, param.evaluate(context)?)
                .ok_or(ExprError::CallError(func.clone())),
            Expr::Var(name) => context.var(name).ok_or(ExprError::VarError(name.clone())),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum BinOp {
    Add,
    Sub,
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

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinOp::Add => "+",
                BinOp::Sub => "-",
                BinOp::Mul => "*",
                BinOp::Div => "/",
                BinOp::Mod => "%",
                BinOp::ShiftLeft => "<<",
                BinOp::ShiftRight => ">>",
                BinOp::Eq => "==",
                BinOp::NotEq => "!=",
                BinOp::Less => "<",
                BinOp::LessEq => "<=",
                BinOp::Greater => ">",
                BinOp::GreaterEq => ">=",
                BinOp::BitAnd => "&",
                BinOp::BitOr => "|",
                BinOp::BitXor => "^",
                BinOp::LogicalAnd => "&&",
                BinOp::LogicalOr => "||",
            }
        )
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum UnOp {
    LogicalNot,
    BitNot,
}

impl Display for UnOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnOp::LogicalNot => "!",
                UnOp::BitNot => "~",
            }
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;

    struct SimpleContext {}

    impl Context for SimpleContext {
        fn var(&self, name: &str) -> Option<Val> {
            match name {
                "x" => Some(Val::Int(1)),
                "y" => Some(Val::Int(2)),
                _ => None,
            }
        }
        fn call(&self, name: &str, param: Val) -> Option<Val> {
            match name {
                "inc" => match param {
                    Val::Int(x) => Some(Val::Int(x + 1)),
                    _ => None,
                },
                _ => None,
            }
        }
    }

    #[test]
    fn test_evaluation() {
        let expr = Expr::BinOp(
            Box::new(Expr::BinOp(
                Box::new(Expr::BinOp(
                    Box::new(Expr::Var("x".to_string())),
                    Box::new(Expr::BinOp(
                        Box::new(Expr::Var("y".to_string())),
                        Box::new(Expr::Literal(Val::Int(3))),
                        BinOp::Mul,
                    )),
                    BinOp::Add,
                )),
                Box::new(Expr::BinOp(
                    Box::new(Expr::BinOp(
                        Box::new(Expr::Literal(Val::Int(4))),
                        Box::new(Expr::Literal(Val::Int(1))),
                        BinOp::Add,
                    )),
                    Box::new(Expr::Call(
                        "inc".to_string(),
                        Box::new(Expr::Literal(Val::Int(2))),
                    )),
                    BinOp::Mul,
                )),
                BinOp::Add,
            )),
            Box::new(Expr::Literal(Val::Int(1))),
            BinOp::Add,
        );
        let context = SimpleContext {};
        assert_eq!(
            expr.evaluate(&context),
            Ok(Val::Int(1 + 2 * 3 + (4 + 1) * (2 + 1) + 1))
        );
    }
}
