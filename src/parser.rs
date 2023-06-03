use crate::expr::{Expr, BinOp, UnOp};
use crate::error::{ExprError, Result};
use crate::tokeniser::Token;

#[derive(PartialEq, Eq)]
enum State {
    // Expecting expression (variable, left bracket, function call, literal).
    ExpectingExpr,
    // Expecting binary operator or right bracket.
    ExpectingOperator,
}

#[derive(Debug, PartialEq, Eq)]
enum Operator {
    BinOp(BinOp),
    UnOp(UnOp),
    Call(String),
    Brackets,
}

impl Operator {
    // Same as Rust: https://doc.rust-lang.org/reference/expressions.html#expression-precedence
    fn precedence(&self) -> u8 {
        match self {
            Operator::UnOp(_) | Operator::Call(_) => 10,
            Operator::BinOp(op) => match op {
                BinOp::Mul | BinOp::Div | BinOp::Mod => 9,
                BinOp::Add | BinOp::Sub => 8,
                BinOp::ShiftLeft | BinOp::ShiftRight => 7,
                BinOp::BitAnd => 6,
                BinOp::BitXor => 5,
                BinOp::BitOr => 4,
                BinOp::Eq
                | BinOp::NotEq
                | BinOp::Less
                | BinOp::LessEq
                | BinOp::Greater
                | BinOp::GreaterEq => 3,
                BinOp::LogicalAnd => 2,
                BinOp::LogicalOr => 1,
            },
            // Not relevant.
            Operator::Brackets => 0,
        }
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Expr> {
    let mut iter = tokens.into_iter().peekable();

    // Parse using the shunting yard algorithm. We also use a state machine
    // to check for valid syntax.
    let mut operator_stack = Vec::new();
    let mut expr_stack = Vec::new();

    let mut state = State::ExpectingExpr;

    // Process an operation that was popped from the top of the operator stack.
    // Return true if it was brackets.
    fn process_op(op: Operator, expr_stack: &mut Vec<Expr>) {
        match op {
            Operator::Brackets => {}
            Operator::BinOp(op) => {
                let rhs = expr_stack.pop().unwrap();
                let lhs = expr_stack.pop().unwrap();
                expr_stack.push(Expr::BinOp(Box::new(lhs), Box::new(rhs), op));
            }
            Operator::UnOp(op) => {
                let operand = expr_stack.pop().unwrap();
                expr_stack.push(Expr::UnOp(Box::new(operand), op));
            }
            Operator::Call(name) => {
                let param = expr_stack.pop().unwrap();
                expr_stack.push(Expr::Call(name, Box::new(param)));
            }
        }
    }

    while let Some(token) = iter.next() {
        match token {
            Token::Literal(val) => {
                if state != State::ExpectingExpr {
                    return Err(ExprError::UnexpectedToken(token));
                }
                state = State::ExpectingOperator;
                expr_stack.push(Expr::Literal(val));
            }
            Token::Identifier(s) => {
                if state != State::ExpectingExpr {
                    return Err(ExprError::UnexpectedToken(Token::Identifier(s)));
                }
                if let Some(Token::LeftBracket) = iter.peek() {
                    // Function call.
                    operator_stack.push(Operator::Call(s.clone()));
                    // Still expecting an operand.
                } else {
                    // Variable.
                    expr_stack.push(Expr::Var(s.clone()));
                    state = State::ExpectingOperator;
                }
            }
            Token::UnOp(op) => {
                if state != State::ExpectingExpr {
                    return Err(ExprError::UnexpectedToken(token));
                }
                // No need to pop operators with higher precedence since there aren't any.
                operator_stack.push(Operator::UnOp(op));
            }
            Token::BinOp(op) => {
                if state != State::ExpectingOperator {
                    return Err(ExprError::UnexpectedToken(token));
                }
                state = State::ExpectingExpr;

                let op = Operator::BinOp(op);
                // Pop operators until we find one with lower precedence (or brackets).
                while let Some(top_op) = operator_stack.pop() {
                    if top_op.precedence() < op.precedence() {
                        // Put it back. This is the easiest way to deal with the ownership.
                        operator_stack.push(top_op);
                        break;
                    }
                    if top_op == Operator::Brackets {
                        break;
                    }
                    process_op(top_op, &mut expr_stack);
                }
                operator_stack.push(op);
            }
            Token::LeftBracket => {
                if state != State::ExpectingExpr {
                    return Err(ExprError::UnexpectedToken(token));
                }
                operator_stack.push(Operator::Brackets);
            }
            Token::RightBracket => {
                if state != State::ExpectingOperator {
                    return Err(ExprError::UnexpectedToken(token));
                }
                state = State::ExpectingOperator;

                // Pop operators until we find a left bracket.
                loop {
                    let Some(top_op) = operator_stack.pop() else {
                        return Err(ExprError::UnmatchedRightBracket);
                    };
                    if top_op == Operator::Brackets {
                        break;
                    }
                    process_op(top_op, &mut expr_stack);
                }
            }
        }
    }

    // Pop operators until the stack is empty.
    while let Some(top_op) = operator_stack.pop() {
        if top_op == Operator::Brackets {
            return Err(ExprError::UnmatchedLeftBracket);
        }
        process_op(top_op, &mut expr_stack);
    }
    // The operand stack should now contain a single expression.
    if expr_stack.len() != 1 {
        return Err(ExprError::InvalidExpression);
    }
    Ok(expr_stack.pop().unwrap())
}

#[cfg(test)]
mod test {
    use crate::{
        expr::{BinOp, UnOp, Val},
        tokenise,
    };

    use super::*;

    #[test]
    fn test_parse() {
        let tokens = tokenise("1 + 2 * 3").unwrap();
        let expr = parse(tokens).unwrap();
        assert_eq!(
            expr,
            Expr::BinOp(
                Box::new(Expr::Literal(Val::Int(1))),
                Box::new(Expr::BinOp(
                    Box::new(Expr::Literal(Val::Int(2))),
                    Box::new(Expr::Literal(Val::Int(3))),
                    BinOp::Mul
                )),
                BinOp::Add
            )
        );
    }

    #[test]
    fn test_simple() {
        let tokens = tokenise("1 * ~2 + 3").unwrap();
        let expr = parse(tokens).unwrap();
        assert_eq!(
            expr,
            Expr::BinOp(
                Box::new(Expr::BinOp(
                    Box::new(Expr::Literal(Val::Int(1))),
                    Box::new(Expr::UnOp(Box::new(Expr::Literal(Val::Int(2))), UnOp::BitNot)),
                    BinOp::Mul
                )),
                Box::new(Expr::Literal(Val::Int(3))),
                BinOp::Add
            )
        );
    }

    #[test]
    fn test_call_precedence() {
        let tokens = tokenise("3 * f(1)").unwrap();
        let expr = parse(tokens).unwrap();
        assert_eq!(
            expr,
            Expr::BinOp(
                Box::new(Expr::Literal(Val::Int(3))),
                Box::new(Expr::Call(
                    "f".to_string(),
                    Box::new(Expr::Literal(Val::Int(1)))
                )),
                BinOp::Mul
            ),
        );
    }

    #[test]
    fn test_unop_precedence() {
        let tokens = tokenise("3 * ~(4)").unwrap();
        let expr = parse(tokens).unwrap();
        assert_eq!(
            expr,
            Expr::BinOp(
                Box::new(Expr::Literal(Val::Int(3))),
                Box::new(Expr::UnOp(Box::new(Expr::Literal(Val::Int(4))), UnOp::BitNot)),
                BinOp::Mul
            )
        );
    }

    #[test]
    fn test_unmatched_right_bracket() {
        let tokens = tokenise("3 * 4)").unwrap();
        let expr = parse(tokens);
        assert_eq!(expr, Err(ExprError::UnmatchedRightBracket));
    }
}
