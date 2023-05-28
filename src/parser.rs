use crate::ast::{Ast, BinOp, UnOp};
use crate::error::{ExprError, Result};
use crate::tokeniser::Token;

#[derive(PartialEq, Eq)]
enum State {
    // Expecting operand (variable, left bracket, function call, literal).
    ExpectingOperand,
    // Expecting binary operator or right bracket.
    ExpectingOperator,
}

#[derive(Debug)]
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

pub fn parse(tokens: Vec<Token>) -> Result<Ast> {
    let mut iter = tokens.into_iter().peekable();

    // Parse using the shunting yard algorithm. We also use a state machine
    // to check for valid syntax.
    let mut operator_stack = Vec::new();
    let mut ast_stack = Vec::new();

    let mut state = State::ExpectingOperand;

    while let Some(token) = iter.next() {
        match token {
            Token::Literal(val) => {
                if state != State::ExpectingOperand {
                    return Err(ExprError::UnexpectedToken(token));
                }
                state = State::ExpectingOperator;
                ast_stack.push(Ast::Literal(val));
            }
            Token::Identifier(s) => {
                if state != State::ExpectingOperand {
                    return Err(ExprError::UnexpectedToken(Token::Identifier(s)));
                }
                if let Some(Token::LeftBracket) = iter.peek() {
                    // Function call.
                    operator_stack.push(Operator::Call(s.clone()));
                    // Still expecting an operand.
                } else {
                    // Variable.
                    ast_stack.push(Ast::Var(s.clone()));
                    state = State::ExpectingOperator;
                }
            }
            Token::UnOp(op) => {
                if state != State::ExpectingOperand {
                    return Err(ExprError::UnexpectedToken(token));
                }
                // No need to pop operators with higher precedence since there aren't any.
                operator_stack.push(Operator::UnOp(op));
            }
            Token::BinOp(op) => {
                if state != State::ExpectingOperator {
                    return Err(ExprError::UnexpectedToken(token));
                }
                state = State::ExpectingOperand;

                let op = Operator::BinOp(op);

                // Pop operators until we find one with lower precedence (or brackets).
                while let Some(top_op) = operator_stack.pop() {
                    if top_op.precedence() < op.precedence() {
                        operator_stack.push(top_op);
                        break;
                    }
                    match top_op {
                        Operator::Brackets => break,
                        Operator::BinOp(op) => {
                            let rhs = ast_stack.pop().unwrap();
                            let lhs = ast_stack.pop().unwrap();
                            ast_stack.push(Ast::BinOp(Box::new(lhs), Box::new(rhs), op));
                        }
                        Operator::UnOp(op) => {
                            let operand = ast_stack.pop().unwrap();
                            ast_stack.push(Ast::UnOp(Box::new(operand), op));
                        }
                        Operator::Call(name) => {
                            // Function call.
                            let param = ast_stack.pop().unwrap();
                            ast_stack.push(Ast::Call(name, Box::new(param)));
                        }
                    }
                }
                operator_stack.push(op);
            }
            Token::LeftBracket => {
                if state != State::ExpectingOperand {
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
                while let Some(top_op) = operator_stack.pop() {
                    match top_op {
                        Operator::Brackets => break,
                        Operator::BinOp(op) => {
                            let rhs = ast_stack.pop().unwrap();
                            let lhs = ast_stack.pop().unwrap();
                            ast_stack.push(Ast::BinOp(Box::new(lhs), Box::new(rhs), op));
                        }
                        Operator::UnOp(op) => {
                            let operand = ast_stack.pop().unwrap();
                            ast_stack.push(Ast::UnOp(Box::new(operand), op));
                        }
                        Operator::Call(name) => {
                            // Function call.
                            let param = ast_stack.pop().unwrap();
                            ast_stack.push(Ast::Call(name, Box::new(param)));
                        }
                    }
                }
            }
        }
    }

    dbg!(&operator_stack);
    dbg!(&ast_stack);

    // Pop operators until the stack is empty.
    while let Some(top_op) = operator_stack.pop() {
        match top_op {
            Operator::BinOp(op) => {
                let rhs = ast_stack.pop().unwrap();
                let lhs = ast_stack.pop().unwrap();
                ast_stack.push(Ast::BinOp(Box::new(lhs), Box::new(rhs), op));
            }
            Operator::UnOp(op) => {
                let operand = ast_stack.pop().unwrap();
                ast_stack.push(Ast::UnOp(Box::new(operand), op));
            }
            Operator::Brackets => {
                return Err(ExprError::UnmatchedLeftBracket);
            }
            Operator::Call(name) => {
                // Function call.
                let param = ast_stack.pop().unwrap();
                ast_stack.push(Ast::Call(name, Box::new(param)));
            }
        }
    }
    // The operand stack should now contain a single AST node.
    if ast_stack.len() != 1 {
        return Err(ExprError::InvalidExpression);
    }
    Ok(ast_stack.pop().unwrap())
}

#[cfg(test)]
mod test {
    use crate::{
        ast::{BinOp, UnOp, Val},
        tokenise,
    };

    use super::*;

    #[test]
    fn test_parse() {
        let tokens = tokenise("1 + 2 * 3").unwrap();
        let ast = parse(tokens).unwrap();
        assert_eq!(
            ast,
            Ast::BinOp(
                Box::new(Ast::Literal(Val::Int(1))),
                Box::new(Ast::BinOp(
                    Box::new(Ast::Literal(Val::Int(2))),
                    Box::new(Ast::Literal(Val::Int(3))),
                    BinOp::Mul
                )),
                BinOp::Add
            )
        );
    }

    #[test]
    fn test_simple() {
        let tokens = tokenise("1 * ~2 + 3").unwrap();
        let ast = parse(tokens).unwrap();
        assert_eq!(
            ast,
            Ast::BinOp(
                Box::new(Ast::BinOp(
                    Box::new(Ast::Literal(Val::Int(1))),
                    Box::new(Ast::UnOp(Box::new(Ast::Literal(Val::Int(2))), UnOp::BitNot)),
                    BinOp::Mul
                )),
                Box::new(Ast::Literal(Val::Int(3))),
                BinOp::Add
            )
        );
    }

    #[test]
    fn test_call_precedence() {
        let tokens = tokenise("3 * f(1)").unwrap();
        let ast = parse(tokens).unwrap();
        assert_eq!(
            ast,
            Ast::BinOp(
                Box::new(Ast::Literal(Val::Int(3))),
                Box::new(Ast::Call(
                    "f".to_string(),
                    Box::new(Ast::Literal(Val::Int(1)))
                )),
                BinOp::Mul
            ),
        );
    }

    #[test]
    fn test_unop_precedence() {
        let tokens = tokenise("3 * ~(4)").unwrap();
        let ast = parse(tokens).unwrap();
        assert_eq!(
            ast,
            Ast::BinOp(
                Box::new(Ast::Literal(Val::Int(3))),
                Box::new(Ast::UnOp(Box::new(Ast::Literal(Val::Int(4))), UnOp::BitNot)),
                BinOp::Mul
            )
        );
    }
}
