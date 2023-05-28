use crate::tokeniser::Token;
use crate::error::{Result, ExprError};
use crate::ast::{Ast, Precedence};

#[derive(PartialEq, Eq)]
enum State {
    // Expecting operand (variable, left bracket, function call, literal).
    ExpectingOperand,
    // Expecting binary operator or right bracket.
    ExpectingOperator,
}

pub fn parse(tokens: Vec<Token>) -> Result<Ast> {
    let mut iter = tokens.into_iter().peekable();

    // Parse using the shunting yard algorithm. We also use a state machine
    // to check for valid syntax.
    let mut operator_stack = Vec::new();
    let mut operand_stack = Vec::new();

    let mut state = State::ExpectingOperand;

    while let Some(token) = iter.next() {
        match token {
            Token::Literal(val) => {
                if state != State::ExpectingOperand {
                    return Err(ExprError::SyntaxError(format!(
                        "unexpected literal: {:?}",
                        val
                    )));
                }
                state = State::ExpectingOperator;
                operand_stack.push(Ast::Literal(val));
            }
            Token::Identifier(s) => {
                if state != State::ExpectingOperand {
                    return Err(ExprError::SyntaxError(format!(
                        "unexpected identifier: {:?}",
                        s
                    )));
                }
                if let Some(Token::LeftBracket) = iter.peek() {
                    // Function call.
                    operator_stack.push(Token::Identifier(s.clone()));
                    // Still expecting an operand.
                } else {
                    // Variable.
                    operand_stack.push(Ast::Var(s.clone()));
                    state = State::ExpectingOperator;
                }
            }
            Token::UnOp(op) => {
                if state != State::ExpectingOperand {
                    return Err(ExprError::SyntaxError(format!(
                        "unexpected unary operator: {:?}",
                        op
                    )));
                }
                operator_stack.push(Token::UnOp(op));
            }
            Token::BinOp(op) => {
                if state != State::ExpectingOperator {
                    return Err(ExprError::SyntaxError(format!(
                        "unexpected binary operator: {:?}",
                        op
                    )));
                }
                state = State::ExpectingOperand;
                // Pop operators with higher precedence from the stack.
                while let Some(Token::BinOp(op2)) = operator_stack.last() {
                    if op2.precedence() >= op.precedence() {
                        let Some(Token::BinOp(op2)) = operator_stack.pop() else { unreachable!(); };
                        let rhs = operand_stack.pop().unwrap();
                        let lhs = operand_stack.pop().unwrap();
                        operand_stack.push(Ast::BinOp(Box::new(lhs), Box::new(rhs), op2));
                    } else {
                        break;
                    }
                }
                operator_stack.push(Token::BinOp(op));
            }
            Token::LeftBracket => {
                if state != State::ExpectingOperand {
                    return Err(ExprError::SyntaxError("unexpected (".to_string()));
                }
                operator_stack.push(Token::LeftBracket);
            }
            Token::RightBracket => {
                if state != State::ExpectingOperator {
                    return Err(ExprError::SyntaxError("unexpected )".to_string()));
                }
                state = State::ExpectingOperator;

                // Pop operators until we find a left bracket.
                while let Some(token) = operator_stack.pop() {
                    match token {
                        Token::LeftBracket => break,
                        Token::BinOp(op) => {
                            let rhs = operand_stack.pop().unwrap();
                            let lhs = operand_stack.pop().unwrap();
                            operand_stack.push(Ast::BinOp(Box::new(lhs), Box::new(rhs), op));
                        }
                        Token::UnOp(op) => {
                            let operand = operand_stack.pop().unwrap();
                            operand_stack.push(Ast::UnOp(Box::new(operand), op));
                        }
                        _ => unreachable!(),
                    }
                }
            }
        }
    }

    // Pop operators until the stack is empty.
    while let Some(token) = operator_stack.pop() {
        match token {
            Token::BinOp(op) => {
                let rhs = operand_stack.pop().unwrap();
                let lhs = operand_stack.pop().unwrap();
                operand_stack.push(Ast::BinOp(Box::new(lhs), Box::new(rhs), op));
            }
            Token::UnOp(op) => {
                let operand = operand_stack.pop().unwrap();
                operand_stack.push(Ast::UnOp(Box::new(operand), op));
            }
            Token::LeftBracket => {
                return Err(ExprError::SyntaxError("unmatched (".to_string()));
            }
            _ => unreachable!(),
        }
    }
    // The operand stack should now contain a single AST node.
    if operand_stack.len() != 1 {
        return Err(ExprError::SyntaxError("invalid expression".to_string()));
    }
    Ok(operand_stack.pop().unwrap())
}

#[cfg(test)]
mod test {
    use crate::{tokenise, ast::{BinOp, Val}};

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
}
