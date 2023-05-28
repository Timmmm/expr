use std::fmt::Display;

use crate::ast::{BinOp, UnOp, Val};
use crate::error::{ExprError, Result};

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token {
    Literal(Val),
    BinOp(BinOp),
    UnOp(UnOp),
    Identifier(String),
    LeftBracket,
    RightBracket,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Literal(v) => write!(f, "{}", v),
            Token::BinOp(op) => write!(f, "{}", op),
            Token::UnOp(op) => write!(f, "{}", op),
            Token::Identifier(s) => write!(f, "{}", s),
            Token::LeftBracket => write!(f, "("),
            Token::RightBracket => write!(f, ")"),
        }
    }
}

#[derive(PartialEq, Eq)]
enum State {
    None,
    Int,
    Identifier,
}

fn parse_int(s: &str) -> std::result::Result<u64, std::num::ParseIntError> {
    if let Some(s) = s.strip_prefix("0x") {
        u64::from_str_radix(s, 16)
    } else if let Some(s) = s.strip_prefix("0o") {
        u64::from_str_radix(s, 8)
    } else if let Some(s) = s.strip_prefix("0b") {
        u64::from_str_radix(s, 2)
    } else {
        u64::from_str_radix(s, 10)
    }
}

pub fn tokenise(input: &str) -> Result<Vec<Token>> {
    // Split on whitespace and word boundaries.

    let mut state = State::None;
    let mut tokens = Vec::new();

    // For literals and identifiers.
    let mut word = String::new();

    let mut iter = input.chars().peekable();

    while let Some(c) = iter.next() {
        // Push current literal/identifier if it has ended.
        if !matches!(c, '0'..='9' | 'A'..='Z' | 'a'..='z' | '_') {
            if !word.is_empty() {
                match state {
                    State::Int => {
                        tokens.push(Token::Literal(Val::Int(
                            parse_int(&word)
                                .map_err(|_| ExprError::IntegerParseError(word))?,
                        )));
                    }
                    State::Identifier => {
                        tokens.push(Token::Identifier(word));
                    }
                    _ => unreachable!(),
                }
                word = String::new();
                state = State::None;
            }
        }

        match c {
            '0'..='9' => {
                if state == State::None {
                    state = State::Int;
                }
                word.push(c);
            }
            'A'..='Z' | 'a'..='z' | '_' => {
                if state == State::None {
                    state = State::Identifier;
                }
                word.push(c);
            }
            '+' => {
                tokens.push(Token::BinOp(BinOp::Add));
            }
            '-' => {
                tokens.push(Token::BinOp(BinOp::Sub));
            }
            '*' => {
                tokens.push(Token::BinOp(BinOp::Mul));
            }
            '/' => {
                tokens.push(Token::BinOp(BinOp::Div));
            }
            '%' => {
                tokens.push(Token::BinOp(BinOp::Mod));
            }
            '!' => match iter.peek() {
                Some('=') => {
                    iter.next();
                    tokens.push(Token::BinOp(BinOp::NotEq));
                }
                _ => {
                    tokens.push(Token::UnOp(UnOp::LogicalNot));
                }
            },
            '~' => {
                tokens.push(Token::UnOp(UnOp::BitNot));
            }
            '&' => match iter.peek() {
                Some('&') => {
                    iter.next();
                    tokens.push(Token::BinOp(BinOp::LogicalAnd));
                }
                _ => {
                    tokens.push(Token::BinOp(BinOp::BitAnd));
                }
            },
            '|' => match iter.peek() {
                Some('|') => {
                    iter.next();
                    tokens.push(Token::BinOp(BinOp::LogicalOr));
                }
                _ => {
                    tokens.push(Token::BinOp(BinOp::BitOr));
                }
            },
            '^' => {
                tokens.push(Token::BinOp(BinOp::BitXor));
            }
            '<' => match iter.peek() {
                Some('<') => {
                    iter.next();
                    tokens.push(Token::BinOp(BinOp::ShiftLeft));
                }
                Some('=') => {
                    iter.next();
                    tokens.push(Token::BinOp(BinOp::LessEq));
                }
                _ => {
                    tokens.push(Token::BinOp(BinOp::Less));
                }
            },
            '>' => match iter.peek() {
                Some('>') => {
                    iter.next();
                    tokens.push(Token::BinOp(BinOp::ShiftRight));
                }
                Some('=') => {
                    iter.next();
                    tokens.push(Token::BinOp(BinOp::GreaterEq));
                }
                _ => {
                    tokens.push(Token::BinOp(BinOp::Greater));
                }
            },
            '=' => match iter.peek() {
                Some('=') => {
                    iter.next();
                    tokens.push(Token::BinOp(BinOp::Eq));
                }
                _ => {
                    return Err(ExprError::UnexpectedCharacter(c));
                }
            },
            '(' => {
                tokens.push(Token::LeftBracket);
            }
            ')' => {
                tokens.push(Token::RightBracket);
            }
            ' ' | '\t' | '\n' => {}
            _ => {
                return Err(ExprError::UnexpectedCharacter(c));
            }
        }
    }

    if !word.is_empty() {
        match state {
            State::Int => {
                tokens.push(Token::Literal(Val::Int(
                    parse_int(&word)
                        .map_err(|_| ExprError::IntegerParseError(word))?,
                )));
            }
            State::Identifier => {
                tokens.push(Token::Identifier(word));
            }
            _ => unreachable!(),
        }
    }

    Ok(tokens)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_tokeniser() {
        assert_eq!(
            tokenise("1 + 2"),
            Ok(vec![
                Token::Literal(Val::Int(1)),
                Token::BinOp(BinOp::Add),
                Token::Literal(Val::Int(2)),
            ])
        );

        assert_eq!(
            tokenise("1+2*foo(bar)"),
            Ok(vec![
                Token::Literal(Val::Int(1)),
                Token::BinOp(BinOp::Add),
                Token::Literal(Val::Int(2)),
                Token::BinOp(BinOp::Mul),
                Token::Identifier("foo".to_string()),
                Token::LeftBracket,
                Token::Identifier("bar".to_string()),
                Token::RightBracket,
            ])
        );

        // Test all operators and edge cases.
        assert_eq!(
            tokenise("1+2-3*4/5%6<<7>>8&9|10^11&&12||13==14!=15<16<=17>18>=19"),
            Ok(vec![
                Token::Literal(Val::Int(1)),
                Token::BinOp(BinOp::Add),
                Token::Literal(Val::Int(2)),
                Token::BinOp(BinOp::Sub),
                Token::Literal(Val::Int(3)),
                Token::BinOp(BinOp::Mul),
                Token::Literal(Val::Int(4)),
                Token::BinOp(BinOp::Div),
                Token::Literal(Val::Int(5)),
                Token::BinOp(BinOp::Mod),
                Token::Literal(Val::Int(6)),
                Token::BinOp(BinOp::ShiftLeft),
                Token::Literal(Val::Int(7)),
                Token::BinOp(BinOp::ShiftRight),
                Token::Literal(Val::Int(8)),
                Token::BinOp(BinOp::BitAnd),
                Token::Literal(Val::Int(9)),
                Token::BinOp(BinOp::BitOr),
                Token::Literal(Val::Int(10)),
                Token::BinOp(BinOp::BitXor),
                Token::Literal(Val::Int(11)),
                Token::BinOp(BinOp::LogicalAnd),
                Token::Literal(Val::Int(12)),
                Token::BinOp(BinOp::LogicalOr),
                Token::Literal(Val::Int(13)),
                Token::BinOp(BinOp::Eq),
                Token::Literal(Val::Int(14)),
                Token::BinOp(BinOp::NotEq),
                Token::Literal(Val::Int(15)),
                Token::BinOp(BinOp::Less),
                Token::Literal(Val::Int(16)),
                Token::BinOp(BinOp::LessEq),
                Token::Literal(Val::Int(17)),
                Token::BinOp(BinOp::Greater),
                Token::Literal(Val::Int(18)),
                Token::BinOp(BinOp::GreaterEq),
                Token::Literal(Val::Int(19)),
            ])
        );
    }
}
