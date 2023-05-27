// Implement custom Result type.

use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
enum ExprError {
    SyntaxError(String),
    TypeError(String),
}

impl fmt::Display for ExprError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "invalid first item to double")
    }
}

type Result<T> = std::result::Result<T, ExprError>;

#[derive(PartialEq, Eq, Debug)]
enum Val {
    Int(u64),
    Bool(bool),
}

enum Ast {
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
                        BinOp::Plus => Ok(Val::Int(a + b)), // TODO: Checked add etc.
                        BinOp::Minus => Ok(Val::Int(a - b)),
                        BinOp::Mul => Ok(Val::Int(a * b)),
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

            _ => todo!()
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
enum BinOp {
    Plus,
    Minus,
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

// TODO: Wrapping add/subtract (and maybe shift) using @+, @- and @<<, @>>.

#[derive(PartialEq, Eq, Debug)]
enum UnOp {
    Not,
    BitNot,
}

#[derive(PartialEq, Eq, Debug)]
enum Token {
    Literal(Val),
    BinOp(BinOp),
    UnOp(UnOp),
    Identifier(String),
    LeftBracket,
    RightBracket,
}

#[derive(PartialEq, Eq)]
enum State {
    None,
    Int,
    Identifier,
}

fn tokenise(input: &str) -> Result<Vec<Token>> {
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
                        tokens.push(Token::Literal(Val::Int(word.parse().map_err(|_| {
                            ExprError::SyntaxError(format!("invalid integer: {}", word))
                        })?)));
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
                tokens.push(Token::BinOp(BinOp::Plus));
            }
            '-' => {
                tokens.push(Token::BinOp(BinOp::Minus));
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
            '!' => {
                if iter.peek() == Some(&'=') {
                    iter.next();
                    tokens.push(Token::BinOp(BinOp::NotEq));
                } else {
                    tokens.push(Token::UnOp(UnOp::Not));
                }
            }
            '~' => {
                tokens.push(Token::UnOp(UnOp::BitNot));
            }
            '&' => {
                if iter.peek() == Some(&'&') {
                    iter.next();
                    tokens.push(Token::BinOp(BinOp::LogicalAnd));
                } else {
                    tokens.push(Token::BinOp(BinOp::BitAnd));
                }
            }
            '|' => {
                if iter.peek() == Some(&'|') {
                    iter.next();
                    tokens.push(Token::BinOp(BinOp::LogicalOr));
                } else {
                    tokens.push(Token::BinOp(BinOp::BitOr));
                }
            }
            '^' => {
                tokens.push(Token::BinOp(BinOp::BitXor));
            }
            '<' => {
                if iter.peek() == Some(&'<') {
                    iter.next();
                    tokens.push(Token::BinOp(BinOp::ShiftLeft));
                } else if iter.peek() == Some(&'=') {
                    iter.next();
                    tokens.push(Token::BinOp(BinOp::LessEq));
                } else {
                    tokens.push(Token::BinOp(BinOp::Less));
                }
            }
            '>' => {
                if iter.peek() == Some(&'>') {
                    iter.next();
                    tokens.push(Token::BinOp(BinOp::ShiftRight));
                } else if iter.peek() == Some(&'=') {
                    iter.next();
                    tokens.push(Token::BinOp(BinOp::GreaterEq));
                } else {
                    tokens.push(Token::BinOp(BinOp::Greater));
                }
            }
            '=' => {
                if iter.peek() == Some(&'=') {
                    iter.next();
                    tokens.push(Token::BinOp(BinOp::Eq));
                } else {
                    return Err(ExprError::SyntaxError(format!(
                        "unexpected character: {}",
                        c
                    )));
                }
            }
            '(' => {
                tokens.push(Token::LeftBracket);
            }
            ')' => {
                tokens.push(Token::RightBracket);
            }
            ' ' | '\t' | '\n' => {}
            _ => {
                return Err(ExprError::SyntaxError(format!(
                    "unexpected character: {}",
                    c
                )))
            }
        }
    }

    if !word.is_empty() {
        match state {
            State::Int => {
                tokens.push(Token::Literal(Val::Int(word.parse().map_err(|_| {
                    ExprError::SyntaxError(format!("invalid integer: {}", word))
                })?)));
            }
            State::Identifier => {
                tokens.push(Token::Identifier(word));
            }
            _ => unreachable!(),
        }
    }

    Ok(tokens)
}

fn parse(tokens: &[Token]) -> Result<Expr> {
    let mut iter = tokens.iter().peekable();

}

fn main() {
    println!("Hello, world!");
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
                Token::BinOp(BinOp::Plus),
                Token::Literal(Val::Int(2)),
            ])
        );

        assert_eq!(
            tokenise("1+2*foo(bar)"),
            Ok(vec![
                Token::Literal(Val::Int(1)),
                Token::BinOp(BinOp::Plus),
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
                Token::BinOp(BinOp::Plus),
                Token::Literal(Val::Int(2)),
                Token::BinOp(BinOp::Minus),
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
