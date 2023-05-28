use crate::tokeniser::Token;
use crate::error::Result;
use crate::ast::Ast;

pub fn parse(tokens: &[Token]) -> Result<Ast> {
    let mut iter = tokens.iter().peekable();

    // Parse using the shunting yard algorithm. We also use a state machine
    // to check for valid syntax.

    todo!()
}
