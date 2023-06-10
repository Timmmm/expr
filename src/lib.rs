mod expr;
mod error;
mod parser;
mod tokeniser;

pub use crate::expr::{Expr, Context};
pub use parser::parse;
pub use tokeniser::tokenise;

#[cfg(test)]
mod test {
    use crate::expr::Val;

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
    fn test_simple() {
        let tokens = tokenise("4 * inc(2) + 1").unwrap();
        let expr = parse(tokens).unwrap();
        let result = expr.evaluate(&SimpleContext {});
        assert_eq!(result, Ok(Val::Int(4 * (2 + 1) + 1)));
    }

    #[test]
    fn test_complex() {
        let tokens = tokenise("x + y * 3 + (4 + 1) * inc(2) + 1").unwrap();
        let expr = parse(tokens).unwrap();
        let result = expr.evaluate(&SimpleContext {});
        assert_eq!(result, Ok(Val::Int(1 + 2 * 3 + (4 + 1) * (2 + 1) + 1)));
    }

    #[test]
    fn test_comprehensive() {
        // Very exhaustive test of all operators, with lots of brackets and
        // function calls.
        let tokens = tokenise(
            "x + y * 3 + (4 + 1) * inc(2) + 1 == 1 + 2 * 3 + (4 + 1) * inc(2) + 1 \
             && x + y * 3 + (4 + 1) * inc(2) + 1 != 1 + 2 * 3 + (4 + 1) * inc(2) + 1 \
             || x + y * 3 + (4 + 1) * inc(2) + 1 < 1 + 2 * 3 + (4 + 1) * inc(2) + 1 \
             && x + y * 3 + (4 + 1) * inc(2) + 1 <= 1 + 2 * 3 + (4 + 1) * inc(2) + 1 \
             || x + y * 3 + (4 + 1) * inc(2) + 1 > 1 + 2 * 3 + (4 + 1) * inc(2) + 1 \
             && x + y * 3 + (4 + 1) * inc(2) + 1 >= 1 + 2 * 3 + (4 + 1) * inc(2) + 1",
        ).unwrap();
        let expr = parse(tokens).unwrap();
        let result = expr.evaluate(&SimpleContext {});
        assert_eq!(result, Ok(Val::Bool(false)));
    }

    // // Generate a random valid expression.
    // fn random_expr() -> Expr {
    //     use rand::distributions::{Distribution, Standard};
    //     use rand::Rng;

    //     let mut rng = rand::thread_rng();
    //     let mut stack = Vec::new();
    //     let mut expr = Expr::Literal(Val::Int(rng.gen()));
    //     let mut depth = 0;
    //     while depth > 0 || rng.gen::<bool>() {
    //         if depth > 0 && rng.gen::<bool>() {
    //             depth -= 1;
    //             expr = stack.pop().unwrap();
    //         } else {
    //             depth += 1;
    //             let op = rng.gen::<BinOp>();
    //             let new_expr = Expr::BinOp(Box::new(expr), Box::new(Expr::Literal(Val::Int(rng.gen()))), op);
    //             stack.push(expr);
    //             expr = new_expr;
    //         }
    //     }
    //     expr
    // }
}
