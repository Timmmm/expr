# Expr

Expr is a very simple integer expression calculator. Its grammar is as follows:

identifier := [A-Za-z_] [A-Za-z_0-9]*
variable := identifier
literal := decimal_literal | hex_literal | binary_literal | octal_literal
binary_op := '+' | '-' | '*' | '/' | '%' |
             '<<' | '>>' |
             '==' | '!=' | '<' | '>' | '<=' | '>=' |
             '&&' | '||' |
             '&' | '|' | '^'
unary_op := '!' | '~' | identifier
brackets := '(' atom ')'
unary := unary_op atom
atom := brackets | unary | literal
expr := atom ( binary_op atom )*

Very standard with two exceptions:

1. There is no unary + or -.
2. Function calls always take exactly 1 argument and do not require brackets (simialr to functional languages with currying).

The only supported types are `bool` and `u64`. There is no type coersion.

Operator meaning and precedence is the same as in Go (or Rust with the exception of `~`).

There is no undefined behaviour. Overflows wrap. Large shifts result in 0.
