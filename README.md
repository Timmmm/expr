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
unary_op := '!' | '~'
brackets := '(' atom ')'
unary := unary_op atom
function_call := itentifier brackets
atom := brackets | unary | literal | function_call
expr := atom ( binary_op atom )*

Very standard with two exceptions:

1. There is no unary + or -.
2. Function calls always take exactly 1 argument.

The only supported types are `bool` and `u64`. There is no type coersion.

All arithmetic operations are checked and cause an error on over/underflow. Shift overflows (e.g. 1 << 100) are allowed and result in 0.

Operator meaning and precedence is the same as in Go.

There is no undefined behaviour. Overflows wrap. Large shifts result in 0.
