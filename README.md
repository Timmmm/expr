# Expr

Expr is a very simple integer expression calculator. Its grammar is as follows:

    identifier := [A-Za-z_] [A-Za-z_0-9]*
    variable := identifier
    decimal_literal := [0-9]+
    hex_literal := '0x' [0-9A-Fa-f]+
    binary_literal := '0b' [0-1]+
    octal_literal := '0o' [0-7]+
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

Operator meaning and precedence is the same [as in Rust](https://doc.rust-lang.org/reference/expressions.html#expression-precedence), with the exception of `~` which is used for bitwise not (Rust uses `!` for both logical and bitwise not).
