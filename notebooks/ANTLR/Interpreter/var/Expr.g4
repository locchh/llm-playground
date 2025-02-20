grammar Expr;

// Entry point of the grammar
root: action+ EOF;

// Definitions of actions
action: NAME ASSIGN expr
      | WRITE NAME
      ;

// Expression definitions with operator precedence
expr: <assoc=right> expr POW expr       // Exponentiation (right-associative)
    | expr (MUL | DIV) expr             // Multiplication and Division (higher precedence than addition and subtraction)
    | expr (ADD | SUB) expr             // Addition and Subtraction (lower precedence)
    | NUM                               // Numeric value
    ;

// Tokens
WRITE: 'write';                         // 'write' has priority over NAME

NAME: [a-z]+;                           // Names consisting of lowercase letters
NUM: [0-9]+;                            // Numeric values (digits only)

ASSIGN: ':=';                           // Assignment operator
POW: '^';                               // Exponentiation operator
MUL: '*';                               // Multiplication operator
DIV: '/';                               // Division operator
ADD: '+';                               // Addition operator
SUB: '-';                               // Subtraction operator

// Whitespace handling (skipped)
WS: [ \n]+ -> skip;