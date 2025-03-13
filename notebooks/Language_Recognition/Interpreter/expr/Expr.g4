grammar Expr;

root: expr EOF;

expr: expr MUL expr # Mul
    | expr DIV expr # Div
    | expr ADD expr # Add
    | expr SUB expr # Sub
    | NUM           # Val
    ;


NUM: [0-9]+ ;

MUL: '*' ;
DIV: '/' ;
ADD: '+' ;
SUB: '-' ;

WS: [ \n] ->skip ;