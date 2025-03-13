grammar Expr;

root: action+ EOF; 

action: 'if' expr action ('else' action)? # Condition
    | 'print' expr                        # Print
    ;


expr: expr GT expr # Gt
    | expr LT expr # Lt
    | NUM          # Value
    ;

GT: '>';
LT: '<';
NUM: [0-9]+;

WS: [ \t\r\n]+ -> skip;








