grammar Condition;

root: action+ EOF;

action: 'iff' expr action ('otherwise' action)? # Condition
    | 'write' expr                              # Print
    | 'next' expr                               # Next
    ;

expr: expr GT expr  # Gt
    | expr LT expr  # Lt
    | expr EQ expr  # Eq
    | expr NEQ expr # Neq
    | expr ADD expr # Sum
    | NUM           # Value
    ;

GT: '>';
LT: '<';
EQ: '==';
NEQ: '!=';
ADD: '+';

NUM: [0-9]+;

WS: [ \t\r\n] -> skip;



