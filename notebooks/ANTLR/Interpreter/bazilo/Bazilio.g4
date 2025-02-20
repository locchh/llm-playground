grammar Bazilio;
root: procDef* EOF;


inss: ins*;
ins: (condition | while_)
    | (input_ | output_ | proc | assign | reprod)
    | (afegit | tall | lookpartitura) ;

input_: '<?>' VAR;     
output_: '<!>' expr+;  

    
condition: 'if' expr LB inss RB ('else' LB inss RB)?;
while_: 'while' expr LB inss RB;



siz: SIZE VAR;
SIZE: '#';

NOTA: [A-G][0-9]?;


PROCNAME: [A-Z][a-zA-Z0-9_]*;
procDef: PROCNAME paramsId  LB inss RB;
proc: PROCNAME paramsExpr (expr)*;

assign: VAR ASSIGN expr;
ASSIGN: '<-';

lookpartitura: LOOK;
LOOK: 'look';

paramsId: (VAR)*;
paramsExpr: (expr)*;


reprod: REPROD expr;
REPROD: '<:>';

tall: TALLA VAR LS expr RS;
TALLA: '8<';

consult: VAR LS expr RS;


afegit: VAR AFEGIT expr;
AFEGIT: '<<';

lista : '{' expr* '}';

expr: expr MUL expr #Mul
    | expr DIV expr #Div
    | expr MOD expr #Mod
    | expr SUM expr #Sum
    | expr MIN expr #Min
    | expr GT expr  #Gt
    | expr GET expr #Get
    | expr LT expr  #Lt
    | expr LET expr #Let
    | expr EQ expr  #Eq
    | expr NEQ expr #Neq
    | VAR           #Var
    | STRING        #String
    | NUM           #Num
    | lista         #lst
    | siz           #sz
    | consult       #consul        
    | NOTA          #Nota
    | LP expr RP    #Parens ;

LC: '{';
RC: '}';
LB: '|:';
RB: ':|';
LP: '(';
RP: ')';
LS: '[';
RS: ']';

SUM: '+';
MIN: '-';
MUL: '*';
DIV: '/';
MOD: '%';
EQ: '=';
NEQ: '/=';
GT: '>';
LT: '<';
GET: '>=';
LET: '<=';


VAR: [a-zA-Z][a-zA-Z0-9]*;
NUM: '-'?[0-9]+('.'[0-9]+)?;
STRING: '"' ( '\\' . | ~('\\'|'"'))* '"';



COMMENT: '~~~' ~[\r\n]* -> skip;

WS: [ \t\r\n]+ -> skip;














