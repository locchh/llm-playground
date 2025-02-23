- *You can parse source code by defining the grammar as a set of characters, numbers, delimiters, and spaces. This ensures 100% coverage, but it is meaningless.*

- *Recognizing characters, numbers, delimiters, and spaces ensures full coverage at the lexical level, but without syntax and semantic rules, it does not enable meaningful parsing.*

```
//  Header (Optional, for Imports & Configuration)

grammar MyLang;

@header {
    package my.antlr.parser;
}
// Root Rule (Entry Point of the Grammar)

program : statement+ EOF ;


// Parser Rules (High-Level Language Structure)

statement
    : ifStatement
    | assignment
    | loop
    | functionCall
    ;

ifStatement
    : 'if' condition 'then' block ('else' block)?
    ;

assignment
    : ID '=' expression ';'
    ;

loop
    : 'while' condition 'do' block
    ;

functionCall
    : ID '(' (expression (',' expression)*)? ')'
    ;

block
    : '{' statement* '}'
    ;


// Expressions (Mathematical & Logical Rules)

expression
    : term (('+' | '-') term)*
    ;

term
    : factor (('*' | '/') factor)*
    ;

factor
    : NUMBER
    | ID
    | '(' expression ')'
    ;


// Keywords
IF : 'if' ;
THEN : 'then' ;
ELSE : 'else' ;
WHILE : 'while' ;
DO : 'do' ;

// Operators & Symbols
PLUS  : '+' ;
MINUS : '-' ;
STAR  : '*' ;
SLASH : '/' ;
EQ    : '=' ;
LPAREN : '(' ;
RPAREN : ')' ;
LBRACE : '{' ;
RBRACE : '}' ;

// Identifiers & Literals
ID : [a-zA-Z_][a-zA-Z_0-9]* ;
NUMBER : [0-9]+ ;
STRING : '"' (~["])* '"' ;

// Fragments (Reusable Character Sets)
fragment DIGIT : [0-9] ;
fragment LETTER : [a-zA-Z] ;

// Special Handling (Whitespace, Comments, Error Handling)

WS : [ \t\r\n]+ -> skip ;
COMMENT : '//' ~[\r\n]* -> skip ;

err : . { System.err.println("Unexpected character: " + $text); } ;

```

[grammars-v4](https://github.com/antlr/grammars-v4)