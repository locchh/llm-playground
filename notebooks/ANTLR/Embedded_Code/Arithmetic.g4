grammar Arithmetic;

@header {
# Define any imports or initialization code here
print("Initializing Arithmetic Parser...")
}

@members {
# Define custom methods or variables used in embedded code
def calculate_result(self, value):
    print(f"Result: {value}")
    return value
}

prog: expr EOF { print("Parsing completed!"); };

expr returns [int value]
    : e1=term {$value = $e1.value;} ('+' e2=term {$value += $e2.value;})* # Add
    ;

term returns [int value]
    : e1=factor {$value = $e1.value;} ('*' e2=factor {$value *= $e2.value;})* # Multiply
    ;

factor returns [int value]
    : INT { $value = int($INT.text); } # Integer
    | '(' e=expr ')' { $value = $e.value; } # Parentheses
    ;

INT: [0-9]+; // Matches integers
WS: [ \t\r\n]+ -> skip; // Skips whitespace
