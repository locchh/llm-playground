grammar Expression;

@header {
# This section runs at the start of the parser initialization
print("Initializing Expression Parser with Ambiguity Handling...")
}

@members {
# Custom method for debugging or resolving ambiguities
def resolve_ambiguity(self, rule, value):
    print(f"Resolving ambiguity at rule: {rule}, computed value: {value}")
    return value
}

prog: expr EOF { print("Finished parsing program."); };

expr returns [int value]
    : e1=term {$value = $e1.value;} ('+' e2=term { $value = self.resolve_ambiguity("addition", $value + $e2.value); })* # Add
    ;

term returns [int value]
    : e1=factor {$value = $e1.value;} ('*' e2=factor { $value = self.resolve_ambiguity("multiplication", $value * $e2.value); })* # Multiply
    ;

factor returns [int value]
    : INT { $value = int($INT.text); } # Integer
    | '(' e=expr ')' { $value = self.resolve_ambiguity("parentheses", $e.value); } # Parentheses
    ;

INT: [0-9]+; // Matches integers
WS: [ \t\r\n]+ -> skip; // Skip whitespace
