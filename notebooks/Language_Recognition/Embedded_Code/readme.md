### **Tutorial: Using Embedded Python Code in ANTLR**

#### **1. Why and When to Use Embedded Python Code in ANTLR**

Embedded Python code in ANTLR grammar files is useful for:
- **Custom Actions**: Perform custom operations during parsing (e.g., calculations, logging).
- **Disambiguation**: Resolve ambiguities or enforce specific rules in parsing.
- **Dynamic Behavior**: Interact with application logic during grammar execution (e.g., symbol tables or semantic checks).
- **Optimization**: Simplify complex post-processing by embedding the required logic directly into parsing actions.

**When to Use Embedded Python Code**:
- You need **inline processing** of parsed data, like computing values in arithmetic expressions.
- To handle **ambiguous grammars** by embedding logic that decides between possible parse paths.
- You want to **extend functionality** of the parser (e.g., logging, validation).

#### **2. How to Embed Python Code in ANTLR**

In ANTLR grammar files (`.g4`), you can embed Python code in several places:

1. **@header Section**:
   - Code in this section is executed once when the parser is initialized.
   - Use it for imports or global initializations.

2. **@members Section**:
   - Define methods or global variables to be used within grammar rules.

3. **Inline Actions**:
   - Add Python code directly within grammar rules using `{ ... }`.

#### **3. Step-by-Step Example**

Let’s create a grammar to evaluate arithmetic expressions with Python code embedded for calculation.

---

### **Grammar File: `Calculator.g4`**

```antlr
grammar Calculator;

// Header section for imports or initialization
@header {
# This code is executed at the start of the parser
print("Calculator Parser Initialized.")
}

// Members section for custom methods or variables
@members {
# Custom method to log results
def log_result(self, operation, result):
    print(f"Operation: {operation}, Result: {result}")
    return result
}

// Top-level rule
prog: expr EOF { print("Parsing complete."); };

// Rule for expressions with addition
expr returns [int value]
    : e1=term {$value = $e1.value;} 
      ('+' e2=term {$value = self.log_result("addition", $value + $e2.value);})* 
    ;

// Rule for terms with multiplication
term returns [int value]
    : e1=factor {$value = $e1.value;} 
      ('*' e2=factor {$value = self.log_result("multiplication", $value * $e2.value);})* 
    ;

// Rule for individual numbers or grouped expressions
factor returns [int value]
    : INT { $value = int($INT.text); } 
    | '(' e=expr ')' { $value = $e.value; }
    ;

// Lexer rules
INT: [0-9]+;       // Matches integers
WS: [ \t\r\n]+ -> skip; // Skips whitespace
```

---

### **Explanation**

1. **@header Section**:
   - Prints a message when the parser initializes. You can also include Python imports here.

2. **@members Section**:
   - Defines a helper function `log_result` for logging operation results.

3. **Inline Actions**:
   - Inside grammar rules, Python code is embedded within `{ ... }` to perform calculations or logging.

---

### **Generate the Parser**

Use the ANTLR tool to generate the lexer and parser in Python:

```bash
antlr4 -Dlanguage=Python3 Calculator.g4
```

This will create the following files:
- `CalculatorLexer.py`
- `CalculatorParser.py`

---

### **Python Script: `main.py`**

```python
from antlr4 import *
from CalculatorLexer import CalculatorLexer
from CalculatorParser import CalculatorParser

# Input expression
input_stream = InputStream("3 + 5 * (2 + 4)")

# Create lexer and parser
lexer = CalculatorLexer(input_stream)
token_stream = CommonTokenStream(lexer)
parser = CalculatorParser(token_stream)

# Parse and evaluate
parser.prog()
```

---

### **Output**

When you run the script, it will output:

```plaintext
Calculator Parser Initialized.
Operation: addition, Result: 6
Operation: multiplication, Result: 30
Parsing complete.
```

---

### **4. Tips for Using Embedded Code**

1. **Keep Embedded Code Minimal**:
   - Use it only for essential operations like logging or small calculations. Avoid embedding large blocks of logic.

2. **Use @members for Shared Functions**:
   - Place reusable methods in the `@members` section to keep grammar rules clean and organized.

3. **Debugging**:
   - If there’s a syntax or runtime error in your embedded code, it will propagate to the generated Python files. Check indentation and syntax carefully.

4. **Testing**:
   - Always test the generated files after adding embedded code to ensure the grammar compiles and runs correctly.

---

This tutorial should help you effectively use embedded Python code in ANTLR for custom functionality. Let me know if you have specific questions or need further clarification!
