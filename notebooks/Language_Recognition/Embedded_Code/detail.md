# Detailed Tutorial: Embedded Python Code in ANTLR

ANTLR (ANother Tool for Language Recognition) is a powerful tool for defining grammars and building parsers for programming languages, DSLs, and more. Embedded Python code in ANTLR allows custom actions, logic, and integration with Python applications directly from the grammar.

---

## **1. What is Embedded Python Code in ANTLR?**

Embedded Python code in ANTLR refers to Python logic written directly in the grammar file (`.g4`). This code executes during parsing or tree walking, allowing:

- Custom processing of parsed data.
- Dynamic rule evaluation.
- Integration with external Python libraries or logic.

### **Why Use Embedded Python Code?**

- **Custom Actions**: To perform calculations, logging, or validations inline with parsing.
- **Disambiguation**: Handle ambiguous cases by embedding decision logic.
- **Semantic Checks**: Validate or transform parsed data during parsing.
- **Efficiency**: Avoid post-processing by embedding processing logic directly.

### **When to Use Embedded Python Code?**

- You need immediate actions on parsed input (e.g., arithmetic evaluations).
- Complex grammar rules require dynamic disambiguation.
- Your application logic depends on parsed data during runtime.

---

## **2. How to Use Embedded Python Code**

ANTLR provides three primary ways to embed Python code:

### **a. @header Section**
- Code in this section runs when the parser or lexer is initialized.
- Use it for global imports or initializations.

**Example:**
```antlr
@header {
# Python imports or setup code
print("Initializing the parser...")
}
```

### **b. @members Section**
- Code in this section defines methods or global variables for use within the grammar.

**Example:**
```antlr
@members {
# Helper method to log results

def log_result(self, operation, result):
    print(f"Operation: {operation}, Result: {result}")
    return result
}
```

### **c. Inline Actions**
- Inline Python code is written within `{ ... }` blocks inside grammar rules. This code executes when the corresponding rule is matched.

**Example:**
```antlr
expr returns [int value]
    : e1=term {$value = $e1.value;} ('+' e2=term {$value += $e2.value;})*;
```

---

## **3. Step-by-Step Example**

Let’s build an arithmetic parser to evaluate expressions with embedded Python code.

### **Step 1: Define the Grammar**

Create a file named `Calculator.g4`:

```antlr
grammar Calculator;

// Header section for initialization
@header {
# Python initialization code
print("Calculator Parser Initialized.")
}

// Members section for helper methods
@members {
# Log operation results
def log_result(self, operation, result):
    print(f"Operation: {operation}, Result: {result}")
    return result
}

// Top-level rule
prog: expr EOF { print("Parsing complete."); };

// Expression rule with addition
expr returns [int value]
    : e1=term {$value = $e1.value;}
      ('+' e2=term {$value = self.log_result("addition", $value + $e2.value);})*;

// Term rule with multiplication
term returns [int value]
    : e1=factor {$value = $e1.value;}
      ('*' e2=factor {$value = self.log_result("multiplication", $value * $e2.value);})*;

// Factor rule for numbers or parentheses
factor returns [int value]
    : INT { $value = int($INT.text); }
    | '(' e=expr ')' { $value = $e.value; };

// Lexer rules
INT: [0-9]+;       // Matches integers
WS: [ \t\r\n]+ -> skip; // Skips whitespace
```

### **Step 2: Generate the Parser**

Run the following command to generate the Python lexer and parser:

```bash
antlr4 -Dlanguage=Python3 Calculator.g4
```

This will generate the following files:
- `CalculatorLexer.py`
- `CalculatorParser.py`
- Supporting files for the lexer and parser.

### **Step 3: Create the Python Driver Script**

Create a Python script `main.py` to test the parser:

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

### **Step 4: Run the Script**

Execute the script:

```bash
python main.py
```

### **Expected Output**

```plaintext
Calculator Parser Initialized.
Operation: addition, Result: 6
Operation: multiplication, Result: 30
Parsing complete.
```

---

## **4. Advanced Use Cases**

### **a. Handling Ambiguities**

Embedded Python code can help resolve ambiguities. For example, in a grammar where the same input could match multiple rules:

```antlr
expr returns [int value]
    : e1=term {$value = $e1.value;}
      ('+' e2=term {
          if $value > 10:
              print("Resolved to addition.")
          $value += $e2.value;
      })*;
```

### **b. Semantic Validation**

Perform validations inline:

```antlr
assign: ID '=' expr {
    if $ID.text not in allowed_variables:
        raise Exception(f"Variable {$ID.text} not allowed.")
};
```

### **c. Dynamic Behavior**

Use external libraries or runtime logic in embedded code:

```antlr
@members {
import math

def calculate_square_root(self, value):
    return math.sqrt(value)
}

expr: 'sqrt(' INT ')' {
    $value = self.calculate_square_root(int($INT.text));
};
```

---

## **5. Best Practices**

1. **Keep Embedded Code Minimal**:
   - Avoid embedding complex logic; use it only for essential operations.

2. **Use @members for Reusable Logic**:
   - Place helper functions in the `@members` section for better organization.

3. **Debug Incrementally**:
   - Test the grammar after adding embedded code to catch errors early.

4. **Check Generated Files**:
   - Ensure indentation and syntax are correct in the generated Python files.

5. **Separate Grammar from Logic**:
   - Use embedded code for actions closely tied to parsing, and handle general application logic elsewhere.

---

This detailed guide covers the why, when, and how of using embedded Python code in ANTLR, with practical examples and best practices. Let me know if you have any questions or need further assistance!

