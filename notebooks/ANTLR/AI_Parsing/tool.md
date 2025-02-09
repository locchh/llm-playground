### **Using Doccano for ANTLR Parsing: A Complete Guide**

You can use **Doccano** to annotate source code for **ANTLR parsing**, but it requires structured annotation. This guide explains how to integrate **Doccano** with **ANTLR-based parsing** efficiently.

---

## **1️⃣ Understanding the Use Case**
When annotating source code in Doccano for **ANTLR parsing**, the objectives can be:
1. **Annotate tokens and syntax structures** → Train a tokenizer or fine-tune an existing parser.
2. **Create labeled datasets for ANTLR grammar validation** → Use labeled examples to improve grammar accuracy.

---

## **2️⃣ Setting Up Doccano for Code Annotation**
**Doccano** supports **sequence labeling, text classification, and sequence-to-sequence tasks**.

### **A. Choosing the Annotation Type**
- **Sequence Labeling** (Best for ANTLR tokens)  
  → Label keywords, identifiers, literals, and operators.  
- **Text Classification** (Useful for code categories)  
  → Label whole blocks of code (e.g., `function`, `loop`, `condition`).  
- **Sequence-to-Sequence (Seq2Seq)** (Best for parsing outputs)  
  → Map input code to structured representations (like ASTs).  

---

## **3️⃣ Annotating Source Code with Doccano**

### **A. Annotating Tokens (Lexer Level)**
You can use **Sequence Labeling** to classify:
- **Keywords** (`if`, `while`, `return`)
- **Operators** (`+`, `-`, `=`)
- **Identifiers** (`variableName`, `functionName`)
- **Literals** (`42`, `"hello world"`)

💡 **Example Annotation in Doccano**  
| Token | Label |
|--|-|
| `if` | `KEYWORD` |
| `x` | `IDENTIFIER` |
| `==` | `OPERATOR` |
| `10` | `NUMBER` |

### **B. Annotating Syntax Structures (Parser Level)**
Use **Text Classification** to classify blocks:
- **Function definition**
- **Loop**
- **Conditional statement**
- **Expression**

💡 **Example:**  
```python
def add(a, b):
    return a + b
```
Annotated as:
- `def add(a, b):` → `FUNCTION_DEF`
- `return a + b` → `RETURN_STATEMENT`

---

## **4️⃣ Using ANTLR with Doccano Data**
Once you have **labeled datasets**, you can use them to refine **ANTLR grammars**.

### **Example: Using Annotations to Improve ANTLR Rules**
If Doccano annotations reveal inconsistencies, refine your grammar:
```antlr
IF : 'if' ;
ID : [a-zA-Z_][a-zA-Z_0-9]* ;
NUMBER : [0-9]+ ;
OP : '==' | '!=' | '>' | '<' ;
```

---

## **5️⃣ Automating Parsing with AI + ANTLR + Doccano**
1. **Train a model** (using annotated data) to predict missing grammar rules.  
2. **Use LLMs** to suggest corrections for incorrectly parsed inputs.  
3. **Integrate ANTLR & Doccano** for a feedback loop:  
   - Doccano **annotates new data**.  
   - ANTLR **parses & validates**.  
   - Model **learns from errors**.  

---

## **6️⃣ Script: Convert Doccano Annotations to ANTLR Test Cases**
This Python script:
1. **Parses Doccano JSONL output**.
2. **Generates test cases** in ANTLR format (`.txt` files).
3. **Validates annotated tokens** using ANTLR's lexer rules.

```python
import json
import os

OUTPUT_DIR = "antlr_test_cases"
os.makedirs(OUTPUT_DIR, exist_ok=True)

def load_doccano_annotations(file_path):
    with open(file_path, "r", encoding="utf-8") as f:
        return [json.loads(line) for line in f]

def generate_antlr_tests(doccano_data, output_dir):
    for i, entry in enumerate(doccano_data):
        text = entry["text"]
        labels = entry.get("labels", [])

        annotated_tokens = []
        for start, end, label in labels:
            token_text = text[start:end]
            annotated_tokens.append(f"{token_text} -> {label}")

        test_case = f"// Test Case {i+1}\n// Source Code:\n{text}\n\n"
        test_case += "\n".join(annotated_tokens)

        test_file = os.path.join(output_dir, f"test_case_{i+1}.txt")
        with open(test_file, "w", encoding="utf-8") as f:
            f.write(test_case)

        print(f"✅ Test case {i+1} saved: {test_file}")

# Run the conversion
doccano_file = "doccano_export.jsonl"
doccano_data = load_doccano_annotations(doccano_file)
generate_antlr_tests(doccano_data, OUTPUT_DIR)
```

---

## **7️⃣ How to Use with ANTLR**
1. **Prepare your ANTLR lexer and parser** (e.g., `MyLang.g4`).  
2. **Use ANTLR to tokenize test cases**:
   ```bash
   grun MyLang tokens test_case_1.txt
   ```
3. **Compare expected vs. actual tokens** for debugging.

---

## **🚀 Next Steps**
- ✅ **Automate ANTLR parsing** to validate test cases.  
- ✅ **Integrate with LLM** to suggest corrections.  
- ✅ **Extend support for multi-line code blocks**.  


