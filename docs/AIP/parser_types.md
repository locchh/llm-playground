# Parser Types

## 1. Top-Down Parsers

### 🔹 Definition:

Start parsing from the **start symbol (root)** and try to rewrite it to match the input string.

### 🔹 Key Characteristics:

* Constructs parse tree from **top (start symbol) to bottom (leaves)**.
* Predicts which rule to apply using lookahead tokens.

### 🔹 Common Types:

* **Recursive Descent Parser** (manual or hand-written)
* **LL(k) Parser** (Left-to-right scan, Leftmost derivation)

### 🔹 Tools:

* ANTLR (builds LL(\*) parser)
* JavaCC

### 🔹 Pros:

* Simple to implement manually.
* Intuitive and easier to debug.

### 🔹 Cons:

* Cannot handle left-recursive grammars without modification.

---

## 2. Bottom-Up Parsers

### 🔹 Definition:

Start from the **input symbols (leaves)** and attempt to construct the parse tree **up to the start symbol**.

### 🔹 Key Characteristics:

* Performs a **shift** (read input) or **reduce** (apply grammar rule).
* Builds parse tree from bottom up.

### 🔹 Common Types:

* **LR(k) Parser** (Left-to-right scan, Rightmost derivation in reverse)
* **SLR, LALR, Canonical LR**

### 🔹 Tools:

* Yacc / Bison
* CUP (for Java)

### 🔹 Pros:

* Can handle a larger class of grammars than LL parsers.
* Suitable for programming language compilers.

### 🔹 Cons:

* More complex to implement.
* Parse tables can become large.

---

## 3. In-the-Middle Type (a.k.a. Hybrid Parsers or GLR)

### 🔹 Definition:

Uses a mix of top-down and bottom-up strategies, or parses non-deterministically.

### 🔹 Key Characteristics:

* Can explore **multiple parsing paths** simultaneously.
* Useful for **ambiguous or context-sensitive grammars**.

### 🔹 Common Types:

* **GLR (Generalized LR)**
* **Earley Parser**
* **Packrat Parser**
* **AI-based Parser**
* **Regex-based Parser**

### 🔹 Tools:

* Elkhound (GLR for C++)
* Java’s *SableCC*, *Bison GLR mode*

### 🔹 Pros:

* Handles **any context-free grammar**.
* Supports ambiguous grammars (e.g., natural language).

### 🔹 Cons:

* Slower than deterministic parsers.
* Higher memory usage.

---

## ✅ Summary Table

| Type              | Start From   | Common Algorithms     | Tools               | Handles Ambiguity | Use Case                            |
| ----------------- | ------------ | --------------------- | ------------------- | ----------------- | ----------------------------------- |
| **Top-Down**      | Start Symbol | LL, Recursive Descent | ANTLR, JavaCC       | ❌                 | Scripting, small languages          |
| **Bottom-Up**     | Input Tokens | LR, LALR, SLR         | Yacc, Bison         | ❌                 | Full compilers, C, Java             |
| **In-the-Middle** | Varies       | GLR, Earley           | Elkhound, Bison GLR | ✅                 | Natural languages, complex grammars |

Let me know if you want diagrams or examples for each type!
