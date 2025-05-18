## Develop Grammar

Here’s a refined and completed version of your section, including a polished “Tips” subsection and maintaining your intended style and tone:

### 🤝 Develop Grammar Process with AI

#### Step 1: Co-Develop Documentation with AI

* Start with an **official language reference document** (or a well-prepared mini reference document of approximately 5 pages).
* Collect **around 10 typical source code files** written in the target programming language.
* Collaborate with an AI assistant to **generate a concise Mini Programming Language Document** in Markdown format.
* The goal is to help you **understand the language’s syntax, structure, and patterns** by building a tailored reference document with the help of AI.

#### 💡 Tips

* You can extract relevant **syntax examples** from the source code files and present them to the AI in isolation. This helps the AI:

  * Recognize and generalize common patterns.
  * Infer the underlying grammar rules.
  * Provide clearer documentation sections and grammar suggestions.
* Include both **valid and invalid** code examples to help the AI identify potential ambiguities or parsing issues.
* Organize examples by category (e.g., variable declaration, control flow, functions) to improve structure and accuracy.

#### Step 2: Write a Pseudo Grammar and Collaborate with AI

* Based on your **Mini Programming Language Documentation** and the **typical source code files**, attempt to **write a draft of the grammar yourself**.
* This grammar can be in **pseudo-grammar format** — it doesn’t need to follow ANTLR or BNF exactly, but it should clearly describe the structure of language constructs (e.g., statements, expressions, blocks).
* Once the pseudo grammar is drafted, ask the AI assistant to **enhance and refine it**, but request that it **strictly follows your original format and structure**.

#### 💡 Tips

* Keep the format consistent across rules (e.g., using `ruleName → components`).
* Use indentation or bullets if needed to show optional parts or alternatives.
* Label each rule clearly (e.g., `function → 'func' identifier '(' parameters ')' block`).
* Mention assumptions, like case sensitivity, reserved words, or comment styles.

#### **Example Prompt to AI**

```text
Here is my pseudo grammar based on my documentation and code samples. Please improve the grammar rules while strictly preserving my format. Do not rewrite the entire structure — only enhance or correct what’s necessary.
```

#### **Step 3: Parse Code Using the Grammar with a Gradual Content-Growth Strategy**

* Use your current grammar to **incrementally parse** code files from the beginning.
* Apply a **gradual content-growth strategy**:

  * Start by parsing a **small portion** of the code (e.g., the first few lines).
  * If it **parses successfully**, gradually **add more lines** of the code.
  * If a **parse error occurs**, **stop** and fix the grammar or the input based on the error.
  * Repeat the process until the **entire file is successfully parsed**.

#### **💡 Why This Strategy Works**

* A **gradual content-growth strategy** helps you:

  * Identify exactly **where the grammar breaks down**.
  * Isolate and **fix errors incrementally**, reducing debugging complexity.
  * Maintain **better control over error handling and grammar refinement**.

#### **Optional Tools**

* Use tools like **ANTLRWorks**, **ANTLR Preview (in VS Code)**, or **custom Python scripts** to test parsing behavior.
* Log successful parses and failures to **track grammar coverage**.

---

### 📘 Mini Programming Language Documentation Format

#### 1. **Usage**

* Describe the purpose and use cases of the mini programming language.
* Include target users and real-world scenarios where it can be applied.

---

#### 2. **Assessment Criteria**

Provide the evaluation framework for reviewing code written in the mini programming language:

* **Input/Output**
  Define expected input formats and output behavior.

* **Dependencies**
  List required libraries, external tools, or runtime environments.

* **Utilities**
  Specify built-in functions, helper scripts, or reusable modules.

* **Data Handling**

  * **Data Declarations**: Syntax for defining variables, constants, etc.
  * **Data Structures**: Supported structures like arrays, records, or objects.
  * **Database/Storage**: Integration details for persistent storage, if applicable.

---

#### 3. **Common Syntax and Examples**

* Showcase frequently used language features with examples.
* Include explanations for control structures (e.g., `if`, `loop`, `function`), assignments, and expressions.

---

#### 4. **Uncommon or Advanced Syntax**

* Highlight rare or advanced language features.
* Mention edge cases or syntax extensions not typically used in beginner code.

---

#### 5. **Code Snippets**

* Provide useful code snippets and idioms.
* Include patterns for error handling, data manipulation, or interaction with utilities.

---

#### 6. **References**

* Link to the official grammar file (e.g., ANTLR `.g4` file).
* Include links to tutorials, documentation, related papers, or libraries.

---

### ⚠️ Grammar Issues (Syntax Errors)

Common parsing errors that may arise when using this language with a parser such as ANTLR:

* **no viable alternative at input** *(most critical)*
  The parser couldn’t decide how to proceed due to ambiguous or incomplete grammar.

* **mismatched input**
  The input did not match the expected token at a given point.

* **extraneous input**
  The parser encountered unexpected tokens not fitting the rule.

---

### 🤖 Prompt Template for AI Collaboration

Use the following prompt when asking an AI assistant for help with parsing or debugging:

```prompt
Explain this error: {error}
When using this grammar: {grammar}
To parse this code: {code}
Suggest a fix or improved grammar rule.
```

---
