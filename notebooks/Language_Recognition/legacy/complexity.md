## 🧩 Types of Complexity

### 1. **Syntactic Complexity**

* **Definition:** How complex the code's **structure or syntax** is (e.g., deeply nested code, long expressions).
* **Measured by:**

  * Line count, nesting depth, number of tokens or AST nodes.
* **Related to:**
  ✅ **Syntax Understanding**
* **Example:** Deeply nested `if` or `switch` statements.

---

### 2. **Cyclomatic Complexity (McCabe Complexity)**

* **Definition:** Measures the number of **independent paths** through a program.
* **Calculated from:**
  ✅ **Control Flow Graph (CFG)**
* **Formula:**
  `M = E - N + 2P`
  where:

  * `E` = edges in the CFG
  * `N` = nodes
  * `P` = number of connected components (usually 1 for a single function)
* **Related to:**
  ✅ **Static Behavior Understanding**
* **Why it matters:**

  * Indicates test case requirements.
  * High complexity → harder to test, understand, and maintain.

---

### 3. **Cognitive Complexity**

* **Definition:** How hard the code is for a human to read and understand.
* **Goes beyond:** Just control structures — considers readability, nesting, and mental effort.
* **Used in tools like:** SonarQube.
* **Related to:**
  ✅ **Syntax + Static Behavior Understanding**

---

### 4. **Runtime Complexity (Dynamic Complexity)**

* **Definition:** Describes how complex the **actual execution** is, considering inputs, data states, and runtime paths.
* **Measured by:**

  * Number of function calls, branching at runtime.
  * Execution trace length or variability.
* **Related to:**
  ✅ **Dynamic Behavior Understanding**
* **Example:** A function that behaves very differently depending on runtime data.

---

## 🧠 Software AI Implications

| Type of Complexity  | Impact on AI Tasks                                       |
| ------------------- | -------------------------------------------------------- |
| **Syntactic**       | Affects code generation and parsing.                     |
| **Cyclomatic**      | Guides test generation and symbolic execution.           |
| **Cognitive**       | Affects explainability, summarization, and code review.  |
| **Runtime/Dynamic** | Influences dynamic analysis, debugging, profiling tasks. |

---

## 📊 Summary Table

| Complexity Type     | Source                | Related Graph/Data | AI Understanding Level |
| ------------------- | --------------------- | ------------------ | ---------------------- |
| **Syntactic**       | Code structure        | AST                | Syntax Understanding   |
| **Cyclomatic**      | Logic branches        | CFG                | Static Behavior        |
| **Cognitive**       | Human readability     | AST + heuristics   | Syntax + Static        |
| **Dynamic/Runtime** | Execution variability | Execution Trace    | Dynamic Behavior       |
