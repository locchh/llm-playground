# 📘 Comprehensive Guide to Mainframe Code Analysis

## 📌 1. Syntax Analysis

Certainly! Here's a clear and complete explanation of **Syntax Analysis** in the context of **mainframe code** (especially COBOL):

---

### 🧠 What is **Syntax Analysis**?

**Syntax Analysis** is the process of analyzing the **structure** of source code to ensure it follows the grammatical rules of the programming language—in this case, **COBOL**.

It is often the **first phase of program analysis** after tokenization and is essential for building tools like:

* Code parsers
* Static analyzers
* Converters and transpilers
* Refactoring tools

### 🔧 What Does It Do?

* **Parses** the COBOL source code.
* Builds a **parse tree** or **abstract syntax tree (AST)**.
* Validates whether the code conforms to the COBOL grammar.
* Identifies **syntax errors** (e.g., missing `PERFORM`, misplaced `IF`, etc.).

### 📄 Example: COBOL Syntax

```cobol
01 WS-AMOUNT PIC 9(5)V99 VALUE 0.

IF WS-AMOUNT > 100
    DISPLAY "TOO MUCH"
ELSE
    DISPLAY "OK".
```

**Parsed Syntax Tree** (simplified):

```
IF-STATEMENT
├── CONDITION: WS-AMOUNT > 100
├── THEN: DISPLAY "TOO MUCH"
└── ELSE: DISPLAY "OK"
```

### 📊 Syntax Analysis Output

| Output Type        | Description                                        |
| ------------------ | -------------------------------------------------- |
| **Parse Tree**     | Concrete structure of code based on full grammar   |
| **AST (Abstract)** | Simplified tree focusing on logic structure        |
| **Errors**         | Invalid tokens, misordered clauses, missing syntax |

### 🧰 Tools for COBOL Syntax Analysis

| Tool                 | Purpose                                      |
| -------------------- | -------------------------------------------- |
| **ANTLR**            | Grammar-based parser generator for COBOL     |
| **Micro Focus**      | Built-in COBOL parsing and debugging         |
| **IBM Rational RAA** | Parses and indexes COBOL for impact analysis |
| **Lex/Yacc**         | Older tools for building grammar parsers     |

### ✅ Why Syntax Analysis Matters

| Purpose                 | Example                                      |
| ----------------------- | -------------------------------------------- |
| Code modernization      | Parse COBOL to translate into Java/Python    |
| Linter/static checking  | Catch invalid patterns before compiling      |
| Custom IDE tooling      | Syntax highlighting, code completion         |
| Semantic analysis stage | Pre-requisite to analyzing control/data flow |

### 🧩 Next Step After Syntax Analysis?

After successful syntax parsing, you typically perform:

* **Control Flow Analysis (CFA)** – To analyze execution order
* **Data Flow Analysis (DFA)** – To track variable usage
* **Dependency Graphs** – To understand relationships between variables, paragraphs, and files

---

## 📌 2. Flowchart Analysis

### 🧭 What is **Flowchart Analysis**?

**Flowchart Analysis** is the process of **visualizing the logical flow** of a COBOL program using **standard flowchart symbols** like decision diamonds, process rectangles, arrows, etc.

This helps developers and analysts:

* Understand program logic at a glance
* Communicate business logic to non-programmers
* Identify loops, branches, and procedural sequences

### 🏗️ Flowchart vs Code

#### Example COBOL Code:

```cobol
IF WS-AMOUNT > 100
    PERFORM APPLY-DISCOUNT
ELSE
    PERFORM CHARGE-FULL
```

#### Corresponding Flowchart:

```
      +-------------------+
      | Start             |
      +-------------------+
               |
               v
    +-----------------------+
    | WS-AMOUNT > 100?      |
    +-----------------------+
          /        \  
       Yes          No
       /              \
+----------------+   +------------------+
| APPLY-DISCOUNT |   | CHARGE-FULL      |
+----------------+   +------------------+
         \             /
          v           v
     +-------------------+
     |      End          |
     +-------------------+
```

### 🧰 What Is Flowchart Analysis Used For?

| Use Case                        | Benefit                                                |
| ------------------------------- | ------------------------------------------------------ |
| **Code comprehension**          | Understand control flow for debugging or onboarding    |
| **Documentation**               | Create clear visual specs for business stakeholders    |
| **Refactoring & reengineering** | Spot redundant or unreachable branches                 |
| **Modernization efforts**       | Map procedural logic to structured or object paradigms |
| **QA and Test Case Design**     | Identify logical paths for test scenarios              |

### 🔁 Common Flowchart Elements in COBOL

| Flowchart Element | COBOL Equivalent                     |
| ----------------- | ------------------------------------ |
| Start/End         | `PROGRAM-ID`, `STOP RUN`             |
| Process           | `MOVE`, `COMPUTE`, `PERFORM`         |
| Decision          | `IF`, `EVALUATE`, `WHEN`             |
| Loop              | `PERFORM UNTIL`, `PERFORM VARYING`   |
| Connector         | `GO TO`, or implicit paragraph jumps |


### 🧠 Flowchart vs Control Flow Analysis

| Feature        | Flowchart Analysis                | Control Flow Analysis                  |
| -------------- | --------------------------------- | -------------------------------------- |
| View           | Human-friendly diagrams           | Graph nodes & edges for tools          |
| Purpose        | Understanding and communication   | Programmatic analysis and static tools |
| Representation | Visual with symbols               | Abstract graph (e.g., CFG)             |
| Use in tools   | Often generated for documentation | Used in optimization, verification     |

### 🛠️ Tools That Generate Flowcharts for COBOL

| Tool                                | Capability                                     |
| ----------------------------------- | ---------------------------------------------- |
| **IBM Rational RAA**                | Generates flowcharts from COBOL code           |
| **Micro Focus Enterprise Analyzer** | Diagram COBOL program logic                    |
| **YaCOBOL + Graphviz**              | Custom parsers with visual output              |
| **VS Code Extensions + AI**         | Auto-generate flowcharts from logic (beta use) |

### 📌 Summary

**Flowchart Analysis** helps:

* Quickly understand complex COBOL logic
* Communicate with business users
* Prepare for modernization and testing

---

## 📌 3. Control Flow Analysis (CFA)

Control Flow Analysis (CFA) in **mainframe code**, typically COBOL, involves understanding the order in which **paragraphs**, **sections**, and **statements** are executed. It's especially valuable for **impact analysis**, **debugging**, and **refactoring legacy systems**.

Here’s a basic example of **COBOL code** and a **control flow graph (CFG)** style explanation.

### 🔹 Example COBOL Code (`PAYROLL.CBL`)

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. PAYROLL.

PROCEDURE DIVISION.
    PERFORM INIT
    PERFORM CALC-PAYMENT
    IF PAY-GREATER-THAN-ZERO
        PERFORM ISSUE-PAYCHECK
    ELSE
        PERFORM LOG-ERROR
    END-IF
    PERFORM CLEANUP
    STOP RUN.

INIT.
    DISPLAY "Initializing".

CALC-PAYMENT.
    COMPUTE SALARY = HOURS-WORKED * RATE
    IF SALARY > 0
        MOVE 'Y' TO PAY-GREATER-THAN-ZERO
    ELSE
        MOVE 'N' TO PAY-GREATER-THAN-ZERO.

ISSUE-PAYCHECK.
    DISPLAY "Issuing paycheck".

LOG-ERROR.
    DISPLAY "Error: No pay to issue".

CLEANUP.
    DISPLAY "Cleaning up".
```

### 🔹 Control Flow Graph (CFG) Summary

```
        +--------+
        | INIT   |
        +--------+
             |
             v
     +---------------+
     | CALC-PAYMENT  |
     +---------------+
             |
             v
     [Is PAY-GREATER-THAN-ZERO = 'Y'?]
          /      \
         /        \
       Yes        No
       /            \
+----------------+  +-------------+
| ISSUE-PAYCHECK |  | LOG-ERROR  |
+----------------+  +-------------+
         \             /
          \           /
           v         v
         +-------------+
         |  CLEANUP    |
         +-------------+
               |
               v
           +--------+
           | STOP   |
           +--------+
```

### 🔹 Explanation of CFA in This Example

1. **Entry point** is the `PROCEDURE DIVISION`, starting with `PERFORM INIT`.
2. Flow proceeds *sequentially* with `PERFORM` statements.
3. A **branch** occurs with the `IF` condition — this causes the control to diverge to two possible paths:

   * `ISSUE-PAYCHECK`
   * `LOG-ERROR`
4. Regardless of the path taken, both converge to `CLEANUP`, then `STOP RUN`.

### 🔹 Use Cases for Control Flow Analysis in Mainframe Projects

* **Dead Code Detection**: Identify paragraphs never reached.
* **Impact Analysis**: Understand what’s affected if you change `CALC-PAYMENT`.
* **Security**: Verify sensitive operations (e.g., payroll) follow proper validation steps.
* **Refactoring**: Modernize COBOL code by flattening or simplifying control flow.
* **Visualization**: Tools like IBM Rational Developer or custom ANTLR parsers can generate CFGs.

The **difference between Control Flow Analysis (CFA)** and **Flowchart Analysis** in mainframe (especially COBOL or PL/I code) lies in their **purpose**, **granularity**, and **outputs**, though they are related.

| Aspect          | **Control Flow Analysis (CFA)**                      | **Flowchart Analysis**                                   |
| --------------- | ---------------------------------------------------- | -------------------------------------------------------- |
| **Purpose**     | Analyze the execution paths of code logically        | Visualize the program structure in diagrammatic form     |
| **Focus**       | Logical execution paths, conditions, loops, branches | Visual blocks representing steps and decisions           |
| **Granularity** | Detailed (e.g., paragraph/statement level)           | Higher-level (e.g., module/section level)                |
| **Output**      | Graph structure (often as control flow graph - CFG)  | Flowchart diagrams with boxes and arrows                 |
| **Use Cases**   | Static analysis, dead code detection, optimization   | Documentation, training, communication                   |
| **Direction**   | Code → Graph (machine-readable)                      | Code → Diagram (human-readable)                          |
| **Tools**       | ANTLR, IBM RAA, Enterprise Analyzer                  | Visio, Flowgorithm, IBM Engineering Lifecycle Management |

#### 🔹 Control Flow Analysis (CFA)

Analyzes paths of actual code execution. It’s **used by static analyzers** to:

* Build Control Flow Graphs (CFGs)
* Detect unreachable code
* Map perform chains and condition branches
* Feed automated tools like compilers, optimizers, or refactoring engines

**Example Insight:**

> "The paragraph `LOG-ERROR` is only reached when `SALARY <= 0`. No other paragraph leads to it."

#### 🔹 Flowchart Analysis

Creates a **visual diagram** showing the sequence of operations, conditionals, and loops using standard **flowchart shapes**.

Used by:

* Developers new to a system
* Business analysts and documentation teams
* Manual auditors

**Example Flowchart**:

```
[Start] → [INIT] → [CALC-PAYMENT] 
   ↓
 [Is Salary > 0?]
   ↓Yes           ↓No
[ISSUE PAYCHECK] [LOG ERROR]
         ↓             ↓
        [CLEANUP] → [STOP]
```

#### 🧠 In Simple Terms

* **Control Flow Analysis**: Think like a **compiler or static analyzer** — "Which paths are possible in this code?"
* **Flowchart Analysis**: Think like a **designer or communicator** — "How do I visually explain what this program does?"

If you’re doing **automated COBOL analysis**, you'd want **Control Flow Analysis**. If you’re explaining the code to someone, **Flowchart Analysis** is more suitable.

---

## 📌 4. Data Structure Analysis

Here’s a clear explanation and example of **Data Structure Analysis** in **mainframe COBOL**, which helps you understand how data fields are declared, nested, and used across a program or system.

### ✅ What Is Data Structure Analysis?

**Data Structure Analysis (DSA)** in a mainframe context involves analyzing the **hierarchical layout and usage** of variables declared in:

* **Working-Storage Section**
* **File Section**
* **Linkage Section**
* **Group-level items** (01, 05, 10 levels)
* **Redefines**, **Occurs**, and **Copybooks**

### 🟨 Example: COBOL Data Structure

```cobol
WORKING-STORAGE SECTION.
01 CUSTOMER-INFO.
   05 CUSTOMER-ID         PIC 9(5).
   05 CUSTOMER-NAME.
      10 FIRST-NAME       PIC X(10).
      10 LAST-NAME        PIC X(15).
   05 CUSTOMER-ADDRESS.
      10 STREET           PIC X(20).
      10 CITY             PIC X(15).
      10 STATE            PIC XX.
      10 ZIP-CODE         PIC 9(5).
01 ORDER-INFO REDEFINES CUSTOMER-INFO.
   05 ORDER-ID            PIC 9(5).
   05 ORDER-TOTAL         PIC 9(5)V99.
```

### 🔍 Data Structure Analysis Breakdown

| Field Name            | Type       | Level | Parent           | Redefined?              | Occurs? | Notes                              |
| --------------------- | ---------- | ----- | ---------------- | ----------------------- | ------- | ---------------------------------- |
| `CUSTOMER-INFO`       | Group      | 01    | —                | Yes                     | No      | Base structure                     |
| `CUSTOMER-ID`         | Elementary | 05    | CUSTOMER-INFO    | No                      | No      | Customer primary key               |
| `CUSTOMER-NAME`       | Group      | 05    | CUSTOMER-INFO    | No                      | No      | Nested name fields                 |
| `FIRST-NAME`          | Elementary | 10    | CUSTOMER-NAME    | No                      | No      | —                                  |
| `LAST-NAME`           | Elementary | 10    | CUSTOMER-NAME    | No                      | No      | —                                  |
| `CUSTOMER-ADDRESS`    | Group      | 05    | CUSTOMER-INFO    | No                      | No      | Nested address group               |
| `STREET`, `CITY`, etc | Elementary | 10    | CUSTOMER-ADDRESS | No                      | No      | —                                  |
| `ORDER-INFO`          | Group      | 01    | — (Redefines)    | Redefines CUSTOMER-INFO | No      | Reuses same memory                 |
| `ORDER-ID`            | Elementary | 05    | ORDER-INFO       | Yes                     | No      | Overwrites fields in CUSTOMER-INFO |

### 🧠 Use Cases for Data Structure Analysis

| Purpose                         | What It Helps With                                           |
| ------------------------------- | ------------------------------------------------------------ |
| **Program Understanding**       | Visualize how deeply nested or reused the data is            |
| **Impact Analysis**             | Track where fields are used, modified, or redefined          |
| **Refactoring**                 | Identify opportunities to modularize or simplify data layout |
| **Copybook Analysis**           | Understand and visualize reused data across programs         |
| **Redefines Auditing**          | Detect conflicting memory layouts (like above)               |
| **Data Mapping/Transformation** | Useful for migrating to JSON, XML, DB, etc.                  |

### 🛠️ Tools That Perform Data Structure Analysis

| Tool                            | Description                                               |
| ------------------------------- | --------------------------------------------------------- |
| IBM Rational RAA                | Shows full structure tree, usage, and field relationships |
| Micro Focus Enterprise Analyzer | Visualization of nested COBOL data                        |
| Custom Python/ANTLR             | Extract group/item hierarchies programmatically           |
| Mainframe Scanners              | In-house tools to extract 01/05/10 structures             |

### 📊 Visual Tree Output (Example)

```plaintext
CUSTOMER-INFO
├── CUSTOMER-ID
├── CUSTOMER-NAME
│   ├── FIRST-NAME
│   └── LAST-NAME
├── CUSTOMER-ADDRESS
│   ├── STREET
│   ├── CITY
│   ├── STATE
│   └── ZIP-CODE
└── (REDEFINED BY) ORDER-INFO
    ├── ORDER-ID
    └── ORDER-TOTAL
```
---

## 📌 5. Data Flow Analysis (DFA)

### ✅ What is **Data Flow Analysis (DFA)** in Mainframe Code?

**Data Flow Analysis (DFA)** tracks **how data is defined, used, and propagated** through a program. In **mainframe languages like COBOL**, it helps identify:

* Variables that are **defined but not used**
* Variables that are **used before being defined**
* How **data values flow** through different **paragraphs**, **sections**, or **modules**
* **Side effects** of `PERFORM`, `MOVE`, `COMPUTE`, etc.

### 🟨 Example COBOL Code

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. DATACHECK.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 A       PIC 9(3).
01 B       PIC 9(3).
01 C       PIC 9(3).

PROCEDURE DIVISION.
    MOVE 100 TO A
    PERFORM CALC-B
    DISPLAY B
    STOP RUN.

CALC-B.
    COMPUTE B = A + C.
```

### 🔍 Data Flow Analysis (DFA) Breakdown

| Variable | Defined (Assigned)  | Used (Read)         | Issues?                       |
| -------- | ------------------- | ------------------- | ----------------------------- |
| `A`      | `MOVE 100 TO A`     | `COMPUTE B = A + C` | ✅ Properly defined before use |
| `B`      | `COMPUTE B = A + C` | `DISPLAY B`         | ✅ Properly defined before use |
| `C`      | ✖️ Never defined    | `COMPUTE B = A + C` | ❌ **Used before definition!** |

### 🔁 DFA Use Cases in Mainframe Systems

1. **Dead Code Elimination**: Remove unused variables or unreachable assignments.
2. **Optimization**: Inline constants, reduce memory usage.
3. **Error Detection**: Catch `USED BEFORE DEFINED` or `DEFINED BUT NEVER USED` patterns.
4. **Impact Analysis**: Understand how changing variable `X` will affect downstream logic.
   
### 🔧 Common DFA Questions in COBOL Systems

| Question                                        | Answered by DFA? | Example                    |
| ----------------------------------------------- | ---------------- | -------------------------- |
| "What variables does `CALC-B` read or write?"   | ✅                | `Reads: A, C`, `Writes: B` |
| "Is `C` ever initialized?"                      | ✅                | ❌ No; error-prone          |
| "Where is `B` modified?"                        | ✅                | In `CALC-B`, via `COMPUTE` |
| "Does any paragraph write to `A` after `INIT`?" | ✅                | No                         |

Here’s a concrete **example of Data Flow Analysis (DFA)** in **mainframe COBOL code**, showing how variables are **defined, used, or left undefined**, along with a **Def-Use chain table** and analysis results.

### 🟨 Example COBOL Source Code

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. INVENTORY.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 ITEM-COUNT     PIC 9(4).
01 RESTOCK-LEVEL  PIC 9(4).
01 REORDER-FLAG   PIC X.

PROCEDURE DIVISION.
    MOVE 30 TO ITEM-COUNT
    PERFORM CHECK-STOCK
    DISPLAY "Reorder Needed: " REORDER-FLAG
    STOP RUN.

CHECK-STOCK.
    IF ITEM-COUNT < RESTOCK-LEVEL
        MOVE 'Y' TO REORDER-FLAG
    ELSE
        MOVE 'N' TO REORDER-FLAG.
```

### 📌 Step-by-Step **Data Flow Analysis (DFA)**

#### 🔸 1. **Identify Variable Assignments and Uses**

| Variable        | Defined At                           | Used At                         |
| --------------- | ------------------------------------ | ------------------------------- |
| `ITEM-COUNT`    | `MOVE 30 TO ITEM-COUNT`              | `IF ITEM-COUNT < RESTOCK-LEVEL` |
| `RESTOCK-LEVEL` | ❌ **Never defined**                  | `IF ITEM-COUNT < RESTOCK-LEVEL` |
| `REORDER-FLAG`  | `MOVE 'Y' TO ...`, `MOVE 'N' TO ...` | `DISPLAY REORDER-FLAG`          |


#### 🔸 2. **Detect Common DFA Issues**

| Issue Type            | Variable        | Explanation                                                       |
| --------------------- | --------------- | ----------------------------------------------------------------- |
| ❌ Used Before Defined | `RESTOCK-LEVEL` | It's used in a condition, but never assigned any value before use |
| ✅ Proper Use          | `ITEM-COUNT`    | Assigned before use                                               |
| ✅ Proper Use          | `REORDER-FLAG`  | Assigned before being displayed                                   |


### 🔁 Def-Use Chains

#### A Def-Use (DU) chain tracks where a variable is **defined** and then **used**:

```
ITEM-COUNT:
  DEF: line 11 → USE: line 14

RESTOCK-LEVEL:
  NO DEF → USE: line 14 (⚠️)

REORDER-FLAG:
  DEF: line 15/17 → USE: line 18
```

### ✅ What DFA Helps You Discover

| Insight                          | Found? | Action Needed                           |
| -------------------------------- | ------ | --------------------------------------- |
| Variable used without definition | ✅ Yes  | Initialize `RESTOCK-LEVEL`              |
| Variable defined but unused      | ❌ No   | -                                       |
| Data flow through paragraphs     | ✅ Yes  | Track how `ITEM-COUNT` → `REORDER-FLAG` |
| Inter-paragraph data effects     | ✅ Yes  | `PERFORM CHECK-STOCK` changes state     |


### 🛠 Example Fix for Undefined Use

To fix the undefined use of `RESTOCK-LEVEL`, we can add:

```cobol
    MOVE 50 TO RESTOCK-LEVEL
```

Before the `PERFORM CHECK-STOCK`.

---

## 📌 6. Data Dependency Analysis

### ✅ What is **Data Dependency Analysis** in Mainframe?

**Data Dependency Analysis** tracks how **data elements (variables or fields)** depend on one another within a **COBOL** or **mainframe application**. This includes **which variables influence other variables** during execution, either **directly** (via assignment) or **indirectly** (via conditions, file reads, subprogram calls).


### 🔍 Purpose

| Goal                           | Description                                              |
| ------------------------------ | -------------------------------------------------------- |
| 🔄 Identify data relationships | Understand how a change in one variable affects others   |
| 🔬 Support impact analysis     | Before modifying a field or layout (e.g., in a copybook) |
| ⚙️ Enable optimization         | Detect redundant or unnecessary intermediate fields      |
| 🧪 Aid testing                 | Focus test cases on variables that influence each other  |
| 🔒 Detect security flaws       | Identify unsafe flows of sensitive data                  |


### 🟨 Example: COBOL Source Code

```cobol
WORKING-STORAGE SECTION.
01 WS-AMOUNT-A     PIC 9(5)V99.
01 WS-AMOUNT-B     PIC 9(5)V99.
01 WS-DISCOUNT     PIC 9(3)V99.
01 WS-TOTAL        PIC 9(5)V99.
01 WS-FINAL        PIC 9(5)V99.

PROCEDURE DIVISION.
    MOVE 100.00 TO WS-AMOUNT-A
    MOVE 50.00 TO WS-AMOUNT-B
    COMPUTE WS-TOTAL = WS-AMOUNT-A + WS-AMOUNT-B
    MOVE 10.00 TO WS-DISCOUNT
    COMPUTE WS-FINAL = WS-TOTAL - WS-DISCOUNT
```

### 📈 Data Dependency Graph

```
WS-AMOUNT-A ─┐
             ├──► WS-TOTAL ──► WS-FINAL
WS-AMOUNT-B ─┘
WS-DISCOUNT ─────────────────►
```

### 📊 Dependency Table

| Target Field | Source Fields                | Type            | Explanation                       |
| ------------ | ---------------------------- | --------------- | --------------------------------- |
| `WS-TOTAL`   | `WS-AMOUNT-A`, `WS-AMOUNT-B` | Data dependency | Result depends on inputs A and B  |
| `WS-FINAL`   | `WS-TOTAL`, `WS-DISCOUNT`    | Data dependency | Final value is influenced by both |


## 🔁 Transitive Dependency

A **transitive dependency** exists when a variable influences another through an intermediate variable.

```
WS-AMOUNT-A → WS-TOTAL → WS-FINAL
```

So `WS-AMOUNT-A` indirectly influences `WS-FINAL`.

## 🛠️ Use Cases in Mainframe Context

| Use Case                     | Description                                                    |
| ---------------------------- | -------------------------------------------------------------- |
| **Copybook migration**       | See how a field like `CUST-ID` affects multiple programs       |
| **Impact of field renaming** | Identify all logic impacted by changing `WS-TOTAL` to `WS-SUM` |
| **Security audits**          | Trace how `SSN` or `PIN` flows through logic                   |
| **Test automation**          | Prioritize test cases based on data impact                     |

## 🧰 Tools for Data Dependency Analysis

| Tool                            | Description                                           |
| ------------------------------- | ----------------------------------------------------- |
| IBM Rational RAA                | Analyzes field-level data flows across programs       |
| Micro Focus Enterprise Analyzer | Tracks inter-field dependencies even across copybooks |
| ANTLR + Python custom parser    | Build your own dependency analyzer                    |
| Mainframe field mapping tools   | Often used during modernization or DB refactoring     |

---

## 📌 7. Call Graph Analysis

Here's a practical **example of a Call Graph** in **mainframe COBOL**, demonstrating how **paragraphs, PERFORMs, subprograms, and CALL statements** interact.

### 🟨 Example COBOL Codebase (Main + Subprogram)

🔹 `MAINPROG.CBL`

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. MAINPROG.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-NAME     PIC X(20).

PROCEDURE DIVISION.
    PERFORM INIT
    PERFORM GET-NAME
    CALL 'GREETER' USING WS-NAME
    PERFORM CLEANUP
    STOP RUN.

INIT.
    DISPLAY "Starting program...".

GET-NAME.
    MOVE "John Doe" TO WS-NAME.

CLEANUP.
    DISPLAY "End of MAINPROG.".
```

---

🔹 `GREETER.CBL`

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. GREETER.

DATA DIVISION.
LINKAGE SECTION.
01 LK-NAME PIC X(20).

PROCEDURE DIVISION USING LK-NAME.
    DISPLAY "Hello, " LK-NAME
    EXIT PROGRAM.
```

### 🔷 What Is a Call Graph?

A **Call Graph** is a **directed graph** showing the calling relationships between **paragraphs** and **subprograms**.

### 📈 Call Graph for `MAINPROG` and `GREETER`

```
          +-------------+
          |  MAINPROG   |
          +-------------+
                |
       +--------+--------+-------------+
       |        |                      |
     [INIT]  [GET-NAME]           [CLEANUP]
                       \
                        \
                         +--> [CALL 'GREETER'] --> +-------------+
                                                   |   GREETER   |
                                                   +-------------+
```

### 🔍 Breakdown of the Call Graph

| Caller     | Callee     | Type    | Notes                                      |
| ---------- | ---------- | ------- | ------------------------------------------ |
| `MAINPROG` | `INIT`     | PERFORM | Internal paragraph                         |
| `MAINPROG` | `GET-NAME` | PERFORM | Sets `WS-NAME`                             |
| `MAINPROG` | `GREETER`  | CALL    | External program call using `USING` clause |
| `MAINPROG` | `CLEANUP`  | PERFORM | Final message                              |

### ✅ Benefits of a Call Graph in Mainframe

| Use Case                  | Explanation                                              |
| ------------------------- | -------------------------------------------------------- |
| **Impact Analysis**       | Know what is affected if you change a paragraph or CALL  |
| **Dead Code Detection**   | Identify paragraphs or subprograms that are never called |
| **Refactoring**           | Helps group frequently used code into reusable modules   |
| **Performance Profiling** | Trace expensive calls for optimization                   |
| **Debugging**             | Quickly trace the source of a crash or failure           |

---

## 📌 8. Dead Code / Missing Artifacts
In mainframe environments (especially in **COBOL** or **PL/I** programs), **"dead" artifacts** refer to **unused or unreferenced parts of code** that increase complexity and risk without adding value.

Here’s a clear explanation of:

### 🔹 Dead File

> A **dead file** is a file (e.g., **VSAM**, **sequential**, or **DB2 table**) that is **declared or defined** in the program but **never read from or written to**.

Example:

```cobol
FD DEAD-FILE
   LABEL RECORD STANDARD
   BLOCK CONTAINS 0 RECORDS
   DATA RECORD IS DEAD-RECORD.

01 DEAD-RECORD PIC X(80).
```

If `DEAD-FILE` is **not used** in any `READ`, `WRITE`, `OPEN`, or `CLOSE` statement, it is a **dead file**.

### 🔹 Dead Variable

> A **dead variable** is a **data item** that is declared in the **WORKING-STORAGE SECTION** (or elsewhere) but is **never referenced** in any **PROCEDURE DIVISION** logic.

#### Example:

```cobol
01 UNUSED-VAR   PIC X(10).   *> Declared
```

If `UNUSED-VAR` is never used in a `MOVE`, `IF`, `DISPLAY`, or passed to any subroutine, it’s a **dead variable**.

### 🔹 Dead Paragraph (Dead Code)

> A **dead paragraph** is a **PROCEDURE DIVISION paragraph** that is **never performed or called**.

Example:

```cobol
UNUSED-PARAGRAPH.
    DISPLAY "This is never called.".
```

If there is **no `PERFORM UNUSED-PARAGRAPH`** or `GOTO` to it, it’s a **dead paragraph**.

### 🔎 How They Occur

| Dead Type | Common Causes                            |
| --------- | ---------------------------------------- |
| File      | Code refactoring but forgot to remove FD |
| Variable  | Copy-paste from template, not cleaned    |
| Paragraph | Replaced by new logic but not deleted    |

### 🧰 How to Detect Them

| Detection Method      | Tools / Techniques                     |
| --------------------- | -------------------------------------- |
| Manual Review         | Look for unused FD, WS, PERFORM        |
| Static Analysis Tools | IBM Rational RAA, Micro Focus Analyzer |
| Custom Scripts        | Python + ANTLR or regex parser         |
| Compiler Warnings     | Some compilers flag unused code        |


### 🧹 Why Clean Dead Code?

| Reason               | Benefit                         |
| -------------------- | ------------------------------- |
| Reduce Complexity    | Easier to maintain              |
| Improve Performance  | Smaller memory/IO footprint     |
| Enable Modernization | Clean code = better refactoring |
| Reduce Risk          | Dead code can hide logic errors |

---

## 📌 9. CRUD Analysis

### ✅ What is **CRUD** in Mainframe?

**CRUD** stands for:

| Letter | Operation | Mainframe Context (COBOL, DB2, VSAM)         |
| ------ | --------- | -------------------------------------------- |
| **C**  | Create    | Insert new records into files or databases   |
| **R**  | Read      | Fetch records using keys or sequential reads |
| **U**  | Update    | Modify existing records                      |
| **D**  | Delete    | Remove records from data stores              |

CRUD operations are used to analyze how **programs interact with datasets**, typically **VSAM files**, **DB2 tables**, or **IMS segments**.

### 🔄 Example: COBOL with DB2 (Embedded SQL)

```cobol
EXEC SQL
   INSERT INTO CUSTOMER (ID, NAME, BALANCE)
   VALUES (:WS-ID, :WS-NAME, :WS-BAL)
END-EXEC.         *> CREATE

EXEC SQL
   SELECT NAME, BALANCE INTO :WS-NAME, :WS-BAL
   FROM CUSTOMER
   WHERE ID = :WS-ID
END-EXEC.         *> READ

EXEC SQL
   UPDATE CUSTOMER
   SET BALANCE = :WS-BAL
   WHERE ID = :WS-ID
END-EXEC.         *> UPDATE

EXEC SQL
   DELETE FROM CUSTOMER
   WHERE ID = :WS-ID
END-EXEC.         *> DELETE
```

### 📁 Example: COBOL with VSAM (File-Based)

```cobol
OPEN OUTPUT CUSTOMER-FILE         *> CREATE

READ CUSTOMER-FILE                *> READ

REWRITE CUSTOMER-RECORD           *> UPDATE

DELETE CUSTOMER-RECORD            *> DELETE
```

### 📊 CRUD Analysis Use Cases

| Goal                       | Description                                             |
| -------------------------- | ------------------------------------------------------- |
| 🔍 Program Impact Analysis | Identify which programs perform what operations on data |
| 📈 Modernization Planning  | Classify apps as Create-heavy, Read-heavy, etc.         |
| 🔐 Security Audits         | Who can delete sensitive records?                       |
| 🧪 Test Coverage           | Ensure each CRUD action is exercised in QA              |
| 🔄 Migration to APIs       | Map legacy CRUD operations to RESTful services          |


### 🛠️ Tools to Perform CRUD Analysis

| Tool/Method                     | What It Does                                    |
| ------------------------------- | ----------------------------------------------- |
| IBM Rational RAA                | Tracks CRUD actions across DB2, IMS, and VSAM   |
| Micro Focus Enterprise Analyzer | Scans COBOL programs to generate CRUD reports   |
| ANTLR + Custom Parser           | Extracts CRUD statements from COBOL code        |
| SQL Log or Audit Tables         | Detects actual run-time inserts/updates/deletes |

