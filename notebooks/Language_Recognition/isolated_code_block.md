# Understanding Isolated Code Objects in Legacy Mainframe Reverse Engineering

## 🧱 What is an Isolated Code Object?

An **isolated code object** in the context of reverse engineering a legacy mainframe system refers to a **logically self-contained unit of code** that can be identified, labeled, and analyzed independently—though it may still depend on or interact with other components.

---

## 🔍 Definitions and Examples

| **Code Object Type**            | **Description**                                                                       | **Examples in Mainframe Context**              |
| ------------------------------- | ------------------------------------------------------------------------------------- | ---------------------------------------------- |
| **Program/Module**              | A full COBOL/PL/I source file representing a business process or batch unit           | `PAYROLL.CBL`, `INVOICE_PROCESS.PLI`           |
| **Paragraph / Section**         | A named block of logic within a COBOL program                                         | `CALCULATE-TAX`, `VALIDATE-DATA`               |
| **Copybook**                    | A reusable data or logic definition                                                   | `CUST-REC.CPY` defining customer record layout |
| **Job (JCL)**                   | A script defining job execution flow and resources                                    | `RUNDAILY.JCL`, steps with `EXEC PGM=...`      |
| **Subroutine / Called Program** | A reusable logic unit invoked by other programs                                       | `CALL 'DBACCESS' USING ...`                    |
| **Database Access Layer**       | Code that performs specific DB interactions (e.g., SQL embedded in COBOL or via CICS) | `EXEC SQL SELECT ...` blocks                   |
| **File Handler / I/O Logic**    | Code segments dealing with sequential or VSAM files                                   | `OPEN INPUT CUST-FILE`                         |
| **Screen Definitions / Maps**   | BMS maps or terminal screen layouts                                                   | `INQMAP.MAP`, `SEND MAP...`                    |

---

## 🎯 Purpose of Isolating Code Objects

* **Modular Analysis**: Understand and evaluate one unit at a time.
* **Parallel Processing**: Let different team members or AI agents process them concurrently.
* **Mapping**: Build a call/data flow graph from these components.
* **Requirement Generation**: Associate business logic with a specific isolated object.

---

## ✅ Criteria to Identify an Isolated Code Object

1. **Logical Boundaries**: Has a beginning and end (e.g., `PROGRAM-ID.` to `END PROGRAM.`).
2. **Defined Purpose**: Performs a distinct task (e.g., reading a file, calculating salary).
3. **Can Be Analyzed Independently**: Even if it calls others, you can inspect it alone.
4. **Resolvable Dependencies**: You can track what it calls or includes (e.g., copybooks).

---

## 🛠️ How to Isolate Them

* Use parsers (ANTLR, IDz, Micro Focus) to:

  * Identify COBOL program boundaries
  * Extract PERFORM trees or CALL graphs
  * Label file/database access points
* Use structural rules like:

  * `IDENTIFICATION DIVISION.` → new COBOL program
  * `EXEC CICS`, `CALL`, `PERFORM` → boundary markers

---

## 🧩 Levels of Isolated Code Blocks

Yes — **isolated code objects** exist at **multiple levels of granularity**, from small code blocks (micro-level) to large application workflows (macro-level). Understanding this hierarchy is crucial in **reverse engineering** and **migration planning**.

| **Level**                  | **Name**                     | **Description**                                                 | **Examples (Mainframe Context)**              |
| -------------------------- | ---------------------------- | --------------------------------------------------------------- | --------------------------------------------- |
| 🟢 **Micro**               | **Code Block / Paragraph**   | Smallest logic unit, often within a single program              | `PERFORM VALIDATE-CUSTOMER`                   |
| 🟡 **Unit**                | **Program / Subroutine**     | A COBOL program, PL/I module, or subroutine                     | `PROGRAM-ID. PROCESS-ORDER`                   |
| 🔵 **Mid**                 | **Job Step / Batch Step**    | One step in a batch job; often invokes one program              | `EXEC PGM=PAYROLLCALC` in JCL                 |
| 🟣 **Macro**               | **Job / Workflow**           | Complete JCL job or series of interdependent programs and steps | `RUNDAILY.JCL` job including multiple steps   |
| 🟠 **Application Process** | **End-to-End Business Flow** | Set of workflows forming a business service                     | `Loan Approval`, `Insurance Claim Processing` |
| 🔴 **System/Application**  | **Whole System**             | The full set of services, databases, workflows                  | `HR System`, `Inventory System`               |

---

## 🎯 How This Helps in Reverse Engineering

| **Level**               | **Use in Analysis**                                                       |
| ----------------------- | ------------------------------------------------------------------------- |
| **Micro / Unit**        | Detect redundant logic, extract business rules, AI-assisted documentation |
| **Mid (Job Step)**      | Reconstruct operational dependencies and runtime scheduling               |
| **Macro (Workflow)**    | Understand orchestration, data handoff between steps                      |
| **Application Process** | Identify end-to-end functional requirements for modernization             |
| **System**              | Define project scope, assess migration risk and timeline                  |

---

## 🔍 Example Breakdown

Suppose you have a payroll system:

```
🔴 Payroll System
 └── 🟠 Monthly Payroll Processing (Application Process)
      └── 🟣 RUNPAYROLL.JCL (Workflow)
           ├── 🔵 Step 1: EXEC PGM=VALIDATEEMP
           │    └── 🟡 Program: VALIDATEEMP.CBL
           │        └── 🟢 Paragraph: CHECK-SALARY-RANGE
           └── 🔵 Step 2: EXEC PGM=CALCSALARY
                └── 🟡 Program: CALCSALARY.CBL
                    └── 🟢 Paragraph: COMPUTE-GROSS-PAY
```

---

## ✅ Final Takeaways

* **Yes, smaller and larger isolated code blocks exist.**
* **Labeling and analyzing at multiple levels is essential**:

  * Micro → good for rule extraction
  * Macro → necessary for workflow understanding and re-architecting
* Each level should be treated as an "isolated unit" for specific kinds of reverse engineering tasks.


Here’s a **layered reverse engineering strategy** tailored to the different **levels of isolated code blocks** in a legacy mainframe system. This approach lets you progressively extract logic, data flow, and business intent from the **micro** level up to the **system** level—combining automation, AI agents, and human analysis.

---

## 🧩 Strategy: Reverse Engineering by Isolation Level

| **Level**                  | **Name**                 | **Goal**                                                                    | **Tools / Methods**                                                                               |
| -------------------------- | ------------------------ | --------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------- |
| 🟢 **Micro**               | Code Block / Paragraph   | Extract logic details and low-level rules                                   | - ANTLR, Regex parsing<br>- AI-based summarizer (e.g., GPT)<br>- Human-in-the-loop for validation |
| 🟡 **Unit**                | Program / Subroutine     | Understand business function, inputs/outputs, dependencies                  | - Control flow analysis<br>- Data flow tracing<br>- CALL/PERFORM graph                            |
| 🔵 **Mid**                 | Job Step / Batch Step    | Reconstruct execution context and runtime behavior                          | - JCL parser<br>- Step analysis<br>- Identify EXEC PGM, DD cards                                  |
| 🟣 **Macro**               | Job / Workflow           | Map program orchestration, job dependencies, and I/O links                  | - JCL DAG visualizer<br>- Event-driven job analysis<br>- Schedule mapping                         |
| 🟠 **Application Process** | End-to-End Business Flow | Map user or system workflows to technical workflows                         | - BPMN modeling<br>- Cross-step data lineage<br>- SME interviews                                  |
| 🔴 **System**              | Whole Application/System | Define architecture, interfaces, system boundaries, and modernization goals | - System inventory<br>- Interface catalog<br>- Tech stack assessment                              |

---

## 🛠️ Step-by-Step Strategy

### 1. **Catalog & Classify Artifacts**

* 🔍 Scan the repository for:

  * COBOL, PL/I programs
  * JCLs, Copybooks, DB2 SQL, VSAM files
* 📑 Tag and organize by type: paragraph, program, JCL, etc.

---

### 2. **Micro-Level Analysis (Paragraphs, Copybooks)**

* 🧠 Use **AI agents** or parsers to:

  * Extract logic per paragraph (`PERFORM`, `IF`, `EVALUATE`)
  * Summarize what each paragraph does
* ✅ Human validation of summaries to ensure accuracy

---

### 3. **Unit-Level Analysis (Programs/Subroutines)**

* 🔍 Analyze:

  * `PROGRAM-ID` boundaries
  * `CALL` and `PERFORM` trees
  * Input/output file and DB access
* 📊 Output:

  * I/O mapping
  * Control flow diagrams
  * Program-level summaries

---

### 4. **Mid-Level Analysis (Job Steps)**

* 🧾 Parse JCL:

  * Identify `EXEC PGM=`, `DD` statements
  * Map programs to job steps
* 🔄 Match JCL steps with COBOL programs
* ⏱️ Optionally include scheduling metadata (e.g., CA-7)

---

### 5. **Macro-Level Analysis (Job Workflows)**

* 🌐 Map entire jobs:

  * Sequence of steps
  * File handoffs between programs
* 🗺️ Visualize job flow as directed graphs (DAGs)
* 🤖 Use AI or rule-based heuristics to describe workflows

---

### 6. **Application Process-Level Analysis**

* 💼 Interview SMEs or use logs to understand:

  * Business processes (e.g., "Monthly Billing", "Loan Approval")
* 🧩 Match to technical workflows (job sequences)
* 🛠 Model with BPMN or flow diagrams

---

### 7. **System-Level Analysis**

* 🧱 Define system boundaries:

  * List included applications
  * Identify interfaces (CICS screens, DB, 3rd-party tools)
* 📦 Group workflows into applications
* 🧭 Use for scope definition and migration planning

---

## 🧠 AI Agent & Human-in-the-Loop

| **Task**                   | **AI Agent Role**                     | **Human Role**                       |
| -------------------------- | ------------------------------------- | ------------------------------------ |
| Paragraph Summarization    | Extract logic in natural language     | Validate critical or ambiguous cases |
| Program Flow Visualization | Generate CALL/PERFORM trees           | Refine misleading links              |
| Workflow Mapping           | Auto-build job graphs from JCL        | Fill gaps in inter-step data flow    |
| Business Process Alignment | Propose matches to business functions | Confirm with domain SMEs             |

---

## 📦 Output Artifacts by Level

| **Level**            | **Deliverables**                                 |
| -------------------- | ------------------------------------------------ |
| Micro (Paragraph)    | Annotated logic, business rules                  |
| Unit (Program)       | Control/data flow graph, program purpose summary |
| Mid (Job Step)       | JCL-to-program map, runtime dependencies         |
| Macro (Job/Workflow) | Job DAGs, orchestration documentation            |
| Application Process  | Business-to-technical workflow map               |
| System               | Full application landscape, modernization scope  |

---

## ✅ Tips for Success

* Automate where structure is regular (e.g., COBOL, JCL)
* Use human review where logic or naming is ambiguous
* Build reusable metadata (e.g., tagged programs, copybook mappings)
* Align with modernization goals (containerization, refactoring, etc.)
