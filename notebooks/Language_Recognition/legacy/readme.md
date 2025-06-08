## 🔍 Static Analysis Capabilities

<p align="center">
  <img src="img.png" alt="Overview" width="800"/>
</p>
<p align="center"><strong>Overview</strong></p>


Unlock key insights into your codebase with advanced static analysis techniques:

### ✅ 1. Syntax Analysis

* Validates that source code conforms to the grammar rules of its language.
* Detects syntax errors early in the development or migration process.

### 🔄 2. Flowchart Generation

* Generates visual diagrams from code to illustrate control flow.
* Simplifies understanding of legacy code logic and decision paths.

### 🔁 3. Control Flow Analysis

* Analyzes the sequence of program instructions.
* Supports debugging, performance tuning, and behavior analysis.

### 📈 4. Data Flow Analysis

* Tracks the lifecycle of data (definition, use, modification).
* Helps detect:

  * Unused variables
  * Dead code
  * Potential runtime issues

### 📦 5. Data Structure Analysis

* Evaluates the usage and efficiency of data structures.
* Aids in refactoring, modernization, and performance optimization.

### 🔗 6. Data Dependency Analysis

* Maps out dependencies between data elements and variables.
* Useful for:

  * Code impact analysis
  * Improving modularity
  * Refactoring legacy logic

---

## 🧠 Reverse Engineering

Easily convert legacy or poorly documented code into comprehensible formats using reverse engineering tools:

<p align="center">
  <img src="pic.png" alt="Reverse engineering" width="800"/>
</p>
<p align="center"><strong>Reverse Engineering</strong></p>


```
📦 Source Code + Data  (Existing legacy systems and artifacts)  
        ↓  
🔍 Discovery   (Discover the codebase to gain insights)
🤖 Use LLMs to summarize modules, extract entry points, and generate system maps  
        ↓  
💡 [Insight]  (Observations about business logic, data use, and structure)  
        ↓  
🧠 Understand  (Understand codebase to identify potential features)
        ↓ 
   🤖 Use AI to detect business rules, identify anomalies, and suggest patterns   
   🤖 Ask LLMs to explain functions and generate flowcharts or call graphs  
        ↓  
🎯 [Target Feature] + 📚 [Knowledge] 
        ↓  
🔍 Search + Retrieve  (Identify the business feature to modernize + Locate related code and data assets)
   🤖 Use AI to map business goals to code segments using embeddings or semantic search   
   🤖 Use vector search + LLM agents to find relevant code, tables, and logic paths  
        ↓  
🧩 [Related Code] + 💡 [Insight] + 📚 [Knowledge]  
        ↓  
🧠 Reverse Engineering  (Analyze and understand legacy logic)
   🤖 Generate contextual summaries for each component using AI
   🤖 Use AI to simulate code execution, convert to pseudocode, or suggest modern equivalents
   🤖 Extract requirements from legacy logic using AI and verify consistency with business goals  
        ↓  
📄 [Requirements]  (Define modern system requirements)
        ↓  
👨‍💻 Developer  (Build implementation plan (tasks, risks, milestones)
        ↓  
   🤖 AI assistant suggests task breakdowns, estimates effort, and highlights risk hotspots  
        ↓  
🛠️ Rebuild  
        ↓ 
   🤖 Use AI pair programming and code transformation tools for assisted rewriting  
        ↓  
✅ Test + Validate
        ↓   
   🤖 Generate test cases, predict missing coverage, and validate edge cases  
        ↓  
🚀 [Modern Code]
        ↓
   🤖 Continuously review code quality and suggest improvements post-migration  
```

| Target Level               | Description                                                   | Example                             |
| -------------------------- | ------------------------------------------------------------- | ----------------------------------- |
| **Business Capability**    | A broad domain or function the business performs              | Claims Management                   |
| **Feature / Use Case**     | A user-facing function or business outcome                    | Submit New Claim                    |
| **User Story / Flow**      | A single user action or step within a feature                 | Fill out claim form, upload receipt |
| **Component / Module**     | A group of related programs or screens supporting the feature | COBOL program `CLM001`, CICS screen |
| **Function / Program**     | Individual programs or routines                               | `calculateDeductible()` in COBOL    |
| **Code Block / Statement** | Specific logic chunks or conditions                           | `IF CLAIM_TYPE = "MED"` block       |
| **Data Element / Record**  | Specific data fields, database tables, or VSAM records        | `POLICY_ID`, `CLAIM_AMOUNT`         |

### Key Deliverables:

* **Flowcharts** to visualize logic and control paths.
* **Abstract models** to represent control/data dependencies.
* **Parsed structures** for easier migration and refactoring.

Ideal for:

* Understanding legacy COBOL or mainframe code
* Supporting modernization to modern architectures
* Enhancing documentation and onboarding

---

## 📚 Resources & Further Reading

Expand your knowledge with these helpful guides and tools:

### 🖥️ Mainframe & COBOL

* [IBM z/OS Basic Skills](https://www.ibm.com/docs/en/zos-basic-skills)
* [Mainframes Tech Help](https://www.mainframestechhelp.com/)
* [IBM Mainframer Portal](https://www.ibmmainframer.com/#)
* [IBM Z Open Editor](https://ibm.github.io/zopeneditor-about/)
* [GnuCOBOL Project](https://gnucobol.sourceforge.io/)
* [Learn COBOL in Y Minutes](https://learnxinyminutes.com/cobol/)

### 🔧 Tools & Community

* [Codefori Documentation](https://codefori.github.io/docs/)
* [Zowe: Modern Mainframe Framework](https://www.zowe.org/)
* [Sourcegraph: Universal Code Search](https://sourcegraph.com/)
* [code2flow (Flowchart Generator)](https://github.com/scottrogowski/code2flow)
