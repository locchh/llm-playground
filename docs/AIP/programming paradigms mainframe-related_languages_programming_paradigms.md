## Mainframe Language Programming Paradigms

Here's a table summarizing the **programming paradigms** of several **mainframe-related languages** and other historical/system languages. The paradigms include:

* **OOP**: Object-Oriented Programming
* **FP**: Functional Programming
* **PP**: Procedural Programming
* **DP**: Declarative Programming
* **LP**: Logic Programming


### 🔹 Paradigm Table for Mainframe and Classic Languages

| Language                       | OOP | FP | PP | DP | LP | Notes                                                                     |
| ------------------------------ | --- | -- | -- | -- | -- | ------------------------------------------------------------------------- |
| **COBOL**                      | ❌   | ❌  | ✅  | ❌  | ❌  | Procedural, business-oriented; OO features in COBOL 2002 but rarely used. |
| **PL/I**                       | ❌   | ❌  | ✅  | ❌  | ❌  | Procedural with some structured programming support.                      |
| **JCL**                        | ❌   | ❌  | ❌  | ✅  | ❌  | Declarative scripting for job control on IBM mainframes.                  |
| **CopyBOOK**                   | ❌   | ❌  | ❌  | ❌  | ❌  | Not a language—just a reusable code fragment in COBOL for data layout.    |
| **ALGOL**                      | ❌   | ❌  | ✅  | ❌  | ❌  | Influential procedural language; no OOP or FP features.                   |
| **Assembly (e.g., IBM HLASM)** | ❌   | ❌  | ✅  | ❌  | ❌  | Low-level, procedural.                                                    |
| **FORTRAN**                    | ❌   | ❌  | ✅  | ❌  | ❌  | Scientific procedural language; modern versions added OOP.                |
| **RPG**                        | ❌   | ❌  | ✅  | ❌  | ❌  | Report generation language for IBM systems.                               |
| **REXX**                       | ❌   | ❌  | ✅  | ✅  | ❌  | Procedural + declarative scripting; used on TSO, z/OS.                    |
| **Prolog**                     | ❌   | ❌  | ❌  | ✅  | ✅  | Classical logic programming language.                                     |
| **LISP**                       | ❌   | ✅  | ✅  | ❌  | ❌  | Early FP language with procedural elements.                               |
| **CLIST**                      | ❌   | ❌  | ✅  | ✅  | ❌  | Scripting language for TSO; procedural + declarative aspects.             |


### 🔸 Legend:

* ✅ = Yes (core support or widely used in that style)
* ❌ = No (not designed for or incompatible with that paradigm)

## Migration Strategy

Here’s a **migration strategy** and **modern target language(s)** for each classic **programming paradigm**, especially relevant when modernizing legacy **mainframe or historical languages**.


### ✅ Summary Table: Migration Strategy by Paradigm

| **Paradigm**                          | **Definition**                                                | **Legacy Language Examples**          | **Modern Target Language(s)**                                              | **Migration Strategy**                                                                                                     |
| ------------------------------------- | ------------------------------------------------------------- | ------------------------------------- | -------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------- |
| **PP (Procedural Programming)**       | Executes step-by-step instructions in functions or procedures | COBOL, PL/I, ALGOL, FORTRAN, Assembly | Python, Go, C, Rust                                                        | Modularize into functions/methods; extract reusable components; use structured data types; replace global variables.       |
| **OOP (Object-Oriented Programming)** | Models data using objects and classes                         | COBOL (OO 2002), Smalltalk (historic) | Java, C#, Python, Kotlin                                                   | Identify data structures + related logic; wrap procedural logic into classes; model business entities as objects.          |
| **FP (Functional Programming)**       | Pure functions, immutability, stateless logic                 | LISP, Scheme                          | Haskell, Scala, Elixir, F#, OCaml                                          | Convert side-effect-heavy code into pure functions; use immutable data; eliminate global state.                            |
| **DP (Declarative Programming)**      | Describes **what** to do, not how                             | JCL, SQL, Copybooks, HTML             | SQL (modern dialects), YAML, Terraform, Ansible, XSLT                      | Identify rule-based logic or configurations; rewrite using DSLs or config files; use orchestration tools where applicable. |
| **LP (Logic Programming)**            | Uses facts and rules to infer conclusions                     | Prolog                                | Prolog (modern), Datalog, CLIPS, or integrate into Python (e.g. PyDatalog) | Convert decision logic into rule engines; use inference frameworks; consider declarative business rules platforms.         |

---

### 🔹 Examples by Legacy Language

| **Legacy Language** | **Paradigm(s)**       | **Suggested Modern Language(s)**        | **Migration Notes**                                                       |
| ------------------- | --------------------- | --------------------------------------- | ------------------------------------------------------------------------- |
| COBOL               | PP (some OOP in 2002) | Java, C#, Python                        | Wrap in services or classes; use microservices to isolate logic.          |
| PL/I                | PP                    | C, Java, Rust                           | Migrate using structured functions or classes.                            |
| JCL                 | DP                    | YAML + CI/CD tools (e.g., Jenkinsfiles) | Replace with orchestration tools like Jenkins, Ansible, or shell scripts. |
| CopyBOOK            | Data Declaration      | JSON, XML Schema, Protobuf              | Convert data layouts into structured schemas or serialization formats.    |
| ALGOL               | PP                    | Python, Go, Rust                        | Use procedural logic refactoring and static typing.                       |
| Assembly            | PP                    | C, Rust, low-level C++                  | Rewrite in higher-level system language with similar memory control.      |
| FORTRAN             | PP                    | Python (NumPy), Julia, C++              | Translate numeric code to Python or Julia with math libraries.            |
| REXX                | PP + DP               | Python, Bash                            | Use scripting languages with rich standard libraries.                     |
| LISP                | FP + PP               | Clojure, Haskell, Elixir                | Leverage immutable data and higher-order functions.                       |
| Prolog              | LP + DP               | Datalog, Drools, PyDatalog              | Map rules to a rule engine or inference system.                           |


### ✅ Strategy Types:

* **Rehost**: Move to a modern runtime (e.g., COBOL on JVM or container)
* **Refactor**: Rewrite code to match new paradigms or architectural goals
* **Replatform**: Shift to a newer tech stack but preserve core logic
* **Replace**: Swap with off-the-shelf tools or services
