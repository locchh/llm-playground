## Roles

| **#** | **Term**       | **Definition** | **Example Tools** | **Group** | **Can ANTLR Create It?** | **How?** |
|------|---------------|--------------|-------------------|-------------------|----------------|--------------------------------------------------------------|
| 1 | **Linter**    | Analyzes source code for stylistic and potential programming errors without executing it. | ESLint (JavaScript), pylint (Python), clang-tidy (C++) | Code Quality & Development Workflow | ✅ Yes  | Use ANTLR to parse code and define grammar-based rules for detecting violations. |
| 2 | **Formatter** | Automatically reformats code to follow a consistent style. | Prettier (JavaScript), Black (Python), clang-format (C++) | Code Quality & Development Workflow | ✅ Yes  | Parse the code, modify AST, and regenerate formatted output. |
| 3 | **Debugger**  | Helps developers inspect and debug code execution by setting breakpoints, stepping through code, and examining state. | gdb (C/C++), pdb (Python), LLDB, Visual Studio Debugger | Code Quality & Development Workflow | ❌ No  | Debuggers work at runtime, whereas ANTLR is for parsing. |
| 4 | **Lexer**     | Tokenizes source code by breaking it into meaningful symbols (tokens). | ANTLR Lexer, flex (C), `tokenize` (Python) | Compilation & Interpretation (Language Processing) | ✅ Yes  | ANTLR generates a lexer that tokenizes input based on defined grammar. |
| 5 | **Parser**    | Analyzes tokenized input to build a structured representation, typically an Abstract Syntax Tree (AST). | ANTLR Parser, Bison (C), `ast` module (Python) | Compilation & Interpretation (Language Processing) | ✅ Yes  | ANTLR generates a parser that constructs an AST from tokens. |
| 6 | **Analyzer**  | Performs deeper analysis of code structure, correctness, or semantics. | Clang Static Analyzer, mypy (Python type checker) | Compilation & Interpretation (Language Processing) | ✅ Yes  | Use ANTLR visitors/listeners to traverse AST and check for errors. |
| 7 | **Compiler**  | Transforms high-level source code into machine code or intermediate code. | GCC (C/C++), javac (Java), LLVM | Compilation & Interpretation (Language Processing) | ✅ Yes  | Parse source, analyze it, optimize, and generate code (e.g., LLVM IR, bytecode). |
| 8 | **Interpreter** | Executes code line by line without converting it to machine code beforehand. | CPython (Python), Node.js (JavaScript runtime) | Compilation & Interpretation (Language Processing) | ✅ Yes  | Use ANTLR to parse the code and execute it dynamically using visitors. |
| 9 | **Assembler** | Converts assembly language into machine code (binary). | NASM, GAS (GNU Assembler), MASM (Microsoft Assembler) | Compilation & Interpretation (Language Processing) | ❌ No  | ANTLR is not designed for converting assembly to machine code. |
| 10 | **Listener** | A tree traversal strategy where callbacks are triggered when entering/exiting nodes in a parse tree. | ANTLR Listener, Event Listeners in UIs | Tree Traversal & Processing | ✅ Yes  | ANTLR automatically generates listeners to respond to parse tree events. |
| 11 | **Visitor**  | A tree traversal strategy that explicitly calls visit methods on nodes, allowing flexible and controlled traversal. | ANTLR Visitor, Visitor Pattern in OOP | Tree Traversal & Processing | ✅ Yes  | ANTLR generates visitors for controlled traversal of the parse tree. |
| 12 | **Reader**    | Reads input data (e.g., files, streams) and provides structured access to it. | `csv.reader` (Python), `FileReader` (Java) | Code Transformation & Automation | ❌ No  | ANTLR processes text after reading but does not handle file I/O. |
| 13 | **Translator** | Converts code or data from one format or language to another. | Babel (JavaScript transpiler), Protobuf Compiler | Code Transformation & Automation | ✅ Yes  | Parse input language and generate equivalent code in another language. |
| 14 | **Generator** | Produces code or other structured output from some input (e.g., templates, models). | Code generation in ANTLR, Jinja (template engine), LLVM IR generator | Code Transformation & Automation | ✅ Yes  | Use ANTLR to transform parsed input into structured generated output. |


## Other Application Concepts

### **Processing & Execution**
- **Processor** – Handles data or tasks by applying transformations or computations.
- **Executor** – Manages and executes tasks, often in a controlled or parallelized environment.
- **Worker** – Performs background tasks or computations, often in a distributed or multi-threaded system.
- **Dispatcher** – Assigns or routes tasks to appropriate workers or subsystems.

### **Data Handling & Transformation**
- **Creator** – Generates or initializes new objects, resources, or data structures.
- **Collector** – Gathers and aggregates data, resources, or results for further processing.
- **Transformer** – Modifies data from one format or structure to another.
- **Validator** – Checks data for correctness, compliance, and consistency.
- **Normalizer** – Adjusts data to a standard format for uniformity.

### **Messaging & Communication**
- **Publisher** – Produces messages or events for subscribers to consume.
- **Subscriber** – Listens for messages or events from a publisher.
- **Router** – Directs messages or tasks to the correct destination.
- **Broker** – Acts as an intermediary to facilitate communication, message passing, or resource management.
- **Mediator** – Acts as an intermediary between components to reduce direct dependencies.
- **Consumer** – Processes and uses data produced by another system or producer.

### **Task & Process Management**
- **Scheduler** – Manages task execution timing, prioritization, and concurrency.
- **Orchestrator** – Coordinates multiple services or processes to ensure smooth workflow execution.
- **Supervisor** – Monitors and restarts failed processes or tasks (common in fault-tolerant systems).

### **Execution & Control Flow**
- **Controller** – Directs and manages the flow of execution in an application (common in MVC architecture).
- **Handler** – Responds to specific events, requests, or errors.
- **Executor** – Runs tasks, commands, or processes in a controlled manner.

### **Resource Management**
- **Allocator** – Manages allocation of resources like memory, threads, or connections.
- **Reaper** – Cleans up unused or expired resources to prevent leaks.
- **Cache Manager** – Handles storage and retrieval of frequently accessed data.

### **Security & Authentication**
- **Authenticator** – Verifies identities and credentials.
- **Authorizer** – Determines permissions and access control.
- **Encryptor** – Secures data using encryption techniques.
- **Auditor** – Tracks actions for compliance and security monitoring.

### **Automation & AI**
- **Predictor** – Uses historical data to forecast future events or trends.
- **Optimizer** – Improves efficiency of processes, algorithms, or resource usage.
- **Recommender** – Suggests relevant actions, content, or items based on input data.


## Grammar

- *You can parse source code by defining the grammar as a set of characters, numbers, delimiters, and spaces. This ensures 100% coverage, but it is meaningless.*

- *Recognizing characters, numbers, delimiters, and spaces ensures full coverage at the lexical level, but without syntax and semantic rules, it does not enable meaningful parsing.*

```
//  Header (Optional, for Imports & Configuration)

grammar MyLang;

@header {
    package my.antlr.parser;
}
// Root Rule (Entry Point of the Grammar)

program : statement+ EOF ;


// Parser Rules (High-Level Language Structure)

statement
    : ifStatement
    | assignment
    | loop
    | functionCall
    ;

ifStatement
    : 'if' condition 'then' block ('else' block)?
    ;

assignment
    : ID '=' expression ';'
    ;

loop
    : 'while' condition 'do' block
    ;

functionCall
    : ID '(' (expression (',' expression)*)? ')'
    ;

block
    : '{' statement* '}'
    ;


// Expressions (Mathematical & Logical Rules)

expression
    : term (('+' | '-') term)*
    ;

term
    : factor (('*' | '/') factor)*
    ;

factor
    : NUMBER
    | ID
    | '(' expression ')'
    ;


// Keywords
IF : 'if' ;
THEN : 'then' ;
ELSE : 'else' ;
WHILE : 'while' ;
DO : 'do' ;

// Operators & Symbols
PLUS  : '+' ;
MINUS : '-' ;
STAR  : '*' ;
SLASH : '/' ;
EQ    : '=' ;
LPAREN : '(' ;
RPAREN : ')' ;
LBRACE : '{' ;
RBRACE : '}' ;

// Identifiers & Literals
ID : [a-zA-Z_][a-zA-Z_0-9]* ;
NUMBER : [0-9]+ ;
STRING : '"' (~["])* '"' ;

// Fragments (Reusable Character Sets)
fragment DIGIT : [0-9] ;
fragment LETTER : [a-zA-Z] ;

// Special Handling (Whitespace, Comments, Error Handling)

WS : [ \t\r\n]+ -> skip ;
COMMENT : '//' ~[\r\n]* -> skip ;

err : . { System.err.println("Unexpected character: " + $text); } ;

```
[antlr4](https://github.com/antlr/antlr4)

[grammars-v4](https://github.com/antlr/grammars-v4)

[lark](https://github.com/lark-parser/lark)

[tree-sitter](https://github.com/tree-sitter/tree-sitter)
