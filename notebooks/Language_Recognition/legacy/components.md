# Modern Codebase Components

A modern codebase is typically organized in a hierarchical structure, with various components at different levels of abstraction:

## 1. Codebase
- The entire collection of source code for a project
- Contains all files, configurations, and resources
- Usually managed by a version control system (e.g., Git)

## 2. Package
- A top-level container for related code
- Often represents a complete feature or library
- Examples: `com.example.project`, `tensorflow`, `numpy`

## 3. Sub-package
- Subdivisions within a package
- Groups related functionality
- Example: `tensorflow.keras`, `numpy.random`

## 4. Module
- Individual source files that group related code
- Can be imported and reused
- Usually corresponds to a single file
- Example: `utils.py`, `constants.js`

## 5. File
- Physical source code files
- Contains actual implementation
- Extensions indicate language: `.py`, `.java`, `.js`, etc.

## 6. Class
- Blueprint for creating objects
- Encapsulates data and behavior
- Contains methods and attributes
```python
class User:
    def __init__(self, name):
        self.name = name
```

## 7. Function/Method
- Reusable blocks of code
- Performs specific tasks
- Can be part of a class or standalone
```python
def calculate_total(items):
    return sum(items)
```

## 8. Variable
- Named storage for data
- Different scopes (global, local, instance)
```python
USER_COUNT = 0  # Global variable
self.name = "John"  # Instance variable
```

## 9. Statement
- Individual instructions
- Basic unit of execution
```python
x = 5  # Assignment statement
if x > 0:  # Conditional statement
    print(x)
```

## 10. Interface/Protocol
- Defines a contract for implementing classes
- Specifies required methods and properties
```python
from abc import ABC, abstractmethod

class DataSource(ABC):
    @abstractmethod
    def fetch_data(self):
        pass
```

## 11. Decorator
- Modifies or enhances function/class behavior
- Enables aspect-oriented programming
```python
def log_execution(func):
    def wrapper(*args, **kwargs):
        print(f"Executing {func.__name__}")
        return func(*args, **kwargs)
    return wrapper
```

## 12. Annotation/Type Hint
- Provides metadata about code elements
- Enables static type checking
```python
from typing import List, Dict

def process_items(items: List[str]) -> Dict[str, int]:
    return {item: len(item) for item in items}
```

## 13. Expression
- Combinations of values and operators
- Evaluates to a single value
```python
result = (x + y) * 2  # Arithmetic expression
condition = a > b and c  # Logical expression
```

## 14. Block
- Group of statements
- Defines scope and structure
```python
if condition:  # Block start
    statement1
    statement2
    if nested:  # Nested block
        statement3
```

## 15. Property/Accessor
- Controls access to class attributes
- Implements getters/setters
```python
class Circle:
    def __init__(self, radius):
        self._radius = radius
    
    @property
    def area(self) -> float:
        return 3.14 * self._radius ** 2
```

## 16. Event/Signal
- Enables communication between components
- Implements observer pattern
```python
class Button:
    def __init__(self):
        self.click_handlers = []
    
    def on_click(self, handler):
        self.click_handlers.append(handler)
```

## 17. Generic/Template
- Enables type-parameterized code
- Promotes code reuse with type safety
```python
from typing import TypeVar, Generic

T = TypeVar('T')

class Stack(Generic[T]):
    def __init__(self):
        self.items: List[T] = []
    
    def push(self, item: T) -> None:
        self.items.append(item)
```

## 18. Namespace
- Organizes code into logical groups
- Prevents naming conflicts
```python
class Math:
    PI = 3.14159
    
    @staticmethod
    def square(x):
        return x * x

# Usage: Math.PI, Math.square(5)
```

## 19. Exception/Error
- Handles error conditions
- Enables structured error handling
```python
class ValidationError(Exception):
    def __init__(self, message):
        self.message = message

try:
    raise ValidationError("Invalid input")
except ValidationError as e:
    print(e.message)
```

## 20. Enum
- Defines a set of named constants
- Provides type safety for options
```python
from enum import Enum

class Status(Enum):
    PENDING = 'pending'
    ACTIVE = 'active'
    COMPLETED = 'completed'
```

## Component Relationships
```
Codebase
└── Package
    └── Sub-package
        └── Module
            └── Namespace
                ├── Interface
                ├── Decorator
                ├── Generic
                ├── Enum
                ├── Exception
                ├── Class
                │   ├── Property
                │   ├── Method
                │   ├── Event
                │   └── Attribute
                └── Function
                    ├── Annotation
                    ├── Block
                    ├── Expression
                    ├── Variable
                    └── Statement
```

This hierarchy helps in:
- Code organization and structure
- Maintainability and readability
- Reusability and modularity
- Type safety and validation
- Event-driven architecture
- Error handling and recovery
- Generic programming
- Namespace management
- Dependency management
- Version control
- Testing and debugging
- Domain modeling

# Legacy Codebase Components

Mainframe systems have a unique architecture and component hierarchy that differs from modern distributed systems. This document outlines the key components of mainframe codebases, particularly focusing on IBM z/OS environments.

## 1. System
- The entire mainframe environment
- Includes hardware, operating system, and applications
- Examples: IBM z/OS, z/VSE, z/TPF

## 2. Subsystem
- Major software components that provide specific functionality
- Run as separate address spaces
- Examples: CICS, IMS, DB2, MQ, JES2/JES3

## 3. Library
- Collection of related programs and resources
- Organized in a hierarchical structure
- Examples: SYS1.LINKLIB, USER.LOADLIB, APP.PROCLIB

## 4. Dataset
- Physical storage unit for programs and data
- Various types: sequential, partitioned, VSAM
- Examples: USER.COBOL.SOURCE, PROD.PAYROLL.DATA

## 5. Member
- Individual component within a partitioned dataset
- Similar to files in modern systems
- Examples: PAYROLL in USER.COBOL.SOURCE(PAYROLL)

## 6. JCL (Job Control Language)
- Scripts that define job execution parameters
- Controls program execution flow
```jcl
//PAYROLL  JOB (ACCT),'MONTHLY PAYROLL',CLASS=A
//STEP1    EXEC PGM=PAYROLL
//SYSOUT   DD SYSOUT=*
//INPUT    DD DSN=PROD.PAYROLL.DATA,DISP=SHR
```

## 7. Program
- Executable unit of code
- Compiled from source code
- Examples: COBOL programs, Assembler routines

## 8. Procedure Division (COBOL)
- Contains the executable statements
- Equivalent to functions in modern languages
```cobol
PROCEDURE DIVISION.
    PERFORM 100-INITIALIZE.
    PERFORM 200-PROCESS-DATA UNTIL EOF.
    PERFORM 900-CLEANUP.
    STOP RUN.
```

## 9. Paragraph (COBOL)
- Named group of statements
- Similar to functions/methods
```cobol
100-INITIALIZE.
    OPEN INPUT EMPLOYEE-FILE.
    OPEN OUTPUT REPORT-FILE.
    READ EMPLOYEE-FILE
        AT END MOVE 'Y' TO EOF-FLAG
    END-READ.
```

## 10. Data Division (COBOL)
- Defines data structures and variables
- Includes file descriptions and working storage
```cobol
DATA DIVISION.
WORKING-STORAGE SECTION.
01 EMPLOYEE-RECORD.
   05 EMPLOYEE-ID       PIC 9(5).
   05 EMPLOYEE-NAME     PIC X(30).
   05 EMPLOYEE-SALARY   PIC 9(7)V99.
```

## 11. Copybook
- Reusable code snippets included in programs
- Similar to header files or imports
```cobol
      * EMPRECRD.cpy
       01 EMPLOYEE-RECORD.
          05 EMPLOYEE-ID       PIC 9(5).
          05 EMPLOYEE-NAME     PIC X(30).
          05 EMPLOYEE-SALARY   PIC 9(7)V99.
```

## 12. Transaction
- Unit of work in transaction processing systems
- Typically in CICS or IMS environments
```cobol
EXEC CICS
    READ DATASET('EMPLOYEE')
    INTO(EMPLOYEE-RECORD)
    RIDFLD(EMPLOYEE-ID)
    RESP(RESPONSE-CODE)
END-EXEC.
```

## 13. Macro (Assembler)
- Code generation templates
- Expanded at assembly time
```assembly
EMPAREA  DSECT
EMPID    DS    CL5
EMPNAME  DS    CL30
EMPSALARY DS   PL5
```

## 14. Control Block
- System data structures
- Manage resources and control program execution
- Examples: TCB (Task Control Block), PSW (Program Status Word)

## 15. REXX Procedure
- Interpreted scripts for automation
- Used for system administration tasks
```rexx
/* List datasets */
address tso
"LISTCAT LEVEL('USER')"
say 'Datasets listed successfully'
```

## 16. CLIST
- Command List procedures
- Used for automating TSO commands
```clist
PROC 0
CONTROL NOLIST NOMSG
LISTDS 'USER.*' MEMBERS
EXIT
```

## 17. Batch Job
- Non-interactive program execution
- Scheduled and executed as a unit
- Defined by JCL

## 18. Online Program
- Interactive applications
- Run under subsystems like CICS or IMS
- Respond to user input

## 19. Panel (ISPF)
- User interface screens
- Defined in panel definition language
```panel
)ATTR
  @ TYPE(INPUT) INTENS(HIGH)
)BODY
%--------------------------- EMPLOYEE INQUIRY ---------------------------
%COMMAND ===>_ZCMD                                                      
+
+ EMPLOYEE ID: @EMPID    +
+
+ PRESS ENTER TO SEARCH OR PF3 TO EXIT
)END
```

## 20. DB2 Object
- Database components
- Tables, views, stored procedures
```sql
CREATE TABLE EMPLOYEE (
    EMPID CHAR(5) NOT NULL,
    EMPNAME VARCHAR(30) NOT NULL,
    SALARY DECIMAL(9,2),
    PRIMARY KEY (EMPID)
);
```

## Detailed Mainframe Programming Language Components

### COBOL Components

#### 1. Program Structure
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       PROCEDURE DIVISION.
```
- **Identification Division**: Program metadata
- **Environment Division**: File and hardware specifications
- **Data Division**: Data definitions
- **Procedure Division**: Program logic

#### 2. Data Items
```cobol
       01  EMPLOYEE-RECORD.
           05  EMP-ID              PIC 9(5).
           05  EMP-NAME.
               10  FIRST-NAME      PIC X(20).
               10  LAST-NAME       PIC X(20).
           05  SALARY              PIC 9(7)V99.
           05  DEPARTMENT          PIC X(15).
```
- **Level Numbers** (01, 05, etc.): Define hierarchy
- **Picture Clauses** (PIC): Define data type and size
- **Group Items**: Contain multiple elementary items
- **Elementary Items**: Basic data items

#### 3. File Definitions
```cobol
       FD  EMPLOYEE-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01  EMPLOYEE-RECORD.
           COPY EMPRECRD.
```
- **FD**: File Description
- **Label Records**: File label specification
- **Block Contains**: Block size specification
- **Recording Mode**: Record format (F, V, U)

#### 4. Verbs and Statements
```cobol
       MOVE EMP-NAME TO OUTPUT-NAME
       IF SALARY > 50000
          PERFORM CALC-BONUS
          ADD BONUS TO TOTAL-BONUS
       END-IF
```
- **Procedural Verbs**: MOVE, ADD, SUBTRACT, etc.
- **Conditional Statements**: IF, EVALUATE, etc.
- **Iteration Statements**: PERFORM

### Copybook Components

#### 1. Data Structure Copybooks
```cobol
      * EMPRECRD.cpy
       01  EMPLOYEE-RECORD.
           05  EMP-ID              PIC 9(5).
           05  EMP-NAME.
               10  FIRST-NAME      PIC X(20).
               10  LAST-NAME       PIC X(20).
```
- Reusable data definitions
- Standardized record layouts
- Shared across programs

#### 2. Procedure Copybooks
```cobol
      * CALCPROC.cpy
           COMPUTE GROSS-PAY = HOURS * RATE
           COMPUTE TAX = GROSS-PAY * TAX-RATE
           COMPUTE NET-PAY = GROSS-PAY - TAX
```
- Reusable procedures
- Common calculations
- Standard routines

### JCL Components

#### 1. JOB Statement
```jcl
//PAYROLL  JOB (ACCT,'DEPT'),'JOB NAME',
//         CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//         NOTIFY=&SYSUID
```
- Job identification
- Execution parameters
- Resource requirements

#### 2. EXEC Statement
```jcl
//STEP1    EXEC PGM=PAYROLL,
//         REGION=0M,
//         PARM='MONTHLY,DEPT=SALES'
```
- Program or procedure execution
- Memory allocation
- Program parameters

#### 3. DD Statement
```jcl
//INFILE   DD DSN=PROD.PAYROLL.DATA,
//            DISP=SHR
//OUTFILE  DD DSN=PROD.PAYROLL.REPORT,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(10,5))
```
- Dataset specifications
- Resource allocation
- I/O definitions

#### 4. PROC Statement
```jcl
//PAYPROC  PROC DEPT=ALL
//STEP1    EXEC PGM=PAYROLL
//INFILE   DD DSN=PROD.&DEPT..DATA
```
- Reusable JCL procedures
- Parameterized jobs
- Standard job steps

### PL/I Components

#### 1. Program Structure
```pli
PAYROLL: PROCEDURE OPTIONS(MAIN);
  DECLARE statements...
  Processing statements...
END PAYROLL;
```
- **Procedure Block**: Main program container
- **Options**: Program attributes
- **Declarations**: Data definitions

#### 2. Data Declarations
```pli
DECLARE
  1 EMPLOYEE,
    2 ID CHAR(5),
    2 NAME,
      3 FIRST CHAR(20),
      3 LAST CHAR(20),
    2 SALARY FIXED DEC(7,2);
```
- **Structure Levels**: Data hierarchy
- **Data Types**: Built-in and user-defined
- **Attributes**: Data characteristics

#### 3. File Declarations
```pli
DECLARE EMPLOYEE_FILE FILE RECORD
        INPUT SEQUENTIAL
        ENV(FB RECSIZE(80));
```
- File attributes
- Record format
- Access method

#### 4. Control Structures
```pli
DO I = 1 TO EMPLOYEE_COUNT;
  IF SALARY(I) > 50000 THEN
    CALL CALC_BONUS(I);
END;
```
- **DO Groups**: Iteration control
- **IF Statements**: Conditional logic
- **CALL Statements**: Procedure invocation

### Common Components Across Languages

#### 1. Comments
```cobol
      * This is a COBOL comment
```
```pli
/* This is a PL/I comment */
```
```jcl
//* This is a JCL comment
```

#### 2. Constants
```cobol
       77  MAX-EMPLOYEES    PIC 9(4)  VALUE 1000.
```
```pli
DECLARE MAX_EMPLOYEES FIXED BIN(31) INIT(1000);
```

#### 3. Error Handling
```cobol
       EVALUATE RETURN-CODE
           WHEN 0  PERFORM NORMAL-PROCESSING
           WHEN 4  PERFORM WARNING-ROUTINE
           WHEN 8  PERFORM ERROR-ROUTINE
       END-EVALUATE
```
```pli
ON ERROR BEGIN;
  /* Error handling */
END;
```

#### 4. File Operations
- Sequential
- Indexed
- Relative
- VSAM

Understanding these language components helps in:
- Code reusability
- Standardization
- Maintainability
- Error handling
- Resource management
- Job control
- Data management
- Program modularity


## Component Relationships
```
System
└── Subsystem
    ├── Batch Job
    │   └── Defined by: JCL (Job Control Language - typically a Member)
    │   └── Executes: Program (typically a Member)
    │       ├── (Internal Program Structure: e.g., COBOL Divisions, Paragraphs)
    │       └── Uses: Copybook (Member), Macro (Member for Assembler)
    ├── Online Program
    │   ├── Is a: Program (typically a Member)
    │   ├── Associated with: Transaction (e.g., CICS Transaction ID, IMS Tran Code)
    │   └── Uses: Panel (Member - for screen definitions), Copybook (Member)
    ├── Library (e.g., PDS/PDSE for source/load modules, JCL libraries, Copybook libraries)
    │   └── Dataset
    │       └── Member (Storage for: Program source/load modules, JCL, Copybooks, Macros, REXX, CLISTs, Panels)
    ├── REXX Procedure (typically a Member - for automation scripts)
    ├── CLIST (typically a Member - for TSO command procedures)
    ├── DB2 Object (e.g., Table, View, Stored Procedure - managed by DB2 Subsystem)
    └── Control Block (System data structure for managing tasks and resources)
```

This hierarchy helps in:
- System organization
- Job scheduling and execution
- Transaction processing
- Data management
- Resource allocation
- Security implementation
- Batch and online processing
- System automation
- Performance optimization
- Disaster recovery
