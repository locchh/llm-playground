# Mainframe Programming Languages

| No. | Technology        | Full Name                                      | Usage                                                                                                                                                       | Online Reference Document                                                                                                                                                              | Modern Analog                                       |
| :-- | :---------------- | :--------------------------------------------- | :---------------------------------------------------------------------------------------------------------------------------------------------------------- | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | :-------------------------------------------------- |
| 1 | COBOL             | Common Business-Oriented Language              | Business data processing, enterprise applications, mission-critical systems on mainframes.                                                                  | [Enterprise COBOL for z/OS documentation library](https://www.ibm.com/support/pages/enterprise-cobol-zos-documentation-library)                                                   | Java, C#, Python (enterprise backend)             |
| 2 | PL/I              | Programming Language One                       | General-purpose (scientific, business, system programming), structured programming on mainframes.                                                         | [Enterprise PL/I for z/OS documentation library](https://www.ibm.com/support/pages/enterprise-pli-zos-documentation-library)                                                     | C++, Java, Python                                 |
| 3 | CopyBook          | (COBOL) Copybook                               | Defines reusable data structures or sections of code for COBOL programs, promoting standardization and modularity.                                          | [The COBOL copybook - IBM](https://www.ibm.com/docs/en/odm/8.0.1?topic=generation-cobol-copybook)                                                                         | DTOs, POJOs, JSON/XML Schemas, Protobuf           |
| 4 | JCL               | Job Control Language                           | Scripting language to instruct z/OS on how to run batch jobs or start tasks, defining resources and execution flow.                                         | [z/OS MVS JCL Reference (PDF)](https://publibz.boulder.ibm.com/epubs/pdf/iea1b630.pdf)                                                                                   | Shell scripts, Python, Airflow, Kubernetes Jobs   |
| 5 | WorkFlow          | Work Flow Language                             | WFL is used for constructing jobs that compile or run programs. WFL is a true programming language with its own compiler that either produces an object code file used in running a job or executes a job interpretively. A WFL job is always recompiled each time it is run. | [Work Flow Language (WFL) - Unisys](https://public.support.unisys.com/aseries/docs/ClearPath-MCP-21.0/86001047-518/section-000025364.html)                                        | Shell scripts, Python, Build tools, Workflow engines |
| 6 | CLIST             | Command List                                   | High-level interpretive scripting language to automate TSO/E commands and perform programming tasks.                                                      | [CLIST programming language - IBM](https://www.ibm.com/docs/en/zos/2.1.0?topic=programming-clist-language)                                                               | Python, Perl, Shell scripting                     |
| 7 | Algol             | Algorithmic Language                           | Primarily scientific and algorithmic computations. Historically used on some mainframes (e.g., Burroughs/Unisys).                                           | [ALGOL Programming Reference Manual - Unisys (PDF)](https://public.support.unisys.com/aseries/docs/ClearPath-MCP-21.0/86000098-519/86000098-519.pdf)                       | Python (NumPy/SciPy), Julia, R, C++               |
| 8 | Assembler         | High Level Assembler (HLASM)                   | Low-level system programming, performance-critical sections of code, direct hardware manipulation.                                                      | [HLASM Language Reference (PDF)](https://publibz.boulder.ibm.com/epubs/pdf/asmr1020.pdf)                                                                                 | C, C++, Rust, WebAssembly                         |
| 9 | VSAM              | Virtual Storage Access Method                  | A high-performance data access method for processing fixed-length and variable-length records on DASD; supports indexed, sequential, and relative organization. | [VSAM Demystified - IBM Redbooks](https://www.redbooks.ibm.com/abstracts/sg246105.html)                                                                                  | NoSQL (Redis, MongoDB), Indexed file systems, SQLite |
| 10 | DASDL             | Data And Structure Definition Language         | High-level language for defining physical and logical characteristics of Unisys DMSII databases.                                                            | [Enterprise Database Server DASDL Programming Reference Manual - Unisys](https://public.support.unisys.com/aseries/docs/ClearPath-MCP-20.0/86000213-422/index.html) | SQL DDL, ORM schemas, Graph DBs                 |
| 11 | DDL               | Data Definition Language                       | Subset of SQL used to define, modify, or drop database objects like tables, views, and indexes in mainframe database systems (e.g., Db2 for z/OS).         | [Db2 12 - SQL statements in Db2 for z/OS - IBM](https://www.ibm.com/docs/en/db2-for-zos/12.0.0?topic=sql-statements)                                                      | SQL DDL, ORM migrations (Alembic, Flyway)         |
| 12 | BMS               | Basic Mapping Support                          | An API within CICS for defining and formatting screens for 3270-type display terminals, separating device logic from application logic.                       | [Component reference: Basic mapping support - IBM](https://www.ibm.com/docs/en/cics-ts/6.x?topic=components-basic-mapping-support)                                         | HTML/CSS/JS, UI Frameworks (React, Angular)       |
| 13 | ISPF              | Interactive System Productivity Facility       | Menu-driven, full-screen interface and development environment for z/OS; used for editing, job submission, browsing output, etc.                            | [Interactive System Productivity Facility (ISPF) - IBM](https://www.ibm.com/docs/en/zos/2.4.0?topic=zos-interactive-system-productivity-facility-ispf)                     | IDEs (VS Code, IntelliJ), CLIs                    |
| 14 | LDL+              | Logic Definition Language Plus     |   | [Logic Definition Language Plus (LDL+) - Unisys](https://public.support.unisys.com/ABSuiteIC-7.0/index.jsp?topic=%2Feae_to_ab_suite_migration_reference%2Fhtml%2Fsection-000053769.htm)                                                                                     | Low-Code/No-Code, Model-Driven Dev, BPM tools     |
| 15 | OGL               | Overlay Generation Language                    | Used to create, store, and manage overlays for printing with form definitions on IBM mainframes (z/OS, VM, VSE).                                          | [Overlay Generation Language (OGL) - IBM](https://www.ibm.com/docs/en/zos/2.1.0?topic=products-overlay-generation-language-ogl)                                           | Reporting tools, PDF libraries, Templating engines |
| 16 | Easytrieve        | CA Easytrieve Report Generator                 | A report generation and data retrieval utility language, designed for quick and simple creation of reports from mainframe data sources.                     | [CA Easytrieve Language Reference - Broadcom](https://techdocs.broadcom.com/us/en/ca-mainframe-software/devops/ca-easytrieve-report-generator/11-6/language-reference.html)     | SQL, Python (Pandas), R, BI Tools (Tableau)     |

Here’s an diagram of a **mainframe system architecture**, showing where each language fits into the system based on its usage. The layout is structured in **layers**, from low-level system code to business logic, job control, user interfaces, and data access.

```
                                    +-------------------------------------+
                                    |         USER INTERFACES            |
                                    |-------------------------------------|
                                    | ISPF  |  BMS  | Easytrieve |  OGL  |
                                    |-------|-------|------------|-------|
                                    | IDE   | 3270  | Reports    | Forms |
                                    +-------------------------------------+

                    +-----------------------------------------------------------+
                    |               JOB & WORKFLOW MANAGEMENT                   |
                    |-----------------------------------------------------------|
                    |         JCL         |      CLIST       |      WFL         |
                    |---------------------|------------------|------------------|
                    | Batch Jobs          | TSO Scripts      | MCP Workflows    |
                    +-----------------------------------------------------------+

+---------------------------------------------------------------------------------------+
|                                  BUSINESS & APP LOGIC                                 |
|----------------------------------------------------------------------------------------|
|    COBOL    |     PL/I     |    LDL+     |    Easytrieve    |        Algol           |
|-------------|--------------|-------------|------------------|-------------------------|
| Core logic  | Business +   | Model-Driven| Report/Query Lang| Scientific apps         |
|             | scientific   | Logic Layer |                  |                         |
+---------------------------------------------------------------------------------------+

          +----------------------------------------------------------------+
          |                    REUSABLE STRUCTURES & DEFINITIONS          |
          |----------------------------------------------------------------|
          |          CopyBook         |       DASDL       |      DDL       |
          |---------------------------|-------------------|----------------|
          | COBOL Data Templates      | DMSII Schema Defs | SQL Schema     |
          +----------------------------------------------------------------+

                            +-------------------------------------+
                            |         SYSTEM-LEVEL CODE           |
                            |-------------------------------------|
                            |       Assembler (HLASM)            |
                            |-------------------------------------|
                            | OS services, optimization, drivers |
                            +-------------------------------------+

                          +---------------------------------------+
                          |     DATA STORAGE / ACCESS METHODS     |
                          |---------------------------------------|
                          |       VSAM        |       DB2         |
                          |-------------------|-------------------|
                          | Indexed Files     | SQL-based RDBMS   |
                          +---------------------------------------+
```

| Layer                    | Purpose                                     | Technologies                         |
| ------------------------ | ------------------------------------------- | ------------------------------------ |
| **User Interfaces**      | Tools for users to interact with the system | ISPF, BMS, Easytrieve, OGL           |
| **Job & Workflow**       | Batch execution and job control             | JCL, CLIST, WFL                      |
| **Business & App Logic** | Core business processing logic              | COBOL, PL/I, LDL+, Easytrieve, Algol |
| **Reusable Definitions** | Reusable data/structure definitions         | CopyBook, DASDL, DDL                 |
| **System Code**          | Hardware-level, OS integration              | Assembler                            |
| **Data Access**          | Storage and database access                 | VSAM, DB2                            |
