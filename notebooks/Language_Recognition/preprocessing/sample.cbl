      *****************************************************************
      * Sample COBOL program to test the parser
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAMPLE.
       AUTHOR. CASCADE-AI.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. X86-64.
       OBJECT-COMPUTER. X86-64.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Include customer record layout
           COPY CUSTOMER.
           
       01  WS-VARIABLES.
           05  WS-RETURN-CODE     PIC S9(4) COMP VALUE ZERO.
           05  WS-CURRENT-DATE    PIC X(10) VALUE SPACES.
           05  WS-COUNTER         PIC 9(4)  VALUE ZERO.
           
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "COBOL Sample Program".
           PERFORM INITIALIZE-ROUTINE.
           PERFORM PROCESS-DATA.
           PERFORM CLEANUP-ROUTINE.
           STOP RUN.
           
       INITIALIZE-ROUTINE.
           MOVE FUNCTION CURRENT-DATE(1:10) TO WS-CURRENT-DATE.
           DISPLAY "Current Date: " WS-CURRENT-DATE.
           
           EXEC SQL
               CONNECT TO mydatabase USER :username USING :password
           END-EXEC.
           
       PROCESS-DATA.
           MOVE SPACES TO CUSTOMER-NAME.
           MOVE ZEROS TO CUSTOMER-ID.
           
           PERFORM VARYING WS-COUNTER FROM 1 BY 1
               UNTIL WS-COUNTER > 5
               DISPLAY "Processing record: " WS-COUNTER
               PERFORM GET-CUSTOMER-DATA
               ADD 1 TO CUSTOMER-COUNT
           END-PERFORM.
           
       GET-CUSTOMER-DATA.
           EXEC SQL
               SELECT customer_id, customer_name, customer_address
               INTO :CUSTOMER-ID, :CUSTOMER-NAME, :CUSTOMER-ADDRESS
               FROM customers
               WHERE customer_id = :WS-COUNTER
           END-EXEC.
           
           IF SQLCODE = 0
               DISPLAY "Customer found: " CUSTOMER-NAME
           ELSE
               DISPLAY "Customer not found for ID: " WS-COUNTER
           END-IF.
           
       CLEANUP-ROUTINE.
           EXEC SQL
               DISCONNECT
           END-EXEC.
           
           DISPLAY "Processing complete".
           MOVE ZERO TO RETURN-CODE.
