      *> Sample COBOL program with preprocessor directives
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEGACYAPP.
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. X8086.
       OBJECT-COMPUTER. X8086.
      
      *> Using COPY to include external code
       COPY "customer.cpy".
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CURRENT-DATE.
          05 WS-YEAR             PIC 9(4).
          05 WS-MONTH            PIC 9(2).
          05 WS-DAY              PIC 9(2).
      
      *> Using REPLACE to substitute text
       REPLACE ==CURRENT-DATE== BY ==FUNCTION CURRENT-DATE==.
      
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Legacy Application Started".
           
           MOVE CURRENT-DATE TO WS-CURRENT-DATE.
           DISPLAY "Current Date: " WS-YEAR "/" WS-MONTH "/" WS-DAY.
           
           PERFORM PROCESS-CUSTOMER.
           
           EXEC SQL
              SELECT customer_name, customer_id 
              FROM customers
              WHERE status = 'ACTIVE'
           END-EXEC.
           
           STOP RUN.
           
       PROCESS-CUSTOMER.
           DISPLAY "Processing customer: " CUSTOMER-NAME.
           DISPLAY "ID: " CUSTOMER-ID.
           DISPLAY "Status: " CUSTOMER-STATUS.
