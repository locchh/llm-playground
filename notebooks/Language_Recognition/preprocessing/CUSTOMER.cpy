      *****************************************************************
      * Customer Record Layout
      *****************************************************************
       01  CUSTOMER-RECORD.
           05  CUSTOMER-ID           PIC 9(6).
           05  CUSTOMER-NAME         PIC X(30).
           05  CUSTOMER-ADDRESS      PIC X(50).
           05  CUSTOMER-PHONE        PIC X(15).
           05  CUSTOMER-EMAIL        PIC X(50).
           05  CUSTOMER-STATUS       PIC X(1).
               88  CUSTOMER-ACTIVE   VALUE 'A'.
               88  CUSTOMER-INACTIVE VALUE 'I'.
               88  CUSTOMER-PENDING  VALUE 'P'.
           05  CUSTOMER-JOIN-DATE    PIC X(10).
           05  CUSTOMER-LAST-ORDER   PIC X(10).
           
       01  CUSTOMER-COUNTS.
           05  CUSTOMER-COUNT        PIC 9(5) VALUE ZERO.
           05  ACTIVE-COUNT          PIC 9(5) VALUE ZERO.
           05  INACTIVE-COUNT        PIC 9(5) VALUE ZERO.
