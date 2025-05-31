      *> Customer data definition
       01 CUSTOMER-RECORD.
          05 CUSTOMER-ID          PIC 9(6).
          05 CUSTOMER-NAME        PIC X(30).
          05 CUSTOMER-ADDRESS.
             10 STREET            PIC X(30).
             10 CITY              PIC X(20).
             10 STATE             PIC X(2).
             10 ZIP-CODE          PIC 9(5).
          05 CUSTOMER-STATUS      PIC X(10).
