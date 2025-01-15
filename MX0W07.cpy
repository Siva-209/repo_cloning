      ******************************************************************
      *
      * COMMON WORK AREA FOR ADDITIONAL CUSTOMER NUMBER RETRIEVAL
      *
      ******************************************************************

      ******************************************************************

       01  MX0W07-CNTL-AREA.
           05  MX0W07-DLR-NO           PIC S9(09)   COMP.
           05  MX0W07-CNTL-ENT-NO      PIC S9(04)   COMP.

       01  MX0W07-WORK-AREA.
           05  MX0W07-STAT-CODE         PIC  X(01).
               88  MX0W07-NO-ERR                      VALUE ' '.
               88  MX0W07-DB2-ERR                     VALUE 'D'.
           05  MX0W07-CPU-ID            PIC  X(04).
           05  MX0W07-ALLIANCE-FLAG     PIC  X(01).
           05  MX0W07-ADDL-CUST-FLAG    PIC  X(01).
           05  MX0W07-CPU-DLR-NO        PIC  X(13).
           05  MX0W07-LANGUAGE-IND      PIC  X(01).
           05  MX0W07-BILL-FORM-NO      PIC S9(04)    COMP.
           05  MX0W07-PRT-CRED-LN-FLAG  PIC  X(01).
           05  MX0W07-PRT-CR-LN-MIN-AMT PIC S9(09)V99 COMP-3.
