       IDENTIFICATION DIVISION.
       PROGRAM-ID. "COB1" 

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.
       DATA DIVISION.
    
       WORKING-STORAGE SECTION.

       LINKAGE SECTION.
       01 PASSED-ADDRESS USAGE POINTER.
       
       01  CUSTOMER-INFO.
           05  CUSTOMER-INFO-NAME  PIC x(6).
           05  CUSTOMER-INFO-JOB   PIC x(8).
           05  CUSTOMER-INFO-DATE  PIC x(9).

       PROCEDURE DIVISION USING PASSED-ADDRESS.
           
           Display "COBOL:>> JAVA Allocated Buffer with address: "
                                                  PASSED-ADDRESS.

           Set address of CUSTOMER-INFO to PASSED-ADDRESS.

           Display "COBOL:>> CUSTOMER-INFO-NAME set by Java: " 
                                       CUSTOMER-INFO-NAME.
           Display "COBOL:>> CUSTOMER-INFO-JOB set by Java: " 
                                        CUSTOMER-INFO-JOB.
           Display "COBOL:>> CUSTOMER-INFO-DATE set by Java: " 
                                       CUSTOMER-INFO-DATE.

           Display "COBOL:>> Changing name from "
                                       CUSTOMER-INFO-NAME 
                                       " to DEEPAK".
           
           MOVE "DEEPAK" TO CUSTOMER-INFO-NAME.
           
           GOBACK.
