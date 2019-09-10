        IDENTIFICATION DIVISION.
        PROGRAM-ID. "COBTOJAV" recursive.
        ENVIRONMENT DIVISION.
        Configuration section.
        Repository.
            Class CobolToJava
                    is "cobolToJava.CobolToJava".
        DATA DIVISION.

        WORKING-STORAGE SECTION.
        77  FILLER               PIC X(16) VALUE '*** BEGIN WS ***'.
        
        01 VM-ARGS-PTR USAGE POINTER.
        
        01 VM-INIT-ARGS.
          05  VERSION             PIC S9(9) BINARY VALUE 65538.
          05  NUMBER-OF-OPTIONS   PIC S9(9) BINARY.
          05  OPTIONS-PTR                          USAGE POINTER.
          05  FILLER              PIC  X(1).

        01 VM-OPTIONS.
          05  OPTIONS-STRING-PTR                   USAGE POINTER.
          05  EXTRA-INFO-PTR                       USAGE POINTER.
          
        01 RC2                     PIC S9(9) BINARY.

        01 JVM-PTR                 USAGE POINTER.
        01 ENV-PTR                 USAGE POINTER.
        
      * CLASSPATH Parameters
        01 CLASSPATH               PIC X(500).
       
        77  FILLER               PIC X(16) VALUE '*** END WS   ***'.

        LOCAL-STORAGE SECTION.
        01 class-ref 
           object reference CobolToJava value null.
        01 class-name             PICTURE X(50).
        01 len                    pic 9(9) binary.
        
        01 METHOD-NAME            PIC X(30).
        01 METHOD-NAME-PTR        USAGE POINTER.
        
        01 JAVA-METHOD-ID         PIC S9(9) BINARY.
        
        01 SIGNATURE-NAME         PIC X(30).
        01 SIGNATURE-NAME-PTR     USAGE POINTER.
       
        01  CUSTOMER-INFO.
           05  CUSTOMER-INFO-NAME  PIC x(6).
           05  CUSTOMER-INFO-JOB   PIC x(8).
           05  CUSTOMER-INFO-DATE  PIC x(9).
       
        01 CUSTOMER-INFO-PTR      USAGE POINTER.
        01 CUSTOMER-INFO-LEN      PIC  S9(18) COMP-5.
        01 CUSTOMER-INFO-BYTE-BUFFER-PTR USAGE POINTER.

        LINKAGE SECTION.
        
        COPY JNI.
        
        01 JVM PIC S9(9) BINARY.
        
        PROCEDURE DIVISION.
            DISPLAY "COBTOJAV: Entering".

            PERFORM JNI-LOOKUPS THRU JNI-LOOKUPS-END.
            
            PERFORM CALL-JAVA THRU CALL-JAVA-END.
            
            DISPLAY "COBTOJAV: Exiting".
            
            GOBACK.
        
      *   JNI-LOOKUPS will use Java Native Interface functions to:
      *   - launch the JVM
      *   - lookup and return the Java class reference
      *   - lookup and return the Java method within the above class
        JNI-LOOKUPS.
              Move z"-Djava.class.path=/tmp" to CLASSPATH.
              
              SET OPTIONS-STRING-PTR TO ADDRESS OF CLASSPATH.
              
              MOVE 1 TO NUMBER-OF-OPTIONS.
              
              SET OPTIONS-PTR TO ADDRESS OF VM-OPTIONS.
              
              SET VM-ARGS-PTR TO ADDRESS OF VM-INIT-ARGS.
              
              CALL "JNI_CreateJavaVM"
                   USING JVM-PTR ENV-PTR VM-INIT-ARGS
                   RETURNING RC2.

              DISPLAY "JNI_CreateJavaVM: " RC2.
        
              DISPLAY "COBTOJAV: Lookup Java class and method".
              
              Set address of JNIEnv to JNIEnvPtr.

              Set address of JNINativeInterface to JNIEnv.
                 
      *   Convert the fully qualified Java class name
      *   from EBCDIC to UTF-8 (ASCII) using __etoa function
              Move z"cobolToJava/CobolToJava" to class-name.
              Call "__etoa" using by value address of class-name
                          returning len.

      *   Issue JNI call FindClass to lookup and load the Java class
              Call FindClass using by value JNIEnvPtr
                      address of class-name returning class-ref.
                 
              If class-ref = null
                 DISPLAY "ERROR LOADING CLASS: " class-name
                 Goback
              End-if.
                 
      *   Convert Java method name from EBCDIC to ASCII
              Move z"runTest" to METHOD-NAME.
              Call "__etoa" using by value address of METHOD-NAME 
                            returning len.
                        
      *   Convert Java signature from EBCDIC to ASCII
              Move z"(Ljava/nio/ByteBuffer;)V" to SIGNATURE-NAME.
              Call "__etoa" using by value address of SIGNATURE-NAME 
                             returning len.

              SET METHOD-NAME-PTR TO ADDRESS OF METHOD-NAME.
              SET SIGNATURE-NAME-PTR TO ADDRESS OF SIGNATURE-NAME.

      *   Look up the reference to the runTest method within 
      *   the CobolToJava Java class.                  
              CALL GetStaticMethodId USING BY VALUE JNIEnvPtr
                                                 class-ref
                                                 METHOD-NAME-PTR
                                                 SIGNATURE-NAME-PTR
                                          RETURNING JAVA-METHOD-ID.
                            
              If JAVA-METHOD-ID = 0
                 Display "Error occurred while getting JAVA-METHOD-ID"
                 Stop run
              End-if. 

      *   Use the JNI function NewDirectByteBuffer
      *   to wrap the existing COBOL storage CUSTOMER-INFO
      *   into a Java ByteBuffer object that can share storage
      *   between Java and COBOL.
              COMPUTE CUSTOMER-INFO-LEN = LENGTH OF CUSTOMER-INFO.
              SET CUSTOMER-INFO-PTR TO ADDRESS OF CUSTOMER-INFO.
              
              Call NewDirectByteBuffer USING BY VALUE JNIEnvPtr
                                        CUSTOMER-INFO-PTR
                                        CUSTOMER-INFO-LEN
                             returning CUSTOMER-INFO-BYTE-BUFFER-PTR.
        JNI-LOOKUPS-END.

        CALL-JAVA.
              MOVE "KEVIN" TO CUSTOMER-INFO-NAME.
              MOVE "Engineer" TO CUSTOMER-INFO-JOB.
              MOVE "02/12/17" TO CUSTOMER-INFO-DATE.
              
              DISPLAY "COBOL>> CUSTOMER-INFO values "
                                        "before calling Java".
              DISPLAY "COBOL>> CUSTOMER-INFO-NAME : " 
                                        CUSTOMER-INFO-NAME.
              DISPLAY "COBOL>> CUSTOMER-INFO-JOB  : " 
                                        CUSTOMER-INFO-JOB.
              DISPLAY "COBOL>> CUSTOMER-INFO-DATE : " 
                                        CUSTOMER-INFO-DATE.
              
      *   Use the JNI function CallStaticVoidMethod to call the
      *   Java runTest method in the CobolToJava class
              CALL CallStaticVoidMethod using 
                               by value JNIEnvPtr
                               by value class-ref
                               by value JAVA-METHOD-ID
                               by value CUSTOMER-INFO-BYTE-BUFFER-PTR.
                                             
              DISPLAY "COBOL>> CUSTOMER-INFO values "
                                        "after calling Java".
              DISPLAY "COBOL>> CUSTOMER-INFO-NAME : " 
                                        CUSTOMER-INFO-NAME.
              DISPLAY "COBOL>> CUSTOMER-INFO-JOB  : " 
                                        CUSTOMER-INFO-JOB.
              DISPLAY "COBOL>> CUSTOMER-INFO-DATE : " 
                                        CUSTOMER-INFO-DATE.
        CALL-JAVA-END.               
            EXIT.