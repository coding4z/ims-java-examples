Process pgmname(longmixed),dll,thread
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "Java_javaToCobol_BufferTest_callCob1"
                                           is recursive.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       LINKAGE SECTION.
       
       01 BB  PIC S9(9) BINARY.
       01 ENV-PTR   USAGE POINTER.
       01 OBJECT-REF  PIC S9(9) BINARY.
       01 JAVA-BUFFER-ADDR USAGE POINTER.
       
       COPY JNI SUPPRESS.
       
       PROCEDURE DIVISION USING BY   VALUE ENV-PTR
                                     OBJECT-REF
                                     BB
                           RETURNING JAVA-BUFFER-ADDR.
           
           Set address of JNIEnv to ENV-PTR
           Set address of JNINativeInterface to JNIEnv
           
           Call GetDirectBufferAddress using by value ENV-PTR BB
              returning JAVA-BUFFER-ADDR
        

           Call "COB1" using JAVA-BUFFER-ADDR
         

           GOBACK.