#!/bin/bash

# This example was only tested with with 31-bit JVM and 31-bit COBOL
# Directory structure for this example:
# /tmp
#	- build-cobtojav.sh
#	- COBTOJAV.cbl
# 	/cobolToJava
#		- CobolToJava.java
# 

# Export paths
export WORK_DIR=/tmp
# Tested with Java 8 and up, 31-bit
export JAVA_HOME=/usr/lpp/java/java180/J8.0
# Tested with COBOL V5R2 and V6R2 and up, 31-bit
export COBOL_INSTALL_DIR=/usr/lpp/cobol/cob620/igyv6r2
export STEPLIB=IGYV6R20.SIGYCOMP
export LIBPATH=${JAVA_HOME}/bin/j9vm:${JAVA_HOME}/bin:${WORK_DIR}
export _CEE_RUNOPTS=XPLINK\(ON\)

# Compile and link edit COBTOJAV.cbl program
"${COBOL_INSTALL_DIR}"/bin/cob2 COBTOJAV.cbl -comprc_ok=4 -v -qPGMNAME\(LONGMIXED\),DLL,RENT,THREAD,EXP -bDYNAM=DLL,CASE=MIXED -I"${COBOL_INSTALL_DIR}"/include -l"//'CEE.SCEELKED'" -l"//'CEE.SCEELKEX'" -o COBTOJAV "${COBOL_INSTALL_DIR}"/lib/igzcjava.x ${JAVA_HOME}/bin/j9vm/libjvm.x

# Compile Java class: CobolToJava.java
"${JAVA_HOME}"/bin/javac ${WORK_DIR}/cobolToJava/CobolToJava.java

# Execute the COBOL program COBTOJAV
${WORK_DIR}/COBTOJAV

# After execution you should see the following output when listing the directory
# ls *                                                         
# a.out             cob1.lst          libWrapper.x      wrapper.o
# build-cobtojav.sh  cob1.o            wrapper.cbl                
# cob1.cbl          libWrapper.so     wrapper.lst                
#                                                              
# javaToCobol:                                                   
# BufferTest.class  BufferTest.java                              

# Program output:
# JAVA:>> main(String argv) invoked. Building buffer and calling COBOL
# COBOL:>> JAVA Allocated Buffer with address: 0503536600             
# COBOL:>> CUSTOMER-INFO-NAME set by Java: KEVIN                      
# COBOL:>> CUSTOMER-INFO-JOB set by Java: Engineer                    
# COBOL:>> CUSTOMER-INFO-DATE set by Java: 02/12/17                   
# COBOL:>> Changing name from KEVIN  to DEEPAK                        
# JAVA:>> ADDRESS OF BYTEBUFFER SENT FROM COBOL: 503536600            
# JAVA:>> Java invoked COBOL to change NAME to: DEEPAK     