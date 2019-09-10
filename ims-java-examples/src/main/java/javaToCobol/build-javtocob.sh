#!/bin/bash

# This example was only tested with with 31-bit JVM and 31-bit COBOL
# Directory structure for this example:
# /tmp
#	- build-javtocob.sh
#	- cob1.cbl
#	- wrapper.cbl
# 	/javaToCobol
#		- BufferTest.java	
# 

# Export paths
export WORK_DIR=/tmp
# Tested with Java 8 and up, 31-bit
export JAVA_HOME=/usr/lpp/java/java180/J8.0
# Tested with COBOL V5R2 and V6R2 and up, 31-bit
export COBOL_INSTALL_DIR=/usr/lpp/cobol/cob620/igyv6r2
export STEPLIB=IGYV6R20.SIGYCOMP
export LIBPATH=${JAVA_HOME}/bin/j9vm:${JAVA_HOME}/bin:${WORK_DIR}

# Compile and link edit cob1.cbl program
"${COBOL_INSTALL_DIR}"/bin/cob2 -comprc_ok=4 -I "${COBOL_INSTALL_DIR}"/include cob1.cbl

# Compile wrapper.cbl and create the DLL (libWarpper.so)
"${COBOL_INSTALL_DIR}"/bin/cob2 -comprc_ok=4 -v -bdll -qdll,thread,list -o libWrapper.so cob1.o wrapper.o "${COBOL_INSTALL_DIR}"/lib/igzcjava.x -I "${COBOL_INSTALL_DIR}"/include wrapper.cbl

#Compile Java class: BufferTest.java
"${JAVA_HOME}"/bin/javac ${WORK_DIR}/javaToCobol/BufferTest.java

# Run the main Java program in javaToCobol.BufferTest
"${JAVA_HOME}"/bin/java -cp ${WORK_DIR} javaToCobol.BufferTest

# After execution you should see the following output when listing the directory
# ls *                                                         
# a.out             cob1.lst          libWrapper.x      wrapper.o
# build-javtocob.sh cob1.o            wrapper.cbl                
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