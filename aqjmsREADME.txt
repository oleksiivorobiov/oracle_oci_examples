/ $Header: aqjmsREADME.txt 05-jun-2007.15:02:54 aatam Exp $
/
/ aqjmsREADME.txt
/
/ Copyright (c) Oracle Corporation 2000. All Rights Reserved.
/
/   NAME
/     aqjmsREADME.txt - <one-line expansion of the name>
/
/   DESCRIPTION
/     <short description of component this file declares/defines>
/
/   NOTES
/     <other useful comments, qualifications, etc.>
/
/   MODIFIED   (MM/DD/YY)
/   aatam       06/05/07 - password need to be consistent
/   qialiu      02/02/07 - ojdbc5.jar
/   qialiu      04/07/06 - jta.jar is in /ade/qialiu_linux_db4/oracle/jlib 
/   qialiu      03/09/06 - move jdk1.4 
/   rbhyrava    09/17/04 - orai18n
/   qialiu      07/14/04 - update jta.jar location 
/   jleinawe    10/28/03 - update for 10g 
/   jleinawe    05/09/03 - update compatibility
/   jleinawe    03/11/03 - add aqjmsdemo10
/   jleinawe    12/20/02 - update classpath info
/   jleinawe    11/19/02 - add aqjmsdemo09
/   vmaganty    10/04/02 - add JMS 1.1 domain unification demo info
/   jleinawe    09/10/02 - add aqjmsdemo07 and kprb
/   jleinawe    05/14/02 - udpate nls_charset<xx>.zip
/   rbhyrava    03/01/02 - ORAData
/   rbhyrava    07/16/01 - port specific
/   rbhyrava    06/19/01 - jndi location
/   rbhyrava    03/12/01 - jndi.jar
/   rbhyrava    03/06/01 - jndi
/   rbhyrava    01/26/01 - classes111
/   rbhyrava    11/14/00 - jdbc path
/   rbhyrava    07/10/00 - fix bug 1319922
/   rbhyrava    07/11/00 - aqjmsdemo.tar
/   rbhyrava    04/14/00 - demo drop
/   rbhyrava    03/20/00 - required init.ora setup
/   rbhyrava    03/15/00 - AQ JMS demo README
/   rbhyrava    03/15/00 - Creation
/
The following files are required for running JMS samples 

 aqjmsdmo.sql     - Setup file for AQ JMS demos
 aqjmsdemo01.java - Enqueue Text Message and Dequeue based on Message Properties
 aqjmsdemo02.java - Message Listener demo- enqueue messages - run aqjmsdemo04 
 aqjmsdemo03.java - depends on aqjmsdemo03 - setup Message Listener and dequeue
 aqjmsdemo04.java - Oracle Type Payload - Dequeue on Payload content 
 aqjmsdemo05.java - Queue Browser Example
 aqjmsdemo06.java - Schedule Propagation between queues in the database
 aqjmsdemo07.java - Send and receive an ADT message containing XML data.
 aqjmsdemo08.java - JMS 1.1 domain unification demo
 aqjmsdemo09.java - JMS Bulk Array Enqueue/Dequeue
 aqjmsdemo10.java - ANYDATA Messaging with JMS Message types and AdtMessage.
 Cars.java        - Jpublisher generated class ; used in aqjmsdemo04.java 
 Emp.java         - Jpublisher generated class ; used in aqjmsdemo04.java 
 MesgListener.java - Message Listener - used in aqjmsdemo03.java 
 Message.java     - Definition of Serializable Object - aqjmsdemo06.java 
 aqjmsdrp.sql     - Cleanup for  AQ JMS demos

The following files are required for running AQ Java API samples 

 aqoradmo.sql     - Setup file for AQ java API demos
 aqorademo01.java - Enqueue and Dequeue RAW messages 
 aqorademo02.java - Enqueue and Dequeue Object Type messages using 
                    ORAData interface
 Address.java     - Jpublisher generated class ; used in aqorademo02.java 
 Person.java      - Jpublisher generated class ; used in aqorademo02.java 
 aqoradrp.sql     - Cleanup for AQ java API demos

The following files are required for running the AQ Java KPRB driver samples

 aqjmskprb01.java - Enqueues and dequeues a message within the database.
 aqjmskprb01a.sql - Setup file for kprb driver demo.
 aqjmskprb01b.sql - Defines java program aqjmskprb01.java as a stored procedure.
 aqjmskprb01c.sql - Executes aqjmskprb01.java as a stored procedure.
 aqjmskprb01d.sql - Cleanup for AQ kprb driver demo.
 

Setup:
------

The CLASSPATH and PATH need to be set appropriately. The JDK need to be at
least JDK 1.5. 

the CLASSPATH need to have the following:
   $ORACLE_HOME/rdbms/jlib/aqapi.jar 
   $ORACLE_HOME/rdbms/jlib/jmscommon.jar 
   $ORACLE_HOME/rdbms/jlib/xdb.jar
   $ORACLE_HOME/lib/xmlparserv2.jar
   $ORACLE_HOME/jdbc/lib/ojdbc5.jar
   $ORACLE_HOME/jlib/orai18n.jar
   $ORACLE_HOME/jlib/jndi.jar 
   $ORACLE_HOME/jlib/jta.jar

Make sure LD_LIBRARY_PATH contain the directory of OCI JDBC driver shared 
  library objects (libocijdbc*.so). 

Eg: 
   for Solaris, add ORACLE_HOME/lib to LD_LIBRARY_PATH 
    In C-Shell
    %setenv LD_LIBRARY_PATH ${ORACLE_HOME}/lib:${LD_LIBRARY_PATH}

 Refer to platform specific documentation for setting up the above. 

Database Setup:
--------------
   Before running the demo, add the following lines to your init.ora file:
        compatible = 9.0.0.0.0 # or higher 
        aq_tm_processes = 1
        job_queue_processes = 2
        
   shutdown and restart the database.

Compilation: 
------------
    
 2. The Jpublisher classes can be optionally generated using the following
    syntax. 

     jpub -user=aqjava/aqjava -sql=ADDRESS,PERSON -case=mixed -methods=false 
     jpub -user=jmsuser/JMSUSER -sql=Emp -case=mixed -methods=false 
     jpub -user=jmsuser/JMSUSER -sql=Cars -case=mixed -methods=false 


How to Run the JMS API demos: 
-----------------------------

1. Verify the CLASSPATH and PATH setup based on platform and jdk version.

2. Compile the demos 

   %javac aqjmsdemo*.java  aqorademo*.java 

   Refer to the individual demos files for more information.

3. Run the setup scripts

    % sqlplus system/manager @aqjmsdmo.sql  
        creates jmsuser user

    % sqlplus system/manager @aqoradmo.sql  
        creates aqjava user

4. Run the demos 

    %java aqjmsdemo01 [SID] [HOST] [PORT] [DRIVER]
    %java aqjmsdemo02 [SID] [HOST] [PORT] [DRIVER]
    %java aqjmsdemo03 [SID] [HOST] [PORT] [DRIVER]
    %java aqjmsdemo04 [SID] [HOST] [PORT] [DRIVER]
    %java aqjmsdemo05 [SID] [HOST] [PORT] [DRIVER]
    %java aqjmsdemo06 [SID] [HOST] [PORT] [DRIVER]
    %java aqjmsdemo07 [SID] [HOST] [PORT] [DRIVER]
    %java aqjmsdemo08 [SID] [HOST] [PORT] [DRIVER]
    %java aqjmsdemo09 [SID] [HOST] [PORT] [DRIVER]
    %java aqjmsdemo10 [SID] [HOST] [PORT] 

    %java aqorademo01 [SID] [HOST] [PORT] [DRIVER]
    %java aqorademo02 [SID] [HOST] [PORT] [DRIVER]

   Example: 
     java aqjmsdemo01 orcl dlsun673 1521 thin 

5. Drop demo scripts 

    % sqlplus system/manager @aqjmsdrp.sql  
        drops jmsuser user

    % sqlplus system/manager @aqoradrp.sql  
        drops aqjava user


How to Run the JMS KPRB Driver demos: 
-------------------------------------

1) Compile the demo 

    % javac aqjmskprb01.java

2) Create the test user and JMS Queue
   (this creates jmsuser1 user and Queue jmsuser1.queue1)

    % sqlplus system/manager @aqjmskprb01a.sql 

3) Load the java demo class file into the database
   
    % loadjava -user jmsuser1/JMSUSER1 -v aqjmskprb01.class

4) Define the java demo as a PL/SQL Java stored procedure
    
    % sqlplus jmsuser1/JMSUSER1 @aqjmskprb01b.sql

5) Execute the demo program by calling it's PL/SQL procedure
 
    % sqlplus jmsuser1/JMSUSER1 @aqjmskprb01c.sql

6) Drop demo scripts
   (this drops user jmsuser1 and user's stored procedures)

    % sqlplus system/manager @aqjmskprb01d.sql


