/
/ $Header: aqxmlREADME.txt 13-may-2008.13:16:32 rbhyrava Exp $
/
/ aqxmlREADME.txt
/
/ Copyright (c) Oracle Corporation 2001. All Rights Reserved.
/
/   NAME
/     aqxmlREADME.txt 
/
/   DESCRIPTION
/     This document contains list and description of aqxml demos 
/   NOTES
/     <other useful comments, qualifications, etc.>
/
/   MODIFIED   (MM/DD/YY)
/   rbhyrava    05/13/08 - aqxmlctl args
/   rbhyrava    10/29/04 - OC4J support 
/   rbhyrava    03/30/01 - Creation
/

The following files are required for running AQXML samples

 aqxmldmo.sql - Setup Users, Queue tables and Queues
 aqxmldrp.sql - Drop/Cleanup AQ xml demo users/queue tables/queues

 aqxml01.xml  AQXmlSend
              Enqueue to ADT single consumer queue with piggyback commit

 aqxml02.xml  AQXmlReceive
              Dequeue from ADT Single consumer queue with piggyback commit

 aqxml03.xml  AQXmlPublish
              Enqueue to ADT (with LOB) multi consumer queue

 aqxml04.xml  AQXmlReceive
              Dequeue from ADT multi consumer queue

 aqxml05.xml  AQXmlCommit
              commit previous operation

 aqxml06.xml  AQXmlSend
              Enqueue to JMS Text single consumer queue with piggyback commit

 aqxml07.xml  AQXmlReceive
              Dequeue from JMS Text single consumer queue with piggyback commit

 aqxml08.xml  AQXmlPublish
              Enqueue JMS MAP message with recipient into multi consumer queue

 aqxml09.xml  AQXmlReceive
              Dequeue JMS MAP message from multi consumer queue

 aqxml10.xml  AQXmlRollback
              rollback previous operation

 aqxmlhtp.sql HTTP Propagation

 AQHttp.java , AQHttpRq.java - Helpers to Post XML Request using HTTPClient

 AQDemoServlet.java - Servlet to Post AQ XML files 
 AQPropServlet.java  - Servlet for AQ HTTP Propagation 

For additional demos using E-business examples unzip 
aqbzdemo.tar (aqbzdemo.zip for Windows).

Steps to run demos using oc4j:
===============================

(1) Make sure classpath include the following:

      For JDK1.2.x or JDK1.3.x: 
       $ORACLE_HOME/jdbc/lib/classes12.zip 
       $ORACLE_HOME/jdbc/lib/nls_charset12.zip
       $ORACLE_HOME/rdbms/jlib/aqapi.jar 
       $ORACLE_HOME/rdbms/jlib/jmscommon.jar 
       $ORACLE_HOME/rdbms/jlib/aqxml.jar 
       $ORACLE_HOME/rdbms/jlib/xsu12.jar 
       $ORACLE_HOME/rdbms/jlib/xdb.jar 
       $ORACLE_HOME/jlib/jndi.jar 
       $ORACLE_HOME/jlib/jta.jar 
       $ORACLE_HOME/jlib/jssl-1_1.jar 
       $ORACLE_HOME/jlib/javax-ssl-1_1.jar 
       $ORACLE_HOME/lib/xmlparserv2.jar 
       $ORACLE_HOME/lib/xsu12.jar 
       $ORACLE_HOME/lib/xschema.jar 
       $ORACLE_HOME/lib/http_client.jar 
       $ORACLE_HOME/lib/servlet.jar 
       $ORACLE_HOME/lib/lclasses12.zip 

      For JDK1.4.x 
       $ORACLE_HOME/jdbc/lib/ojdbc14.jar 
       $ORACLE_HOME/jdbc/lib/orai18n.zip
       $ORACLE_HOME/rdbms/jlib/aqapi.jar 
       $ORACLE_HOME/rdbms/jlib/jmscommon.jar 
       $ORACLE_HOME/rdbms/jlib/aqxml.jar 
       $ORACLE_HOME/rdbms/jlib/xdb.jar 
       $ORACLE_HOME/jlib/jndi.jar 
       $ORACLE_HOME/jlib/jta.jar 
       $ORACLE_HOME/jlib/jssl-1_1.jar 
       $ORACLE_HOME/jlib/javax-ssl-1_1.jar 
       $ORACLE_HOME/lib/xmlparserv2.jar 
       $ORACLE_HOME/lib/xsu12.jar 
       $ORACLE_HOME/lib/xschema.jar 
       $ORACLE_HOME/lib/http_client.jar 
       $ORACLE_HOME/lib/servlet.jar 
       $ORACLE_HOME/lib/lclasses12.zip 

       NOTE :
       http_client.jar, jssl-1_1.jar, javax-ssl-1_1.jar 
       are required by HTTPClient used in AQHttp.java AQHttpRq.java.

(2) Compile AQHttpRq.java:

   cd $ORACLE_HOME/rdbms/demo
   javac AQHttpRq.java AQHttp.java 

(3) Database Setup:

   Make sure init.ora file contains: 
      job_queue_processes=2
      compatible=10.2.0 or above 
   restart database and listener.

(4) Users Authentication setup for restricted access and queues:
    cd $ORACLE_HOME/rdbms/demo
    SQL> @aqxmldmo.sql
   
(5) Configure AQDemoServlet:
    (deploy,start may be already done at your installation during install, see
     step (6)). Otherwise,

    cd $ORACLE_HOME/bin 
    follow the steps for deploy, start of the servlet 

      To Deploy the servlet:  sh aqxmlctl deploy welcome
      To Start OC4J instance: sh aqxmlctl start welcome
      To Stop OC4J instance:  sh aqxmlctl stop welcome
   
(6) Verify the setup:

    #############------------------------------##############
    NOTE: The default deployment is done using https protocol.
    #############------------------------------##############

    Refer to $ORACLE_HOME/rdbms/demo/aqxml.ini for status of servlet. 
    Refer to $ORACLE_HOME/oc4j/j2ee/OC4J_AQ/config/rmi.xml 
    Refer to $ORACLE_HOME/oc4j/j2ee/OC4J_AQ/config/http-web-site.xml 
    for information on protocol,port number used for deploying this servlet. 

    From the browser try,
    https://<hostname>:<portnumber>/aqserv/servlet/AQDemoServlet 
    replace hostname, portnumber, protocol with webserver host/port,protocol
used. 
    Eg: https request 
    https://my-pc:5760:aqserv/servlet/AQDemoServlet 
 
    Eg: http request 
    http://my-pc:5740:aqserv/servlet/AQDemoServlet 

    This should display (after prompting for user/password- john/welcome)
      Sample AQ Servlet 
      AQxmlServlet is working!

   Refer to OC4J documentation for https configuration using admin.jar. 

   (a) Creating an SSL Certificate and generating keystore.
     Refer to keytool documentation for maintaining or creating 
     your own keystore. 

     $ORACLE_HOME/rdbms/demo/keystore, 
     $ORACLE_HOME/rdbms/demo/aqxmloc4j.cert is provided for demo purposes.
    
   (b) The following tags in
     $ORACLE_HOME/oc4j/j2ee/OC4J_AQ/config/http-web-site.xml indicate 
     the website is secure and keystore is used for SSL authentication. 

    Modify http-web-site.html for secure tag and adding ssl-config.
           <web-site port="443" secure="true">
           ....
              <ssl-config 
                     keystore="$ORACLE_HOME/oc4j/j2ee/home/keystore" 
                     keystore-password="welcome" />
           </web-site>

    To make the site to access only http requests, remove the secure="true" and 
    <ssl-config> from the http-web-site.xml.
    Re-start the OC4J instance. 

    Note:
    The above step can also be done using admin.jar provided by OC4J Standalone.
 

(7) Http(s) Access: POST a request:
   
    Usage: 
    java AQHttpRq <wserv> <wport> <POST> <PROTOCOL> <url> webuser pswd xmlfile xmlfile ..

    Example:

    cd $ORACLE_HOME/rdbms/demo;
    Replace <my-pc> 7777,http in the sample requests below with appropriate 
    values of host, port and protocol(http/https) used in Step (6).
    
    (a) Enqueue to ADT single consumer queue with piggyback commit:
    java AQHttpRq <my-pc> 7777 POST http /aqserv/servlet/AQDemoServlet john welcome aqxml01.xml 

    (b) Dequeue from ADT Single consumer queue with piggyback commit:
    java AQHttpRq <my-pc> 7777 POST http /aqserv/servlet/AQDemoServlet john welcome aqxml02.xml 

    (c) Enqueue to ADT (with LOB) multi consumer queue and Commit 
    java AQHttpRq <my-pc> 7777 POST http /aqserv/servlet/AQDemoServlet john welcome aqxml03.xml  aqxml05.xml 

    (d) Dequeue from ADT multi consumer queue and Commit:
    java AQHttpRq <my-pc> 7777 POST http /aqserv/servlet/AQDemoServlet john welcome aqxml04.xml  aqxml05.xml 

    (e) Enqueue to JMS Text single consumer queue with piggyback commit:
    java AQHttpRq <my-pc> 7777 POST http /aqserv/servlet/AQDemoServlet john welcome aqxml06.xml 

    (f) Dequeue from JMS Text single consumer queue with piggyback commit
    java AQHttpRq <my-pc> 7777 POST http /aqserv/servlet/AQDemoServlet john welcome aqxml07.xml 

    (g) Enqueue JMS MAP message with recipient into multi consumer queue-Commit 
    java AQHttpRq <my-pc> 7777 POST http /aqserv/servlet/AQDemoServlet john welcome aqxml08.xml  aqxml05.xml

    (h) Dequeue JMS MAP message from multi consumer queue and Rollback
    java AQHttpRq <my-pc> 7777 POST http /aqserv/servlet/AQDemoServlet john welcome aqxml09.xml  aqxml10.xml

    (i) Dequeue JMS MAP message from multi consumer queue and Commit 
    to commit dequeue call 
    java AQHttpRq <my-pc> 7777 POST http /aqserv/servlet/AQDemoServlet john welcome aqxml09.xml  aqxml05.xml

(8) HTTP Propagation: 

   (a) Make sure the AQPropServlet is working.See Step(6)

   (b) Run AQ http propagation demo.
       Specify arguments protocol, hostname, portnumber for creating dblink.

       SQL>@aqxmlhtp http <my-pc> <7777>

(9) HTTPS Propagation: 

   (a) Make sure the AQPropServlet is working.See Step(6)
   (b) Set up Wallet:
        -create a new wallet using Oracle Wallet Manager
        - Import trusted cerificate ($ORACLE_HOME/rdbms/demo/aqxmloc4j.cert)
        - New trustpoint will be added 
        - select AutoLogin button under Wallet menu option 
        - Save the wallet to a directory location (eg: $ORACLE_HOME/rdbms/demo)
        - Verify the two files created under the specified directory
          cwallet.sso, ewallet.p12
          Exit Wallet Manager

        - add to network/admin/sqlnet.ora(replace $ORACLE_HOME)
           eg:
           oss.source.my_wallet=
           (SOURCE=(METHOD=FILE)(METHOD_DATA=
              (DIRECTORY=$ORACLE_HOME/rdbms/demo)))

        - from sqlplus 
          SQL>@aqxmlhtp https <my-pc> <443> 
         Specify arguments protocol, hostname, portnumber for creating dblink.
        
(10) Cleanup:
   (a) run aqxmldrp.sql to drop users/queues 
