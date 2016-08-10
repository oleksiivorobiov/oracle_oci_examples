Rem
Rem $Header: aqxmldmo.sql 15-nov-2004.15:26:34 rbhyrava Exp $
Rem
Rem aqxmldmo.sql
Rem
Rem Copyright (c) 2001, 2004, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      aqxmldmo.sql -  AQ xml demo - setup users, queues and queue tables
Rem
Rem    DESCRIPTION
Rem      AQ XML demos to perform AQ operations over the Internet 
Rem      using the Internet Data Access Presentation
Rem
REM      This program is used to setup users, queue tables, queues to
REM      demonstrate AQ xml access and propagation.
Rem
Rem    NOTES
Rem      This sql file creates AQ adminstrator , aq database user
Rem      grant required privileges to AQ internet super user 
Rem      grant database access to internet AQ agents  
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    rbhyrava    11/15/04 - agent
Rem    rbhyrava    10/29/04 - add user setup 
Rem    rbhyrava    05/09/01 - remove REM from plsql blocK
Rem    rbhyrava    04/12/01 - Merged rbhyrava_aqxmldemos
Rem    rbhyrava    04/11/01 - 
Rem    rbhyrava    03/30/01 - Created
Rem

SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100
SET ECHO ON

REM SETUP AQXML USERS 
REM ================= 

REM 
REM create admin and user accounts and grant http access to user
REM
CONNECT SYSTEM/MANAGER; 
CREATE USER aqxmladmn IDENTIFIED BY aqxmladmn;
CREATE USER aqxmluser IDENTIFIED BY aqxmluser;

GRANT CONNECT, RESOURCE, AQ_ADMINISTRATOR_ROLE TO aqxmladmn;
GRANT CONNECT, RESOURCE, AQ_USER_ROLE TO aqxmluser;

GRANT EXECUTE ON dbms_lock TO aqxmladmn,aqxmluser ;

REM
REM grant access to admin packages 
REM
GRANT EXECUTE ON dbms_aqadm TO aqxmladmn;
GRANT EXECUTE ON dbms_aq TO aqxmluser;

REM 
REM Grant Create proxy session privileges to AQ servlet super-user  
REM 
GRANT CREATE SESSION TO scott;
ALTER USER aqxmluser GRANT CONNECT THROUGH scott;

REM
REM Create AQ Agent to access AQ Servlet using HTTP
REM Used for 9.2/10.2 database compatibility for agent names 
EXECUTE dbms_aqadm.create_aq_agent(agent_name=>'AQDEMO.COM/JOHN', enable_http =>true);
EXECUTE dbms_aqadm.create_aq_agent(agent_name=>'"AQDEMO.COM/JOHN"', enable_http =>true);

REM
REM Map AQ Agent to Database user 
REM
REM Used for 9.2/10.2 database compatibility for agent names 
EXECUTE dbms_aqadm.enable_db_access('AQDEMO.COM/JOHN', 'aqxmluser');
EXECUTE dbms_aqadm.enable_db_access('"AQDEMO.COM/JOHN"', 'aqxmluser');

REM
REM view registered AQ agents
REM
SELECT agent_name, db_username, http_enabled 
FROM aq$internet_users ;


REM SETUP QUEUES 
REM ================= 

CONNECT aqxmladmn/aqxmladmn;
SET SERVEROUTPUT ON 

rem create ADT 
CREATE OR REPLACE TYPE book_typ AS OBJECT ( 
           title           VARCHAR2(100), 
           authors         VARCHAR2(100), 
           ISBN            VARCHAR2(20), 
           price           NUMBER); 
/ 

GRANT EXECUTE ON book_typ TO PUBLIC ;

CREATE OR REPLACE TYPE cars_typ AS OBJECT(
   carno        VARCHAR2(10),
   year         NUMBER,
   model        VARCHAR2(20),
   color        VARCHAR2(10),
   car_details      CLOB,
   price        NUMBER(12,2)); 
/

GRANT EXECUTE ON cars_typ TO PUBLIC ;

REM
REM create 8.1 compatible single consumer queue 
REM 
BEGIN 
dbms_aqadm.create_queue_table( 
           queue_table => 'book_queue_tab', 
           queue_payload_type => 'BOOK_TYP'
        );
END;
/

REM
REM Create 8.1 compatible multi consumer queue table for ADT with CLOB
REM
BEGIN 
dbms_aqadm.create_queue_table( 
           queue_table => 'cars_queue_tab', 
           queue_payload_type => 'CARS_TYP',
           multiple_consumers => true 
        );
END;
/

REM
REM Create single consumer queue table for JMS TEXT Message 
REM
BEGIN 
dbms_aqadm.create_queue_table( 
           queue_table => 'jmstext_queue_tab', 
           queue_payload_type => 'SYS.AQ$_JMS_TEXT_MESSAGE'
        );
END;
/

REM
REM Create multi consumer queue table for JMS MAP Message 
REM
BEGIN 
dbms_aqadm.create_queue_table( 
           queue_table => 'jmsmap_queue_tab', 
           queue_payload_type => 'SYS.AQ$_JMS_MAP_MESSAGE',
           multiple_consumers => true
        );
END;
/

REM create queues 
REM
BEGIN
dbms_aqadm.create_queue( queue_name  => 'book_q1', 
                         queue_table => 'book_queue_tab');

dbms_aqadm.create_queue( queue_name  => 'cars_q1', 
                         queue_table => 'cars_queue_tab');

dbms_aqadm.create_queue( queue_name  => 'cars_q2', 
                         queue_table => 'cars_queue_tab');

dbms_aqadm.create_queue( queue_name  => 'jms_text_q1', 
                         queue_table => 'jmstext_queue_tab');

dbms_aqadm.create_queue( queue_name  => 'jms_map_q1', 
                         queue_table => 'jmsmap_queue_tab');
END;
/

REM
Rem Add subscribers to multi consumer queues
REM
BEGIN 
  dbms_aqadm.add_subscriber( queue_name=> 'CARS_Q1', 
           subscriber=> sys.aq$_agent('DEALS_CARS_Q1',null, null), 
           rule => 'tab.user_data.price > 30000');

  dbms_aqadm.add_subscriber( queue_name=> 'CARS_Q2', 
           subscriber=> sys.aq$_agent('DEALS_CARS_Q2',null, null));


  dbms_aqadm.add_subscriber( queue_name=> 'JMS_MAP_Q1', 
           subscriber=> sys.aq$_agent('SUB1MAP',null, null));

END;
/

REM
REM Start queues 
REM
BEGIN 
  dbms_aqadm.start_queue(queue_name => 'BOOK_Q1');
  dbms_aqadm.start_queue(queue_name => 'CARS_Q1');
  dbms_aqadm.start_queue(queue_name => 'CARS_Q2');
  dbms_aqadm.start_queue(queue_name => 'JMS_TEXT_Q1');
  dbms_aqadm.start_queue(queue_name => 'JMS_MAP_Q1');
END;
/

REM
REM grant enqueue/dequeue privileges to aq database user
REM
BEGIN 
  dbms_aqadm.grant_queue_privilege('ENQUEUE', 'BOOK_Q1', 'aqxmluser', FALSE) ;
  dbms_aqadm.grant_queue_privilege('DEQUEUE', 'BOOK_Q1', 'aqxmluser', FALSE) ;

  dbms_aqadm.grant_queue_privilege('ENQUEUE', 'CARS_Q1', 'aqxmluser', FALSE) ;
  dbms_aqadm.grant_queue_privilege('DEQUEUE', 'CARS_Q1', 'aqxmluser', FALSE) ;

  dbms_aqadm.grant_queue_privilege('ENQUEUE', 'CARS_Q2', 'aqxmluser', FALSE) ;
  dbms_aqadm.grant_queue_privilege('DEQUEUE', 'CARS_Q2', 'aqxmluser', FALSE) ;

  dbms_aqadm.grant_queue_privilege('ENQUEUE', 'JMS_TEXT_Q1','aqxmluser',FALSE) ;
  dbms_aqadm.grant_queue_privilege('DEQUEUE', 'JMS_TEXT_Q1','aqxmluser',FALSE) ;

  dbms_aqadm.grant_queue_privilege('ENQUEUE', 'JMS_MAP_Q1','aqxmluser',FALSE) ;
  dbms_aqadm.grant_queue_privilege('DEQUEUE', 'JMS_MAP_Q1','aqxmluser',FALSE) ;
END;
/

EXIT ;

