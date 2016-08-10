Rem
Rem $Header: aqjmskprb01a.sql 05-jun-2007.15:02:52 aatam Exp $
Rem
Rem aqjmskprb01a.sql
Rem
Rem Copyright (c) 2002, 2007, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      aqjmskprb01a.sql 
Rem
Rem    DESCRIPTION
Rem      creates user and queue for AQ/OJMS kprb driver demo
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    aatam       06/05/07 - password need to be consistent
Rem    jleinawe    09/04/02 - jleinawe_new_aq_demos
Rem    jleinawe    08/30/02 - Created
Rem

SET ECHO ON
CONNECT system/manager
SET SERVEROUTPUT on

--
-- create the user "jmsuser1" 
--
CREATE USER jmsuser1 IDENTIFIED BY JMSUSER1;

GRANT CONNECT, RESOURCE, AQ_ADMINISTRATOR_ROLE, AQ_USER_ROLE TO jmsuser1;
GRANT EXECUTE ON DBMS_AQADM TO jmsuser1;
GRANT EXECUTE ON DBMS_AQ TO jmsuser1;

execute dbms_java.grant_permission('JMSUSER1', 'java.net.SocketPermission', 'localhost:1024-', 'accept, listen, resolve');

execute dbms_java.grant_permission( 'JMSUSER1', 'SYS:java.lang.RuntimePermission', ' getClassLoader', '' )

execute dbms_java.grant_permission( 'JMSUSER1', 'SYS:java.lang.RuntimePermission' , 'setContextClassLoader', '' )

CONNECT jmsuser1/JMSUSER1;

--
-- create and start the JMS Queue "queue1"
--
execute dbms_aqadm.create_queue_table(queue_table => 'queue1', queue_payload_type => 'SYS.AQ$_JMS_TEXT_MESSAGE', comment => 'a test queue', multiple_consumers => false, compatible => '8.1.0');

execute dbms_aqadm.create_queue( queue_name  => 'queue1', queue_table => 'queue1' );

execute dbms_aqadm.start_queue(queue_name => 'queue1');

quit;
