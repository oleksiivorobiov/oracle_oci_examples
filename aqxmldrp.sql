Rem
Rem $Header: aqxmldrp.sql 02-nov-2004.17:21:31 rbhyrava Exp $
Rem
Rem aqxmldrp.sql
Rem
Rem Copyright (c) 2001, 2004, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      aqxmldrp.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem      <short description of component this file declares/defines>
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    rbhyrava    11/02/04 - drop users 
Rem    rbhyrava    04/12/01 - Merged rbhyrava_aqxmldemos
Rem    rbhyrava    04/09/01 - Created
Rem

SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100
SET ECHO ON

connect aqxmladmn/aqxmladmn
REM Stop queues 
BEGIN 
  dbms_output.put_line ('Stopping Queues ...');
  dbms_aqadm.stop_queue(queue_name => 'BOOK_Q1');
  dbms_aqadm.stop_queue(queue_name => 'CARS_Q1');
  dbms_aqadm.stop_queue(queue_name => 'CARS_Q2');
  dbms_aqadm.stop_queue(queue_name => 'JMS_TEXT_Q1');
  dbms_aqadm.stop_queue(queue_name => 'JMS_MAP_Q1');
END;
/

REM Drop queues 
BEGIN 
  dbms_output.put_line ('Dropping Queues ...');
  dbms_aqadm.drop_queue(queue_name => 'BOOK_Q1');
  dbms_aqadm.drop_queue(queue_name => 'CARS_Q1');
  dbms_aqadm.drop_queue(queue_name => 'CARS_Q2');
  dbms_aqadm.drop_queue(queue_name => 'JMS_TEXT_Q1');
  dbms_aqadm.drop_queue(queue_name => 'JMS_MAP_Q1');
END;
/

BEGIN
    dbms_output.put_line ('Dropping Queue Tables ...');
    dbms_aqadm.drop_queue_table ( 
        queue_table => 'book_queue_tab',
        force => TRUE
    );
    dbms_output.put_line ('Dropped Queue Table book_queue_tab.');
    dbms_aqadm.drop_queue_table (
        queue_table => 'cars_queue_tab',
        force => TRUE
    );
    dbms_output.put_line ('Dropped Queue Table cars_queue_tab.');

    dbms_aqadm.drop_queue_table (
        queue_table => 'jmstext_queue_tab',
        force => TRUE
    );
    dbms_output.put_line ('Dropped Queue Table jmstext_queue_tab.');

    dbms_aqadm.drop_queue_table (
        queue_table => 'jmsmap_queue_tab',
        force => TRUE
    );
    dbms_output.put_line ('Dropped Queue Table jmsmap_queue_tab.');

END;
/

connect system/manager

EXECUTE dbms_aqadm.drop_aq_agent('"aqdemo.com/john"') ;
EXECUTE dbms_aqadm.drop_aq_agent('"AQDEMO.COM/JOHN"') ;
ALTER USER aqxmluser REVOKE CONNECT THROUGH scott;

drop user aqxmladmn cascade ;
drop user aqxmluser cascade ;

