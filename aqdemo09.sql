Rem
Rem $Header: aqdemo09.sql 16-nov-2004.16:37:16 rbhyrava Exp $
Rem
Rem aqdemo10.sql
Rem
Rem Copyright (c) 2003, 2004, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      aqdemo09.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem      <short description of component this file declares/defines>
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    rbhyrava    11/16/04 - user
Rem    aahluwal    10/17/03 - aahluwal_create_arrenqdeq_demos 
Rem    aahluwal    10/07/03 - Created
Rem
rem     NAME
rem     aqdemo09.sql, aqdemo10.sql, aqdemo11.sql, aqdemo12.sql,
rem     ociaqarrayenq.c, ociaqarraydeq.c
rem
rem     DESCRIPTION
rem     This set of scripts serve as an example for building an
rem     application, using Oracle Advanced Queues, to perform
rem     asynchronous database operations. These examples specifically
rem     make use of the array enq/deq interfaces for applications
rem     requiring high throughput. 
rem
rem     The scripts do the following:
rem
rem     aqdemo09.sql (Login as SYS, and type '@aqdemo09')
rem     1) Create aquser as an user of AQ
rem     2) Create my_queue AQ queue within my_queue_tab AQ Queue Tab in 
rem        AQUSER's schema
rem     3) Add a subscriber to my_queue
rem
rem     aqdemo10.sql
rem     1) Performs an array enq of a batch of messages into my_queue
rem 
rem     aqdemo11.sql
rem     1) Performs an array deq of a batch of messages from my_queue
rem
rem     ociaqarrayenq.c, ociaqarraydeq.c
rem     1) Perform an array enq and array deq (respectively) on my_queue
rem        from the OCI AQ array operations interfaces.
rem
rem     aqdemo12.sql (Login as SYS, and type '@aqdemo09')
rem     1) Cleans up all objects 
rem
rem

SET ECHO ON
SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100


set serveroutput on
set echo on
spool aqdemo09.log

rem ====================================================================
rem create a queue user
rem ====================================================================
create user aquser identified by aquser;
grant connect, resource , aq_administrator_role to aquser
/
grant execute on dbms_aq to aquser
/
grant execute on dbms_aqadm to aquser
/

connect aquser/aquser;

rem ====================================================================
rem
rem Create a type message for the payload of the queue. Also create types
rem message_tbl and message_arr which are used for subsequent array 
rem operations.
rem
rem ====================================================================
CREATE OR REPLACE TYPE message as OBJECT (
data VARCHAR2(10)) ;
/

CREATE OR REPLACE TYPE message_tbl AS TABLE OF message;
/

CREATE OR REPLACE TYPE message_arr AS VARRAY(2000) OF message;
/

rem ==========================================
rem Create a queue table  my_queue_tab
rem ==========================================
begin 
dbms_aqadm.create_queue_table(
  queue_table => 'my_queue_tab',
  multiple_consumers => TRUE,
  queue_payload_type => 'message',
  compatible => '9.2.0.0.0');
end;
/

rem ==========================================
rem Create a queue my_queue
rem ==========================================
begin 
dbms_aqadm.create_queue(
  queue_name => 'my_queue',
  queue_table => 'my_queue_tab');
end;
/

rem ====================================
rem Start queue my_queue
rem ====================================
begin 
dbms_aqadm.start_queue(
  queue_name => 'my_queue',
  dequeue => TRUE,
  enqueue => TRUE);
end;
/

rem ========================================
rem Create queue subscriber
rem ========================================
declare
app1 sys.aq$_agent;
begin
app1 := sys.aq$_agent('sub1', NULL, NULL);
dbms_aqadm.add_subscriber('my_queue',app1);
end;
/
        
rem ====================================================================
rem   Setup complete
rem ====================================================================

spool off
