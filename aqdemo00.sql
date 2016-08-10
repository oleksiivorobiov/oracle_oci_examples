Rem
Rem $Header: aqdemo00.sql 16-nov-2004.15:59:53 rbhyrava Exp $
Rem
Rem aqdemo01.sql
Rem
Rem Copyright (c) 2000, 2004, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      aqdemo01.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem      <short description of component this file declares/defines>
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    rbhyrava    11/16/04 - create user 
Rem    lzhao       06/26/01 - set echo off for aqdemo01, aqdemo03
Rem    rbhyrava    04/29/01 - add comments for aqdemo08, aqdemo09
Rem    rbhyrava    01/26/01 - obselete job_queue_interval
Rem    rbhyrava    07/10/00 - Bug - 1319922
Rem    rbhyrava    07/10/00 - Created
Rem
Rem
Rem aqdemo00.sql
Rem
Rem Copyright (c) 2000, 2004, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      aqdemo00.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem      <short description of component this file declares/defines>
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem
rem     NAME
rem     aqdemo00.sql
rem
rem     DESCRIPTION
rem     This set of scripts serve as an example for building an
rem     application, using Oracle Advanced Queues, to perform
rem     asynchronous database operations.
rem
rem     The scripts do the following:
rem
rem     aqdemo00
rem     1) Create aquser as an user of AQ
rem     2) Create tables prog1_processed_data, prog2_processed_data and
rem        prog3_processed_data
rem
rem     aqdemo01
rem     1) Create two queue tables - input_queue_table, prop_queue_table
rem     2) Create two queues - input_queue belonging to input_queue_table,
rem        prop_queue belonging to prop_queue_table
rem     3) Create two subscribers to input_queue - prog1, prog2
rem     4) Create one subscribers to input_queue - prog3 at prop_queue
rem     5) Schedule propagation between input_queue and other queues in
rem        the database
rem
rem     aqdemo02
rem     1) Enqueue 100 messages into the input_queue
rem
rem     aqdemo03
rem     1) Installs the dequeue procedures
rem
rem     aqdemo04
rem     1) prog3 performs a blocking dequeue from prop_queue. Messages
rem        in prop_table were propagated from input_queue.
rem
rem     aqdemo05
rem     1) A listener program listens on input_table and calls
rem        prog1 or prog2 to dequeue from input_queue based on the
rem        kind of message received.
rem
rem     aqdemo06
rem     1) This script cleans up all the tables, types, queues,
rem        queue_tables, users etc. created by
rem        aqdemo00 - aqdemo05
rem
rem     aqdemo07
rem     1) This script demonstates using XMLType queues and 
rem        dequeue, subscribe using  XPATH expressions on XMLType datatype 
rem        aqdemo07
rem
rem     aqdemo08
rem     1) This script demonstrates using Server to Server , Email 
rem        notifications with default presentation of XML presentation
rem        Modify email host and sender info before running the demo.
rem        aqdemo08 
rem
rem    NOTES
rem      This file contains the sql script that drives the demo.
rem      Before running the demo, add the following lines to
rem      your init.ora file:
rem        ## compatible can be 8.1.0 or higher 
rem        compatible = 8.1.0  
rem        aq_tm_processes = 1
rem        job_queue_processes = 2
rem      shutdown and restart the database.
rem
rem      Run this demo as SYS using SQLPLUS.  Just login as SYS
rem      in SQLPLUS and type '@aqdemo00'.
rem      aqdemo00 calls aqdemo01 and aqdemo03.
rem
rem      Log into another SQLPUS session and type '@aqdemo02'.
rem      This enqueus 100 messages into input_queue
rem
rem      Log into another SQLPLUS session and type '@aqdemo04'.
rem      This program blocks on prop_queue for approxmately 2 minutes and
rem      dequeues messages.
rem
rem      Log into another SQLPLUS session and type '@aqdemo05'.
rem      This program listens for approxmately 2 minutes on input_queue
rem      and calls the dequeue  for prog1 and prog2 appropriately.
rem
rem      aqdemo02(enqueue), aqdemo04(blocking dequeue) and
rem      aqdemo05(listen) can be run concurrently
rem
rem      aqdemo06(cleanup script) has to be run by SYS.
rem      Login as SYS in SQLPLUS and type '@aqdemo06'.
rem

set serveroutput on
set echo on
spool aqdemo00.log
rem ====================================================================
rem create a queue user
rem ====================================================================

drop user aquser cascade ;
create user aquser identified by aquser;
grant connect, resource, aq_administrator_role to aquser;

grant execute on dbms_aq to aquser
/
grant execute on dbms_aqadm to aquser
/
grant execute on dbms_lock to aquser
/


connect aquser/aquser
set serveroutput on
set echo on

rem ====================================================================
rem
rem Create a type
rem
rem ====================================================================

create type message as object (
    id NUMBER,
    city VARCHAR2(30),
    priority NUMBER)
/

rem ====================================================================
rem
rem  Create the table to store the dequeued data
rem
rem ====================================================================
drop table prog1_processed_data
/
drop table prog2_processed_data
/
drop table prog3_processed_data
/

create table prog1_processed_data
(
    id NUMBER,
    city VARCHAR2(30),
    priority NUMBER
)
/

create table prog2_processed_data
(
    id NUMBER,
    city VARCHAR2(30),
    priority NUMBER
)
/

create table prog3_processed_data
(
    id NUMBER,
    city VARCHAR2(30),
    priority NUMBER
)
/

rem ====================================================================
rem   Setup complete
rem ====================================================================


rem Set up queue tables, queues, subscribers etc.
set echo off
@@aqdemo01.sql
rem Load dequeue procedures
set echo off
@@aqdemo03.sql



rem ====================================================================
rem   Setup complete
rem ====================================================================

spool off
