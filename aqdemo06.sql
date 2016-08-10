Rem
Rem $Header: template.sql 06-feb-96.13:23:14 kosinski Exp $
Rem
Rem aqdemo06.sql
Rem
Rem  Copyright (c) Oracle Corporation 2000. All Rights Reserved.
Rem
Rem    NAME
rem      aqdemo06.sql - This script cleans up all the tables, types
rem      queues, queue_tables, users etc. created by
rem      aqdemo00 - aqdemo05.
Rem
Rem    DESCRIPTION
Rem      <short description of component this file declares/defines>
Rem
Rem    NOTES
rem      Run this script as SYS using SQLPLUS and type '@aqdemo06'.
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    rbhyrava    07/10/00 - Bug 1319922
rem    mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem    kmeiyyap    01/27/99 - clean up script for aqdemo
rem    kmeiyyap    01/27/99 - Created
rem

set serveroutput on
set echo on
spool aqdemo06.log

rem ==========================================
rem Drop Input Queue Table
rem ==========================================

DECLARE
BEGIN
    dbms_output.put_line ('Dropping Queue Table input_queue_table...');
    dbms_aqadm.drop_queue_table
    (
        queue_table => 'aquser.input_queue_table',
        force => TRUE
    );

    dbms_output.put_line ('Dropped Queue Table input_queue_table.');

END;
/


rem ==========================================
rem Drop Prop Queue Table
rem ==========================================

DECLARE
BEGIN
    dbms_output.put_line ('Dropping Queue Table prop_queue_table...');
    dbms_aqadm.drop_queue_table
    (
        queue_table => 'aquser.prop_queue_table',
        force => TRUE
    );

    dbms_output.put_line ('Dropped Queue Table prop_queue_table.');

END;
/

revoke execute on dbms_lock from aquser
/

drop user aquser cascade
/

spool off
