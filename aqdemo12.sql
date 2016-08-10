Rem
Rem $Header: aqdemo12.sql 17-oct-2003.16:57:18 aahluwal Exp $
Rem
Rem aqdemo12.sql
Rem
Rem Copyright (c) 2003, Oracle Corporation.  All rights reserved.  
Rem
Rem    NAME
Rem      aqdemo12.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem      Removes objects and users created by aqdemo09.sql.
Rem
Rem    NOTES
Rem      Run this script as SYS using SQLPLUS and type '@aqdemo12'.
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    aahluwal    10/17/03 - aahluwal_create_arrenqdeq_demos 
Rem    aahluwal    10/07/03 - Created
Rem

SET ECHO ON
SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100

set serveroutput on
set echo on
spool aqdemo12.log

rem ==========================================
rem Drop My_Queue_Tab Queue Table 
rem ==========================================
DECLARE
BEGIN
    dbms_output.put_line ('Dropping Queue Table my_queue_tab...');
    dbms_aqadm.drop_queue_table
    (
        queue_table => 'aquser.my_queue_tab',
        force => TRUE
    );

    dbms_output.put_line ('Dropped Queue Table my_queue_tab.');

END;
/

drop user aquser cascade
/

spool off
