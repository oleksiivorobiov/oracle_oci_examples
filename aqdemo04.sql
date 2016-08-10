Rem
Rem $Header: aqdemo04.sql 25-sep-2001.11:19:27 lzhao Exp $
Rem
Rem aqdemo04.sql
Rem
Rem Copyright (c) 2000, 2001, Oracle Corporation.  All rights reserved.  
Rem
Rem    NAME
Rem      aqdemo04.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
rem     Dequeue messages by blocking on prop_queue for prog3 for
rem     approximately 2 minutes
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    lzhao       09/25/01 - set feedback on
Rem    rbhyrava    07/10/00 - bug 1319922
rem    mjaeger    07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem    kmeiyyap   01/27/99 - spool to aqdemo04.log
rem    kmeiyyap   09/15/98 - Created
rem
set feedback on
connect aquser/aquser
set serveroutput on
set echo on
spool aqdemo04.log

DECLARE
BEGIN
    demo_prop_dequeue('prog3');
END;
/

rem Show the number of messages in prog3_processed_data table
select count(*) from prog3_processed_data;

spool off
