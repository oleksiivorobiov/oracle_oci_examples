Rem
Rem $Header: aqjmskprb01c.sql 05-jun-2007.15:02:53 aatam Exp $
Rem
Rem aqjmskprb01c.sql
Rem
Rem Copyright (c) 2002, 2007, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      aqjmskprb01c.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem      Calls java stored procedure for AQ/OJMS kprb driver demo
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    aatam       06/05/07 - password need to be consistent
Rem    jleinawe    09/04/02 - jleinawe_new_aq_demos
Rem    jleinawe    08/30/02 - Created
Rem

set echo on
connect jmsuser1/JMSUSER1

--
-- run the kprb driver demo
--
set serveroutput on

call dbms_java.set_output(30000);

call jmsdemo.start_me('hello');
call jmsdemo.start_me('hello again');

quit;

