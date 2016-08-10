Rem
Rem $Header: aqjmskprb01b.sql 05-jun-2007.15:02:53 aatam Exp $
Rem
Rem aqjmskprb01b.sql
Rem
Rem Copyright (c) 2002, 2007, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      aqjmskprb01b.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem      defines java stored procedure for AQ/OJMS kprb driver demo
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
set serveroutput on
connect jmsuser1/JMSUSER1

--
-- define the kprb driver demos in the database
--

create or replace package jmsdemo authid current_user as
  procedure start_me(t1 VARCHAR2);
end jmsdemo;
/
create or replace package body jmsdemo as
  procedure start_me(t1 VARCHAR2) is language java
  name 'aqjmskprb01.main (java.lang.String[])';
end jmsdemo;
/
show errors


quit;
