rem
rem $Header: cdemodsa.sql 14-jul-99.13:51:36 mjaeger Exp $
rem
rem cdemodsa.sql
rem
rem Copyright (c) 1997, 1999, Oracle Corporation.  All rights reserved.
rem
rem    NAME
rem      cdemodsa.sql - <one-line expansion of the name>
rem
rem    DESCRIPTION
rem      <short description of component this file declares/defines>
rem
rem    NOTES
rem      <other useful comments, qualifications, etc.>
rem
rem    MODIFIED   (MM/DD/YY)
rem    mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem    svedala     09/11/98 - a "/" required after create type - bug 717842
rem    azhao       06/03/97 - add connect scott/tiger
rem    sgollapu    05/30/97 - Created
rem

connect scott/tiger;

set echo on;

drop table customerobj;
drop type address_object;
drop type person;
drop table empnml;
drop table empref;

create type address_object as object (state char(2), zip char(10));
/

create table customerobj (custno number, addr REF address_object);

create type person as object(name char(20), age number,
                             address address_object);
/

create table empnml (emp_id number, emp_info person);
create table empref (emp_id number, emp_info REF person);

commit;

set echo off;
