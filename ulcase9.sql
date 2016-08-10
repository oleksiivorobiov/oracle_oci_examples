rem
rem $Header: ulcase9.sql 14-jul-99.14:28:05 mjaeger Exp $
rem
rem ulcase9.sql
rem
rem Copyright (c) 1998, 1999, Oracle Corporation.  All rights reserved.
rem
rem    NAME
rem      ulcase9.sql - setup for SQL Loader example 9
rem
rem    DESCRIPTION
rem      Add RESUME column to EMP for example of using SQL Loader to load LOBs
rem
rem    NOTES
rem      Assumes an EMP table already exists
rem
rem    MODIFIED   (MM/DD/YY)
rem    mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem    jstenois    06/17/99 - cleanup tables before load and show feedback
rem    jstenois    10/26/98 - demo of 8.1 features for sqlldr
rem    jstenois    10/26/98 - Created
rem

set termout off

rem host write sys$output "Building case 9 demonstration tables.  Please wait"

drop table emp;

create table emp
       (empno number(4) not null,
        ename char(10),
        job char(9),
        mgr number(4),
        hiredate date,
        sal number(7,2),
        comm number(7,2),
        deptno number(2),
        resume clob);

exit
