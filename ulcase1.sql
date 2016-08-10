rem
rem $Header: ulcase1.sql 14-jul-99.14:22:19 mjaeger Exp $
rem
rem Copyright (c) 1991, 1999, Oracle Corporation.  All rights reserved.
rem
rem    NAME
rem      ulcase1.sql - <one-line expansion of the name>
rem    DESCRIPTION
rem      <short description of component this file declares/defines>
rem    RETURNS
rem
rem    NOTES
rem      <other useful comments, qualifications, etc.>
rem    MODIFIED   (MM/DD/YY)
rem     mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
rem     jstenois   06/17/99 -  cleanup tables before load and show feedback
rem     ksudarsh   03/01/93 -  comment out vms specific host command
rem     ksudarsh   12/29/92 -  Creation
rem     cheigham   08/28/91 -  Creation
rem

set termout off

rem host write sys$output "Building first demonstration tables.  Please wait"

drop table emp;
drop table dept;

create table emp
       (empno number(4) not null,
        ename char(10),
        job char(9),
        mgr number(4),
        hiredate date,
        sal number(7,2),
        comm number(7,2),
        deptno number(2));

create table dept
       (deptno number(2),
        dname char(14) ,
        loc char(13) ) ;

exit
