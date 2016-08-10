rem
rem $Header: ulcase6.sql 14-jul-99.14:25:48 mjaeger Exp $
rem
rem Copyright (c) 1991, 1999, Oracle Corporation.  All rights reserved.
rem
rem    NAME
rem      ulcase6.sql - <one-line expansion of the name>
rem    DESCRIPTION
rem      <short description of component this file declares/defines>
rem    RETURNS
rem
rem    NOTES
rem      <other useful comments, qualifications, etc.>
rem    MODIFIED   (MM/DD/YY)
rem     mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
rem     jstenois   06/17/99 -  cleanup tables before load and show feedback
rem     ksudarsh   02/21/94 -  create unique index "empix"
rem     ksudarsh   03/11/93 -  comment out vms specific host command
rem     ksudarsh   12/29/92 -  Creation
rem     ksudarsh   12/28/92 -  Don't drop dept
rem     cheigham   08/28/91 -  Creation
rem     cheigham   03/19/91 -         Creation

set termout off
rem host write sys$output "Building case 6 demonstration tables.  Please wait"

drop table emp;

create table emp
       (empno number(4) not null,
        ename char(10),
        job char(9),
        mgr number(4),
        hiredate date,
        sal number(7,2),
        comm number(7,2),
        deptno number(2));

create unique index empix on emp(empno);

exit
