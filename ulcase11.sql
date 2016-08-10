Rem
Rem $Header: ulcase11.sql 06-feb-2001.14:26:12 rpfau Exp $
Rem
Rem ulcase11.sql
Rem
Rem  Copyright (c) Oracle Corporation 2001. All Rights Reserved.
Rem
Rem    NAME
Rem      ulcase11.sql - Set up for SQL Loade example 11
Rem
Rem    DESCRIPTION
Rem      Create table emp for example loading little endian unicode (UTF-16)
Rem	 data.
Rem
Rem    NOTES
Rem      None
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    rpfau       02/06/01 - Merged rpfau_sqlldr_add_case_study_11
Rem    rpfau       01/30/01 - Created
Rem
set termout off

rem host write sys$output "Building demonstration tables for case study 11.  Please wait"

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
	projno number,
	loadseq number);

exit
