rem
rem $Header: viewdemo.sql 12-aug-2004.06:18:09 rsriragh Exp $
rem
rem Copyright (c) 1996, 2004, Oracle. All rights reserved.  
rem
rem Owner  : mchien
rem
rem NAME
rem   viewdemo.sql - Oracle 8 demo of object views
rem
rem DESCRIPTON
rem   This demo features Oracle8's object extensions to views, object views.
rem
rem MODIFIED   (MM/DD/YY)
rem   rsriragh  08/12/04 - fix order by diffs
rem   hyeh      01/11/00 - enhance order by clause
rem   hyeh      08/10/99 - use sqlplus syntax
rem   mjaeger   07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem   yaggarwa  08/06/98 - Add New syntax
rem   cchau     08/18/97 - enable dictionary protection
rem   mchien    05/29/97 - fix type syntax
rem   mchien    07/02/96 - born
rem
rem

REMARK >>>> Set System Variables For Current SQLPlus Session <<<<
SET FEEDBACK 1
SET NUMWIDTH 10
SET PAGESIZE 24
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET DEFINE '^'

set echo on
set numwidth  6
connect sys/knl_test7 as sysdba
grant connect,resource,dba to vrr1 identified by vrr1;

connect vrr1/vrr1

REM  create base tables

CREATE TABLE department (
deptno  NUMBER(3),
dname   VARCHAR2(20),
loc     VARCHAR2(20),
CONSTRAINT pr1 primary key(deptno)
) partition BY RANGE (deptno)
  (partition p1 VALUES less than (100),
  partition p2 VALUES less than (200),
  partition p3 VALUES less than (maxvalue)
  );

CREATE TYPE dept_t  AS OBJECT (
deptno  NUMBER(3),
dname   VARCHAR2(20),
loc     VARCHAR2(20));
/

CREATE TABLE employee (
empno   NUMBER(4) PRIMARY KEY,
ename   VARCHAR2(20),
job     VARCHAR2(20),
deptno  NUMBER(3),
  CONSTRAINT fk1 FOREIGN KEY (deptno) REFERENCES department) -- foreign key
  partition BY RANGE (empno)
  (partition p1 VALUES less than (100),
  partition p2 VALUES less than (200),
  partition p3 VALUES less than (maxvalue)
  );

insert into department select deptno, dname, loc from scott.dept;
insert into employee select empno, ename, job, deptno from scott.emp;
commit;

----------------------------------------------------------------------.
REM  Updatable views

REM  typed view

CREATE VIEW dept_ext_view OF dept_t
WITH OBJECT OID (deptno) AS
SELECT  d.deptno, d.dname, d.loc
FROM    department d;

REM  view with ref view column

CREATE VIEW emp_dept_view (empno, ename, job, edept) AS
SELECT  empno, ename, job, MAKE_REF(dept_ext_view, deptno)
FROM    employee;

select * from user_updatable_columns
  where table_name = 'DEPT_EXT_VIEW' order by 3;
select * from user_updatable_columns
  where table_name = 'EMP_DEPT_VIEW' order by 3;

REM  now perform select, insert, update, delete on the views

select * from dept_ext_view order by deptno;

-- deref through pkref 'edept'
select empno, ename, job,
       e.edept.deptno "DNO", e.edept.dname "DNAME", e.edept.loc "LOC"
  from emp_dept_view e
  order by empno;

-- deref with distinct

select distinct sys_op_dump(deref(edept)), empno from
  emp_dept_view order by empno;

-- deref through pkref 'edept' in where clause
select empno, ename, job,
       ed.edept.deptno "DNO", ed.edept.dname "DNAME", ed.edept.loc "LOC"
  from emp_dept_view ed, dept_ext_view de
  where ed.edept.deptno = de.deptno
  order by empno;

-- deref through pkref 'edept' in order by clause
select empno, ename, job,
       e.edept.deptno "DNO", e.edept.dname "DNAME", e.edept.loc "LOC"
  from emp_dept_view e
  order by e.edept.deptno, empno;

-- deref through pkref 'edept' in group by clause
select e.edept.deptno, max(empno)
  from emp_dept_view e
  group by e.edept.deptno order by 1;

----------------------------------------------------------------------.
-- now perform dml on updatable columns
insert into dept_ext_view values (100, 'LRT', 'Redwood City');
insert into dept_ext_view values (200, 'ST', 'Redwood Shores');
-- update oid - ok because no foreign key reference
update dept_ext_view set deptno = 0 where dname = 'LRT';

-- test for oid access optimization
select empno, ename, job,
       ed.edept.deptno "DNO", ed.edept.dname "DNAME", ed.edept.loc "LOC"
  from emp_dept_view ed, dept_ext_view de
  where ref(de) = (ed.edept)
  order by empno;

select empno, ename, job,
       ed.edept.deptno "DNO", ed.edept.dname "DNAME", ed.edept.loc "LOC"
  from emp_dept_view ed, dept_ext_view de
  where ref(de) = (ed.edept) and ref(de) is not null
  order by empno;

select empno, ename, job,
       ed.edept.deptno "DNO", ed.edept.dname "DNAME", ed.edept.loc "LOC"
  from emp_dept_view ed, dept_ext_view de
  where ref(de) = (ed.edept) or ref(de) is null
  order by empno;

-- update oid - fail because foreign key reference
update dept_ext_view set deptno = 99 where dname = 'SALES';
commit;

select sys_op_dump(value(p)) from dept_ext_view p
  order by 1;
select sys_op_dump(deref(ref(p))) from dept_ext_view p
  order by 1;

insert into emp_dept_view (empno, ename, job)
  values (1234, 'John Doe', 'manager');
commit;

select empno, ename, job, reftohex(edept)
  from emp_dept_view
  where empno = 1234
  order by empno;

connect sys/knl_test7 as sysdba
drop user vrr1 cascade;
set echo off
