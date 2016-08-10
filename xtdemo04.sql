Rem
Rem $Header: xtdemo04.sql 07-jun-2001.14:03:39 rsahani Exp $
Rem
Rem xtdemo04.sql
Rem
Rem  Copyright (c) Oracle Corporation 2001. All Rights Reserved.
Rem
Rem    NAME
Rem      xtdemo04.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem      <short description of component this file declares/defines>
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    rsahani     06/07/01 - Merged rsahani_add_xt_demos_010605
Rem    rsahani     06/06/01 - Created
Rem

SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 108
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 200
SET ECHO ON

connect sys/knl_test7 as sysdba

Rem Demo 4: External Table - Loading data into multiple tables
Rem ######################

Rem Create Directory
@xtsetup.sql

Rem Drop and create external table
drop table emp_proj_XT;
CREATE TABLE emp_proj_XT
(
  EMPNO		NUMBER(4),
  ENAME		CHAR(10),
  DEPTNO	NUMBER(2),
  MGR		NUMBER(4),
  PROJ1		NUMBER,
  PROJ2		NUMBER,
  PROJ3		NUMBER
)
ORGANIZATION external
(
  TYPE oracle_loader
  DEFAULT DIRECTORY def_dir
  ACCESS PARAMETERS
  (
    RECORDS DELIMITED BY NEWLINE CHARACTERSET WE8DEC
    BADFILE 'emp_proj.bad'
    DISCARDFILE 'emp_proj.dis'
    LOGFILE 'emp_proj.log'
    FIELDS LDRTRIM
    (
      EMPNO  (1:4)   INTEGER EXTERNAL(4),
      ENAME  (6:15)  CHAR(10),
      DEPTNO (17:18) CHAR(2),
      MGR    (20:23) INTEGER EXTERNAL(4),
      PROJ1  (25:27) INTEGER EXTERNAL(4),
      PROJ2  (29:31) INTEGER EXTERNAL(3),
      PROJ3  (32:34) INTEGER EXTERNAL(4)
    )
  )
  location ('xtdemo04.dat')
)
REJECT LIMIT UNLIMITED;

Rem Query external table
select * from emp_proj_XT order by empno, ename;

Rem Create regular tables in which data will be inserted
drop table emp;
create table emp
(
  EMPNO         NUMBER(4),
  ENAME         CHAR(10),
  DEPTNO        NUMBER(2),
  MGR           NUMBER(4)
);

select count(*) from emp;

drop table proj;
create table proj
(
  EMPNO        NUMBER(4),  
  PROJID       NUMBER
);

select count(*) from proj;

Rem Load data into EMP and PROJ table from EMP_PROJ_XT table
Rem using MULTI-TABLE INSERT
insert all
when 1=1 then
  into emp (empno, ename, deptno, mgr)
  values (empno, ename, deptno, mgr)
when proj1 is not null then
  into proj (empno, projid)
  values (empno, proj1)
when proj2 is not null then
  into proj (empno, projid)
  values (empno, proj2)
when proj3 is not null then
  into proj (empno, projid)
  values (empno, proj3)
select * from emp_proj_XT;

Rem Verify that data has been loaded correctly
select * from emp order by empno;
select * from proj order by 1, 2;

Rem Cleanup
drop table emp;
drop table proj;
drop table emp_proj_XT;
drop directory def_dir;
