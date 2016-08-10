Rem
Rem $Header: xtdemo02.sql 07-jun-2001.14:03:36 rsahani Exp $
Rem
Rem xtdemo02.sql
Rem
Rem  Copyright (c) Oracle Corporation 2001. All Rights Reserved.
Rem
Rem    NAME
Rem      xtdemo02.sql - External Table Demo 2
Rem
Rem    DESCRIPTION
Rem      <short description of component this file declares/defines>
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    rsahani     06/07/01 - Merged rsahani_add_xt_demos_010605
Rem    rsahani     06/05/01 - Created
Rem

SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100
SET ECHO ON

connect sys/knl_test7 as sysdba

Rem Demo 2: External Table - Using fixed-format fields
Rem ######################
Rem This demo corresponds to sql*ldr demo Case 2 in rdbms/demo/ulcase.sh
Rem - POSITIONAL_SPEC is used to identify a column name and the location
Rem   of data in the datafile to be loaded into that column
Rem - the datatypes (integer external, char, decimal external) identify
Rem   the datatype of data fields in file and not of corresponding 
Rem   columns in the empXT table
Rem ######################

Rem Create directory
@xtsetup.sql

Rem Drop and Create external table
drop table empXT;
CREATE TABLE empXT 
(
  EMPNO NUMBER(4),
  ENAME VARCHAR2(10),
  JOB VARCHAR2(9),
  MGR NUMBER(4),
  SAL NUMBER(7,2),
  COMM NUMBER(7,2),
  DEPTNO NUMBER(2)
)
ORGANIZATION external
(
  TYPE oracle_loader
  DEFAULT DIRECTORY def_dir
  ACCESS PARAMETERS
  (
    RECORDS DELIMITED BY NEWLINE CHARACTERSET WE8DEC
    BADFILE def_dir:'empXT.bad'
    LOGFILE def_dir:'empXT.log'
    FIELDS LDRTRIM
    (
      EMPNO (1:4) INTEGER EXTERNAL(4),
      ENAME (6:15) CHAR(10),
      JOB (17:25) CHAR(9),
      MGR (27:30) INTEGER EXTERNAL(4),
      SAL (32:39) INTEGER EXTERNAL(8),
      COMM (41:48) INTEGER EXTERNAL(8),
      DEPTNO (50:51) INTEGER EXTERNAL(2)
    )
  )
  location (def_dir:'ulcase2.dat')
)
REJECT LIMIT UNLIMITED;

Rem Query External Table
select * from empXT order by empno;

Rem Cleanup
drop table empXT;
drop directory def_dir;
