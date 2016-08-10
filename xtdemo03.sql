Rem
Rem $Header: xtdemo03.sql 07-jun-2001.14:03:38 rsahani Exp $
Rem
Rem xtdemo03.sql
Rem
Rem  Copyright (c) Oracle Corporation 2001. All Rights Reserved.
Rem
Rem    NAME
Rem      xtdemo03.sql - External Table Demo 3
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
SET LINESIZE 108
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 200
SET ECHO ON

connect sys/knl_test7 as sysdba

Rem Demo 3: External Table - Using a Delimited, Free-Format File
Rem ######################
Rem This demo corresponds to sql*ldr demo Case 3 in rdbms/demo/ulcase.sh
Rem - general specifications are overidden with declarations for 
Rem   individual fields:
Rem   some data is enclosed in quotation marks, some is set off by commas
Rem   and the values for DEPTNO and PROJNO are separated by colon
Rem - %a (agent number), %p (process number) is appended to 
Rem   BAD filename and LOG filename
Rem ######################

Rem Create Directory
@xtsetup.sql

Rem Drop and create external table
drop table xtdemo03;
CREATE TABLE xtdemo03
(
  EMPNO NUMBER(4),
  ENAME CHAR(10),
  JOB CHAR(9),
  MGR NUMBER(4),
  HIREDATE DATE,
  SAL NUMBER(7,2),
  COMM NUMBER(7,2),
  DEPTNO NUMBER(2),
  PROJNO NUMBER
)
ORGANIZATION external
(
  TYPE oracle_loader
  DEFAULT DIRECTORY def_dir
  ACCESS PARAMETERS
  (
    RECORDS DELIMITED BY NEWLINE CHARACTERSET WE8DEC
    BADFILE 'empXT_%a_%p.bad' 
    LOGFILE 'empXT_%a_%p.log'
    FIELDS TERMINATED BY "," OPTIONALLY ENCLOSED BY '"' LDRTRIM
    (
      EMPNO CHAR(255),
      ENAME CHAR(255),
      JOB CHAR(255),
      MGR CHAR(255),
      HIREDATE CHAR(20)
        DATE_FORMAT DATE MASK "DD-Month-YYYY",
      SAL CHAR(255),
      COMM CHAR(255),
      DEPTNO CHAR(255)
        TERMINATED BY ":" OPTIONALLY ENCLOSED BY '"',
      PROJNO CHAR(255)
    )
  )
  location ('xtdemo03.dat')
)
REJECT LIMIT UNLIMITED;

Rem Query external table
select * from xtdemo03 order by empno;

Rem Cleanup
drop table xtdemo03;
drop directory def_dir;

