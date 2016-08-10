Rem
Rem $Header: xtdemo01.sql 07-jun-2001.14:03:36 rsahani Exp $
Rem
Rem xtdemo01.sql
Rem
Rem  Copyright (c) Oracle Corporation 2001. All Rights Reserved.
Rem
Rem    NAME
Rem      xtdemo01.sql - External Table Demo 1
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

Rem Demo 1: External Table - Using variable-length data
Rem ######################
Rem This demo corresponds to sql*ldr demo Case 1 in rdbms/demo/ulcase.sh
Rem - FIELDS TERMINATED BY specifies that data is terminated by commas,
Rem   but may also be enclosed by quotation marks
Rem - datatypes for all fields defaults to CHAR
Rem ######################

Rem Create Directory
@xtsetup.sql

Rem Drop and create external table
drop table xtdemo01;
CREATE TABLE xtdemo01
(
  DEPTNO NUMBER(2),
  DNAME CHAR(14),
  LOC CHAR(13)
)
ORGANIZATION external
(
  TYPE oracle_loader
  DEFAULT DIRECTORY def_dir
  ACCESS PARAMETERS
  (
    RECORDS DELIMITED BY NEWLINE CHARACTERSET WE8DEC
    BADFILE 'deptXT.bad'
    LOGFILE 'deptXT.log'
    FIELDS TERMINATED BY "," OPTIONALLY ENCLOSED BY '"' LDRTRIM
  )
  location ('xtdemo01.dat')
)
REJECT LIMIT UNLIMITED;

Rem Query external table
select * from xtdemo01 order by deptno;

Rem Cleanup
drop table xtdemo01;
drop directory def_dir;
