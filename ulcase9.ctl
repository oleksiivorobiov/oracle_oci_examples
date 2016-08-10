-- Copyright (c) 1991, 2004 Oracle.  All rights reserved.
-- NAME
-- ulcase9.ctl - SQL*Loader Case Study 9: Loading LOBFILEs (CLOBs)
--
-- DESCRIPTION
-- This case study demonstrates the following:
-- Adding a CLOB column called resume to table emp.
--
-- Using a filler field (res_file).
--
-- Loading multiple LOBFILEs into the emp table.
--
-- TO RUN THIS CASE STUDY:
-- 1. Before executing this control file, log in to SQL*Plus as
--    scott/tiger. Enter @ulcase9 to execute the SQL script for
--    this case study. This prepares and populates tables and
--    then returns you to the system prompt.
--      
-- 2. At the system prompt, invoke the case study as follows:
-- sqlldr USERID=scott/tiger CONTROL=ulcase9.ctl LOG=ulcase9.log
--
-- NOTES ABOUT THIS CONTROL FILE
-- This is an example of using SQL Loader to load LOBs from 
-- secondary data file.
--
-- There is one file per resume (the "TERMINATED BY EOF" clause 
-- indicates this) and the name of the file containing the resume 
-- is in field res_file.
--
-- res_file is a filler field. The filler field is assigned values
-- from the data field to which it is mapped. This means that the
-- file name stored in the field is not loaded into any field in
-- the table.
--
-- The resume column is loaded as a CLOB. The LOBFILE function specifies
-- the field name in which the name of the file that contains data for
-- LOB field is provided.
--
-- The field name for column RESUME is in quotation marks because
-- RESUME is also a keyword for SQL*Loader. The quotation marks force 
-- SQL*Loader to treat it as a column name instead.
--
LOAD DATA
INFILE *
INTO TABLE EMP
REPLACE
FIELDS TERMINATED BY ','
( EMPNO    INTEGER EXTERNAL,
  ENAME    CHAR,
  JOB      CHAR,
  MGR      INTEGER EXTERNAL,
  SAL      DECIMAL EXTERNAL,
  COMM     DECIMAL EXTERNAL,
  DEPTNO   INTEGER EXTERNAL,
  RES_FILE FILLER CHAR,
  "RESUME" LOBFILE (RES_FILE) TERMINATED BY EOF NULLIF RES_FILE = 'NONE'
)

BEGINDATA
7782,CLARK,MANAGER,7839,2572.50,,10,ulcase91.dat
7839,KING,PRESIDENT,,5500.00,,10,ulcase92.dat
7934,MILLER,CLERK,7782,920.00,,10,ulcase93.dat
7566,JONES,MANAGER,7839,3123.75,,20,ulcase94.dat
7499,ALLEN,SALESMAN,7698,1600.00,300.00,30,ulcase95.dat
7654,MARTIN,SALESMAN,7698,1312.50,1400.00,30,ulcase96.dat
7658,CHAN,ANALYST,7566,3450.00,,20,NONE
