-- Copyright (c) 1991, 2004 Oracle.  All rights reserved. 
-- NAME
-- ulcase2.ctl - SQL*Loader Case Study 2: Loading Fixed-Format Files
--
-- DESCRIPTION
-- This control file demonstrates the following:
-- Use of a separate data file.
--
-- Data conversions.
--   
-- TO RUN THIS CASE STUDY:
-- 1. Before executing this control file, log in to SQL*Plus as
--    scott/tiger. Enter @ulcase1 to execute the SQL script for
--    this case study. This prepares and populates tables and
--    then returns you to the system prompt. It is the same script
--    used to prepare for case study 1, so if you have already
--    run case study 1, you can skip this step.
--      
-- 2. At the system prompt, invoke the case study as follows:
-- sqlldr USERID=scott/tiger CONTROL=ulcase2.ctl LOG=ulcase2.log
--
-- NOTES ABOUT THIS CONTROL FILE
-- The LOAD DATA statement is required at the beginning of the 
-- control file.
--
-- The name of the file containing data follows the INFILE parameter.
--
-- The INTO TABLE statement is required to identify the table to 
-- be loaded into.
--
-- empno, ename, job, and so on are names of columns in table emp. 
-- The datatypes (INTEGER EXTERNAL, CHAR, DECIMAL EXTERNAL) identify 
-- the datatype of data fields in the file, not of corresponding 
-- columns in the emp table.
--
-- Note that the set of column specifications is enclosed in 
-- parentheses.
--
-- Records loaded in this example from the emp table contain 
-- department numbers. Unless the dept table is loaded first, 
-- referential integrity checking rejects these records (if 
-- referential integrity constraints are enabled for the emp table). 

-- 
LOAD DATA
INFILE 'ulcase2.dat'
INTO TABLE EMP 

( EMPNO    POSITION(01:04) INTEGER EXTERNAL,
  ENAME    POSITION(06:15) CHAR,
  JOB      POSITION(17:25) CHAR,
  MGR      POSITION(27:30) INTEGER EXTERNAL,
  SAL      POSITION(32:39) DECIMAL EXTERNAL,
  COMM     POSITION(41:48) DECIMAL EXTERNAL,
  DEPTNO   POSITION(50:51) INTEGER EXTERNAL)

