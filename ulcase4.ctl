-- Copyright (c) 1991, 2004 Oracle.  All rights reserved.
-- NAME
-- ulcase4.ctl - SQL*Loader Case Study 4: Loading Combined Physical Records
--
-- DESCRIPTION
-- This case study demonstrates the following:
-- Combining multiple physical records to form one logical 
-- record with CONTINUEIF.
-- 
-- Inserting negative numbers.
-- 
-- Using REPLACE to indicate that the table should be emptied 
-- before the new data is inserted.
--
-- Specifying a discard file in the control file using DISCARDFILE.
--
-- Specifying a maximum number of discards using DISCARDMAX.
--
-- Rejecting records due to duplicate values in a unique index 
-- or due to invalid data values.
-- 
-- TO RUN THIS CASE STUDY:
-- 1. Before executing this control file, log in to SQL*Plus as
--    scott/tiger. Enter @ulcase4 to execute the SQL script for
--    this case study. This prepares and populates tables and
--    then returns you to the system prompt.
--      
-- 2. At the system prompt, invoke the case study as follows:
-- sqlldr USERID=scott/tiger CONTROL=ulcase4.ctl LOG=ulcase4.log
--
-- NOTES ABOUT THIS CONTROL FILE
-- DISCARDFILE specifies a discard file named ulcase4.dsc.
--
-- DISCARDMAX specifies a maximum of 999 discards allowed before 
-- terminating the run. For all practical purposes, this allows 
-- all discards for this test case. In real-world situations, 
-- there may well be more than 999 discarded records.
--
-- REPLACE specifies that if there is data in the table being loaded, 
-- then SQL*Loader should delete that data before loading new data.
--
-- CONTINUEIF specifies that if an asterisk is found in column 1 
-- of the current record, then the next physical record after that 
-- record should be appended to it from the logical record. Note that 
-- column 1 in each physical record should then contain either an 
-- asterisk or a nondata value.
--
-- The data file (ulcase4.dat) for this case study shows asterisks 
-- in the first position and, though not visible, a newline character 
-- is in position 20. Note that clark's commission is -10, and 
-- SQL*Loader loads the value, converting it to a negative number.
--
-- The resulting log file will show that the last two records are
-- rejected, given two assumptions. If a unique index is created on 
-- column empno, then the record for chin will be rejected because 
-- his empno is identical to chan's. If empno is defined as NOT NULL, 
-- then chen's record will be rejected because it has no value for 
-- empno.

-- 
LOAD DATA
INFILE "ulcase4.dat"
DISCARDFILE "ulcase4.dsc"	
DISCARDMAX 999	
REPLACE
CONTINUEIF (1) = '*'
INTO TABLE EMP 

( EMPNO    POSITION(01:04) INTEGER EXTERNAL,
  ENAME    POSITION(06:15) CHAR,
  JOB      POSITION(17:25) CHAR,
  MGR      POSITION(27:30) INTEGER EXTERNAL,
  SAL      POSITION(32:39) DECIMAL EXTERNAL,
  COMM     POSITION(41:48) DECIMAL EXTERNAL,
  DEPTNO   POSITION(50:51) INTEGER EXTERNAL,
  HIREDATE POSITION(52:60) INTEGER EXTERNAL)
