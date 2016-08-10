-- Copyright (c) 1991, 2004 Oracle.  All rights reserved.
-- NAME
-- ulcase11.ctl - SQL*Loader Case Study 11: Load Data in the Unicode
--                Character Set UTF-16
--
-- DESCRIPTION
-- This case study demonstrates the following:
-- Using SQL*Loader to load data in the Unicode character set, UTF16.
--
-- Using SQL*Loader to load data in a fixed-width, multibyte character set.
--
-- Using character-length semantics.
--
-- Using SQL*Loader to load data in little-endian byte order. SQL*Loader 
-- checks the byte order of the system on which it is running. If necessary, 
-- SQL*Loader swaps the byte order of the data to ensure that any 
-- byte-order-dependent data is correctly loaded.
--
-- TO RUN THIS CASE STUDY:
-- 1. Before executing this control file, log in to SQL*Plus as
--    scott/tiger. Enter @ulcase11 to execute the SQL script for
--    this case study. This prepares and populates tables and
--    then returns you to the system prompt.
--      
-- 2. At the system prompt, invoke the case study as follows:
-- sqlldr USERID=scott/tiger CONTROL=ulcase11.ctl LOG=ulcase11.log
-- 
-- NOTES ABOUT THIS CONTROL FILE
--
-- The character set specified with the CHARACTERSET keyword is UTF16. 
-- SQL*Loader will convert the data from the UTF16 character set to 
-- the database character set. Because UTF16 is specified as the 
-- character set, character-length semantics are used for the load.
--
-- BYTEORDER LITTLE tells SQL*Loader that the data in the datafile is 
-- in little-endian byte order. SQL*Loader checks the byte order of the 
-- system on which it is running to determine if any byte-swapping is 
-- necessary. In this example, all the character data in UTF16 is 
-- byte-order dependent.
--
-- The TERMINATED BY and OPTIONALLY ENCLOSED BY clauses both specify 
-- hexadecimal strings. The X'002c' is the encoding for a comma (,) in 
-- UTF-16 big-endian format. The X'0022' is the encoding for a double 
-- quotation mark (") in big-endian format. Because the datafile is in 
-- little-endian format, SQL*Loader swaps the bytes before checking for 
-- a match. If these clauses were specified as character strings instead 
-- of hexadecimal strings, SQL*Loader would convert the strings to the 
-- datafile character set (UTF16) and byte-swap as needed before checking 
-- for a match.
--
-- Because character-length semantics are used, the maximum length for 
-- the empno, hiredate, and deptno fields is interpreted as characters, 
-- not bytes.
--
-- The TERMINATED BY clause for the deptno field is specified using the 
-- character string ":". SQL*Loader converts the string to the datafile 
-- character set (UTF16) and byte-swaps as needed before checking for a match.

LOAD DATA 
CHARACTERSET utf16
BYTEORDER little
INFILE ulcase11.dat
REPLACE

INTO TABLE EMP
FIELDS TERMINATED BY X'002c' OPTIONALLY ENCLOSED BY X'0022'
(empno integer external (5), ename, job, mgr,
 hiredate DATE(20) "DD-Month-YYYY",
 sal, comm,
 deptno   CHAR(5) TERMINATED BY ":",
 projno,
 loadseq  SEQUENCE(MAX,1) )		
