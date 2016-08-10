-- Copyright (c) 1991, 2004 Oracle.  All rights reserved.
-- NAME
-- ulcase3.ctl - SQL*Loader Case Study 3: Loading a Delimited, 
-- Free-format File
--
-- DESCRIPTION
-- This case study demonstrates the following:
-- Loading data (enclosed and terminated) in stream format.
--
-- Loading dates using the DATE datatype.
--
-- Using SEQUENCE numbers to generate unique keys for loaded data.
--
-- Using APPEND to indicate that the table need not be empty before 
-- inserting new records.
--
-- Using comments in the control file set off by two hyphens.
--
-- TO RUN THIS CASE STUDY:
-- 1. Before executing this control file, log in to SQL*Plus as
--    scott/tiger. Enter @ulcase3 to execute the SQL script for
--    this case study. This prepares and populates tables, adds 
--    the projno and loadseq columns to table emp, and then returns
--    you to the system prompt.
--      
-- 2. At the system prompt, invoke the case study as follows:
-- sqlldr USERID=scott/tiger CONTROL=ulcase3.ctl LOG=ulcase3.log
--
-- NOTES ABOUT THIS CONTROL FILE
-- This control file loads the same table as in case 2, but it 
-- loads three additional columns (hiredate, projno, and loadseq).
-- The projno and loadseq columns are added to the emp table when
-- you run the ulcase3.sql script.
--
-- INFILE * specifies that the data is found at the end of the 
-- control file.
--
-- APPEND specifies that the data can be loaded even if the table
-- already contains rows. That is, the table need not be empty.
--
-- The default terminator for the data fields is a comma, and some
-- fields may be enclosed by double quotation marks (").
--
-- The data to be loaded into column hiredate appears in the format 
-- DD-Month-YYYY. The length of the date field is specified to have 
-- a maximum of 20. The maximum length is in bytes, with default 
-- byte-length semantics. If character-length semantics were used 
-- instead, the length would be in characters. If a length is not 
-- specified, then the length depends on the length of the date mask.
--
-- The SEQUENCE function generates a unique value in the column loadseq. 
-- This function finds the current maximum value in column loadseq and 
-- adds the increment (1) to it to obtain the value for loadseq for 
-- each row inserted.
--
-- BEGINDATA specifies the end of the control information and the 
-- beginning of the data.
--
-- Although each physical record equals one logical record, the fields
-- vary in length, so that some records are longer than others. Note
-- also that several rows have null values for comm.

LOAD DATA 
INFILE *
APPEND

INTO TABLE EMP
FIELDS TERMINATED BY "," OPTIONALLY ENCLOSED BY '"'	
(empno, ename, job, mgr,
 hiredate DATE(20) "DD-Month-YYYY",
 sal, comm,
 deptno   CHAR TERMINATED BY ':',
 projno,
 loadseq  SEQUENCE(MAX,1) )		

BEGINDATA
7782, "Clark", "Manager", 7839, 09-June-1981, 2572.50,, 10:101
7839, "King", "President", , 17-November-1981, 5500.00,, 10:102
7934, "Miller", "Clerk", 7782, 23-January-1982, 920.00,, 10:102
7566, "Jones", "Manager", 7839, 02-April-1981, 3123.75,, 20:101
7499, "Allen", "Salesman", 7698, 20-February-1981, 1600.00, 300.00, 30:103
7654, "Martin", "Salesman", 7698, 28-September-1981, 1312.50, 1400.00, 30:103
7658, "Chan", "Analyst", 7566, 03-May-1982, 3450,, 20:101
