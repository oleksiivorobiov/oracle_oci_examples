-- Copyright (c) 1991, 2004 Oracle. All rights reserved.
-- NAME
-- ulcase7.ctl - Extracting Data From a Formatted Report
--
-- DESCRIPTION
-- This case study demonstrates the following:
-- Use of SQL*Loader with an INSERT trigger.
--
-- Use of the SQL string to manipulate data.
--
-- Use of different initial and trailing delimiters.
--
-- Use of SYSDATE.
--
-- Use of the TRAILING NULLCOLS clause.
--
-- Ambiguous field length warnings.
--
-- Use of a discard file.
--
-- TO RUN THIS CASE STUDY:
-- 1. Before executing this control file, log in to SQL*Plus as
--    scott/tiger. Enter @ulcase7s to execute the SQL script for
--    this case study. This creates a BEFORE INSERT trigger that
--    is required to fill in the department number, job name, 
--    and manager's number when these fields are not present on 
--    a data line. When values are present, they should be saved 
--    in a global variable. When values are not present, the 
--    global variables are used.
--      
-- 2. At the system prompt, invoke the case study as follows:
-- sqlldr USERID=scott/tiger CONTROL=ulcase7.ctl LOG=ulcase7.log
--
-- 3. After you have run the case study and finished with it, you 
--    must run the ulcase7e.sql script before you can successfully
--    run other case studies. This script drops the INSERT trigger
--    and the global variables package. Log in to SQL*Plus as
--    scott/tiger. Enter @ulcase7e.
--
-- NOTES ABOUT THIS CONTROL FILE
-- The WHEN (57) = '.' clause indicates that the decimal point 
-- in column 57 (the salary field) identifies a line with data 
-- on it. All other lines in the report are discarded.
--
-- The TRAILING NULLCOLS clause causes SQL*Loader to treat any fields 
-- that are missing at the end of a record as null. Because the 
-- commission field is not present for every record, this clause says 
-- to load a null commission instead of rejecting the record when only 
-- seven fields are found instead of the expected eight.
--
-- The  hiredate is filled in using the current system date (SYSDATE).
--
-- The specification for deptno will generate a warning message in
-- the log file because the specified length does not agree with 
-- the length determined by the field's position. The specified 
-- length (3) is used. The length is in bytes with the default 
-- byte-length semantics. If character-length semantics were used 
-- instead, this length would be in characters.
--
-- The NULLIF clause says that because the report shows only department 
-- number, job, and manager when the value changes, these fields may 
-- be blank. This control file causes them to be loaded as null, and 
-- an insert trigger fills in the last valid value.
--
-- For the job field, the SQL string changes the job name to 
-- uppercase letters.
--
-- For the mgr field, it is necessary to specify starting position. 
-- If the job field and the manager field were both blank, then the 
-- job field's TERMINATED BY WHITESPACE clause would cause SQL*Loader 
-- to scan forward to the employee name field. Without the POSITION 
-- clause, the employee name field would be mistakenly interpreted 
-- as the manager field.
--
-- For the sal field, the SQL string translates the field from a 
-- formatted character string into a number. The numeric value takes 
-- less space and can be printed with a variety of formatting options.
--
-- For the comm field, different initial and trailing delimiters pick the 
-- numeric value out of a formatted field. The SQL string then converts 
-- the value to its stored form.
--
LOAD DATA
INFILE 'ulcase7.dat'
DISCARDFILE 'ulcase7.dsc'
APPEND
INTO TABLE emp
  WHEN (57)='.'
  TRAILING NULLCOLS
  (hiredate SYSDATE,
   deptno   POSITION(1:2)  INTEGER EXTERNAL(3)  
            NULLIF deptno=BLANKS,
   job      POSITION(7:14)   CHAR  TERMINATED BY WHITESPACE  
            NULLIF job=BLANKS  "UPPER(:job)",
   mgr      POSITION(28:31)  INTEGER EXTERNAL TERMINATED BY WHITESPACE 
            NULLIF mgr=BLANKS,
   ename    POSITION (34:41) CHAR  TERMINATED BY WHITESPACE  
            "UPPER(:ename)",
   empno    INTEGER EXTERNAL  TERMINATED BY WHITESPACE,
   sal      POSITION(51)  CHAR  TERMINATED BY WHITESPACE
            "TO_NUMBER(:sal,'$99,999.99')",
   comm     INTEGER EXTERNAL  ENCLOSED BY '(' AND '%'  
            ":comm * 100"
  )
