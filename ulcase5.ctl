-- Copyright (c) 1991, 2004 Oracle.  All rights reserved.
-- NAME
-- ulcase5.ctl - SQL*Loader Case Study 5: Loading Data Into Multiple Tables
-- DESCRIPTION
-- This case study demonstrates the following:
-- Loading multiple tables.
--
-- Using SQL*Loader to break down repeating groups in a flat file 
-- and to load the data into normalized tables. In this way, one 
-- file record may generate multiple database rows.
-- 
-- Deriving multiple logical records from each physical record.
--
-- Using a WHEN clause.
--
-- Loading the same field (empno) into multiple tables.
--
-- TO RUN THIS CASE STUDY:
-- 1. Before executing this control file, log in to SQL*Plus as
--    scott/tiger. Enter @ulcase5 to execute the SQL script for
--    this case study. This prepares and populates tables and
--    then returns you to the system prompt.
--      
-- 2. At the system prompt, invoke the case study as follows:
-- sqlldr USERID=scott/tiger CONTROL=ulcase5.ctl LOG=ulcase5.log
--
-- NOTES ABOUT THIS CONTROL FILE
-- REPLACE specifies that if there is data in the tables to be 
-- loaded (emp and proj), SQL*loader should delete the data before 
-- loading new rows.
--
-- Multiple INTO TABLE clauses load two tables, emp and proj. 
-- The same set of records is processed three times, using different 
-- combinations of columns each time to load table proj.
--
-- WHEN loads only rows with nonblank project numbers. When projno 
-- is defined as columns 25...27, rows are inserted into proj only 
-- if there is a value in those columns.
--
-- When projno is defined as columns 29...31, rows are inserted 
-- into proj only if there is a value in those columns.
--
-- When projno is defined as columns 33...35, rows are inserted 
-- into proj only if there is a value in those columns. 
--
LOAD DATA
INFILE 'ulcase5.dat'
BADFILE 'ulcase5.bad'
DISCARDFILE 'ulcase5.dsc'
REPLACE

INTO TABLE EMP
  (EMPNO    POSITION(1:4)   INTEGER EXTERNAL,
   ENAME    POSITION(6:15)  CHAR,
   DEPTNO   POSITION(17:18) CHAR,
   MGR      POSITION(20:23) INTEGER EXTERNAL)

INTO TABLE PROJ
-- PROJ has two columns, both not null: EMPNO and PROJNO
WHEN PROJNO != '   '
  (EMPNO    POSITION(1:4)   INTEGER EXTERNAL,
   PROJNO   POSITION(25:27) INTEGER EXTERNAL)   -- 1st proj

INTO TABLE PROJ
WHEN PROJNO != '   '
  (EMPNO    POSITION(1:4)   INTEGER EXTERNAL,
   PROJNO   POSITION(29:31) INTEGER EXTERNAL)   -- 2nd proj

INTO TABLE PROJ
WHEN PROJNO != '   '
  (EMPNO    POSITION(1:4)   INTEGER EXTERNAL,
   PROJNO   POSITION(33:35) INTEGER EXTERNAL)   -- 3rd proj	
