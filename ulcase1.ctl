-- Copyright (c) 1991, 2004 Oracle.  All rights reserved.
-- NAME
-- ulcase1.ctl - SQL*Loader Case Study 1: Loading Variable-Length Data
--
-- DESCRIPTION
-- This case study demonstrates the following:
--
-- A simple control file identifying one table and three columns 
-- to be loaded.
--
-- Including data to be loaded from the control file itself, so 
-- there is no separate datafile.
--
-- Loading data in stream format, with both types of delimited 
-- fields: terminated and enclosed.
--
-- TO RUN THIS CASE STUDY:
-- 1. Before executing this control file, log in to SQL*Plus as
--    scott/tiger. Enter @ulcase1 to execute the SQL script for
--    this case study. This prepares and populates tables and
--    then returns you to the system prompt.
--      
-- 2. At the system prompt, invoke the case study as follows:
-- sqlldr USERID=scott/tiger CONTROL=ulcase1.ctl LOG=ulcase1.log
--
-- NOTES ABOUT THIS CONTROL FILE
-- The LOAD DATA statement is required at the beginning of the 
-- control file.
--
-- INFILE * specifies that the data is found in the control file 
-- and not in an external file.
--
-- The INTO TABLE statement is required to identify the table to 
-- be loaded (dept) into. By default, SQL*Loader requires the 
-- table to be empty before it inserts any records.
--
-- FIELDS TERMINATED BY specifies that the data is terminated by 
-- commas, but may also be enclosed by quotation marks. Datatypes 
-- for all fields default to CHAR.
--
-- The names of columns to load are enclosed in parentheses. 
-- If no datatype or length is specified and the field is delimited
-- with ENCLOSED BY or with TERMINATED BY, then the default
-- datatype is CHAR and the default length is 255. If ENCLOSED BY
-- or TERMINATED BY is not specified, then the default type is CHAR
-- and the default length is 1.
--
--  BEGINDATA specifies the beginning of the data.
-- 
LOAD DATA
INFILE * 
INTO TABLE DEPT	
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"'
(DEPTNO, DNAME, LOC)
BEGINDATA
12,RESEARCH,"SARATOGA" 	
10,"ACCOUNTING",CLEVELAND
11,"ART",SALEM
13,FINANCE,"BOSTON"
21,"SALES",PHILA.
22,"SALES",ROCHESTER
42,"INT'L","SAN FRAN"
