-- Copyright (c) 1991, 2004 Oracle.  All rights reserved.
-- NAME
-- ulcase6.ctl - SQL*Loader Case Study 6: Loading Data Using the 
-- Direct Path Load Method
--
-- DESCRIPTION
-- This case study demonstrates the following:
-- Use of the direct path load method to load and index data.
--
-- How to specify the indexes for which the data is presorted.
--
-- Use of the NULLIF clause.
--
-- Loading all-blank numeric fields as NULL.
--   
-- TO RUN THIS CASE STUDY:
-- 1. Before executing this control file, log in to SQL*Plus as
--    scott/tiger. Enter @ulcase6 to execute the SQL script for
--    this case study. This prepares and populates tables and
--    then returns you to the system prompt.
--      
-- 2. At the system prompt, invoke the case study as follows:
-- sqlldr USERID=scott/tiger CONTROL=ulcase6.ctl LOG=ulcase6.log DIRECT=TRUE
--
-- NOTES ABOUT THIS CONTROL FILE
-- The SORTED INDEXES statement identifies the indexes on which 
-- the data is sorted. This statement indicates that the datafile 
-- is sorted on the columns in the empix index. It allows 
-- SQL*Loader to optimize index creation by eliminating the sort 
-- phase for this data when using the direct path load method.
--
-- The NULLIF...BLANKS clause specifies that the column should 
-- be loaded as NULL if the field in the datafile consists of 
-- all blanks. 
-- 
LOAD DATA
INFILE 'ulcase6.dat'
REPLACE
INTO TABLE emp
SORTED INDEXES (empix)
(empno POSITION(1:4),
ename POSITION(6:15),
job POSITION(17:25),
mgr POSITION(27:30) NULLIF mgr=blanks,
sal POSITION(32:39) NULLIF sal=blanks,
comm POSITION(41:48) NULLIF comm=blanks,
deptno POSITION(50:51) NULLIF empno=blanks)
