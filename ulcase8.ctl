-- Copyright (c) 1991, 2004 Oracle.  All rights reserved.
-- NAME
-- ulcase8.ctl - SQL*Loader Case Study 8: Loading Partitioned Tables
--
-- DESCRIPTION
-- This case study demonstrates the following:
-- Partitioning of data.
--
-- Explicitly defined field positions and datatypes.
--
-- Loading data using the fixed-record-length option. 
--
-- TO RUN THIS CASE STUDY:
-- 1. Before executing this control file, log in to SQL*Plus as
--    scott/tiger. Enter @ulcase8 to execute the SQL script for
--    this case study. This prepares and populates tables and
--    then returns you to the system prompt.
--      
-- 2. At the system prompt, invoke the case study as follows:
-- sqlldr USERID=scott/tiger CONTROL=ulcase8.ctl LOG=ulcase8.log
--
-- NOTES ABOUT THIS CONTROL FILE
-- This control file loads the lineitem table with fixed-length
-- records, partitioning the data according to shipment date.
--
-- The INFILE clause specifies that each record in the datafile is 
-- of fixed length (129 bytes in this example).
--
-- The PARTITION clause identifies the column name and location of the
-- data in the datafile to be loaded into each column.
--
LOAD DATA
  INFILE 'ulcase8.dat' "fix 129"
BADFILE 'ulcase8.bad'
TRUNCATE
INTO TABLE lineitem
PARTITION (ship_q1)
  (l_orderkey      position    (1:6) char,
   l_partkey       position   (7:11) char,
   l_suppkey       position  (12:15) char,
   l_linenumber    position  (16:16) char,
   l_quantity      position  (17:18) char,
   l_extendedprice position  (19:26) char,
   l_discount      position  (27:29) char,
   l_tax           position  (30:32) char,
   l_returnflag    position  (33:33) char,
   l_linestatus    position  (34:34) char,
   l_shipdate      position  (35:43) char,
   l_commitdate    position  (44:52) char,
   l_receiptdate   position  (53:61) char,
   l_shipinstruct  position  (62:78) char,
   l_shipmode      position  (79:85) char,
   l_comment       position (86:128) char)
