-- Copyright (c) 1991, 2004 Oracle.  All rights reserved.
-- NAME
-- ulcase10.ctl - SQL*Loader Case Study 10: Loading REF Fields and VARRAYs
--
-- DESCRIPTION
-- This case study demonstrates the following:
--
-- Loading a customer table that has a primary key as its OID and 
-- stores order items in a VARRAY.
--
-- Loading an order table that has a reference to the customer table and 
-- the order items in a VARRAY.
--
-- TO RUN THIS CASE STUDY:
-- 1. Before executing this control file, log in to SQL*Plus as
--    scott/tiger. Enter @ulcase10 to execute the SQL script for
--    this case study. This prepares and populates tables and
--    then returns you to the system prompt.
--      
-- 2. At the system prompt, invoke the case study as follows:
-- sqlldr USERID=scott/tiger CONTROL=ulcase1.ctl0 LOG=ulcase10.log
--
-- NOTES ABOUT THIS CONTROL FILE
-- cust_no and item_list_count are FILLER fields. The FILLER field is 
-- assigned values from the data field to which it is mapped. 
--
-- The cust field is created as a REF field. 
--
-- item_list is stored in a VARRAY.
--
-- The second occurrence of item_list identifies the datatype of each
-- element of the VARRAY. Here, the datatype is COLUMN OBJECT.
--
-- The listing of item, cnt, price shows all attributes of the column
-- object that are loaded for the VARRAY. The list is enclosed in parentheses.
--
-- The data is contained in the control file and is preceded by the
-- BEGINDATA parameter.
--
LOAD DATA
INFILE * 
CONTINUEIF THIS (1) = '*'

INTO TABLE customers
REPLACE
FIELDS TERMINATED BY ","
(
  cust_no                       CHAR,
  name                          CHAR,
  addr                          CHAR
)

INTO TABLE orders
REPLACE
FIELDS TERMINATED BY ","
(
  order_no                      CHAR,
  cust_no             FILLER    CHAR,
  cust                          REF (CONSTANT 'CUSTOMERS', cust_no),
  item_list_count     FILLER    CHAR,
  item_list                     VARRAY COUNT (item_list_count)
  (
    item_list                   COLUMN OBJECT
    (
      item                      CHAR,
      cnt                       CHAR,
      price                     CHAR
    )
  )  
)

BEGINDATA
*00001,Spacely Sprockets,15 Space Way,
*00101,00001,2,
*Sprocket clips, 10000, .01,
 Sprocket cleaner, 10, 14.00
*00002,Cogswell Cogs,12 Cogswell Lane,
*00100,00002,4,
*one quarter inch cogs,1000,.02,
*one half inch cog, 150, .04,
*one inch cog, 75, .10,
 Custom coffee mugs, 10, 2.50
