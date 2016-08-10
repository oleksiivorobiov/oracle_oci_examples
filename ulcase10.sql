rem
Rem $Header: ulcase10.sql 11-may-00.13:48:45 rpfau Exp $
rem
rem ulcase10.sql
rem
rem Copyright (c) 1998, 1999,, 2000 Oracle Corporation.  All rights reserved.
rem
rem    NAME
rem      ulcase10.sql - set up for SQL Loader example 10
rem
rem    DESCRIPTION
rem      Create tables need to for example of using SQL Loader to load
rem      VARRAYS and references
rem
rem    NOTES
rem      none
rem
rem    MODIFIED   (MM/DD/YY)
Rem    rpfau       05/11/00 - Remove slashes on create table commands so it 
Rem			      runs successfully using sqlplus.
Rem    cmlim       07/27/99 - add /
rem    mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem    jstenois    06/17/99 - cleanup tables before load and show feedback
rem    jstenois    10/26/98 - demo of 8.1 features for sqlldr
rem    jstenois    10/26/98 - Created
rem

rem host write sys$output "Building case 10 demonstration tables.  Please wait"

rem do all cleanup

drop table orders;
drop table customers;
drop type item_list_type;
drop type item_type;
drop type customer_type;

rem Create an ORDER record that has a VARRAY for the items that comprise the
rem order and has a reference field to a record in the CUSTOMER table for
rem the customer placing the order.

rem create customer type

create type customer_type as object (
  cust_no   char(5),
  name      char(20),
  addr      char(20)
);
/

rem create object table for customer type

create table customers of customer_type
        (primary key (cust_no))
        object id primary key;

rem create type for order items

create type item_type as object (
  item      varchar(50),
  cnt       number,
  price     number(7,2)
);
/

rem create varray type for order items

create type item_list_type as varray (1000) of item_type;
/

rem create orders table with varray for items and ref to object table

create table orders (
  order_no      char(5),
  cust          ref customer_type references customers,
  item_list     item_list_type
);

exit;
/
