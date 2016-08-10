rem
Rem $Header: ulcase8.sql 20-jul-99.18:20:38 cmlim Exp $
rem
rem ulcase8.sql
rem
rem Copyright (c) 1998, 1999, Oracle Corporation.  All rights reserved.
rem
rem    NAME
rem      ulcase8.sql - Setup for SQL Loader example 8
rem
rem    DESCRIPTION
rem      Create partitioned table for example 8
rem
rem    NOTES
rem      Note that all partitions are created in the default tablespace.
rem      Normally, each partition would be in a saparate tablespace, but we
rem      use the same tablespace to keep the example simple.
rem
rem    MODIFIED   (MM/DD/YY)
Rem    cmlim       07/20/99 - fix syntax for create table
rem    mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem    jstenois    06/17/99 - cleanup tables before load and show feedback
rem    jstenois    11/06/98 - example of loading fix record length file
rem    jstenois    11/06/98 - Created
rem

set termout off

rem host write sys$output "Building case 8 demonstration tables.  Please wait"

drop table lineitem;

create table lineitem
(l_orderkey     number,
l_partkey       number,
l_suppkey       number,
l_linenumber    number,
l_quantity      number,
l_extendedprice number,
l_discount      number,
l_tax           number,
l_returnflag    char,
l_linestatus    char,
l_shipdate      date,
l_commitdate    date,
l_receiptdate   date,
l_shipinstruct  char(17),
l_shipmode      char(7),
l_comment       char(43))
partition by range (l_shipdate)
(
partition ship_q1 values less than (TO_DATE('01-APR-1996', 'DD-MON-YYYY')),
partition ship_q2 values less than (TO_DATE('01-JUL-1996', 'DD-MON-YYYY')),
partition ship_q3 values less than (TO_DATE('01-OCT-1996', 'DD-MON-YYYY')),
partition ship_q4 values less than (TO_DATE('01-JAN-1997', 'DD-MON-YYYY'))
);

exit
