rem
rem $Header: cdemdplp.sql 04-apr-2001.11:17:49 eegolf Exp $
rem
rem cdemdplp.sql
rem
rem Copyright (c) 2001 Oracle Corporation.  All rights reserved.
rem
rem    NAME
rem      cdemdplp.sql - C Demo Direct Path api for LineItem Partition table
rem
rem    DESCRIPTION
rem      - creates a lineitem partition tbl for loading data w/ direct path api
rem        via direct path API.
rem      - execute this script before running cdemodp driver
rem        w/cdemdplp client.
rem    NOTES
rem
rem    MODIFIED   (MM/DD/YY)
rem    eegolf      04/04/01 - Merged eegolf_demo_update
rem    eegolf      03/04/01 - updated for 9i
rem    cmlim       09/16/98 - Created
rem

set echo off;
connect scott/tiger;

rem     for direct path
drop table LINEITEM_DP;
create table LINEITEM_DP
 (L_ORDERKEY                               NUMBER,
  L_PARTKEY                                NUMBER,
  L_SUPPKEY                                NUMBER,
  L_LINENUMBER                             NUMBER,
  L_QUANTITY                               NUMBER,
  L_EXTENDEDPRICE                          NUMBER,
  L_DISCOUNT                               NUMBER,
  L_TAX                                    NUMBER,
  L_RETURNFLAG                             CHAR(1),
  L_LINESTATUS                             CHAR(1),
  L_SHIPDATE                               DATE,
  L_COMMITDATE                             DATE,
  L_RECEIPTDATE                            DATE,
  L_SHIPINSTRUCT                           VARCHAR2(25),
  L_SHIPMODE                               VARCHAR2(10),
  L_COMMENT                                VARCHAR2(44))
  partition by range (L_ORDERKEY)
  (partition LINEITEM1 values less than (12250)    ,
   partition LINEITEM2 values less than (24500)    ,
   partition LINEITEM3 values less than (36750)    ,
   partition LINEITEM4 values less than (49000)    ,
   partition LINEITEM5 values less than (61250)    ,
   partition LINEITEM6 values less than (73500)    ,
   partition LINEITEM7 values less than (maxvalue) );

rem     for conventional path
drop table LINEITEM_CV;
create table LINEITEM_CV
 (L_ORDERKEY                               NUMBER,
  L_PARTKEY                                NUMBER,
  L_SUPPKEY                                NUMBER,
  L_LINENUMBER                             NUMBER,
  L_QUANTITY                               NUMBER,
  L_EXTENDEDPRICE                          NUMBER,
  L_DISCOUNT                               NUMBER,
  L_TAX                                    NUMBER,
  L_RETURNFLAG                             CHAR(1),
  L_LINESTATUS                             CHAR(1),
  L_SHIPDATE                               DATE,
  L_COMMITDATE                             DATE,
  L_RECEIPTDATE                            DATE,
  L_SHIPINSTRUCT                           VARCHAR2(25),
  L_SHIPMODE                               VARCHAR2(10),
  L_COMMENT                                VARCHAR2(44))
  partition by range (L_ORDERKEY)
  (partition LINEITEM1 values less than (12250)    ,
   partition LINEITEM2 values less than (24500)    ,
   partition LINEITEM3 values less than (36750)    ,
   partition LINEITEM4 values less than (49000)    ,
   partition LINEITEM5 values less than (61250)    ,
   partition LINEITEM6 values less than (73500)    ,
   partition LINEITEM7 values less than (maxvalue) );

exit;
