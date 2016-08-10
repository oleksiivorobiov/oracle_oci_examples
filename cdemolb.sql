rem
rem $Header: cdemolb.sql 14-jul-99.13:52:13 mjaeger Exp $
rem
rem cdemolb.sql
rem
rem Copyright (c) 1996, 1999, Oracle Corporation.  All rights reserved.
rem
rem    NAME
rem      cdemolb.sql - <one-line expansion of the name>
rem
rem    DESCRIPTION
rem      <short description of component this file declares/defines>
rem
rem    NOTES
rem      <other useful comments, qualifications, etc.>
rem
rem    MODIFIED   (MM/DD/YY)
rem    mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem    aroy        07/22/96 - SQL program to create tables for cdemolb
rem    aroy        07/22/96 - Created
rem

connect scott/tiger;
DROP TABLE CLBTAB;
CREATE TABLE CLBTAB (name char(20), essay CLOB);
exit;

