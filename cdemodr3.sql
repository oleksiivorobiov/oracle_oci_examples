rem
rem $Header: cdemodr3.sql 14-jul-99.13:51:06 mjaeger Exp $
rem
rem cdemodr3.sql
rem
rem Copyright (c) 1997, 1999, Oracle Corporation.  All rights reserved.
rem
rem    NAME
rem      cdemodr3.sql - setup for cdemodr3.c
rem
rem    DESCRIPTION
rem      create type and table used for cdemodr3.c
rem
rem    NOTES
rem      <other useful comments, qualifications, etc.>
rem
rem    MODIFIED   (MM/DD/YY)
rem    mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem    svedala     09/11/98 - a "/" required after create type - bug 717842
rem    azhao       06/03/97 - Created
rem

connect scott/tiger

drop table extaddr;
drop type address_object;

create type address_object as object (state char(2), zip char(10));
/

create table extaddr of address_object;

quit

