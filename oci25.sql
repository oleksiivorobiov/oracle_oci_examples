rem
rem $Header: oci25.sql 11-oct-2006.15:55:35 azhao Exp $
rem
rem Copyright (c) 1995, 2006, Oracle. All rights reserved.  
rem
rem    NAME
rem      oci25.sql
rem    DESCRIPTION
rem      Script for A22400 OCI Techniques White Paper
rem      Demo script for oci25.c
rem    MODIFIED   (MM/DD/YY)
rem     azhao      10/11/06  - case-senstive password change
rem     mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
rem     cchau      08/18/97 -  enable dictionary protection
rem     echen      01/10/97 -  change internal to sys/change_on_install
rem     vraghuna   03/01/95 -  Creation

set echo on;
connect sys/knl_test7 as sysdba;

rem
rem Create a new user - call it ocitest
rem
drop user ocitest cascade;
create user ocitest identified by OCITEST;
grant connect, resource to ocitest;

rem
rem Created needed tables
rem
connect ocitest/OCITEST;
drop table test1;
drop table test2;
drop table test3;

