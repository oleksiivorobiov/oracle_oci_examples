rem
rem $Header: ulcase7e.sql 14-jul-99.14:26:51 mjaeger Exp $
rem
rem Copyright (c) 1991, 1999, Oracle Corporation.  All rights reserved.
rem
rem    NAME
rem      ulcase7e.sql - <one-line expansion of the name>
rem    DESCRIPTION
rem      <short description of component this file declares/defines>
rem    RETURNS
rem
rem    NOTES
rem      <other useful comments, qualifications, etc.>
rem    MODIFIED   (MM/DD/YY)
rem     mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
rem     jstenois   06/17/99 -  cleanup tables before load and show feedback
rem     ksudarsh   03/11/93 -  comment out vms specific host command
rem     ksudarsh   12/30/92 -  Creation
rem     ksudarsh   12/27/92 -  Creation
rem
rem ULDEMO7E.SQL
rem   End-script for SQL*Loader Examples, Case 7

set termout off
rem host write sys$output "Cleaning up Case 7 Trigger and Package."

DROP PACKAGE uldemo7;
DROP TRIGGER uldemo7_emp_insert;

EXIT
