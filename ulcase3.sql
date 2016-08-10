rem
rem $Header: ulcase3.sql 14-jul-99.14:23:36 mjaeger Exp $
rem
rem Copyright (c) 1991, 1999, Oracle Corporation.  All rights reserved.
rem
rem    NAME
rem      ulcase3.sql - <one-line expansion of the name>
rem    DESCRIPTION
rem      <short description of component this file declares/defines>
rem    RETURNS
rem
rem    NOTES
rem      ulcase2.sql must be executed before this pocedure.
rem    MODIFIED   (MM/DD/YY)
rem     mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
rem     jstenois   06/17/99 -  cleanup tables before load and show feedback
rem     ksudarsh   03/11/93 -  comment out vms specific host command
rem     ksudarsh   12/29/92 -  Creation
rem     cheigham   08/28/91 -  Creation
rem

set termout off

rem Do not clean up table because this example shows appending to existing
rem rows in table that also has new columns.

rem host write sys$output "Adding columns to emp.  Please wait."

alter table emp add (projno number, loadseq number);

exit
