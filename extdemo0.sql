rem
rem $Header: extdemo0.sql 14-jul-99.13:55:39 mjaeger Exp $
rem
rem extdemo0.sql
rem
rem Copyright (c) 1998, 1999, Oracle Corporation.  All rights reserved.
rem
rem    NAME
rem      extdemo0.sql - explain plan script
rem
rem    DESCRIPTION
rem      This file contains the SQL statements to print
rem      the output of explain plan.
rem
rem    NOTES
rem      <other useful comments, qualifications, etc.>
rem
rem    MODIFIED   (MM/DD/YY)
rem    mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem    hdnguyen    07/26/99 - sqlplus conversion
rem    rmurthy     09/25/98 - minor changes
rem    rmurthy     07/15/98 - extensibility - explain plan script
rem    rmurthy     07/15/98 - Created

SELECT  operation operations,
         decode(options, 'BY INDEX ROWID', 'BY ROWID',
         'BY USER ROWID', 'BY ROWID',
         options) options,
         object_name
FROM    plan_table
ORDER BY id;

TRUNCATE TABLE plan_table;

