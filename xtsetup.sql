Rem
Rem $Header: xtsetup.sql 06-oct-2003.13:23:55 rsahani Exp $
Rem
Rem xtsetup.sql
Rem
Rem Copyright (c) 2001, 2003, Oracle Corporation.  All rights reserved.  
Rem
Rem    NAME
Rem      xtsetup.sql - Create DEFAULT DIRECTORY required by
Rem                    External Table Demos
Rem
Rem    DESCRIPTION
Rem      <short description of component this file declares/defines>
Rem
Rem    NOTES
Rem      SQL statements for creating default directory should be
Rem      uncommented and modified to point to location where
Rem      data files referenced in external table are located
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    rsahani     10/06/03 - uncomment "create directory"
Rem    rsahani     06/07/01 - Merged rsahani_add_xt_demos_010605
Rem    rsahani     06/05/01 - Created
Rem

SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100
SET ECHO ON

connect sys/knl_test7 as sysdba

Rem Drop and create default directory
Rem - &1 in create directory statement below must be replaced by
Rem   absolute path of the directory where data files referenced
Rem   in external table demos are stored
drop directory def_dir;

create directory def_dir as '&1';
