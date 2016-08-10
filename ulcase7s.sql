rem
rem $Header: ulcase7s.sql 26-jul-99.09:39:37 mjaeger Exp $
rem
rem Copyright (c) 1991, 1999, Oracle Corporation.  All rights reserved.
rem
rem    NAME
rem      ulcase7s.sql - <one-line expansion of the name>
rem    DESCRIPTION
rem      <short description of component this file declares/defines>
rem    RETURNS
rem
rem    NOTES
rem      <other useful comments, qualifications, etc.>
rem    MODIFIED   (MM/DD/YY)
rem     mjaeger    07/26/99 -  bug 808870: OCCS: convert tabs, no long lines
rem     jstenois   06/17/99 -  cleanup tables before load and show feedback
rem     ksudarsh   03/11/93 -  comment out vms specific host command
rem     ksudarsh   12/30/92 -  Creation
rem     ksudarsh   12/27/92 -  Creation
rem
rem ULDEMO7S.SQL
rem   Start-script for SQL*Loader Examples, Case 7

rem The variables the insert-trigger uses to save the last valid value
rem  are defined in a package so they will persist between calls.

rem Since these values will be accessed by anyone inserting into EMP, only
rem  the user doing the load should have access to EMP during this time
rem  (Alternatively, the trigger could be modified to check the USERENV fnction
rem  in a WHEN clause and only perform its functions for a particular user.)

set termout off
rem host write sys$output "Building Package and Trigger for Case 7.Please wait"

CREATE OR REPLACE PACKAGE uldemo7 AS
    last_deptno  NUMBER;
    last_job     CHAR(9);
    last_mgr     NUMBER;
END uldemo7;
/

CREATE OR REPLACE TRIGGER uldemo7_emp_insert
  BEFORE INSERT ON emp
  FOR EACH ROW

  BEGIN
  IF :new.deptno IS NOT NULL THEN
     uldemo7.last_deptno := :new.deptno;   -- save value for later use
  ELSE
     :new.deptno := uldemo7.last_deptno;   -- use last valid value
  END IF;

  IF :new.job IS NOT NULL THEN
     uldemo7.last_job := :new.job;   -- save value for later use
  ELSE
     :new.job := uldemo7.last_job;   -- use last valid value
  END IF;

  IF :new.mgr IS NOT NULL THEN
     uldemo7.last_mgr := :new.mgr;   -- save value for later use
  ELSE
     :new.mgr := uldemo7.last_mgr;   -- use last valid value
  END IF;

  END;
/

EXIT
