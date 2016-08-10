Rem
Rem $Header: dattime3.sql 09-apr-2007.15:52:50 chli Exp $
Rem
Rem dattime3.sql
Rem
Rem Copyright (c) 2001, 2007, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      dattime3.sql - Using Built-in SQL function
Rem                     TO_YMINTERVAL();
Rem                     TO_DSINTERVAL();
Rem
Rem    DESCRIPTION
Rem      <short description of component this file declares/defines>
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    chli        04/09/07 - lowercase password
Rem    jxfan       04/10/01 - Merged jxfan_demo
Rem    jxfan       04/10/01 - Created
Rem

SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100
SET ECHO OFF

COL employee_id FORMAT 9999
COL first_name FORMAT A20
COL last_name FORMAT A20
COL employee_id HEADING ID
COL first_name HEADING "FIRST NAME"
COL last_name HEADING "LAST NAME"
COL hire_date HEADING "HIRE DATE"
CONNECT HR/hr

PROMPT
PROMPT
PROMPT Add 1 year 2 months to hire date by using Built-in SQL function TO_YMINTERVAL 
PROMPT
PROMPT

SELECT employee_id, first_name, last_name, hire_date, 
       hire_date + TO_YMINTERVAL('01-02') "NEW DATE"
FROM employees
WHERE department_id = 30;

PROMPT
PROMPT
rem PAUSE Press enter to continue ...
PROMPT
PROMPT Subtract 10 days from hire date by using Built-in SQL function TO_DSINTERVAL
PROMPT
PROMPT

SELECT employee_id, first_name, last_name, hire_date,
       hire_date - TO_DSINTERVAL('10 00:00:00') "NEW DATE"
FROM employees
WHERE department_id = 30;

PROMPT
PROMPT
rem PAUSE Press enter to continue ...
PROMPT
PROMPT Add 24 hours to hire date using INTERVAL
PROMPT
PROMPT
SELECT employee_id, first_name, last_name, hire_date,
       hire_date + INTERVAL '24' HOUR "NEW DATE"
FROM employees
WHERE department_id = 30;
PROMPT
PROMPT

