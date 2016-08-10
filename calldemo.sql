rem
rem $Header: calldemo.sql 21-sep-2004.11:03:44 stsun Exp $
rem
rem Copyright (c) 1991, 2004, Oracle. All rights reserved.  
rem
rem    NAME
rem      calldemo.sql - <one-line expansion of the name>
rem    DESCRIPTION
rem      <short description of component this file declares/defines>
rem    RETURNS
rem
rem    NOTES
rem      <other useful comments, qualifications, etc.>
rem    MODIFIED   (MM/DD/YY)
rem     stsun      09/21/04  - add order by the query 
rem     mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
rem     rkooi2     11/27/92 -  Creation
CREATE OR REPLACE PACKAGE calldemo AS

   TYPE char_array IS TABLE OF VARCHAR2(20)
       INDEX BY BINARY_INTEGER;
   TYPE num_array IS TABLE OF FLOAT
       INDEX BY BINARY_INTEGER;

   PROCEDURE get_employees(
     dept_number IN     number,    -- department to query
     batch_size  IN     INTEGER,   -- rows at a time
     found       IN OUT INTEGER,   -- rows actually returned
     done_fetch  OUT    INTEGER,   -- all done flag
     emp_name    OUT    char_array,
     job         OUT    char_array,
     sal         OUT    num_array);

END calldemo;
/

CREATE OR REPLACE PACKAGE BODY calldemo AS

   CURSOR get_emp (dept_number IN number) IS
       SELECT ename, job, sal FROM emp
           WHERE deptno = dept_number
           ORDER BY ename, job, sal;

   -- Procedure "get_employees" fetches a batch of employee
   -- rows (batch size is determined by the client/caller
   -- of the procedure).  It can be called from other
   -- stored procedures or client application programs.
   -- The procedure opens the cursor if it is not
   -- already open, fetches a batch of rows, and
   -- returns the number of rows actually retrieved. At
   -- end of fetch, the procedure closes the cursor.

   PROCEDURE get_employees(
     dept_number IN     number,
     batch_size  IN     INTEGER,
     found       IN OUT INTEGER,
     done_fetch  OUT    INTEGER,
     emp_name    OUT    char_array,
     job         OUT    char_array,
     sal         OUT    num_array) IS

   BEGIN
       IF NOT get_emp%ISOPEN THEN      -- open the cursor if
           OPEN get_emp(dept_number);  -- not already open
       END IF;

       -- Fetch up to "batch_size" rows into PL/SQL table,
       -- tallying rows found as they are retrieved. When all
       -- rows have been fetched, close the cursor and exit
       -- the loop, returning only the last set of rows found.

       done_fetch := 0;  -- set the done flag FALSE
       found := 0;

       FOR i IN 1..batch_size LOOP
           FETCH get_emp INTO emp_name(i), job(i), sal(i);
           IF get_emp%NOTFOUND THEN    -- if no row was found
               CLOSE get_emp;
               done_fetch := 1;   -- indicate all done
               EXIT;
           ELSE
               found := found + 1;  -- count row
           END IF;
       END LOOP;
   END;
END;
/
