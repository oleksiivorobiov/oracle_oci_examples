Rem
Rem $Header: resultcache.sql 01-jun-2007.10:01:28 achaudhr Exp $
Rem
Rem resultcache.sql
Rem
Rem Copyright (c) 2007, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      resultcache.sql - Result Cache
Rem
Rem    DESCRIPTION
Rem      A demonstration of the Result Cache feature.
Rem
Rem    NOTES
Rem      Requires the Result Cache to be enabled.
Rem
Rem    USAGE
Rem      sqlplus /nolog @$ORACLE_HOME/rdbms/demo/resultcache.sql > rc.log
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    achaudhr    05/31/07 - Split into Basic and Intemediate
Rem    achaudhr    04/13/07 - Created
Rem

SET ECHO ON
SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 100
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 1000
COL PLAN_TABLE_OUTPUT FOR A100

--- --------------------- ---
---  Basic Demonstration  ---
--- --------------------- ---

connect scott/tiger

--- It's generally not possible to tell if the Result Cache was used by simply
--- looking at the output of a query, so we use a function with a side-effect to
--- detect a cache miss.

--- Function used for detecting a cache miss (i.e. when the cache isn't used)
create or replace function CacheMiss return number DETERMINISTIC is
begin
  dbms_output.put_line('CACHE MISS');
  return null;
end;
/
set serveroutput on

--- The first execution of a query will be a cache-miss
select /*+ result_cache */ * from dept where CacheMiss is null;

--- But the second execution answers from the cache
select /*+ result_cache */ * from dept where CacheMiss is null;

--- A query still sees its own changes (by bypassing the cache)
update dept set dname = rpad(dname, 12, '*');
select /*+ result_cache */ * from dept where CacheMiss is null;

--- Committing changes invalidates the result which can subsequently be re-cached
commit;
select /*+ result_cache */ * from dept where CacheMiss is null;
select /*+ result_cache */ * from dept where CacheMiss is null;

-- A different session also uses the same cached result
connect / as sysdba
set serveroutput on

alter session set current_schema = scott;

select /*+ result_cache */ * -- Comments,
from   dept                  -- spaces,
where  CACHEMISS IS NULL     -- and case don't matter.
/

--- Restore the dept table (to original values)
connect scott/tiger
update dept set dname = rtrim(dname, '*');
commit;

--- ---------------------------- ---
---  Intermediate Demonstration  ---
--- ---------------------------- ---

connect scott/tiger

--- Verify that the queries will use the cache
explain plan for
select /*+ result_cache */ * from dept;

set echo off
@$ORACLE_HOME/rdbms/admin/utlxpls
set echo on

explain plan for
with e as 
(
select /*+ result_cache */ deptno, count(*) emp_count
from emp 
group by deptno
)
select dname, emp_count
from   dept d, e
where  e.deptno = d.deptno;

set echo off
@$ORACLE_HOME/rdbms/admin/utlxpls
set echo on

--- Cache each query's result and then scan (i.e. use) the cached result once
select /*+ result_cache */ * from dept;
/

with e as 
(
select /*+ result_cache */ deptno, count(*) emp_count
from emp 
group by deptno
)
select dname, emp_count
from   dept d, e
where  e.deptno = d.deptno;
/

--- Create a caching function and call it from sql 
create or replace function EMP_COUNT(dno number) return number
  result_cache relies_on (emp)
is
  cnt number;
begin
  select count(*) into cnt from emp where deptno = dno;
  return cnt;
end;
/

select dname, EMP_COUNT(deptno) emp_count
from dept where dname = 'SALES';
/

--- Invalidate the first query by modifying dept
update dept set loc = loc;
commit;

connect / as sysdba

--- View memory allocation and usage statistics
set serveroutput on
execute dbms_result_cache.memory_report
column name       format a62
column scan_count format 9999
column statistic  format a35
select name statistic, value from v$result_cache_statistics;
select type, status, scan_count,  namespace, name 
from v$result_cache_objects 
order by type, status, creation_timestamp;

--- Flush the cache and verify that memory was released
execute dbms_result_cache.flush
execute dbms_result_cache.memory_report

--- Finally, recreate dependencies (post-flush)
connect scott/tiger
select /*+ result_cache */ distinct loc from emp e, dept d 
where e.deptno=d.deptno and CacheMiss is null;

exit
