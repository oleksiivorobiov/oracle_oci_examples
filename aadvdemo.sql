--
-- $Header: aadvdemo.sql 29-may-2007.13:15:19 achoi Exp $
--
-- aadvdemo.sql
--
-- Copyright (c) 2003, 2007, Oracle. All rights reserved.  
--
--    NAME
--      aadvdemo.sql - SQLAccess Advisor documentation examples
--
--    DESCRIPTION
--     Runs documentation examples pertaining to the SQLAccess Advisor.
--     Chapter 17 in the Data Warehousing Guide.
--
--    NOTES
--      Before running this script, the directories ADVISOR_RESULTS
--      and TUNE_RESULTS need to be defined as first described in 
--      Section 17.2.23.  If they are not setup, the dbms_advisor.create_file
--      calls will not succeed.
--
--    MODIFIED   (MM/DD/YY)
--    achoi       05/29/07 - fix order by
--    gssmith     02/09/07 - Remove Summary Advisor
--    gvincent    11/02/06 - downcase connects for secure verifiers
--    gssmith     05/09/06 - STS conversion 
--    pabingha    02/02/05 - BUG 4148628 - disable search parameter 
--    mmoy        09/01/04 - Add order by. 
--    gssmith     07/20/04 - Add utlxaa to the script 
--    lburgess    06/07/04 - add order by clause 
--    gssmith     04/30/04 - Add demonstration of 10gR2 features
--    gssmith     04/20/04 - Adjust FAILED count 
--    lburgess    11/04/03 - bug 3164691 fixed, uncomment testcase 
--    lburgess    10/28/03 - lburgess_doc_examples 
--    lburgess    09/17/03 - Created
---------------------------------------------------------------------------
--
--  RUNS_STANDALONE Yes
--
SET ECHO ON
SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100


----------------------------------------------------------------------
-- Data Warehousing Guide Examples from Chapter 17 - SQLAccess Advisor
----------------------------------------------------------------------

-- Section 17.2.2 SQLAccess Advisor Privileges 
----------------------------------------------
-- Grant the ADVISOR privilege to the user.

connect /as sysdba;
grant ADVISOR to sh;

-- Connect to user
connect sh/sh;


-- Section 17.2.3 Creating Tasks
--------------------------------

VARIABLE task_id NUMBER;
VARIABLE task_name VARCHAR2(255);
EXECUTE :task_name := 'MYTASK';
EXECUTE DBMS_ADVISOR.CREATE_TASK ('SQL Access Advisor', :task_id, :task_name);



-- Section 17.2.5 Creating Templates
------------------------------------

-- 1. Create a template called MY_TEMPLATE

VARIABLE template_id NUMBER;
VARIABLE template_name VARCHAR2(255);
EXECUTE :template_name := 'MY_TEMPLATE';
EXECUTE DBMS_ADVISOR.CREATE_TASK('SQL Access Advisor',:template_id, -
                                 :template_name, is_template => 'TRUE');


-- 2. Set template parameters.


-- set naming conventions for recommended indexes/mvs
EXECUTE DBMS_ADVISOR.SET_TASK_PARAMETER ( -
   :template_name, 'INDEX_NAME_TEMPLATE', 'SH_IDX$$_<SEQ>');

EXECUTE DBMS_ADVISOR.SET_TASK_PARAMETER ( -
   :template_name, 'MVIEW_NAME_TEMPLATE', 'SH_MV$$_<SEQ>');


-- First create the tablespaces used in SET_TASK_PARAMETER
-- doc example.

connect /as sysdba;

create tablespace sh_indexes datafile 'shidx.f' size 2m reuse 
autoextend on default storage (initial 2k next 2k pctincrease 0
maxextents unlimited);

create tablespace sh_mviews datafile 'shmv.f' size 2m reuse 
autoextend on default storage (initial 2k next 2k pctincrease 0
maxextents unlimited);


-- Now continue with the SET_TASK_PARAMETER example.

connect sh/sh;

EXECUTE DBMS_ADVISOR.SET_TASK_PARAMETER ( -
   :template_name, 'DEF_INDEX_TABLESPACE', 'SH_INDEXES');

EXECUTE DBMS_ADVISOR.SET_TASK_PARAMETER ( -
   :template_name, 'DEF_MVIEW_TABLESPACE', 'SH_MVIEWS');



-- 3. This template can now be used as a starting point 
--    to create a task as follows:


EXECUTE DBMS_ADVISOR.DELETE_TASK('MYTASK');

VARIABLE task_id NUMBER;
VARIABLE task_name VARCHAR2(255);
EXECUTE :task_name := 'MYTASK';
EXECUTE DBMS_ADVISOR.CREATE_TASK('SQL Access Advisor', :task_id, -
                                 :task_name, template=>'MY_TEMPLATE');



-- Use pre-defined template SQLACCESS_WAREHOUSE

EXECUTE DBMS_ADVISOR.DELETE_TASK('MYTASK');

EXECUTE DBMS_ADVISOR.CREATE_TASK('SQL Access Advisor', -
   :task_id, :task_name,  template=>'SQLACCESS_WAREHOUSE');





-- Example 17-1 Creating a Workload
-----------------------------------

VARIABLE workload_name VARCHAR2(255);
EXECUTE :workload_name := 'MYWORKLOAD';
EXECUTE DBMS_ADVISOR.CREATE_SQLWKLD(:workload_name,'This is my first workload');


-- Example 17-2 Creating a Workload from a Template
---------------------------------------------------

-- 1. Create the variables.

VARIABLE template_id NUMBER;
VARIABLE template_name VARCHAR2(255);


-- 2. Create a template called MY_WK_TEMPLATE.

EXECUTE :template_name := 'MY_WK_TEMPLATE';
EXECUTE DBMS_ADVISOR.CREATE_SQLWKLD(:template_name, is_template=>'TRUE');


-- 3. Set template parameters. For example, the following sets the filter so only tables in the sh schema are tuned:

-- set USERNAME_LIST filter to SH
EXECUTE DBMS_ADVISOR.SET_SQLWKLD_PARAMETER( -
   :template_name, 'USERNAME_LIST', 'SH');


-- 4. Now create a workload using the template:

EXECUTE DBMS_ADVISOR.DELETE_SQLWKLD('MYWORKLOAD');

VARIABLE workload_name VARCHAR2(255);
EXECUTE :workload_name := 'MYWORKLOAD';
EXECUTE DBMS_ADVISOR.CREATE_SQLWKLD ( -
   :workload_name, 'This is my first workload', 'MY_WK_TEMPLATE');



-- Section 17.2.8 Linking a Task and a Workload
-----------------------------------------------

EXECUTE DBMS_ADVISOR.ADD_SQLWKLD_REF('MYTASK', 'MYWORKLOAD');




-- Section 17.2.9.1 SQL Tuning Set
----------------------------------

-- First create a SQL Tuning Set called MY_STS_WORKLOAD

declare
 sql_stmt   varchar2(1000);
begin

sql_stmt :=
'SELECT /* Query */
 t.week_ending_day, p.prod_subcategory, 
          sum(s.amount_sold) AS dollars,
          s.channel_id, s.promo_id
 FROM sales s, times t, products p
 WHERE s.time_id = t.time_id
   AND s.prod_id = p.prod_id
   AND s.prod_id > 10 AND s.prod_id < 50
 GROUP BY t.week_ending_day, p.prod_subcategory, 
          s.channel_id, s.promo_id';

  execute immediate sql_stmt;
end;
/


-- Grant privilege to sh for using sql tuning set

connect /as sysdba;
grant ADMINISTER SQL TUNING SET to sh;

connect sh/sh;



DECLARE
   sqlsetname  VARCHAR2(30);                                    
   sqlsetcur   dbms_sqltune.sqlset_cursor;            
   refid    NUMBER;                           
BEGIN
   sqlsetname := 'MY_STS_WORKLOAD';

   dbms_sqltune.create_sqlset(sqlsetname, 'Test loading from cursor cache');

   OPEN sqlsetcur FOR
     SELECT VALUE(P)                              
     FROM TABLE(
      dbms_sqltune.select_cursor_cache(
             'sql_text like ''SELECT /* Query%''',
              NULL,    
              NULL,
              NULL,     
              NULL,    
              NULL,    
              null)   
      ) P;                                   

   dbms_sqltune.load_sqlset(sqlsetname, sqlsetcur);

end;
/


-- Now create a workload from the SQL Tuning Set.

VARIABLE sqlsetname  VARCHAR2(30);
VARIABLE workload_name VARCHAR2(30);
VARIABLE saved_stmts NUMBER;
VARIABLE failed_stmts NUMBER;

EXECUTE   :sqlsetname := 'MY_STS_WORKLOAD';
EXECUTE   :workload_name := 'MY_WORKLOAD';

EXECUTE DBMS_ADVISOR.CREATE_SQLWKLD (:workload_name);
EXECUTE DBMS_ADVISOR.IMPORT_SQLWKLD_STS (:workload_name , -
     :sqlsetname, 'NEW', 1, :saved_stmts, :failed_stmts);




-- Section 17.2.9.2 Loading a User-Defined Workload
---------------------------------------------------

--
--   Create a user-workload table that will contain SQL statements.  
--

set echo off

@utlxaa

CREATE INDEX aadv_uwk_idx_01
   ON user_workload (module);

CREATE INDEX aadv_uwk_idx_02
   ON user_workload (username);

--
--   Clean up any prior activity data
--

truncate table user_workload;

--
--   Insert sample SQL statements into the user-workload table
--

--
--   query 1: aggregation with selection
--

INSERT INTO user_workload
(username, module, action, priority, elapsed_time, cpu_time, buffer_gets,
 disk_reads, rows_processed, executions, optimizer_cost, last_execution_date,
 stat_period, sql_text)
VALUES ('SH', 'Example1', 'Action', 2, 1, 1, 1, 1, 1, 1000, 1, SYSDATE, 1, 
'SELECT   t.week_ending_day, p.prod_subcategory, 
          sum(s.amount_sold) AS dollars,
          s.channel_id, s.promo_id
 FROM sales s, times t, products p
 WHERE s.time_id = t.time_id
   AND s.prod_id = p.prod_id
   AND s.prod_id > 10 AND s.prod_id < 50
 GROUP BY t.week_ending_day, p.prod_subcategory, 
          s.channel_id, s.promo_id
');

--
--   query 2: aggregation with selection
--

INSERT INTO user_workload
(username, module, action, priority, elapsed_time, cpu_time, buffer_gets,
 disk_reads, rows_processed, executions, optimizer_cost, last_execution_date,
 stat_period, sql_text)
VALUES ('SH', 'Example1', 'Action', 2, 1, 1, 1, 1, 1, 1000, 1, SYSDATE, 1, 
' SELECT   t.calendar_month_desc, sum(s.amount_sold) AS dollars
FROM     sales s , times t
WHERE    s.time_id = t.time_id
  AND    s.time_id between TO_DATE(''01-JAN-2000'', ''DD-MON-YYYY'')
                       AND TO_DATE(''01-JUL-2000'', ''DD-MON-YYYY'')
GROUP BY t.calendar_month_desc
');

--
--   query 3: star query 
--

INSERT INTO user_workload
(username, module, action, priority, elapsed_time, cpu_time, buffer_gets,
 disk_reads, rows_processed, executions, optimizer_cost, last_execution_date,
 stat_period, sql_text)
VALUES ('SH', 'Example1', 'Action', 2, 1, 1, 1, 1, 1, 1000, 1, SYSDATE, 1,
'SELECT ch.channel_class, c.cust_city, t.calendar_quarter_desc,
   SUM(s.amount_sold) sales_amount
FROM sales s, times t, customers c, channels ch
WHERE s.time_id = t.time_id
AND   s.cust_id = c.cust_id
AND   s.channel_id = ch.channel_id
AND   c.cust_state_province = ''CA''
AND   ch.channel_desc in (''Internet'',''Catalog'')
AND   t.calendar_quarter_desc IN (''1999-Q1'',''1999-Q2'')
GROUP BY ch.channel_class, c.cust_city, t.calendar_quarter_desc
');

--
--   query 4: order by 
--

INSERT INTO user_workload
(username, module, action, priority, elapsed_time, cpu_time, buffer_gets,
 disk_reads, rows_processed, executions, optimizer_cost, last_execution_date,
 stat_period, sql_text)
VALUES ('SH', 'Example1', 'Action', 2, 1, 1, 1, 1, 1, 1000, 1, SYSDATE, 1, 
' SELECT c.country_id, c.cust_city, c.cust_last_name
FROM customers c
WHERE c.country_id in (''US'', ''UK'')
ORDER BY c.country_id, c.cust_city, c.cust_last_name
');

commit;


-- now load the user-defined workload

VARIABLE saved_stmts NUMBER;
VARIABLE failed_stmts NUMBER;
EXECUTE DBMS_ADVISOR.IMPORT_SQLWKLD_USER( -
   'MYWORKLOAD', 'NEW', 'SH', 'USER_WORKLOAD', :saved_stmts, :failed_stmts);



-- Section 17.2.9.3 Loading a SQL Cache Workload
------------------------------------------------

VARIABLE saved_stmts NUMBER;
VARIABLE failed_stmts NUMBER;

EXECUTE DBMS_ADVISOR.IMPORT_SQLWKLD_SQLCACHE (-
   'MYWORKLOAD', 'APPEND', 2, :saved_stmts, :failed_stmts);



-- Section 17.2.9.4 Using a Hypothetical Workload 
-------------------------------------------------


VARIABLE workload_name VARCHAR2(255);
VARIABLE saved_stmts NUMBER;
VARIABLE failed_stmts NUMBER;
EXECUTE :workload_name := 'SCHEMA_WKLD';
EXECUTE DBMS_ADVISOR.CREATE_SQLWKLD(:workload_name);
EXECUTE DBMS_ADVISOR.SET_SQLWKLD_PARAMETER (:workload_name, -
                                           'USERNAME_LIST', 'SH');
EXECUTE DBMS_ADVISOR.IMPORT_SQLWKLD_SCHEMA ( -
   :workload_name, 'NEW', 2, :saved_stmts, :failed_stmts);




-- Section 17.2.9.5 Using a 9i Workload
---------------------------------------


-- Before the example in the documentation can be run, an Oracle9i
-- workload must first be created.

-- Run some SQL statements so we have something in the cache

set echo off
connect / as sysdba;
alter system flush shared_pool;
connect sh/sh;
set echo off
SELECT SUM(amount_sold), promo_id FROM sales where promo_id < 8 group by promo_id order by promo_id;
SELECT SUM(amount_sold), channel_id FROM sales group by channel_id order by channel_id;
set echo on

-- load the SQL CACHE workload and check that it is there

VARIABLE collection_id NUMBER;
VARIABLE no_recs NUMBER;

-- The setup is complete, now the documentation example can be shown.

-- 1. Create some variables.

VARIABLE workload_name VARCHAR2(255);
VARIABLE saved_stmts NUMBER;
VARIABLE failed_stmts NUMBER;


-- Section 17.2.9.6 SQLAccess Advisor Workload Parameters
---------------------------------------------------------

-- Order statements by OPTIMIZER_COST
EXECUTE DBMS_ADVISOR.SET_SQLWKLD_PARAMETER ( -
   'MYWORKLOAD', 'ORDER_LIST', 'OPTIMIZER_COST');

-- Max number of statements 3
EXECUTE DBMS_ADVISOR.SET_SQLWKLD_PARAMETER('MYWORKLOAD', 'SQL_LIMIT', 3);



-- Section 17.2.10 SQL Workload Journal
---------------------------------------

-- You can turn journaling off before importing workload as follows:

EXECUTE DBMS_ADVISOR.SET_SQLWKLD_PARAMETER('MYWORKLOAD', 'JOURNALING', 0);


-- To view only fatal messages:

EXECUTE DBMS_ADVISOR.SET_SQLWKLD_PARAMETER('MYWORKLOAD', 'JOURNALING', 4);




-- Section 17.2.11 Adding SQL Statements to a Workload
------------------------------------------------------

VARIABLE sql_text VARCHAR2(400);
EXECUTE :sql_text := 'SELECT AVG(amount_sold) FROM sales';
EXECUTE DBMS_ADVISOR.ADD_SQLWKLD_STATEMENT ( -
   'MYWORKLOAD', 'MONTHLY', 'ROLLUP', priority=>1, executions=>10, -
    username => 'SH',  sql_text => :sql_text);



-- Section 17.2.12 Deleting SQL Statements from a Workload
----------------------------------------------------------

-- Delete using sql_id

EXECUTE DBMS_ADVISOR.DELETE_SQLWKLD_STATEMENT('MYWORKLOAD', 10);



-- Section 17.2.13 Changing SQL Statements in a Workload
--------------------------------------------------------


EXECUTE DBMS_ADVISOR.UPDATE_SQLWKLD_STATEMENT( -
   'MYWORKLOAD', 2, priority=>3);




-- Section 17.2.14.1 Setting Workload Attributes
------------------------------------------------

EXECUTE DBMS_ADVISOR.UPDATE_SQLWKLD_ATTRIBUTES ( -
   'MYWORKLOAD', read_only=> 'TRUE');




-- Section 17.2.14.2 Resetting Workloads
----------------------------------------


-- need to first set it to be readable
EXECUTE DBMS_ADVISOR.UPDATE_SQLWKLD_ATTRIBUTES ( -
   'MYWORKLOAD', read_only=> 'FALSE');


EXECUTE DBMS_ADVISOR.RESET_SQLWKLD('MYWORKLOAD');




-- Section 17.2.14.3 Removing a Link Between a Workload and a Task
------------------------------------------------------------------


EXECUTE DBMS_ADVISOR.DELETE_SQLWKLD_REF('MYTASK', 'MYWORKLOAD');



-- Section 17.2.15 Removing Workloads 
-------------------------------------


EXECUTE DBMS_ADVISOR.DELETE_SQLWKLD('MYWORKLOAD');



-- Section 17.2.16 Recommendation Options
-----------------------------------------

EXECUTE DBMS_ADVISOR.SET_TASK_PARAMETER('MYTASK','STORAGE_CHANGE', 100);


EXECUTE DBMS_ADVISOR.SET_TASK_PARAMETER ( -
   'MYTASK', 'VALID_TABLE_LIST', 'SH.SALES, SH.CUSTOMERS');


-- clean-up
EXECUTE dbms_advisor.delete_task('MYTASK');



-- Section 17.2.17 Generating Recommendations
---------------------------------------------


-- Steps 1 through 6 are the required setup steps to be able to execute a 
-- task and have some worthwhile data to view after its execution.

-- Step 1 Prepare the USER_WORKLOAD table

-- The USER_WORKLOAD table is loaded with SQL statements as follows:

connect sh/sh;
-- aggregation with selection
INSERT INTO user_workload (username, module, action, priority, sql_text)
VALUES ('SH', 'Example1', 'Action', 2, 
'SELECT   t.week_ending_day, p.prod_subcategory, 
          SUM(s.amount_sold) AS dollars, s.channel_id, s.promo_id
 FROM sales s, times t, products p WHERE s.time_id = t.time_id
 AND s.prod_id = p.prod_id AND s.prod_id > 10 AND s.prod_id < 50
 GROUP BY t.week_ending_day, p.prod_subcategory, 
          s.channel_id, s.promo_id')
/

-- aggregation with selection
INSERT INTO user_workload (username, module, action, priority, sql_text)
VALUES ('SH', 'Example1', 'Action', 2, 
 'SELECT   t.calendar_month_desc, SUM(s.amount_sold) AS dollars
  FROM     sales s , times t
  WHERE    s.time_id = t.time_id
  AND    s.time_id between TO_DATE(''01-JAN-2000'', ''DD-MON-YYYY'')
                       AND TO_DATE(''01-JUL-2000'', ''DD-MON-YYYY'')
GROUP BY t.calendar_month_desc')
/

--Load all SQL queries.
INSERT INTO user_workload (username, module, action, priority, sql_text)
VALUES ('SH', 'Example1', 'Action', 2, 
'SELECT ch.channel_class, c.cust_city, t.calendar_quarter_desc,
   SUM(s.amount_sold) sales_amount
FROM sales s, times t, customers c, channels ch
WHERE s.time_id = t.time_id AND s.cust_id = c.cust_id
AND s.channel_id = ch.channel_id AND c.cust_state_province = ''CA''
AND   ch.channel_desc IN (''Internet'',''Catalog'')
AND   t.calendar_quarter_desc IN (''1999-Q1'',''1999-Q2'')
GROUP BY ch.channel_class, c.cust_city, t.calendar_quarter_desc')
/


-- order by
INSERT INTO user_workload (username, module, action, priority, sql_text)
VALUES ('SH', 'Example1', 'Action', 2, 
  'SELECT c.country_id, c.cust_city, c.cust_last_name
FROM customers c WHERE c.country_id in (''US'', ''UK'')
ORDER BY c.country_id, c.cust_city, c.cust_last_name')
/
COMMIT;

connect sh/sh;
set serveroutput on;

VARIABLE task_id NUMBER;
VARIABLE task_name VARCHAR2(255);
VARIABLE workload_name VARCHAR2(255);
VARIABLE saved_stmts NUMBER;
VARIABLE failed_stmts NUMBER;


-- Step 2 Create a workload named MYWORKLOAD

EXECUTE :workload_name := 'MYWORKLOAD';

EXECUTE DBMS_ADVISOR.CREATE_SQLWKLD(:workload_name);

-- Step 3 Load the workload from user-defined table SH.USER_WORKLOAD

EXECUTE DBMS_ADVISOR.IMPORT_SQLWKLD_USER (:workload_name, 'APPEND', 'SH', -
   'USER_WORKLOAD', :saved_stmts, :failed_stmts);
PRINT :saved_stmts;
PRINT :failed_stmts;


-- Step 4 Create a task named MYTASK

EXECUTE :task_name := 'MYTASK';

EXECUTE DBMS_ADVISOR.CREATE_TASK('SQL Access Advisor', :task_id, :task_name);

-- Step 5 Set task parameters

EXECUTE DBMS_ADVISOR.SET_TASK_PARAMETER(:task_name, 'STORAGE_CHANGE', 100);

EXECUTE DBMS_ADVISOR.SET_TASK_PARAMETER ( -
   :task_name, 'EXECUTION_TYPE', 'INDEX_ONLY');

-- Step 6 Create a link between workload and task

EXECUTE DBMS_ADVISOR.ADD_SQLWKLD_REF(:task_name, :workload_name);





-- Now execute the task for section 17.2.17.1

EXECUTE DBMS_ADVISOR.EXECUTE_TASK('MYTASK');




-- Section 17.2.18 Viewing the Recommendations 
----------------------------------------------


VARIABLE workload_name VARCHAR2(255);
VARIABLE task_name VARCHAR2(255);
EXECUTE :task_name := 'MYTASK';
EXECUTE :workload_name := 'MYWORKLOAD';

SELECT REC_ID, RANK, BENEFIT
FROM USER_ADVISOR_RECOMMENDATIONS WHERE TASK_NAME = :task_name;


SELECT sql_id, rec_id, precost, postcost,
       (precost-postcost)*100/precost AS percent_benefit
FROM USER_ADVISOR_SQLA_WK_STMTS
WHERE TASK_NAME = :task_name 
order by sql_id;



SELECT 'Action Count', COUNT(DISTINCT action_id) cnt
FROM user_advisor_actions WHERE task_name = :task_name;


-- see the actions for each recommendations

SELECT rec_id, action_id, SUBSTR(command,1,30) AS command
FROM user_advisor_actions WHERE task_name = :task_name
ORDER BY rec_id, action_id;


-- The following PL/SQL procedure can be used to print out some of 
-- the attributes of the recommendations.

connect sh/sh;
CREATE OR REPLACE PROCEDURE show_recm (in_task_name IN VARCHAR2) IS 
CURSOR curs IS
  SELECT DISTINCT action_id, command, attr1, attr2, attr3, attr4
  FROM user_advisor_actions
  WHERE task_name = in_task_name
  ORDER BY action_id;
  v_action        number;
  v_command     VARCHAR2(32);
  v_attr1       VARCHAR2(4000);
  v_attr2       VARCHAR2(4000);
  v_attr3       VARCHAR2(4000);
  v_attr4       VARCHAR2(4000);
  v_attr5       VARCHAR2(4000);
BEGIN
  OPEN curs;
  DBMS_OUTPUT.PUT_LINE('=========================================');
  DBMS_OUTPUT.PUT_LINE('Task_name = ' || in_task_name);
  LOOP
     FETCH curs INTO  
       v_action, v_command, v_attr1, v_attr2, v_attr3, v_attr4 ;
   EXIT when curs%NOTFOUND;
   DBMS_OUTPUT.PUT_LINE('Action ID: ' || v_action);
   DBMS_OUTPUT.PUT_LINE('Command : ' || v_command);
   DBMS_OUTPUT.PUT_LINE('Attr1 (name)      : ' || SUBSTR(v_attr1,1,30));
   DBMS_OUTPUT.PUT_LINE('Attr2 (tablespace): ' || SUBSTR(v_attr2,1,30));
   DBMS_OUTPUT.PUT_LINE('Attr3             : ' || SUBSTR(v_attr3,1,30));
   DBMS_OUTPUT.PUT_LINE('Attr4             : ' || v_attr4);
   DBMS_OUTPUT.PUT_LINE('Attr5             : ' || v_attr5);
   DBMS_OUTPUT.PUT_LINE('----------------------------------------');  
   END LOOP;   
   CLOSE curs;      
   DBMS_OUTPUT.PUT_LINE('=========END RECOMMENDATIONS============');
END show_recm;
/


-- see what the actions are using sample procedure

set serveroutput on size 99999
EXECUTE show_recm(:task_name);



-- Section 17.2.19 Access Advisor Journal 
-----------------------------------------

-- This section moved to the end of section 17.2.28 since
-- it requires the task to be reset.



-- Section 17.2.20 Stopping the recommendation Process
------------------------------------------------------


-- Note, a task must be executing for this to succeed.  This
-- testcase just proves the syntax is correct.  Expect an error
-- such as "task must be executing to be cancelled".

EXECUTE DBMS_ADVISOR.CANCEL_TASK('MYTASK');



-- Section 17.2.21 Marking Recommendations
------------------------------------------

EXECUTE DBMS_ADVISOR.MARK_RECOMMENDATION('MYTASK', 2, 'REJECT');



-- Section 17.2.22 Modifying Recommendations
--------------------------------------------

-- Before the TABLESPACE recommendation attribute can be updated,
-- recommendations of the CREATE type must exist.  Steps 1 through 6
-- do the required setup for this.   

-- 1. Create task

VARIABLE task_id2 NUMBER;
VARIABLE task_name2 VARCHAR2(255);
EXECUTE :task_name2 := 'MYTASK2';
EXECUTE DBMS_ADVISOR.CREATE_TASK ('SQL Access Advisor', :task_id2, -
  :task_name2);

-- 2. Create workload

VARIABLE workload_name2 VARCHAR2(255);
EXECUTE :workload_name2 := 'MYWORKLOAD2';
EXECUTE DBMS_ADVISOR.CREATE_SQLWKLD(:workload_name2,'This is my test workload');

-- 3. Link workload and task

EXECUTE DBMS_ADVISOR.ADD_SQLWKLD_REF(:task_name2, :workload_name2);


-- 4. Add a sql statement to the workload

VARIABLE sql_text2 VARCHAR2(400);
EXECUTE :sql_text2 := 'SELECT AVG(amount_sold) FROM sales';
EXECUTE DBMS_ADVISOR.ADD_SQLWKLD_STATEMENT ( -
   :workload_name2, 'MONTHLY', 'ROLLUP', priority=>1, executions=>10, -
    username => 'SH',  sql_text => :sql_text2);


-- 5. Execute the task

EXECUTE DBMS_ADVISOR.EXECUTE_TASK(:task_name2);

-- 6. See the actions recommended

SELECT rec_id, action_id, SUBSTR(command,1,30) AS command
FROM user_advisor_actions WHERE task_name = :task_name2
ORDER BY rec_id, action_id;



-- The following example modifies the attribute TABLESPACE for recommendation
-- ID 1, action ID 1 to SH_MVIEWS.

EXECUTE DBMS_ADVISOR.UPDATE_REC_ATTRIBUTES(:task_name2, 1, 2, 'TABLESPACE', -
 'SH_MVIEWS');




-- Section 17.2.23 Generating SQL Scripts
-----------------------------------------


-- Define a directory "ADVISOR_RESULTS" and grant permissions to 
-- read/write to it.  
--
-- connect sh/sh;
-- CREATE DIRECTORY ADVISOR_RESULTS AS '/mydir';
-- GRANT READ ON DIRECTORY ADVISOR_RESULTS TO PUBLIC;
-- GRANT WRITE ON DIRECTORY ADVISOR_RESULTS TO PUBLIC;
 

-- save script CLOB to file


-- Note: When "get_task_script" passed in as a parameter you get the error
--       'invalid file script'.  This is a problem with how SQLPlus handles
--       CLOBs.  Doc changed to call GET_TASK_SCRIPT directly in CREATE_FILE.

-- generate script as a CLOB
--variable scriptbuf CLOB;
--EXECUTE :scriptbuf := 'DBMS_ADVISOR.GET_TASK_SCRIPT(''MYTASK'')';


-- doesn't work
--EXECUTE DBMS_ADVISOR.CREATE_FILE(:scriptbuf, 'ADVISOR_RESULTS',-
--                                 'advscript.sql');

-- works
EXECUTE DBMS_ADVISOR.CREATE_FILE(DBMS_ADVISOR.GET_TASK_SCRIPT('MYTASK'), 'ADVISOR_RESULTS','advscript.sql');



-- Section 17.2.24 When Recommendations are No Longer Required
--------------------------------------------------------------


EXECUTE DBMS_ADVISOR.RESET_TASK('MYTASK');



-- Section 17.2.25 Performing a Quick Tune
------------------------------------------


VARIABLE task_name VARCHAR2(255);
VARIABLE sql_stmt VARCHAR2(4000);
EXECUTE :sql_stmt := 'SELECT COUNT(*) FROM customers -
                   WHERE cust_state_province=''CA''';
EXECUTE :task_name  := 'MY_QUICKTUNE_TASK';

EXECUTE DBMS_ADVISOR.QUICK_TUNE(DBMS_ADVISOR.SQLACCESS_ADVISOR, -
     :task_name, :sql_stmt);



-- Section 17.2.26.1 Updating Task Attributes
---------------------------------------------

EXECUTE DBMS_ADVISOR.UPDATE_TASK_ATTRIBUTES('MYTASK', 'TUNING1');


-- The following example marks the task TUNING1 to read only

EXECUTE DBMS_ADVISOR.UPDATE_TASK_ATTRIBUTES('TUNING1', read_only => 'TRUE');


-- The following example marks the task MYTASK as a template.

EXECUTE DBMS_ADVISOR.UPDATE_TASK_ATTRIBUTES('TUNING1', is_template=>'TRUE');



-- Section 17.2.26.2 Deleting Tasks
-----------------------------------
-- need an existing task to delete, use TUNING1 instead of MYTASK
-- but first make it readable.

EXECUTE DBMS_ADVISOR.UPDATE_TASK_ATTRIBUTES('TUNING1', read_only => 'FALSE');

EXECUTE DBMS_ADVISOR.DELETE_TASK('TUNING1');


-------------------------------------------------------------------------
--Section 17.2.28 Examples of Using the SQLAccess Advisor
-------------------------------------------------------------------------

-- cleanup from previous example

EXECUTE DBMS_ADVISOR.DELETE_SQLWKLD('MYWORKLOAD');


-- Section 17.2.28.1 Recommendations From a User-Defined Workload
-----------------------------------------------------------------

-- The following example imports workload from a user-defined table, 
-- SH.USER_WORKLOAD. It then creates a task called MYTASK, sets the 
-- storage budget to 100 MB and runs the task. The recommendations 
-- are printed out using a PL/SQL procedure. Finally, it generates a 
-- script, which can be used to implement the recommendations.

-- Step 1 Prepare the USER_WORKLOAD table

-- The USER_WORKLOAD table is loaded with SQL statements as follows:

connect sh/sh;
-- aggregation with selection
INSERT INTO user_workload (username, module, action, priority, sql_text)
VALUES ('SH', 'Example1', 'Action', 2, 
'SELECT   t.week_ending_day, p.prod_subcategory, 
          SUM(s.amount_sold) AS dollars, s.channel_id, s.promo_id
 FROM sales s, times t, products p WHERE s.time_id = t.time_id
 AND s.prod_id = p.prod_id AND s.prod_id > 10 AND s.prod_id < 50
 GROUP BY t.week_ending_day, p.prod_subcategory, 
          s.channel_id, s.promo_id')
/

-- aggregation with selection
INSERT INTO user_workload (username, module, action, priority, sql_text)
VALUES ('SH', 'Example1', 'Action', 2, 
 'SELECT   t.calendar_month_desc, SUM(s.amount_sold) AS dollars
  FROM     sales s , times t
  WHERE    s.time_id = t.time_id
  AND    s.time_id between TO_DATE(''01-JAN-2000'', ''DD-MON-YYYY'')
                       AND TO_DATE(''01-JUL-2000'', ''DD-MON-YYYY'')
GROUP BY t.calendar_month_desc')
/

--Load all SQL queries.
INSERT INTO user_workload (username, module, action, priority, sql_text)
VALUES ('SH', 'Example1', 'Action', 2, 
'SELECT ch.channel_class, c.cust_city, t.calendar_quarter_desc,
   SUM(s.amount_sold) sales_amount
FROM sales s, times t, customers c, channels ch
WHERE s.time_id = t.time_id AND s.cust_id = c.cust_id
AND s.channel_id = ch.channel_id AND c.cust_state_province = ''CA''
AND   ch.channel_desc IN (''Internet'',''Catalog'')
AND   t.calendar_quarter_desc IN (''1999-Q1'',''1999-Q2'')
GROUP BY ch.channel_class, c.cust_city, t.calendar_quarter_desc')
/

-- order by
INSERT INTO user_workload (username, module, action, priority, sql_text)
VALUES ('SH', 'Example1', 'Action', 2, 
  'SELECT c.country_id, c.cust_city, c.cust_last_name
FROM customers c WHERE c.country_id in (''US'', ''UK'')
ORDER BY c.country_id, c.cust_city, c.cust_last_name')
/
COMMIT;

connect sh/sh;
set serveroutput on;

VARIABLE task_id NUMBER;
VARIABLE task_name VARCHAR2(255);
VARIABLE workload_name VARCHAR2(255);
VARIABLE saved_stmts NUMBER;
VARIABLE failed_stmts NUMBER;


EXECUTE :workload_name := 'MYWORKLOAD';


EXECUTE DBMS_ADVISOR.CREATE_SQLWKLD(:workload_name);

-- Step 3 Load the workload from user-defined table SH.USER_WORKLOAD

EXECUTE DBMS_ADVISOR.IMPORT_SQLWKLD_USER (:workload_name, 'APPEND', 'SH', -
   'USER_WORKLOAD', :saved_stmts, :failed_stmts);
PRINT :saved_stmts;
PRINT :failed_stmts;

-- Step 4 Create a task named MYTASK

EXECUTE :task_name := 'MYTASK';

EXECUTE DBMS_ADVISOR.CREATE_TASK('SQL Access Advisor', :task_id, :task_name);

-- Step 5 Set task parameters

EXECUTE DBMS_ADVISOR.SET_TASK_PARAMETER(:task_name, 'STORAGE_CHANGE', 100);

EXECUTE DBMS_ADVISOR.SET_TASK_PARAMETER ( -
   :task_name, 'EXECUTION_TYPE', 'INDEX_ONLY');

-- Step 6 Create a link between workload and task

EXECUTE DBMS_ADVISOR.ADD_SQLWKLD_REF(:task_name, :workload_name);

-- Step 7 Execute the task

EXECUTE DBMS_ADVISOR.EXECUTE_TASK(:task_name);

-- Step 8 View the recommendations

-- See the number of recommendations and the status of the task.

SELECT rec_id, rank, benefit
FROM user_advisor_recommendations WHERE task_name = :task_name
order by rec_id;


-- See "Viewing the Recommendations" for further details.

-- See recommendation for each query.
SELECT sql_id, rec_id, precost, postcost,
      (precost-postcost)*100/precost AS percent_benefit
FROM user_advisor_sqla_wk_stmts
WHERE task_name = :task_name 
order by sql_id;

-- See the actions for each recommendations.
SELECT rec_id, action_id, substr(command,1,30) AS command
FROM user_advisor_actions
WHERE task_name = :task_name
ORDER BY rec_id, action_id;

-- See what the actions are using sample procedure.
SET SERVEROUTPUT ON SIZE 99999
EXECUTE show_recm(:task_name);

-- Step 9 Generate a Script to Implement the Recommendations

EXECUTE DBMS_ADVISOR.CREATE_FILE(DBMS_ADVISOR.GET_TASK_SCRIPT(:task_name),-
                                 'ADVISOR_RESULTS', 'Example1_script.sql');



-- Cleanup
EXECUTE dbms_advisor.delete_task(:task_name);
EXECUTE dbms_advisor.delete_sqlwkld(:workload_name);
EXECUTE DBMS_ADVISOR.DELETE_TASK('MY_TEMPLATE');



-- Section 17.2.28.2 Generate Recommendations Using a Task Template
-------------------------------------------------------------------

-- The following example creates a template and then uses it to create 
-- a task. It then uses this task to generate recommendations from a 
-- user-defined table, similar to "Recommendations From a User-Defined 
-- Workload".

connect sh/sh;
VARIABLE template_id NUMBER;
VARIABLE template_name VARCHAR2(255);


-- Step 1 Create a template called MY_TEMPLATE

EXECUTE :template_name := 'MY_TEMPLATE';



EXECUTE DBMS_ADVISOR.CREATE_TASK ( -
   'SQL Access Advisor',:template_id, :template_name, is_template=>'TRUE');

-- Step 2 Set template parameters

--Set naming conventions for recommended indexes/materialized views.
EXECUTE DBMS_ADVISOR.SET_TASK_PARAMETER ( -
   :template_name,  'INDEX_NAME_TEMPLATE', 'SH_IDX$$_<SEQ>');
EXECUTE DBMS_ADVISOR.SET_TASK_PARAMETER ( -
   :template_name, 'MVIEW_NAME_TEMPLATE', 'SH_MV$$_<SEQ>');

--Set default owners for recommended indexes/materialized views.
EXECUTE DBMS_ADVISOR.SET_TASK_PARAMETER ( -
   :template_name, 'DEF_INDEX_OWNER', 'SH');
EXECUTE DBMS_ADVISOR.SET_TASK_PARAMETER ( -
   :template_name, 'DEF_MVIEW_OWNER', 'SH');

--Set default tablespace for recommended indexes/materialized views.
EXECUTE DBMS_ADVISOR.SET_TASK_PARAMETER ( -
   :template_name, 'DEF_INDEX_TABLESPACE', 'SH_INDEXES');
EXECUTE DBMS_ADVISOR.SET_TASK_PARAMETER ( -
   :template_name, 'DEF_MVIEW_TABLESPACE', 'SH_MVIEWS');

-- Step 3 Create a task using the template

VARIABLE task_id NUMBER;

VARIABLE task_name VARCHAR2(255);
EXECUTE :task_name := 'MYTASK';
EXECUTE DBMS_ADVISOR.CREATE_TASK ( -
   'SQL Access Advisor', :task_id, :task_name, template => 'MY_TEMPLATE');

--See the parameter settings for task
SELECT parameter_name, parameter_value
FROM user_advisor_parameters
WHERE task_name = :task_name AND (parameter_name LIKE '%MVIEW%' 
   OR parameter_name LIKE '%INDEX%') order by 1,2;

-- Step 4 Create a workload named MYWORKLOAD

VARIABLE workload_name VARCHAR2(255);

VARIABLE saved_stmts NUMBER;
VARIABLE failed_stmts NUMBER;
EXECUTE :workload_name := 'MYWORKLOAD';
EXECUTE DBMS_ADVISOR.CREATE_SQLWKLD(:workload_name);

-- Step 5 Load the workload from user-defined table SH.USER_WORKLOAD

EXECUTE DBMS_ADVISOR.IMPORT_SQLWKLD_USER ( -
   :workload_name, 'APPEND', 'SH', 'USER_WORKLOAD', :saved_stmts,:failed_stmts);

-- Step 6 Create a link between the workload and the task

EXECUTE DBMS_ADVISOR.ADD_SQLWKLD_REF(:task_name, :workload_name);

-- Step 7 Execute the task

EXECUTE DBMS_ADVISOR.EXECUTE_TASK(:task_name);


-- Step 8 Generate a script


EXECUTE DBMS_ADVISOR.CREATE_FILE(DBMS_ADVISOR.GET_TASK_SCRIPT(:task_name),-
                                 'ADVISOR_RESULTS', 'Example2_script.sql');


-- Clean-up

EXECUTE dbms_advisor.delete_task(:task_name);
EXECUTE dbms_advisor.delete_sqlwkld(:workload_name);




-- Section 17.2.28.3 Filter a Workload from the SQL Cache
---------------------------------------------------------

-- The following example illustrates collection of a workload from a 
-- SQL cache. We first load the cache with a bunch of SQL statements. 
-- We then setup some filters to pick only a subset of those statements 
-- and import them into a SQLAccess Advisor workload. The workload is 
-- then used to generate recommendations.

-- Step 1 Loading the SQL cache

--The following statements are executed so they will be in the SQL cache:

CONNECT /AS SYSDBA;

--Clear any prior contents of the cache.
ALTER SYSTEM FLUSH SHARED_POOL;
connect sh/sh;



SELECT   t.calendar_month_desc, SUM(s.amount_sold) AS dollars
FROM     sales s, times t WHERE s.time_id = t.time_id
AND  s.time_id between TO_DATE('01-JAN-2000', 'DD-MON-YYYY')
                       AND TO_DATE('01-JUL-2000', 'DD-MON-YYYY')
GROUP BY t.calendar_month_desc
ORDER BY t.calendar_month_desc;


-- Order by
SELECT c.country_id, c.cust_city, c.cust_last_name
FROM customers c WHERE c.country_id IN ('US', 'UK') AND ROWNUM<20
ORDER BY c.country_id, c.cust_city, c.cust_last_name;

-- Queries to illustrate filtering
connect scott/tiger;
SELECT e.ename, d.dname
FROM emp e, dept d WHERE e.deptno = d.deptno
order by e.ename;

SELECT COUNT(*) FROM dept;

connect sh/sh;

VARIABLE task_id NUMBER;
VARIABLE task_name VARCHAR2(255);
VARIABLE workload_name VARCHAR2(255);
VARIABLE saved_stmts NUMBER;
VARIABLE failed_stmts NUMBER;

-- Step 2 Create a workload named MY_CACHE_WORKLOAD

EXECUTE :workload_name := 'MY_CACHE_WORKLOAD';

EXECUTE DBMS_ADVISOR.CREATE_SQLWKLD(:workload_name);

-- Step 3 Set up filters

--Load only SQL statements containing SH tables

EXECUTE DBMS_ADVISOR.SET_SQLWKLD_PARAMETER ( -
   :workload_name, 'USERNAME_LIST', 'SH');

-- Step 4 Load the workload from SQL Cache
--
--    NOTE: Because the SQL cache contains internal or recursive SQL
--          statements, the failed count is somewhat unpredictable in 
--          this context.  Therefore, we typically don't pay much 
--          attention to its value.  A more interesting view of the
--          results occurs in the journal, where detail statistics
--          describe the filtering results.

EXECUTE DBMS_ADVISOR.IMPORT_SQLWKLD_SQLCACHE ( -
   :workload_name, 'APPEND', 2, :saved_stmts, :failed_stmts);
PRINT :saved_stmts;

--See the workload statements in catalog views
--Note, create_date removed from query since results would be non-deterministic
SELECT num_select_stmt
FROM user_advisor_sqlw_sum
WHERE workload_name = :workload_name;

SELECT sql_id, username, optimizer_cost, SUBSTR(sql_text, 1, 30)
FROM user_advisor_sqlw_stmts
WHERE workload_name = :workload_name
ORDER BY sql_id;

-- Step 5 Add a single statement to the workload

EXECUTE DBMS_ADVISOR.ADD_SQLWKLD_STATEMENT (:workload_name, username => 'SH', -
    priority => 1, executions => 10,  sql_text =>  -
   'select count(*) from customers where cust_state_province=''CA''');

--Note, create_date removed from query since results would be non-deterministic
SELECT num_select_stmt
FROM user_advisor_sqlw_sum
WHERE workload_name = :workload_name;

-- Step 6 Update a statement in the workload

--VARIABLE updated_stmts NUMBER;

--EXECUTE DBMS_ADVISOR.UPDATE_SQLWKLD_STATEMENT ( -
--   :workload_name, 'executions < 10', :updated_stmts, priority => 3);

--PRINT :updated_stmts;

--See that the change has been made.
--SELECT sql_id, username, executions, priority
--FROM user_advisor_sqlw_stmts
--WHERE workload_name = :workload_name
--ORDER BY sql_id;

-- Step 7 Create a task named MYTASK

EXECUTE :task_name := 'MYTASK';

EXECUTE DBMS_ADVISOR.CREATE_TASK ('SQL Access Advisor', :task_id, :task_name);

-- Step 8 Create a link between a workload and a task

EXECUTE DBMS_ADVISOR.ADD_SQLWKLD_REF(:task_name, :workload_name);

-- Step 9 Execute the task

EXECUTE DBMS_ADVISOR.EXECUTE_TASK(:task_name);

-- Step 10 Generate a script

EXECUTE DBMS_ADVISOR.CREATE_FILE(DBMS_ADVISOR.GET_TASK_SCRIPT(:task_name),-
                                 'ADVISOR_RESULTS', 'Example3_script.sql');





-- Section 17.2.28.4 Evaluate Current Usage of Indexes and Materialized Views
-----------------------------------------------------------------------------

-- This example illustrates how SQLAccess Advisor may be used to evaluate 
-- the utilization of existing indexes and materialized views. We assume 
-- the workload is loaded into USER_WORKLOAD table as in "Recommendations 
-- From a User-Defined Workload". The indexes and materialized views that 
-- are being currently used (by the given workload) will appear as RETAIN 
-- actions in the SQLAccess Advisor recommendations.

connect sh/sh;
VARIABLE task_id NUMBER;
VARIABLE task_name VARCHAR2(255);
VARIABLE workload_name VARCHAR2(255);
VARIABLE saved_stmts NUMBER;
VARIABLE failed_stmts NUMBER;


-- Step 1 Create a workload named WORKLOAD

EXECUTE :workload_name := 'MYWORKLOAD';

EXECUTE DBMS_ADVISOR.CREATE_SQLWKLD(:workload_name);

-- Step 2 Load the workload from user-defined table SH.USER_WORKLOAD

EXECUTE DBMS_ADVISOR.IMPORT_SQLWKLD_USER ( -
   :workload_name, 'APPEND', 'SH','USER_WORKLOAD', :saved_stmts, :failed_stmts);

PRINT :saved_stmts;
PRINT :failed_stmts;

-- Step 3 Create a task named MY_EVAL_TASK

EXECUTE :task_name := 'MY_EVAL_TASK';

EXECUTE DBMS_ADVISOR.CREATE_TASK ('SQL Access Advisor', :task_id, :task_name);

-- Step 4 Create a link between workload and task

EXECUTE DBMS_ADVISOR.ADD_SQLWKLD_REF(:task_name, :workload_name);

-- Step 5 Set task parameters to indicate EVALUATION ONLY task

EXECUTE DBMS_ADVISOR.SET_TASK_PARAMETER (:task_name, 'EVALUATION_ONLY', 'TRUE');

-- Step 6 Execute the task

EXECUTE DBMS_ADVISOR.EXECUTE_TASK(:task_name);

-- Step 7 View evaluation results

--See the number of recommendations and the status of the task.

SELECT rec_id, rank, benefit
FROM user_advisor_recommendations WHERE task_name = :task_name;

--See the actions for each recommendation.
SELECT rec_id, action_id, SUBSTR(command, 1, 30) AS command
FROM user_advisor_actions WHERE task_name = :task_name
ORDER BY rec_id, action_id;




-- Section 17.2.19 Access Advisor Journal
-----------------------------------------

-- During the analysis process (execute_task), the Access Advisor will dump 
-- useful information regarding the analysis to a journal. The journal can 
-- be viewed using the view USER_ADVISOR_JOURNAL. The amount of information 
-- output varies depending on the setting of task parameter journaling.

-- setup, need to reset the task first

EXECUTE DBMS_ADVISOR.RESET_TASK('MYTASK');

-- You can turn journaling off as follows:

EXECUTE DBMS_ADVISOR.SET_TASK_PARAMETER('MYTASK', 'JOURNALING', 0);


-- To view only informational messages:

EXECUTE DBMS_ADVISOR.SET_TASK_PARAMETER('MYTASK', 'JOURNALING', 4);




-- Section 17.3 Tuning Materialized Views for Fast Refresh and Query Rewrite
----------------------------------------------------------------------------

-- 17.3.1.4 Script Generation DBMS_ADVISOR Function and Procedure
-----------------------------------------------------------------


-- Define a directory "TUNE_RESULTS" and grant permissions to 
-- read/write to it.  

-- CREATE DIRECTORY TUNE_RESULTS AS  '/tmp/script_dir';
-- GRANT READ, WRITE ON DIRECTORY TUNE_RESULTS TO PUBLIC;



EXECUTE DBMS_ADVISOR.CREATE_FILE(DBMS_ADVISOR.GET_TASK_SCRIPT(:task_name), -
 'TUNE_RESULTS', 'mv_create.sql');

EXECUTE DBMS_ADVISOR.CREATE_FILE(DBMS_ADVISOR.GET_TASK_SCRIPT(:task_name, 'UNDO'), 'TUNE_RESULTS', 'mv_undo.sql');




-- Example 17-3 Optimizing the Defining Query for Fast Refresh
--------------------------------------------------------------

VARIABLE task_cust_mv VARCHAR2(30);
VARIABLE create_mv_ddl VARCHAR2(4000);
EXECUTE :task_cust_mv := 'cust_mv';
EXECUTE :create_mv_ddl := ' -
CREATE MATERIALIZED VIEW cust_mv -
REFRESH FAST -
DISABLE QUERY REWRITE AS -
SELECT s.prod_id, s.cust_id, SUM(s.amount_sold) sum_amount -
FROM sales s, customers cs -
WHERE s.cust_id = cs.cust_id -
GROUP BY s.prod_id, s.cust_id';

EXECUTE DBMS_ADVISOR.TUNE_MVIEW(:task_cust_mv, :create_mv_ddl);



-- Example 17-4 Access IMPLEMENTATION Output Through USER_TUNE_MVIEW View
--------------------------------------------------------------------------

SELECT * FROM USER_TUNE_MVIEW
WHERE TASK_NAME= :task_cust_mv AND SCRIPT_TYPE='IMPLEMENTATION'
ORDER BY action_id;


-- Example 17-5 Save IMPLEMENTATION Output in a Script File
-----------------------------------------------------------

-- Define a directory "TUNE_RESULTS" and grant permissions to 
-- read/write to it.  

-- CREATE DIRECTORY TUNE_RESULTS AS  '/myscript';
-- GRANT READ, WRITE ON DIRECTORY TUNE_RESULTS TO PUBLIC;


EXECUTE DBMS_ADVISOR.CREATE_FILE(DBMS_ADVISOR.GET_TASK_SCRIPT(:task_cust_mv), -
  'TUNE_RESULTS', 'mv_create.sql');



-- Example 17-6 Enable Query Rewrite by Creating Multiple Materialized Views
----------------------------------------------------------------------------


EXECUTE :task_cust_mv := 'cust_mv2';

EXECUTE :create_mv_ddl := ' -
CREATE MATERIALIZED VIEW cust_mv -
ENABLE QUERY REWRITE AS -
SELECT s.prod_id, s.cust_id, COUNT(*) cnt, SUM(s.amount_sold) sum_amount -
FROM sales s, customers cs, countries cn -
WHERE s.cust_id = cs.cust_id AND cs.country_id = cn.country_id -
AND cn.country_name IN (''USA'',''Canada'') -
GROUP BY s.prod_id, s.cust_id -
UNION -
SELECT s.prod_id, s.cust_id, COUNT(*) cnt, SUM(s.amount_sold) sum_amount -
FROM sales s, customers cs -
WHERE s.cust_id = cs.cust_id AND s.cust_id IN (1005,1010,1012) -
GROUP BY s.prod_id, s.cust_id';

EXECUTE DBMS_ADVISOR.TUNE_MVIEW(:task_cust_mv, :create_mv_ddl);


-- Example 17-7 Access IMPLEMENTATION Output Through USER_TUNE_MVIEW View
-------------------------------------------------------------------------

SELECT * FROM USER_TUNE_MVIEW
WHERE TASK_NAME='cust_mv2' AND SCRIPT_TYPE='IMPLEMENTATION'
ORDER BY action_id;



-- Example 17-8 Save IMPLEMENTATION Output in a Script File
-----------------------------------------------------------

--save CREATE output in a script file

-- Define a directory "TUNE_RESULTS" and grant permissions to 
-- read/write to it.
  
-- CREATE DIRECTORY TUNE_RESULTS AS  '/tmp/script_dir';
-- GRANT READ, WRITE ON DIRECTORY TUNE_RESULTS TO PUBLIC;

EXECUTE DBMS_ADVISOR.CREATE_FILE(DBMS_ADVISOR.GET_TASK_SCRIPT('cust_mv2'),-
'TUNE_RESULTS', 'mv_create2.sql');



-- Example 17-9 Optimized Sub-Materialized View for Fast Refresh
----------------------------------------------------------------
EXECUTE :task_cust_mv := 'cust_mv3';

EXECUTE :create_mv_ddl := '-
CREATE MATERIALIZED VIEW cust_mv -
REFRESH FAST ON DEMAND -
ENABLE QUERY REWRITE AS -
SELECT s.prod_id, s.cust_id, COUNT(*) cnt, SUM(s.amount_sold) sum_amount -
FROM sales s, customers cs -
WHERE s.cust_id = cs.cust_id AND s.cust_id IN (2005,1020) -
GROUP BY s.prod_id, s.cust_id UNION -
SELECT s.prod_id, s.cust_id, COUNT(*) cnt, SUM(s.amount_sold) sum_amount -
FROM sales s, customers cs -
WHERE s.cust_id = cs.cust_id AND s.cust_id IN (1005,1010,1012) -
GROUP BY s.prod_id, s.cust_id';

EXECUTE DBMS_ADVISOR.TUNE_MVIEW(:task_cust_mv, :create_mv_ddl);




-- Example 17-10 Access IMPLEMENTATION Output Through USER_TUNE_MVIEW View
--------------------------------------------------------------------------

-- The following query accesses the IMPLEMENTATION output through 
-- USER_TUNE_MVIEW:

SELECT * FROM USER_TUNE_MVIEW 
WHERE TASK_NAME= 'cust_mv3' AND SCRIPT_TYPE='IMPLEMENTATION'
ORDER BY action_id;



-- Example 17-11 Save IMPLEMENTATION Output in a Script File
------------------------------------------------------------

-- The following statements save the CREATE output in a script file 
-- located at /myscript/mv_create3.sql:


EXECUTE DBMS_ADVISOR.CREATE_FILE(DBMS_ADVISOR.GET_TASK_SCRIPT('cust_mv3'), -
  'TUNE_RESULTS', 'mv_create3.sql');


-- 10gR2 enhancements
------------------------------------------------------------

variable name varchar2(30);

EXECUTE :name := 'MYWORKLOAD';

-- setup, need to recreate the task first

EXECUTE DBMS_ADVISOR.DELETE_TASK('%');
EXECUTE DBMS_ADVISOR.DELETE_SQLWKLD('%');

EXECUTE DBMS_ADVISOR.CREATE_TASK('SQL Access Advisor','MYTASK');
EXECUTE DBMS_ADVISOR.CREATE_SQLWKLD(:name);
EXECUTE DBMS_ADVISOR.ADD_SQLWKLD_REF('MYTASK','MYWORKLOAD');

-- Add a special SQL statement that currently produces an
-- exact-text match materialized view

EXECUTE DBMS_ADVISOR.ADD_SQLWKLD_STATEMENT ( -
   'MYWORKLOAD', 'MONTHLY', 'ROLLUP', 0,0,0,0,1,0,10,1,SYSDATE,1, -
    'SH', 'SELECT count(count(*)) FROM sales GROUP BY prod_id');

-- Disable exact-text match materialized views

EXECUTE DBMS_ADVISOR.SET_TASK_PARAMETER('MYTASK', 'RECOMMEND_MV_EXACT_TEXT_MATCH', 'TRUE');

-- Execute task

EXECUTE DBMS_ADVISOR.EXECUTE_TASK('MYTASK');

-- We should have an exact-text-match MV that matches the special workload
-- statement that we added to the workload.
--
-- Notice that ATTR5 contains the SELECT statement for the materialized view.
-- For exact-text match situations, ATTR5 will match the original SQL
-- statement from the workload.

select task_name,command,attr5 from user_advisor_actions;

-- setup, need to reset the task first

EXECUTE DBMS_ADVISOR.RESET_TASK('MYTASK');

-- Disable exact-text match materialized views

EXECUTE DBMS_ADVISOR.SET_TASK_PARAMETER('MYTASK', 'RECOMMEND_MV_EXACT_TEXT_MATCH', 'FALSE');

EXECUTE DBMS_ADVISOR.EXECUTE_TASK('MYTASK');

-- There should be no exact-text-match MV that matches the special workload
-- statement that we added to the workload

select task_name,command,attr5 from user_advisor_actions;





