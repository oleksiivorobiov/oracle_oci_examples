Rem
Rem $Header: sadv91.sql 07-jun-2004.13:50:01 lburgess Exp $
Rem
Rem sadv91.sql
Rem
Rem Copyright (c) 2000, 2004, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      sadv91.sql - Summary Advisor demo for Oracle 9i
Rem
Rem    DESCRIPTION
Rem      This file demonstrates the use of the Oracle 9i Summary Advisor
Rem      API calls
Rem
Rem    NOTES
Rem
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    lburgess    06/07/04 - add order by clause 
Rem    btao        03/02/01 - Change Interfaces
Rem    btao        10/31/00 - Created
Rem
Rem ====================================================================
Rem Setup for demos
Rem ====================================================================
connect system/manager
grant select on mview_recommendations to sh;
grant select on mview_workload to sh;
grant select on mview_filter to sh;
grant alter system to sh;
disconnect

Rem ********************************************************************
Rem * Demo 1: Materialized View Recommendation With User Workload      *
Rem ********************************************************************
Rem ====================================================================
Rem Step 1. Define user workload table and add artificial workload 
Rem         queries, and gather stats for schema objects
Rem ====================================================================
connect sh/sh
@sadvuwk.sql
insert into advisor_user_workload values
(
  'select sum(s.quantity_sold) 
   from sales s, products p
   where s.prod_id = p.prod_id and
     p.prod_category = ''Boys''
   group by p.prod_category 
  ',
  'SH', 'app1', 10, NULL, 5, NULL, NULL, NULL, NULL
)
/
insert into advisor_user_workload values
(
  'select sum(s.amount_sold) 
   from sales s, products p
   where s.prod_id = p.prod_id and
     p.prod_category = ''Girls''
   group by p.prod_category 
  ',
  'SH', 'app1', 10, NULL, 6, NULL, NULL, NULL, NULL
)
/
insert into advisor_user_workload values
(
  'select sum(quantity_sold) 
   from sales s, products p
   where s.prod_id = p.prod_id and
     p.prod_category = ''Men''
   group by p.prod_category 
  ',
  'SH', 'app1', 11, NULL, 3, NULL, NULL, NULL, NULL
)
/
insert into advisor_user_workload values
(
  'select sum(quantity_sold) 
   from sales s, products p
   where s.prod_id = p.prod_id and
     p.prod_category in (''Women'', ''Men'')
   group by p.prod_category 
  ',
  'SH', 'app1', 1, NULL, 8, NULL, NULL, NULL, NULL
)
/
execute dbms_stats.gather_schema_stats('SH', 1);

Rem ====================================================================
Rem Step 2. Create a new identifier to identify a new collection in the 
Rem         internal repository and load the user-defined workload into Rem         the workload collection
Rem ====================================================================
variable workload_id number;
execute dbms_olap.create_id(:workload_id);
execute dbms_olap.load_workload_user(:workload_id,-
   dbms_olap.workload_new,-
   dbms_olap.filter_none, 'SH', 'ADVISOR_USER_WORKLOAD');
select count(*) from system.mview_workload
   where workloadid = :workload_id;

Rem ====================================================================
Rem Step 3. Create a new identifier to identify a new filter object. Add
Rem         two filter items such that the filter can filter out 
Rem         workload queries with priority >= 5 and frequency <= 10
Rem ====================================================================
variable filter_id number;
execute dbms_olap.create_id(:filter_id);
execute dbms_olap.add_filter_item(:filter_id, 'PRIORITY',-
   NULL, 5, NULL, NULL, NULL);
execute dbms_olap.add_filter_item(:filter_id, 'FREQUENCY',-
   NULL, NULL, 10, NULL, NULL);
select count(*) from system.mview_filter
   where filterid = :filter_id;

Rem ====================================================================
Rem Step 4. Recommend materialized views with part of the previous 
Rem         workload collection that satisfy the filter conditions. 
Rem         Create a new identifier to identify the recommendation 
Rem         output.
Rem ====================================================================
variable run_id number;
execute dbms_olap.create_id(:run_id);
execute dbms_olap.recommend_mview_strategy(:run_id, :workload_id,-
   :filter_id, 100000, 100, NULL, NULL);
select count(*) from system.mview_recommendations;

Rem ====================================================================
Rem Step 5. Generate HTML reports on the output
Rem         Notes: The user must have the priviledge to grant directory
Rem                access permission. The commented out example
Rem                only applies to UNIX systems. Your specific 
Rem                operating system may require different path name
Rem                specification.
Rem ====================================================================
Rem execute dbms_java.grant_permission('SH',-
Rem    'java.io.FilePermission', '/tmp', 'read,write');
Rem
Rem execute dbms_olap.generate_mview_report('/tmp/output1.html',-
Rem    :run_id, dbms_olap.rpt_recommendation);

Rem ====================================================================
Rem Step 6. Cleanup current output, filter and workload collection
Rem         from the internal repository, truncate the user workload 
Rem         table for new user workloads.
Rem ====================================================================
execute dbms_olap.purge_results(:run_id);
execute dbms_olap.purge_filter(:filter_id);
execute dbms_olap.purge_workload(:workload_id);
select count(*) from system.mview_workload
   where workloadid = :workload_id;
truncate table advisor_user_workload;

drop table advisor_user_workload;
disconnect

Rem ********************************************************************
Rem * Demo 2: Materialized View Recommendation With SQL Cache          *
Rem ********************************************************************
connect sh/sh

Rem ====================================================================
Rem Step 1. Clear SQL cache and run some applications or some SQL 
Rem         queries, so that the Oracle SQL cache is populated with 
Rem         target queries. 
Rem ====================================================================
alter system flush shared_pool;
select sum(s.quantity_sold) 
from sales s, products p
where s.prod_id = p.prod_id 
 group by p.prod_category
 order by p.prod_category;

select sum(s.amount_sold) 
from sales s, products p
where s.prod_id = p.prod_id 
group by p.prod_category
order by p.prod_category;

select t.calendar_month_desc, sum(s.amount_sold) as dollars
from sales s, times t
where s.time_id = t.time_id
group by t.calendar_month_desc
order by t.calendar_month_desc;

select t.calendar_month_desc, sum(s.amount_sold) as dollars
from sales s, times t
where s.time_id = t.time_id
group by t.calendar_month_desc
order by t.calendar_month_desc;

Rem ====================================================================
Rem Step 2. Create a new identifier to identify a new collection in the 
Rem         internal repository and grab a snapshot of the Oracle SQL 
Rem         cache into the new collection
Rem ====================================================================
execute dbms_olap.create_id(:workload_id);
execute dbms_olap.load_workload_cache(:workload_id,-
   dbms_olap.workload_new,-
   dbms_olap.filter_none, NULL, 1);
select count(*) from system.mview_workload
   where workloadid = :workload_id;

Rem ====================================================================
Rem Step 3. Recommend materialized views with all of the workload 
Rem         collection and no filtering
Rem ====================================================================
execute dbms_olap.create_id(:run_id);
execute dbms_olap.recommend_mview_strategy(:run_id, :workload_id,-
   dbms_olap.filter_none, 100000, 100, NULL, NULL);
select count(*) from system.mview_recommendations;

Rem ====================================================================
Rem Step 4. Generate HTML reports on the output
Rem ====================================================================
Rem execute dbms_olap.generate_mview_report('/tmp/output2.html',-
Rem    :run_id, dbms_olap.rpt_recommendation);

Rem ====================================================================
Rem Step 5. Evaluate materialized views
Rem ====================================================================
execute dbms_olap.create_id(:run_id);
execute dbms_olap.evaluate_mview_strategy(-
   :run_id, :workload_id, dbms_olap.filter_none);

Rem ====================================================================
Rem Step 6. Generate HTML reports on the output
Rem ====================================================================
Rem execute dbms_olap.generate_mview_report('/tmp/output3.html',-
Rem    :run_id, dbms_olap.rpt_usage);

Rem ====================================================================
Rem Step 7. Cleanup current output, and workload collection
Rem         from the internal repository
Rem ====================================================================
execute dbms_olap.purge_results(:run_id);
execute dbms_olap.purge_workload(:workload_id);
disconnect

Rem ====================================================================
Rem Cleanup for demos
Rem ====================================================================
connect system/manager
revoke select on mview_recommendations from sh;
revoke select on mview_workload from sh;
revoke select on mview_filter from sh;
revoke alter system from sh;
disconnect
