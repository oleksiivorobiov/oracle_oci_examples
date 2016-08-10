Rem
Rem $Header: cdcdemo.sql 22-may-2006.07:34:50 bpanchap Exp $
Rem
Rem cdcdemo.sql
Rem
Rem Copyright (c) 2004, 2006, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      cdcdemo.sql - Change Data Capture Demo
Rem
Rem    DESCRIPTION
Rem      This script is a demonstration of Change Data Capture (CDC)
Rem      capabilities. It operates in asynchronous HotLog mode.
Rem
Rem      This demo shows how CDC can be used to supplement information
Rem      stored in the Human Resources (HR) sample schema without changing
Rem      the tables already defined there. The HR schema tracks current
Rem      salary for each employee but does not maintain a history of 
Rem      salary changes. This demo creates a new HR.SALARY_HISTORY table
Rem      and uses CDC to populate it. HR.SALARY_HISTORY can be used to
Rem      track information such as an employee's salary over time, or
Rem      average amount and frequency of raises given by a particular 
Rem      department.
Rem
Rem      This script creates a new user CDCPUB to be the CDC publisher.
Rem      It uses the existing user HR as both the owner of the source
Rem      table and as the CDC subscriber.
Rem
Rem    NOTES
Rem      This script must be run on a database that is set up for
Rem      asynchronous HotLog Change Data Capture. The following should
Rem      be verified before running this script:
Rem
Rem      1. Database initialization parameters are set according to the
Rem         table "Initialization Parameters for Asynchronous HotLog
Rem         Publishing" in the Change Data Capture chapter of the Oracle
Rem         Data Warehousing Guide.
Rem
Rem      2. The database is running in archivelog mode, force logging mode 
Rem         and is performing at least minimal database-level supplemental 
Rem         logging.
Rem
Rem      For more information about Change Data Capture, please refer to
Rem      the Change Data Capture chapter in the Oracle Data Warehousing Guide.
Rem
Rem      To run this script, do the following actions:
Rem         sqlplus /nolog
Rem         @cdcdemo
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    bpanchap    05/22/06 - Adding ddl_markers to create_change_table 
Rem    pabingha    07/12/04 - increase subscrip window wait 
Rem    pabingha    03/26/04 - pabingha_cdc_demo 
Rem    pabingha    03/10/04 - Created
Rem

set echo on
set pagesize 80

connect hr/hr

--
-- Create a copy of HR.EMPLOYEES so that this script can perform
-- employee DML operations without changing the original sample 
-- schema data. This copy, HR.CDC_DEMO_EMPLOYEES, is the CDC source
-- table for this demo.
--
create table cdc_demo_employees as select * from employees;

--
-- Create new HR.SALARY_HISTORY table.
--
create table salary_history (
  employee_id        number(6)    not null,
  job_id             varchar2(10) not null,
  department_id      number(4),
  old_salary         number(8,2),
  new_salary         number(8,2),
  percent_change     number(4,2),
  salary_action_date date
);

--
-- Instantiate the source table so that the underlying Oracle
-- Streams environment records the information it needs to capture
-- the source table's changes.
--
connect / as sysdba
begin
  dbms_capture_adm.prepare_table_instantiation(
    table_name => 'HR.CDC_DEMO_EMPLOYEES');
end;
/

--
-- Create CDCPUB user to be the CDC publisher.
--
connect / as sysdba
create user cdcpub identified BY cdcpub;
grant unlimited tablespace to cdcpub;
grant create session to cdcpub;
grant create table to cdcpub;
grant create sequence to cdcpub;
grant select_catalog_role to cdcpub;
grant execute_catalog_role to cdcpub;
grant connect, resource, dba to cdcpub;
execute dbms_streams_auth.grant_admin_privilege(grantee=>'CDCPUB');

--
-- Connect as the CDC publisher and create the objects needed to publish
-- the changes to the HR.CDC_DEMO_EMPLOYEES source table.
--
connect cdcpub/cdcpub

--
-- Create asynchronous HotLog change set CDC_DEMO_SET.
--
begin
  dbms_cdc_publish.create_change_set(
    change_set_name    => 'CDC_DEMO_SET',
    description        => 'change set for CDC demo',
    change_source_name => 'HOTLOG_SOURCE',
    stop_on_ddl        => 'Y',
    begin_date         => NULL,
    end_date           => NULL);
end;
/

--
-- Create change table CDCPUB.CDC_DEMO_EMP_CT and grant select access
-- on it to the subscriber.
--
begin
  dbms_cdc_publish.create_change_table(
    owner             => 'CDCPUB',
    change_table_name => 'CDC_DEMO_EMP_CT',
    change_set_name   => 'CDC_DEMO_SET',
    source_schema     => 'HR',
    source_table      => 'CDC_DEMO_EMPLOYEES',
    column_type_list  => 'EMPLOYEE_ID NUMBER, FIRST_NAME VARCHAR2(20),
                          LAST_NAME VARCHAR2(25), EMAIL VARCHAR2(25),
                          PHONE_NUMBER VARCHAR2(20), HIRE_DATE DATE,
                          JOB_ID VARCHAR2(10), SALARY NUMBER,
                          COMMISSION_PCT NUMBER, MANAGER_ID NUMBER,
                          DEPARTMENT_ID NUMBER',
    capture_values    => 'BOTH',
    rs_id             => 'y',
    row_id            => 'n',
    user_id           => 'y',
    timestamp         => 'y',
    object_id         => 'n',
    source_colmap     => 'n',
    target_colmap     => 'y',
    ddl_markers       => 'n',
    options_string    => null);
end;
/
grant select on cdc_demo_emp_ct to hr;

--
-- Enable capture for the CDC_DEMO_SET change set.
--
begin
  dbms_cdc_publish.alter_change_set(
    change_set_name => 'CDC_DEMO_SET',
    enable_capture  => 'y');
end;
/

--
-- Connect as the CDC subscriber and create the CDC subscription that
-- will be used to populate the new HR.SALARY_HISTORY table.
--
connect hr/hr

--
-- Create subscription CDC_DEMO_EMP_SUB.
--
begin
  dbms_cdc_subscribe.create_subscription(
    change_set_name   => 'CDC_DEMO_SET',
    description       => 'subscription to cdc_demo_employees',
    subscription_name => 'CDC_DEMO_EMP_SUB');
end;
/

--
-- Create subscriber view HR.CDC_DEMO_EMP_SUB_VIEW to display
-- changes to the HR.CDC_DEMO_EMPLOYEES source table.
--
begin
  dbms_cdc_subscribe.subscribe(
    subscription_name => 'CDC_DEMO_EMP_SUB',
    source_schema     => 'HR',
    source_table      => 'CDC_DEMO_EMPLOYEES',
    column_list       => 'EMPLOYEE_ID, FIRST_NAME, LAST_NAME, EMAIL,
                          PHONE_NUMBER, HIRE_DATE, JOB_ID, SALARY,
                          COMMISSION_PCT, MANAGER_ID, DEPARTMENT_ID',
    subscriber_view   => 'CDC_DEMO_EMP_SUB_VIEW');
end;
/

--
-- Activate subscription CDC_DEMO_EMP_SUB.
--
begin
  dbms_cdc_subscribe.activate_subscription(
    subscription_name => 'CDC_DEMO_EMP_SUB');
end;
/

--
-- Create procedure HR.UPDATE_SALARY_HISTORY to populate the new
-- HR.SALARY_HISTORY table. This procedure extends the subscription
-- window of the CDC_DEMP_EMP_SUB subscription to get the most recent
-- set of source table changes. It uses the HR.CDC_DEMO_EMP_SUB_VIEW
-- subscriber view to scan the changes and insert them into the
-- HR.SALARY_HISTORY table. It then purges the subscription window to
-- indicate that it is finished with this set of changes.
--
create or replace procedure update_salary_history is
  cursor cur is 
    select * from (
        select 'I' opt, cscn$, rsid$, employee_id, job_id, department_id,
               0 old_salary, salary new_salary, commit_timestamp$
        from cdc_demo_emp_sub_view
        where operation$ = 'I '
      union all
         select 'D' opt, cscn$, rsid$, employee_id, job_id, department_id,
                salary old_salary, 0 new_salary, commit_timestamp$
         from cdc_demo_emp_sub_view
         where operation$ = 'D '
      union all
         select 'U' opt , v1.cscn$, v1.rsid$, v1.employee_id, v1.job_id,
                v1.department_id, v1.salary old_salary, v2.salary new_salaryi, 
                v1.commit_timestamp$
         from cdc_demo_emp_sub_view v1, cdc_demo_emp_sub_view v2
         where v1.operation$ = 'UO' and v2.operation$ = 'UN' and
               v1.cscn$ = v2.cscn$ and v1.rsid$ = v2.rsid$ and
               abs(v1.salary - v2.salary) > 0)
    order by cscn$, rsid$;
  percent number;

begin

  -- Get the next set of changes to HR.CDC_DEMO_EMPLOYEES source table.
  dbms_cdc_subscribe.extend_window(
    subscription_name => 'CDC_DEMO_EMP_SUB');

  -- Process each change.
  for row in cur loop
    if row.opt = 'I' then
      insert into salary_history values (
        row.employee_id, row.job_id, row.department_id, 0, row.new_salary,
        NULL, row.commit_timestamp$);
    end if;
    if row.opt = 'D' then
      insert into salary_history values (
        row.employee_id, row.job_id, row.department_id, row.old_salary, 0,
        NULL, row.commit_timestamp$);
    end if;
    if row.opt = 'U' then
      percent := (row.new_salary - row.old_salary) / row.old_salary * 100;
      insert into salary_history values (
        row.employee_id, row.job_id, row.department_id, row.old_salary, 
        row.new_salary, percent, row.commit_timestamp$);
    end if;
  end loop;

  -- Indicate subscriber is finished with this set of changes.
  dbms_cdc_subscribe.purge_window(
    subscription_name => 'CDC_DEMO_EMP_SUB');

end update_salary_history;
/

--
-- Create procedure CDCPUB.WAIT_FOR_CHANGES to enable this demo to run
-- predictably. The asynchronous nature of CDC HotLog mode means that
-- there is a delay for source table changes to appear in the CDC
-- change table and the subscriber view. By default this procedure
-- waits up to 5 minutes for the change table and 1 additional minute
-- for the subscriber view. This can be adjusted if it is insufficient.
-- The caller must specify the name of the change table and the number
-- of rows expected to be in the change table. The caller may also
-- optionally specify a different number of seconds to wait for changes
-- to appear in the change table.
--
connect cdcpub/cdcpub
create or replace function wait_for_changes(
  change_table_name  in varchar2,           -- name of change table
  rowcount           in number,             -- number of rows to wait for
  maxwait_seconds    in number := 300)      -- maximum time to wait, in seconds
return varchar2 authid current_user as

  num_of_rows  number         := 0;           -- number of rows in change table
  slept        number         := 0;           -- total time slept
  sleep_time   number         := 3;           -- number of seconds to sleep
  return_msg   varchar2(500);                 -- informational message
  query_txt    varchar2(200)  := 'select count(*) from ' || change_table_name; 
  keep_waiting boolean        := TRUE;        -- whether to keep waiting

begin

  while (keep_waiting) loop
    dbms_lock.sleep(sleep_time);
    slept := slept+sleep_time;
    execute immediate query_txt into num_of_rows;

    -- Got expected number of rows.
    if num_of_rows >= rowcount then
      keep_waiting := FALSE;
      return_msg   :=  'Change table ' || change_table_name ||
                       ' contains at least ' || rowcount || ' rows.';
      goto DONE;

    -- Reached maximum number of seconds to wait.
    elsif slept > maxwait_seconds then
      return_msg :=  ' !!! - Timed out while waiting for ' ||
                     change_table_name ||
                     ' to reach ' || rowcount || ' rows !!! ';
      goto DONE;
    end if;
  end loop;

  <<DONE>>
  -- additional wait time for changes to become available to subscriber view
  dbms_lock.sleep(120);

  return return_msg;
end ;
/

--
-- First set of DML operations on source table HR.CDC_DEMO_EMPLOYEES.
--
connect hr/hr
update cdc_demo_employees set salary = salary + 500 where job_id = 'SH_CLERK';
update cdc_demo_employees set salary = salary + 1000 where job_id = 'ST_CLERK';
update cdc_demo_employees set salary = salary + 1500 where job_id = 'PU_CLERK';
commit;

insert into cdc_demo_employees values
  (207, 'Mary', 'Lee', 'MLEE', '310.234.4590', 
   to_date('10-JAN-2003','DD-MON-YYYY'), 'SH_CLERK', 4000, NULL, 121, 50);
insert into cdc_demo_employees values
  (208, 'Karen', 'Prince', 'KPRINCE', '345.444.6756', 
   to_date('10-NOV-2003','DD-MON-YYYY'), 'SH_CLERK', 3000, NULL, 111, 50);
insert into cdc_demo_employees values
  (209, 'Frank', 'Gate', 'FGATE', '451.445.5678', 
   to_date('13-NOV-2003','DD-MON-YYYY'), 'IT_PROG', 8000, NULL, 101, 50);
insert into cdc_demo_employees values
  (210, 'Paul', 'Jeep', 'PJEEP', '607.345.1112', 
   to_date('28-MAY-2003','DD-MON-YYYY'), 'IT_PROG', 8000, NULL, 101, 50);
commit;

--
-- Expecting 94 rows to appear in the change table CDCPUB.CDC_DEMO_EMP_CT.
-- This first capture may take a few minutes. Later captures should be
-- faster.
--
connect cdcpub/cdcpub
select wait_for_changes('CDCPUB.CDC_DEMO_EMP_CT', 94, 1000) message from dual;

--
-- Execute subscriber routine to update HR.SALARY_HISTORY.
--
connect hr/hr
execute update_salary_history;

--
-- Look at the contents of HR.SALARY_HISTORY.
--
select employee_id, job_id, department_id, old_salary, new_salary,
       percent_change
from salary_history
order by employee_id, old_salary, new_salary;

--
-- Second set of DML operations on source table HR.CDC_DEMO_EMPLOYEES.
--
connect hr/hr
delete from cdc_demo_employees 
where first_name = 'Mary' and last_name = 'Lee';
delete from cdc_demo_employees
where first_name = 'Karen' and last_name = 'Prince';
delete from cdc_demo_employees 
where first_name = 'Frank' and last_name = 'Gate';
delete from cdc_demo_employees 
where first_name = 'Paul' and last_name = 'Jeep';
commit;

update cdc_demo_employees set salary = salary + 5000 
where job_id = 'AD_VP';
update cdc_demo_employees set salary = salary - 1000 
where job_id = 'ST_MAN';
update cdc_demo_employees set salary = salary - 500 
where job_id = 'FI_ACCOUNT';
commit;

--
-- Expecting 122 rows to appear in the change table CDCPUB.CDC_DEMO_EMP_CT.
-- (94 rows from the first set of DMLs and 28 from the second set)
--
connect cdcpub/cdcpub
select wait_for_changes('CDCPUB.CDC_DEMO_EMP_CT', 122, 1000) message from dual;

--
-- Execute subscriber routine to update HR.SALARY_HISTORY.
--
connect hr/hr
execute update_salary_history;

--
-- Look at the contents of HR.SALARY_HISTORY.
--
select employee_id, job_id, department_id, old_salary, new_salary,
       percent_change
from salary_history
order by employee_id, old_salary, new_salary;

--
-- Clean up all objects created for this demo.
--
connect hr/hr
begin
  dbms_cdc_subscribe.drop_subscription(
    subscription_name => 'CDC_DEMO_EMP_SUB');
end;
/

connect cdcpub/cdcpub
begin
  dbms_cdc_publish.drop_change_table(
    owner             => 'CDCPUB',
    change_table_name => 'CDC_DEMO_EMP_CT',
    force_flag        => 'Y');
end;
/
begin
  dbms_cdc_publish.drop_change_set(
    change_set_name   => 'CDC_DEMO_SET');
end;
/
drop function wait_for_changes;

connect hr/hr
drop table salary_history purge;
drop table cdc_demo_employees;
drop procedure update_salary_history;

connect / as sysdba
drop user cdcpub cascade;

exit
