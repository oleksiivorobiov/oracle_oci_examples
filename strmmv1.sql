Rem
Rem $Header: strmmv1.sql 13-dec-2006.02:59:43 davzhang Exp $
Rem
Rem strmmv1.sql
Rem
Rem Copyright (c) 2006, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      strmmv1.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem      <short description of component this file declares/defines>
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    davzhang    12/13/06 - drop job before drop user
Rem    davzhang    08/29/06 - where clause when selecting from
Rem                           dba_apply_instantiated_objects
Rem    liwong      05/16/06 - sync capture cleanup 
Rem    davzhang    04/18/06 - wait for job queue before drop user
Rem    wesmith     01/19/06 - Created
Rem

set echo on
set pages 10000
set serveroutput on
COLUMN STREAMS_TYPE HEADING 'STREAMS_TYPE' FORMAT A19

connect system/manager

--
-- create user for MVs
--
drop user tabowner cascade;
grant connect, resource, create materialized view to tabowner 
  identified by tabowner;

-- 
-- create procedure to wait for job queue processes to finish
--
set echo off
grant create any job to system;
CREATE OR REPLACE PROCEDURE wait_for_jobq AS
  JQ_COUNT NUMBER :=1;
  WAIT_SEC NUMBER :=600;
  cursor stradm_jobs is 
    select owner,job_name from dba_scheduler_jobs
      where owner='STRADM';
BEGIN
  for rec in stradm_jobs loop
    sys.dbms_scheduler.drop_job(
      job_name => rec.owner || '.' || rec.job_name,
      force    => true);
  end loop;
  -- disable job queue, make sure no jobs are started from now
  -- wait for given time
  SYS.DBMS_IJOB.SET_ENABLED(FALSE);
  WAIT_SEC := WAIT_SEC * 2;
  FOR I IN 1..WAIT_SEC LOOP
   DBMS_LOCK.SLEEP(0.5);
   -- any removed job still running?
   SELECT COUNT(*) INTO JQ_COUNT
     FROM V$LOCK V 
     WHERE V.TYPE = 'JQ' AND NOT EXISTS
     (SELECT * FROM DBA_JOBS J WHERE V.ID2=J.JOB);
   IF JQ_COUNT<=0 THEN
     EXIT;
   END IF;
  END LOOP;
  IF JQ_COUNT>0 THEN
    -- ISSUE USER-DEFINED ERROR MESSAGE. 
    RAISE_APPLICATION_ERROR(-20101, 'Time Out Waiting Jobs To Finish');
  END IF;
  SYS.DBMS_IJOB.SET_ENABLED(TRUE);
END;
/
set echo on
--
-- create tables, mvlogs, mvs
--
connect tabowner/tabowner
create table foo (c1 number primary key, c2 varchar2(20));
create table bar (c1 number, c2 varchar2(20));

create materialized view log on foo;
create materialized view log on bar with rowid;

create materialized view foo_s refresh fast as select * from foo;
create materialized view bar_s refresh fast with rowid as select * from bar;


-- grant necessary privileges for streams admin
connect system/manager

drop user stradm cascade;
grant dba to stradm identified by stradm;
begin
  dbms_streams_auth.grant_admin_privilege(
    grantee          => 'stradm',    
    grant_privileges => true);
end;
/

--
-- create package async_mv_pkg for async on-commit MV support
--
connect stradm/stradm
set echo off
@strmmvp1
set echo on

--
-- streams setup
--
begin
  dbms_streams_adm.set_up_queue(
    queue_table  => 'stradm.ASYNC_MV_QT',
    queue_name   => 'stradm.ASYNC_MV_Q');
end;
/

variable site1 varchar2(80);
execute select global_name into :site1 from global_name;
print site1;

-- set up streams for all tables with MV logs
exec async_mv_pkg.register_tables('CAPTURE_MV', 'APPLY_MV');

begin
  dbms_apply_adm.set_parameter(
    apply_name  => 'APPLY_MV', 
    parameter   => 'disable_on_error', 
    value       => 'n');
end;
/

-- assign precommit handler to apply process 
begin
  dbms_apply_adm.alter_apply(
    apply_name        => 'APPLY_MV',
    precommit_handler => 'stradm.async_mv_pkg.async_mv_commit_hdlr');
end;
/

begin
  dbms_apply_adm.start_apply(
    apply_name  => 'APPLY_MV');
end;
/

begin
  dbms_capture_adm.start_capture(
    capture_name  => 'CAPTURE_MV');
end;
/

-- check streams metadata
select streams_type, streams_name, table_owner, table_name, 
  rule_owner, rule_name
from dba_streams_table_rules
where streams_name in ('CAPTURE_MV', 'APPLY_MV')
order by streams_type, streams_name, table_owner, table_name;

select source_object_owner, source_object_name, source_database, 
       instantiation_scn
from dba_apply_instantiated_objects 
where source_object_owner='TABOWNER'
order by source_database, source_object_owner, source_object_name;

select object_owner, object_name, operation_name, user_procedure
from dba_apply_dml_handlers
where apply_name = 'APPLY_MV'
order by object_owner, object_name, operation_name;

-- do some DMLs
connect tabowner/tabowner
insert into foo values (1, 'foo_a');
insert into foo values (2, 'foo_b');
insert into bar values (1, 'bar_a');
insert into bar values (2, 'bar_b');
commit;

update foo set c2 = 'foo_b_updated' where c1=2;
delete from foo where c1=1;
commit;

update bar set c2 = 'bar_b_updated' where c1=2;
delete from bar where c1=1;
commit;

-- wait for streams to finish
set serveroutput on
set timing on
exec verify_tables('foo','foo_s');
exec verify_tables('bar','bar_s');
set timing off

-- check for convergence (verify that refresh_dependent was called)
select * from foo;
select * from foo_s;

select * from bar;
select * from bar_s;

-- submit a job to register new tables having MV logs every minute
connect stradm/stradm
declare
  l_jobnum number;
  str      varchar2(100);
begin
  str := 
    'begin ' ||
    '  stradm.async_mv_pkg.register_tables(''CAPTURE_MV'',''APPLY_MV''); ' ||
    '  commit; ' ||
    'end;';

  dbms_scheduler.create_job(
   job_name        =>  dbms_scheduler.generate_job_name,
   job_type        =>  'PLSQL_BLOCK',
   job_action      =>  str,
   start_date      =>  sysdate,
   repeat_interval => 'FREQ=minutely',
   enabled         =>  TRUE);

  commit;
end;
/

-- create another table with an MV log and MV on it and verify that
-- it is set up for streams
connect tabowner/tabowner
create table foo2 (c1 number primary key, c2 varchar2(20));
create materialized view log on foo2;
create materialized view foo2_s refresh fast as select * from foo2;

-- wait for the register_tables() job to finish
set serveroutput on
exec verify_table_rules('TABOWNER', 'FOO2', true);

-- do some dmls
insert into foo2 values (1, 'foo_a');
insert into foo2 values (2, 'foo_b');
commit;
update foo2 set c2 = 'foo2_b_updated' where c1=2;
delete from foo2 where c1=1;
commit;

-- check streams metadata: should include foo2
connect stradm/stradm
select streams_type, streams_name, table_owner, table_name, 
  rule_owner, rule_name
from dba_streams_table_rules
where streams_name in ('CAPTURE_MV', 'APPLY_MV')
order by streams_type, streams_name, table_owner, table_name;

select source_object_owner, source_object_name, source_database,
       instantiation_scn
from dba_apply_instantiated_objects 
where source_object_owner='TABOWNER'
order by source_database, source_object_owner, source_object_name;

select object_owner, object_name, operation_name, user_procedure
from dba_apply_dml_handlers
where apply_name = 'APPLY_MV'
order by object_owner, object_name, operation_name;

-- wait for streams to finish
connect tabowner/tabowner
set serveroutput on
set timing on
exec verify_tables('foo2','foo2_s');
set timing off

-- check for convergence (verify that refresh_dependent was called)
select * from foo2;
select * from foo2_s;

-- submit a job to unregister tables no longer having MV logs every minute
connect stradm/stradm
declare
  l_jobnum number;
  str      varchar2(100);
begin
  str := 
    'begin ' ||
    '  stradm.async_mv_pkg.unregister_tables(''CAPTURE_MV'',''APPLY_MV''); ' ||
    '  commit; ' ||
    'end;';

  dbms_scheduler.create_job(
   job_name        =>  dbms_scheduler.generate_job_name,
   job_type        =>  'PLSQL_BLOCK',
   job_action      =>  str,
   start_date      =>  sysdate,
   repeat_interval => 'FREQ=minutely',
   enabled         =>  TRUE);

  commit;
end;
/

--- now drop an mvlog from foo, all streams metadata should be removed for foo
connect tabowner/tabowner
drop materialized view log on foo;

-- wait for the unregister_tables() job to finish
set serveroutput on
exec verify_table_rules('TABOWNER', 'FOO', false);

-- check streams metadata: should not include foo
connect stradm/stradm
select streams_type, streams_name, table_owner, table_name, 
  rule_owner, rule_name
from dba_streams_table_rules
where streams_name in ('CAPTURE_MV', 'APPLY_MV')
order by streams_type, streams_name, table_owner, table_name;

select source_object_owner, source_object_name, source_database,
       instantiation_scn
from dba_apply_instantiated_objects 
where source_object_owner='TABOWNER'
order by source_database, source_object_owner, source_object_name;

select object_owner, object_name, operation_name, user_procedure
from dba_apply_dml_handlers
where apply_name = 'APPLY_MV'
order by object_owner, object_name, operation_name;

-- drop mvlog on bar, foo2
connect tabowner/tabowner
drop materialized view log on bar;
drop materialized view log on foo2;

-- remove async_mv streams configuration
connect stradm/stradm
exec async_mv_pkg.remove_async_mv_streams('CAPTURE_MV', 'APPLY_MV');

select 1 from dba_capture where capture_name = 'CAPTURE_MV';
select 1 from dba_apply where apply_name = 'APPLY_MV';

select streams_type, streams_name, table_owner, table_name, 
  rule_owner, rule_name
from dba_streams_table_rules
where streams_name in ('CAPTURE_MV', 'APPLY_MV');

select source_object_owner, source_object_name, source_database,
       instantiation_scn
from dba_apply_instantiated_objects 
where source_object_owner='TABOWNER'
order by source_database, source_object_owner, source_object_name;

select object_owner, object_name, operation_name, user_procedure
from dba_apply_dml_handlers
where apply_name = 'APPLY_MV'
order by object_owner, object_name, operation_name;

-- cleanup
connect system/manager
exec wait_for_jobq;
drop user tabowner cascade;
drop user stradm cascade;
