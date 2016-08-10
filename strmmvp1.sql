Rem
Rem $Header: strmmvp1.sql 29-jan-2007.10:31:25 davzhang Exp $
Rem
Rem strmmvp1.sql
Rem
Rem Copyright (c) 2006, 2007, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      strmmvp1.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem      <short description of component this file declares/defines>
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    davzhang    01/29/07 - increase max_time for verify_table_rules
Rem    wesmith     03/28/06 - Created
Rem

create or replace package async_mv_pkg AUTHID CURRENT_USER as
  type name_arr is table of varchar2(30);
  type mycurtyp IS REF cursor;

  -- collection of table owners
  tab_owners name_arr := name_arr();
  -- collection of table names
  tab_names  name_arr := name_arr();

  -- sets up streams, if necessary, for all tables with MV logs 
  procedure register_tables (capture_name varchar2, apply_name varchar2);

  -- removes streams, if necessary, for all tables that no longer have MV logs
  procedure unregister_tables (capture_name varchar2, apply_name varchar2);

  -- removes ASYNC_MV streams configuration
  procedure remove_async_mv_streams(capture_name varchar2, 
                                    apply_name varchar2);

  -- apply DML handler
  procedure async_mv_dml_hdlr(in_any IN ANYDATA);

  -- apply pre-commit handler
  procedure async_mv_commit_hdlr(commit_scn number);

  -- calls dbms_mview.refresh_dependent() if required
  procedure refresh_dependent(tab_owners name_arr, 
                              tab_names name_arr, commit_scn number);
end;
/

create or replace package body async_mv_pkg as

-- Sets up streams for all tables with MV logs
-- Does the following:
--   set table instatiation scn
--   add apply rule
--   assign dml handler for insert/update/delete
--   add capture rule
procedure register_tables (capture_name varchar2, apply_name varchar2) is

  owner_name varchar2(80);           -- table_owner.table_name
  inst_scn   number;                 -- instantiation SCN
  db         varchar2(128);
  cnt        pls_integer := 0;

  -- tables with MV logs that are not set up for streams
  mycur  mycurtyp;
  need_reg    varchar2(10000) := 
    'select log_owner tabown, master tabname ' ||
    'from dba_mview_logs ' ||
    'minus ' ||
    'select table_owner tabown, table_name tabname ' ||
    'from dba_streams_table_rules ' ||
    'where streams_type = ''APPLY'' ' || 
    'and streams_name = :apply_name';
  tabowner varchar2(30);
  tabname  varchar2(30);


begin
  select global_name into db from global_name;

  open mycur for need_reg using apply_name;

  -- set up streams for all tables needing registration
  loop
    FETCH mycur INTO tabowner, tabname;
    EXIT WHEN mycur%NOTFOUND;        -- exit loop when last row is fetched

    -- construct "<owner>"."<name>"
    owner_name := '"' || tabowner || '"."' || tabname || '"';

    inst_scn := dbms_flashback.get_system_change_number();

    -- if table already has an instantiation scn registered, don't call
    -- dbms_apply_adm.set_table_instantiation_scn()
    select count(*) into cnt
    from dba_apply_instantiated_objects 
    where source_object_owner = tabowner
    and source_object_name = tabname
    and source_object_type = 'TABLE'
    and source_database = db;

    if (cnt = 0) then
      dbms_apply_adm.set_table_instantiation_scn(
        source_object_name    => owner_name,
        source_database_name  => db,
        instantiation_scn     => inst_scn);
    end if;

    -- add apply rule
    dbms_streams_adm.add_table_rules(
      table_name         => owner_name,
      streams_type       => 'apply', 
      streams_name       => apply_name,
      queue_name         => 'stradm.ASYNC_MV_Q',
      include_tagged_lcr => true,
      include_dml        => true,
      include_ddl        => false,
      source_database    => db);

    dbms_apply_adm.set_dml_handler(
      object_name         => owner_name,
      object_type         => 'TABLE',
      operation_name      => 'DEFAULT',
      error_handler       => false,
      user_procedure      => 'stradm.async_mv_pkg.async_mv_dml_hdlr',
      apply_database_link => NULL,
      apply_name          => apply_name);

    -- add capture rule
    dbms_streams_adm.add_table_rules(
      table_name         => owner_name,   
      streams_type       => 'capture',
      streams_name       => capture_name,
      queue_name         => 'stradm.ASYNC_MV_Q',
      include_tagged_lcr => true,
      include_dml        => true,
      include_ddl        => false,
      inclusion_rule     => true);

  end loop;
  close mycur;

exception when others then
  if mycur%isopen then
    close mycur;
  end if;
  raise;
end;

-- Removes streams for all tables no longer having MV logs
-- For each table, it does the following:
--   removes dml handler for insert/update/delete
--   remove capture rule
--   remove apply rule
procedure unregister_tables (capture_name varchar2, apply_name varchar2) is
  owner_name  varchar2(80);
  inst_scn    number;
  db          varchar2(128);

  mycur       mycurtyp;                      --- declare cursor variable
  tabowner    varchar2(30);
  tabname     varchar2(30);
  rowner      varchar2(30);
  rname       varchar2(30);
  strtype     varchar2(11);
  strname     varchar2(30);

  -- tables set up for streams that no longer have MV logs
  need_unreg  varchar2(10000) := 
    'select table_owner tabowner, table_name tabname ' ||
    'from dba_streams_table_rules str ' ||
    'where streams_type = ''APPLY'' ' ||
    'and streams_name = :apply_name ' ||
    'minus ' ||
    'select log_owner tabowner, master tabname ' ||
    'from dba_mview_logs';

  -- corresponding capture/apply rules to remove for the tables 
  -- in the cursor need_unreg
  remove_rules  varchar2(10000) := 
    'select streams_name, streams_type, rule_owner, rule_name ' ||
    'from dba_streams_table_rules str ' ||
    'where ((streams_type = ''CAPTURE'' and streams_name = :capture_name ) ' ||
    '       OR ' ||
    '       (streams_type = ''APPLY'' and streams_name = :apply_name )) ' ||
    'and not exists  ' ||
    '(select 1 from dba_mview_logs mvl ' ||
    ' where str.table_owner = mvl.log_owner ' ||
    ' and str.table_name = mvl.master)';

begin
  select global_name into db from global_name;

  open mycur for need_unreg using apply_name;

  loop
    FETCH mycur INTO tabowner, tabname;
    EXIT WHEN mycur%NOTFOUND;        -- exit loop when last row is fetched

    -- construct "<owner>"."<name>"
    owner_name := '"' || tabowner || '"."' || tabname || '"';

    dbms_apply_adm.set_dml_handler(
      object_name         => owner_name,
      object_type         => 'TABLE',
      operation_name      => 'DEFAULT',
      error_handler       => false,
      user_procedure      => NULL,
      apply_database_link => NULL,
      apply_name          => apply_name);

  end loop;
  close mycur;

  open mycur for remove_rules using capture_name, apply_name;
  loop
    FETCH mycur INTO strname, strtype, rowner, rname;
    EXIT WHEN mycur%NOTFOUND;        -- exit loop when last row is fetched

    -- construct "<owner>"."<name>"
    owner_name := '"' || rowner || '"."' || rname || '"';

    dbms_streams_adm.remove_rule(
      rule_name     => owner_name,   
      streams_type   => strtype,
      streams_name   => strname);
  end loop;
  close mycur;

exception when others then
  if mycur%isopen then
    close mycur;
  end if;
  raise;
end;

-- removes ASYNC_MV streams configuration
procedure remove_async_mv_streams(capture_name varchar2, 
                                  apply_name varchar2) is

  owner_name  varchar2(80);
  inst_scn    number;
  db          varchar2(128);
  cnt         pls_integer := 0;

  -- selects tables set up for ASYNC_MV apply
  cursor apply_tables(apply_name varchar2) is
    select table_owner, table_name
    from dba_streams_table_rules
    where streams_type = 'APPLY'
    and streams_name = apply_name;

  cursor instantiated_objects is
    select source_object_owner table_owner, source_object_name table_name
    from dba_apply_instantiated_objects
    where SOURCE_OBJECT_TYPE = 'TABLE';

begin
  select global_name into db from global_name;

  -- stop capture and apply
  dbms_capture_adm.stop_capture(capture_name);
  dbms_apply_adm.stop_apply(apply_name);

  for rec in apply_tables(apply_name) loop
    -- construct "<owner>"."<name>"
    owner_name := '"' || rec.table_owner || '"."' || rec.table_name || '"';

    -- remove dml handlers
    dbms_apply_adm.set_dml_handler(
      object_name         => owner_name,
      object_type         => 'TABLE',
      operation_name      => 'DEFAULT',
      error_handler       => false,
      user_procedure      => NULL,
      apply_database_link => NULL,
      apply_name          => apply_name);

  end loop;

  -- remove capture and apply
  dbms_apply_adm.drop_apply(apply_name, true);
  dbms_capture_adm.drop_capture(capture_name, true);

  -- remove queues
  dbms_streams_adm.remove_queue('ASYNC_MV_Q');

  -- if there are no apply processes left, remove any dangling 
  -- table instantiation scns
  select count(*) into cnt
  from dba_apply;

  if (cnt = 0) then
    for rec in instantiated_objects loop
      -- construct "<owner>"."<name>"
      owner_name := '"' || rec.table_owner || '"."' || rec.table_name || '"';

      dbms_apply_adm.set_table_instantiation_scn(
        source_object_name    => owner_name,
        source_database_name  => db,
        instantiation_scn     => null);
    end loop;
  else
    -- other apply processes exist, so print a message to the caller
    dbms_output.put_line('NOTE: Other apply processes exist so '||
                         'table instantiation scns were not removed.');
    dbms_output.put_line('To remove them, call ' ||
                         'dbms_apply_adm.set_table_instantiation_scn');
    dbms_output.put_line('Example:');
    dbms_output.put_line('  dbms_apply_adm.set_table_instantiation_scn(');
    dbms_output.put_line('    source_object_name    => ''tabowner.foo'',');
    dbms_output.put_line('    source_database_name  => db,');
    dbms_output.put_line('    instantiation_scn     => null);');
  end if;

end;

-- apply DML handler 
procedure async_mv_dml_hdlr(in_any IN ANYDATA) is
  lcr          SYS.LCR$_ROW_RECORD;
  rc           PLS_INTEGER;
  tab_owner    varchar2(30);
  tab_name     varchar2(30);
  next_pos     pls_integer;
  source_db    varchar2(128);
begin
  -- get LCR info
  rc        := in_any.getobject(lcr);
  tab_owner := lcr.get_object_owner();
  tab_name  := lcr.get_object_name();
  source_db := lcr.get_source_database_name();

  -- see if name already exists in table cache
  if (async_mv_pkg.tab_owners.count > 0) then
    for i in async_mv_pkg.tab_owners.first..async_mv_pkg.tab_owners.last loop
      if (tab_owner = async_mv_pkg.tab_owners(i) and
          tab_name  = async_mv_pkg.tab_names(i)) then
        return;  -- found entry, done
      end if;
    end loop;
  end if;

  -- new table: add to pkg state cache
  if (async_mv_pkg.tab_owners.count = 0) then
    next_pos := 1;
  else
    next_pos := async_mv_pkg.tab_owners.last + 1;
  end if;

  async_mv_pkg.tab_owners.extend;
  async_mv_pkg.tab_owners(next_pos) := tab_owner;
  async_mv_pkg.tab_names.extend;
  async_mv_pkg.tab_names(next_pos)  := tab_name;

end;

--
-- autonmous block to create a scheduler job (this api commits)
--
procedure create_scheduler_job(str varchar2) is
  pragma autonomous_transaction;
begin
  -- submit the refresh job
  dbms_scheduler.create_job(
    job_name    =>  dbms_scheduler.generate_job_name,
    job_type    =>  'PLSQL_BLOCK',
    job_action  =>  str,
    start_date  =>  sysdate,
    enabled     =>  TRUE);
end;

-- pre-commit handler
procedure async_mv_commit_hdlr(commit_scn number) is
  l_jobnum   number;
  str        varchar2(10000);
  ownerlist  varchar2(10000);
  namelist   varchar2(10000);
begin

  -- no table to process, done
  if (async_mv_pkg.tab_owners.count = 0) then
    return;
  end if;

  -- create comma-separated list of table owners and table names
  for i in async_mv_pkg.tab_owners.first..async_mv_pkg.tab_owners.last loop
    if (i > async_mv_pkg.tab_owners.first) then
      ownerlist := ownerlist || ',';
      namelist  := namelist  || ',';
    end if;
    ownerlist := ownerlist || '''' || async_mv_pkg.tab_owners(i) || '''';
    namelist  := namelist  || '''' || async_mv_pkg.tab_names(i)  || '''';
  end loop;

  -- submit a stradm.async_mv_pkg.refresh_dependent() job
  str := 
    'begin ' ||
       'stradm.async_mv_pkg.refresh_dependent(' ||
         'async_mv_pkg.name_arr(' || ownerlist || '), ' ||
         'async_mv_pkg.name_arr(' || namelist || '), ' ||
         commit_scn || '); ' ||
    'end;';

  create_scheduler_job(str);

  -- reset tab_owners, tab_names collections
  async_mv_pkg.tab_owners.delete;
  async_mv_pkg.tab_names.delete;
end;

-- Given a collection of tables, this determines the minimal set of
-- tables to pass to dbms_mview.refresh_dependent()
-- by comparing the commit scn of a transaction with the minimum
-- refresh scns of all MVs dependent on the tables.
procedure refresh_dependent(tab_owners name_arr, tab_names name_arr, 
                            commit_scn number) is
  min_refscn number;
  tablist    varchar2(10000);
  is_first   boolean := TRUE;
  num_fail   number :=0;
begin

  -- construct comma-separated list of table names, 
  -- skipping tables if dependent MVs do not need refreshing
  for i in tab_owners.first..tab_owners.last loop
    begin
      select oldest_refresh_scn into min_refscn
      from all_refresh_dependencies
      where owner = tab_owners(i)
      and table_name = tab_names(i);

      if commit_scn >= min_refscn then
        -- add table to the list
        if not is_first then
          tablist := tablist || ',';
        end if;
        tablist := tablist || 
                   '"' || tab_owners(i) || '"."' || tab_names(i) || '"';
        is_first := FALSE;
      end if;

    exception when no_data_found then
      null;
    end;
  end loop;

  -- do the refresh
  if tablist is not null then
    dbms_mview.refresh_dependent(number_of_failures => num_fail, 
                                 list => tablist, 
                                 refresh_after_errors => TRUE,
                                 atomic_refresh => FALSE,
                                 nested => TRUE);
  end if;
end;

end async_mv_pkg;
/

-- Procedure for verifying results
create or replace procedure verify_tables(tab1 in varchar2, 
                                          tab2 in varchar2, 
                                          max_time in integer default  3600) 
  authid current_user as 

   tab2_sql varchar2(1000);
   tab1_sql varchar2(1000);
   diff_sql varchar2(1000);
   cur      integer;
   ret      integer;
   num_rows integer;
   slept    integer := 0;
   wait_for_convergence boolean := true;
    
begin
  dbms_output.enable ( 10000 ) ; 

  tab1_sql := 'select *  from ' || tab1;

  tab2_sql := 'select *  from ' || tab2;

-- diff_sql is the query that should return 0 rows if the snapshot rows
-- have converged
  diff_sql := '('       || 
               tab1_sql ||
              ' minus '   ||
               tab2_sql ||
               ')'      ||
              ' union '   ||
              '('       ||
               tab2_sql ||
              ' minus '   ||
               tab1_sql ||
              ')' ;


-- get the no. of rows returned by diff_sql. 
     cur := dbms_sql.open_cursor;
     dbms_sql.parse(cur, 
     'select count(*) from ( ' || diff_sql || ' )', dbms_sql.v7);
     sys.dbms_sql.define_column(cur, 1, num_rows);
   begin
     ret := dbms_sql.execute_and_fetch(cur);
     dbms_sql.column_value(cur,1,num_rows);
   exception when others then 
       if dbms_sql.is_open(cur) then
          dbms_sql.close_cursor(cur);
       end if;
   dbms_output.put_line ('-- error while counting rows in diff_sql');
   raise;
   end;

while (wait_for_convergence) loop
   ret := dbms_sql.execute_and_fetch(cur);
   dbms_sql.column_value(cur,1,num_rows);

-- begin num_rows if
   if num_rows = 0 then
     dbms_output.put_line(
   '---------------------------------------------------------------------------'
     );
     dbms_output.put_line('-- Tables/views '||tab1 ||' and  '||tab2 || ' are identical');
     dbms_output.put_line(
   '---------------------------------------------------------------------------'
     );
   wait_for_convergence := false;
   else
     dbms_lock.sleep(3);
     slept := slept + 3;
     if (slept >= max_time ) then  
     dbms_output.put_line(
   '---------------------------------------------------------------------------'
     );
       dbms_output.put_line('-- WARNING: maximum wait time of '|| slept ||' seconds exceeded');
       dbms_output.put_line('-- Tables/views '||tab1 ||' and  '||tab2 || ' are NOT identical');
       dbms_output.put_line('-- Check trace files to determine what is going wrong');
     dbms_output.put_line(
   '---------------------------------------------------------------------------'
     );
       wait_for_convergence := false;
     end if;
   end if;
end loop;
   dbms_sql.close_cursor(cur);

end;
/

create or replace public synonym verify_tables for verify_tables;
grant execute on verify_tables to public;

-- verifies that a table's rules have been added or removed
--   if check_exists = TRUE, it waits until the rules exist
--   else if FALSE, it waits until the rules no longer exist
create or replace procedure verify_table_rules(towner       varchar2, 
                                               tname        varchar2,
                                               check_exists boolean,
                                               max_time     pls_integer := 600)
is
  sleep_total  pls_integer := 0;
  cnt1         pls_integer;
  cnt2         pls_integer;
  done         boolean := false;
begin

  while (not done) loop
    select count(*) into cnt1
    from dba_streams_table_rules
    where streams_name = 'CAPTURE_MV'
    and table_owner = towner
    and table_name = tname;

    select count(*) into cnt2
    from dba_streams_table_rules
    where streams_name = 'APPLY_MV'
    and table_owner = towner
    and table_name = tname;

    if (check_exists) then
      -- check if table is set up for capture/apply
      if (cnt1 > 0 and cnt2 > 0) then
        exit;
      end if;
    else   
      -- check if table is not set up for capture/apply
      if (cnt1 = 0 and cnt2 = 0) then
        exit;
      end if;
    end if;

    -- try again after 3 seconds
    dbms_lock.sleep(3);
    sleep_total := sleep_total + 3;
    if (sleep_total >= max_time ) then  
      dbms_output.put_line('-- Tables rules are NOT verified');
    end if;
  end loop;
  dbms_output.put_line('-- Tables rules are verified');
end;
/

create or replace public synonym verify_table_rules for verify_table_rules;
grant execute on verify_table_rules to public;
