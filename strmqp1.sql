Rem
Rem $Header: strmqp1.sql 23-jun-2006.15:31:45 wesmith Exp $
Rem
Rem strmqp1.sql
Rem
Rem Copyright (c) 2006, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      strmqp1.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem      <short description of component this file declares/defines>
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    wesmith     06/23/06 - register_query: add parameter queue_name
Rem    wesmith     06/12/06 - register_query: add include_extra_attribute()
Rem    wesmith     01/19/06 - Created
Rem

----------------------
-- supporting objects
----------------------

-- master table metadata
create or replace type stq_table_t as object (
  table_owner    varchar2(30),                           /* base table owner */
  table_name     varchar2(30),                            /* base table name */
  refresh_query  varchar2(4000),    /* refresh query used by the dml handler */
  commit_query   varchar2(4000) /* refresh query used by the pre-commit hdlr */
)
/
create or replace type stq_table_list is table of stq_table_t
/

-- query metadata
create or replace type stq_query_t as object (
  query_owner     varchar2(30),                         /* query table owner */
  query_name      varchar2(30),                          /* query table name */
  inst_scn        number,                   /* query table instantiation scn */
  key_cols        stmv_column_list,               /* query table key columns */
  base_tabs       stq_table_list                           /* MV base tables */
)
/
create or replace type stq_queries_t is table of stq_query_t
/

-- store streams query registration metadata
create table stq_reg of stq_query_t (primary key(query_owner, query_name))
  nested table key_cols store as stq_qry_keycol_st
  nested table base_tabs store as stq_tab_st;


------------
-- packages
------------
create or replace package streams_qry_refresh_adm AUTHID CURRENT_USER as
  type mycurtyp IS REF cursor;
  reg_qrys      stq_queries_t := stq_queries_t();

  -- constants
  source_dbname  constant varchar2(256) := dbms_standard.database_name; 

  -- sets up a query for streams-driven refresh
  procedure register_query(query_owner         varchar2, 
                           query_name          varchar2, 
                           base_tables         stq_table_list,
                           capture_name        varchar2 :='CAPTURE_QRY_MASTER',
                           apply_name          varchar2 :='APPLY_QRY_MASTER',
                           queue_name          varchar2 := NULL);

  -- removes a query from streams-driven refresh
  procedure unregister_query(query_owner      varchar2, 
                             query_name       varchar2,
                             capture_name  varchar2 := 'CAPTURE_QRY_MASTER', 
                             apply_name    varchar2 := 'APPLY_QRY_MASTER');

  -- removes streams configuration for streams-driven query refresh
  procedure remove_streams_qry_refresh(
      capture_name varchar2 := 'CAPTURE_QRY_MASTER', 
      apply_name   varchar2 := 'APPLY_QRY_MASTER');

end streams_qry_refresh_adm;
/


--
create or replace package streams_qry_refresh AUTHID CURRENT_USER as

  -- collection of table owners, names in a transaction
  txn_tabs   streams_mv_refresh.txn_tab_list := 
               streams_mv_refresh.txn_tab_list();

  -- dml types
  insert_dml   constant pls_integer   := 1;
  update_dml   constant pls_integer   := 2;
  delete_dml   constant pls_integer   := 4;

  rowid_col    constant varchar2(5)   := 'ROWID';
  scn_bind     constant varchar2(5)   := 'SCN';
  rowid_bind   constant varchar2(5)   := 'RID';

  -- dml handler used by streams-driven refresh
  procedure source_dml_handler(lcr_anydata in sys.anydata);

  -- pre-commit handler used by streams-driven refresh
  procedure source_commit_hdlr(commit_scn number);

  procedure print_trace(mesg varchar2);
end streams_qry_refresh;
/


--
create or replace package streams_qry_lcr AUTHID CURRENT_USER as
  -- applies an LCR to an MV during streams-based refresh
  procedure process_lcr(lcr sys.lcr$_row_record);
end streams_qry_lcr;
/


--
create or replace package body streams_qry_refresh_adm as

  cursor query_reg is select sys_nc_rowinfo$ from stq_reg;

--
-- register a query for streams-driven refresh
--
procedure register_query(query_owner         varchar2, 
                         query_name          varchar2, 
                         base_tables         stq_table_list,
                         capture_name        varchar2 := 'CAPTURE_QRY_MASTER',
                         apply_name          varchar2 := 'APPLY_QRY_MASTER',
                         queue_name          varchar2 := NULL) is

  canon_qry_owner  varchar2(30);
  canon_qry_name   varchar2(30);
  canon_tab_owner  varchar2(30);
  canon_tab_name   varchar2(30);
  canon_capture    varchar2(30);
  canon_apply      varchar2(30);
  canon_queue      varchar2(30);
  owner_name       varchar2(80);           -- table_owner.table_name
  qryinfo          stq_query_t := NULL;
  cnt              pls_integer;
  select_pos       pls_integer := 0;
  select_start     pls_integer := 0;
  inst_scn         number;                 -- instantiation SCN
  key_cols         stmv_column_list;

  canon_schema     varchar2(30);
  canon_part1      varchar2(30);
  canon_part2      varchar2(30);
  canon_dblink     varchar2(128);
  canon_part1_type number;
  object_number    number;

begin
  streams_qry_refresh.print_trace('register_query()+');
  streams_qry_refresh.print_trace('query_owner='||query_owner);
  streams_qry_refresh.print_trace('query_name='||query_name);
  streams_qry_refresh.print_trace('capture_name='||capture_name);
  streams_qry_refresh.print_trace('apply_name='||apply_name);
  streams_qry_refresh.print_trace('queue_name='||queue_name);

  -- canonicalize inputs
  dbms_utility.canonicalize(query_owner, canon_qry_owner, 30);
  dbms_utility.canonicalize(query_name, canon_qry_name, 30);
  dbms_utility.canonicalize(capture_name, canon_capture, 30);
  dbms_utility.canonicalize(apply_name, canon_apply, 30);
  dbms_utility.canonicalize(queue_name, canon_queue, 30);

   -- make sure the apply process is down
  select count(*) into cnt from dba_apply 
  where apply_name = canon_apply 
  and status = 'ENABLED';
  if (cnt > 0) then
    raise_application_error(-20000, 'Apply process must be disabled');
  end if;

  -- get queue_name from capture and apply process views if NULL
  if (canon_queue is NULL) then
    declare
      apply_queue   varchar2(30);
      capture_queue varchar2(30);
    begin
      -- get queue for capture and apply
      select queue_name into capture_queue
      from dba_capture
      where capture_name = canon_capture;

      select queue_name into apply_queue
      from dba_apply
      where apply_name = canon_apply;

      -- capture and apply must share the same queue
      if apply_queue != capture_queue then
        raise_application_error(-20000,
          'capture and apply must share the same queue');
      end if;

      canon_queue := capture_queue;

    exception
      when NO_DATA_FOUND then
        raise_application_error(-20000, 'must specify queue_name');
    end;
  end if;

  --
  -- make sure base_tables is not empty
  --
  if (base_tables is null or base_tables.count = 0) then
    raise_application_error(-20000, 
                            'base_tables collection must not be empty');
  end if;

  --
  -- error if query registration metadata already exists
  --
  select count(*) into cnt
  from stq_reg
  where query_owner = canon_qry_owner
  and query_name = canon_qry_name;
  if (cnt > 0) then
    owner_name := '"' || canon_qry_owner || '"."' || canon_qry_name || '"';
    raise_application_error(-20000, 
      'Query ' || owner_name || ' already registered');
  end if;

  --
  -- check that tables exist
  --
  -- construct "<owner>"."<name>"
  owner_name := '"' || canon_qry_owner || '"."' || canon_qry_name || '"';
  dbms_utility.name_resolve(owner_name, 2, canon_schema, canon_part1,
    canon_part2, canon_dblink, canon_part1_type, object_number);

  for i in base_tables.first .. base_tables.last loop
    dbms_utility.canonicalize(base_tables(i).table_owner, canon_tab_owner, 30);
    dbms_utility.canonicalize(base_tables(i).table_name, canon_tab_name, 30);
    owner_name := '"' || canon_tab_owner || '"."' || canon_tab_name || '"';

    dbms_utility.name_resolve(owner_name, 2, canon_schema, canon_part1,
      canon_part2, canon_dblink, canon_part1_type, object_number);
  end loop;

  --
  -- validate refresh query: 
  --   begins with select
  --   has :rid, :scn binds
  for i in base_tables.first .. base_tables.last loop
    dbms_utility.canonicalize(base_tables(i).table_owner, canon_tab_owner, 30);
    dbms_utility.canonicalize(base_tables(i).table_name, canon_tab_name, 30);
    owner_name := '"' || canon_tab_owner || '"."' || canon_tab_name || '"';

    if (instr(upper(base_tables(i).refresh_query), 'SELECT') != 1 or
        instr(upper(base_tables(i).refresh_query), ':RID') = 0 or
        instr(upper(base_tables(i).refresh_query), ':SCN') = 0) then
      raise_application_error(-20000, 
        'Refresh query for ' || owner_name || ' has errors');
    end if;
  end loop;

  -- get query key columns
  key_cols := streams_mv_refresh_adm.get_keycols(canon_qry_owner, 
                                                 canon_qry_name);
  if (key_cols.count = 0) then
    raise_application_error(-20000, 
         'query table must have a PK or use dbms_apply_adm.set_key_columns()');
  end if;
  if (key_cols.count > 0) then
    for i in key_cols.first .. key_cols.last loop
      streams_qry_refresh.print_trace('key_col='||key_cols(i));
    end loop;
  end if;

  --
  -- initialize query metadata
  --
  qryinfo := stq_query_t(canon_qry_owner, canon_qry_name, null, 
                         stmv_column_list(), stq_table_list());
  -- get table keycols
  qryinfo.key_cols := key_cols;

  -- store table information
  qryinfo.base_tabs := base_tables;

  -- process all base tables
  for i in base_tables.first .. base_tables.last loop
    dbms_utility.canonicalize(base_tables(i).table_owner, canon_tab_owner, 30);
    dbms_utility.canonicalize(base_tables(i).table_name, canon_tab_name, 30);

    -- save canonicalized tab owner/name in query metadata
    qryinfo.base_tabs(i).table_owner := canon_tab_owner;
    qryinfo.base_tabs(i).table_name  := canon_tab_name;

    -- construct "<owner>"."<name>"
    owner_name := '"' || canon_tab_owner || '"."' || canon_tab_name || '"';

    -- add apply rule
    dbms_streams_adm.add_table_rules(
      table_name         => owner_name,
      streams_type       => 'apply', 
      streams_name       => canon_apply,
      queue_name         => canon_queue,
      include_tagged_lcr => true,
      include_dml        => true,
      include_ddl        => false,
      source_database    => streams_qry_refresh_adm.source_dbname);

    dbms_apply_adm.set_dml_handler(
      object_name         => owner_name,
      object_type         => 'TABLE',
      operation_name      => 'DEFAULT',
      error_handler       => false,
      user_procedure      => 'streams_qry_refresh.source_dml_handler',
      apply_database_link => NULL,
      apply_name          => canon_apply);

    -- add capture rule
    dbms_streams_adm.add_table_rules(
      table_name         => owner_name,   
      streams_type       => 'capture',
      streams_name       => canon_capture,
      queue_name         => canon_queue,
      include_tagged_lcr => true,
      include_dml        => true,
      include_ddl        => false,
      inclusion_rule     => true);

  end loop;

  -- capture rowids, too
  dbms_capture_adm.include_extra_attribute(canon_capture, 'row_id', true);

  -- TODO: instantiate query table
  --
  -- get instantiation scn by using current scn 
  -- (must be done after setting capture and apply table rules)
  --
  inst_scn := dbms_flashback.get_system_change_number();

  -- set instantiation scn
  for i in base_tables.first .. base_tables.last loop
    dbms_utility.canonicalize(base_tables(i).table_owner, canon_tab_owner, 30);
    dbms_utility.canonicalize(base_tables(i).table_name, canon_tab_name, 30);

    -- construct "<owner>"."<name>"
    owner_name := '"' || canon_tab_owner || '"."' || canon_tab_name || '"';

    -- if table already has an instantiation scn registered, don't call
    -- dbms_apply_adm.set_table_instantiation_scn()
    select count(*) into cnt
    from dba_apply_instantiated_objects 
    where source_object_owner = canon_tab_owner
    and source_object_name = canon_tab_name
    and source_object_type = 'TABLE'
    and source_database = streams_qry_refresh_adm.source_dbname;

    if (cnt = 0) then
      dbms_apply_adm.set_table_instantiation_scn(
        source_object_name    => owner_name,
        source_database_name  => streams_qry_refresh_adm.source_dbname,
        instantiation_scn     => inst_scn);
    end if;
  end loop;

  qryinfo.inst_scn := inst_scn;

  --
  -- save query registration metadata to disk
  --
  insert into stq_reg values(qryinfo);
  commit;

  streams_qry_refresh.print_trace('register_query()-');

end;


--
-- remove a query from streams-driven refresh
--
procedure unregister_query(query_owner   varchar2, 
                           query_name    varchar2,
                           capture_name  varchar2 := 'CAPTURE_QRY_MASTER', 
                           apply_name    varchar2 := 'APPLY_QRY_MASTER') is

  mycur           mycurtyp;
  tabowner        varchar2(30);
  tabname         varchar2(30);
  owner_name      varchar2(80);
  cnt             pls_integer;
  canon_qry_owner varchar2(30);
  canon_qry_name  varchar2(30);
  canon_capture   varchar2(30);
  canon_apply     varchar2(30);

  -- all table rules associated with the specified capture, apply and table
  cursor table_rules(capture_name varchar2, apply_name varchar2,
                     table_owner varchar2, table_name varchar2) is
    select streams_name, streams_type, rule_owner, rule_name 
    from dba_streams_table_rules str 
    where streams_type = 'CAPTURE'
    and streams_name = capture_name
    and table_owner = table_rules.table_owner
    and table_name = table_rules.table_name
    union 
    select streams_name, streams_type, rule_owner, rule_name 
    from dba_streams_table_rules str 
    where streams_type = 'APPLY'
    and streams_name = apply_name
    and table_owner = table_rules.table_owner
    and table_name = table_rules.table_name;

  -- select all master tables registered by this query that is not used
  -- by other registered queries
  qry_base_tables varchar2(1000) :=
'select bt.table_owner, bt.table_name
 from stq_reg q, table(q.base_tabs) bt
 where q.query_owner = :qry_owner
 and   q.query_name  = :qry_name
 and not exists
 (select 1
  from stq_reg q2, table(q2.base_tabs) bt2
  where not (q2.query_owner = q.query_owner 
             and q2.query_name = q.query_name)
  and   bt.table_owner = bt2.table_owner
  and   bt.table_name  = bt2.table_name)';

begin
  streams_qry_refresh.print_trace('unregister_query()+');

  -- canonicalize inputs
  dbms_utility.canonicalize(query_owner, canon_qry_owner, 30);
  dbms_utility.canonicalize(query_name, canon_qry_name, 30);
  dbms_utility.canonicalize(capture_name, canon_capture, 30);
  dbms_utility.canonicalize(apply_name, canon_apply, 30);

  -- make sure the apply process is down
  select count(*) into cnt from dba_apply 
  where apply_name = canon_apply 
  and status = 'ENABLED';
  if (cnt > 0) then
      raise_application_error(-20000, 
        'Apply process must be disabled');
  end if;

  owner_name := '"' || canon_qry_owner || '"."' || canon_qry_name || '"';

  --
  -- get query base tables
  --
  open mycur for qry_base_tables using canon_qry_owner, canon_qry_name;

  loop
    fetch mycur into tabowner, tabname;
    exit when mycur%notfound;             -- exit loop when last row is fetched

    -- construct "<owner>"."<name>"
    owner_name := '"' || tabowner || '"."' || tabname || '"';

    -- remove DML handlers
    dbms_apply_adm.set_dml_handler(
      object_name         => owner_name,
      object_type         => 'TABLE',
      operation_name      => 'DEFAULT',
      error_handler       => false,
      user_procedure      => NULL,
      apply_database_link => NULL,
      apply_name          => canon_apply);

    -- remove rules
    for rec in table_rules(canon_capture, canon_apply, tabowner, tabname) loop
      -- construct "<owner>"."<name>"
      owner_name := '"' || rec.rule_owner || '"."' || rec.rule_name || '"';

      dbms_streams_adm.remove_rule(
        rule_name     => owner_name,   
        streams_type  => rec.streams_type,
        streams_name  => rec.streams_name);
    end loop;
  end loop;

  -- delete query from registration metadata
  delete from stq_reg 
  where query_owner = canon_qry_owner 
  and query_name = canon_qry_name;

  commit;

  streams_qry_refresh.print_trace('unregister_query()-');

exception when others then
  if mycur%isopen then
    close mycur;
  end if;
  raise;
end;


--
-- remove streams used for streams-driven refresh
--
procedure remove_streams_qry_refresh(
    capture_name varchar2 := 'CAPTURE_QRY_MASTER', 
    apply_name   varchar2 := 'APPLY_QRY_MASTER') is

  owner_name      varchar2(80);
  inst_scn        number;
  db              varchar2(128);
  cnt             pls_integer := 0;
  canon_capture   varchar2(30);
  canon_apply     varchar2(30);
  capture_queue   varchar2(30);
  apply_queue     varchar2(30);

  -- selects tables set up for query refresh apply
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
  streams_qry_refresh.print_trace('remove_streams_qry_refresh()+');

  -- canonicalize inputs
  dbms_utility.canonicalize(capture_name, canon_capture, 30);
  dbms_utility.canonicalize(apply_name, canon_apply, 30);

  -- stop capture and apply
  dbms_capture_adm.stop_capture(canon_capture);
  dbms_apply_adm.stop_apply(canon_apply);

  -- get queue_name from capture process
  begin
    select queue_name into capture_queue
    from dba_capture
    where capture_name = canon_capture;
  exception
    when NO_DATA_FOUND then
      raise_application_error(-20000, 'capture not found');
  end;

  -- get queue_name from apply process
  begin
    select queue_name into apply_queue
    from dba_apply
    where apply_name = canon_apply;
  exception
    when NO_DATA_FOUND then
      raise_application_error(-20000, 'apply not found');
  end;

  -- capture and apply must share the same queue
  if apply_queue != capture_queue then
    raise_application_error(-20000,
      'capture and apply must share the same queue');
  end if;

  for rec in apply_tables(canon_apply) loop
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
      apply_name          => canon_apply);

  end loop;

  -- remove capture and apply
  dbms_apply_adm.drop_apply(canon_apply, true);
  dbms_capture_adm.drop_capture(canon_capture, true);

  -- remove queues
  dbms_streams_adm.remove_queue(capture_queue);

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
        source_database_name  => streams_qry_refresh_adm.source_dbname,
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

  -- delete any query registration metadata
  delete from stq_reg;
  commit;

  streams_qry_refresh.print_trace('remove_streams_qry_refresh()-');

end;

--
-- package instantiation code
--
begin
  -- read in query registration metadata
  for rec in query_reg loop
    reg_qrys.extend;
    reg_qrys(reg_qrys.last) := rec.sys_nc_rowinfo$;
  end loop;
end streams_qry_refresh_adm;
/


--
create or replace package body streams_qry_refresh as

--
-- prints debug statements to the trace file
--
procedure print_trace(mesg varchar2) is
begin
  -- enable this line if you want tracing
  --sys.dbms_system.ksdwrt(1, mesg);

  null;
end;


--
-- returns list of registered queries
--
function get_qrys return stq_queries_t is
begin
  return streams_qry_refresh_adm.reg_qrys;
end;


--
-- create flashback query relative to a base table in the query
--
function create_query(delete_or_upsert  varchar2,  
                      qry_no            pls_integer,
                      tab_no            pls_integer,
                      is_commit_query   boolean) return varchar2 is

  query_after  varchar2(32000);                          -- query at commit scn
  query        varchar2(32000);
  prev_pos     number :=1;
  cur_pos      number;

begin
  print_trace('create_query()+');

  if (is_commit_query) then
    query_after := 
      get_qrys()(qry_no).base_tabs(tab_no).commit_query;
  else
    query_after := 
      get_qrys()(qry_no).base_tabs(tab_no).refresh_query;
  end if;

  -- replace all instances of ':SCN' with ':SCN-1'
  -- to create a query which will get the 'before' image
  while (true) loop
    cur_pos := instr(upper(query_after), ':SCN', prev_pos);

    if (cur_pos = 0) then
      exit;
    end if;

    query := query || substr(query_after, prev_pos, cur_pos + 4 - prev_pos) 
                   || '-1';
    prev_pos := cur_pos +4;
  end loop;

  query := query || substr(query_after, prev_pos);

  if (delete_or_upsert = 'DELETE') then
    query := query || ' MINUS ' || query_after;
  else
    query := query_after || ' MINUS ' || query;
  end if;

  print_trace('sql_stmt='||query);
  print_trace('create_query()-');
  return query;
   
end create_query;


--
-- find the next query that has owner.name as a base table 
-- qry_no, tab_no keeps track of current position in query 
--   registration metadata
--
function find_next_qry(owner   varchar2, 
                       name    varchar2,
                       qry_no  in out pls_integer,
                       tab_no  in out pls_integer) return boolean is

begin
  print_trace('find_next_qry()+');

  if qry_no is null then
    qry_no := get_qrys().first;
    tab_no := get_qrys()(qry_no).base_tabs.first;
  else
    tab_no := get_qrys()(qry_no).base_tabs.next(tab_no);
    if tab_no is null then
      qry_no := get_qrys().next(qry_no);
      if qry_no is not null then
        tab_no := get_qrys()(qry_no).base_tabs.first;
      end if;
    end if;
  end if;

  -- find next query that has owner.name as a base table
  while qry_no is not null loop
    while tab_no is not null loop
      if (get_qrys()(qry_no).base_tabs(tab_no).table_owner = owner 
          and
          get_qrys()(qry_no).base_tabs(tab_no).table_name = name) then

         print_trace('found query: '|| get_qrys()(qry_no).query_owner || '.' ||
                     get_qrys()(qry_no).query_name);
         print_trace('find_next_qry()-');
         return TRUE;
      end if;

      tab_no := get_qrys()(qry_no).base_tabs.next(tab_no);
    end loop;

    qry_no := get_qrys().next(qry_no);
    if (qry_no is not null) then
      tab_no := get_qrys()(qry_no).base_tabs.first;
    end if;

  end loop;

  print_trace('find_next_qry()-');
  return FALSE;
end find_next_qry;


--
-- for each row fetched from flashback query, creates and executes LCRs
--
-- NOTE: no lobs in this version 
procedure build_and_process_lcrs(sql_cursor      pls_integer, 
                                 num_cols        pls_integer,
                                 desc_table      sys.dbms_sql.desc_tab,
                                 source_dbname   varchar2,
                                 cmd_type        varchar2,
                                 obj_owner       varchar2,
                                 obj_name        varchar2,
                                 key_cols        stmv_column_list) is

  num_rows    number;
  lcr         sys.lcr$_row_record;
  is_key_col  stmv_num_list  := stmv_num_list(0);
  first_time  boolean := TRUE;

begin
  print_trace('build_and_process_lcrs()+');
  print_trace('cmd_type='||cmd_type);
  print_trace('obj_owner='||obj_owner);
  print_trace('obj_name='||obj_name);

  -- set up is_key_col array
  -- initialize
  is_key_col.extend(num_cols-1,1);
  -- set elements that are key cols
  for i in 1..num_cols loop
    for j in key_cols.first .. key_cols.last loop
      if (key_cols(j) = desc_table(i).col_name) then
        is_key_col(i) := 1;
        exit;
      end if;
    end loop;
  end loop;

  -- execute the select statement
  num_rows := dbms_sql.execute(sql_cursor);

  lcr := sys.lcr$_row_record.construct(
           source_database_name=>source_dbname,
           command_type=>cmd_type,
           object_owner=>obj_owner,
           object_name=>obj_name,
           old_values=>NULL,
           new_values=>NULL); 

  -- loop until no more rows are returned
  while dbms_sql.fetch_rows(sql_cursor) > 0 loop
    print_trace('fetched row');

    streams_mv_refresh.get_column_values(sql_cursor, num_cols, desc_table, 
                                         is_key_col, lcr, cmd_type, 
                                         first_time);

    first_time := FALSE;

    streams_qry_lcr.process_lcr(lcr);

  end loop;

  print_trace('build_and_process_lcrs()-');
end;
 

--
--
--
procedure execute_delta_query(qry_idx          pls_integer, 
                              tab_idx          pls_integer, 
                              dml_type         varchar2, 
                              is_commit_query  boolean,
                              commit_scn       number,
                              rowids           streams_mv_refresh.rowid_list) 
is

  sql_stmt    varchar2(32000);
  sql_cursor  pls_integer; 
  num_cols    pls_integer;
  desc_table  sys.dbms_sql.desc_tab;
  query_owner varchar2(30);
  query_name  varchar2(30);
  key_cols    stmv_column_list;

begin
  print_trace('execute_delta_query()+');

  query_owner := get_qrys()(qry_idx).query_owner;
  query_name  := get_qrys()(qry_idx).query_name;
  key_cols    := get_qrys()(qry_idx).key_cols;

  sql_stmt := create_query(dml_type, qry_idx, tab_idx, is_commit_query);
  sql_cursor := dbms_sql.open_cursor;     
  dbms_sql.parse(sql_cursor, sql_stmt, sys.dbms_sql.v7); 
  dbms_sql.describe_columns(sql_cursor, num_cols, desc_table);      

  -- loop through the columns and do the define columns
  streams_mv_refresh.define_columns(sql_cursor, num_cols, desc_table);

  -- bind the scn variable once
  dbms_sql.bind_variable(sql_cursor, streams_qry_refresh.scn_bind, commit_scn);

  if (is_commit_query) then
    build_and_process_lcrs(
      sql_cursor, 
      num_cols, 
      desc_table,
      streams_qry_refresh_adm.source_dbname,
      dml_type,
      query_owner,
      query_name, 
      key_cols);
  else
    for i in rowids.first .. rowids.last loop
      dbms_sql.bind_variable(sql_cursor, streams_qry_refresh.rowid_bind, 
                             rowids(i));
      build_and_process_lcrs(
        sql_cursor, 
        num_cols, 
        desc_table,
        streams_qry_refresh_adm.source_dbname,
        dml_type,
        query_owner,
        query_name, 
        key_cols);

    end loop;
  end if;

  dbms_sql.close_cursor(sql_cursor);
  print_trace('execute_delta_query()-');

exception
  when others then
    if dbms_sql.is_open(sql_cursor) then
      dbms_sql.close_cursor(sql_cursor);
    end if;
    raise;
end;


--
-- DML handler
--
procedure source_dml_handler(lcr_anydata sys.anydata) is

  lcr                 sys.lcr$_row_record;
  t                   pls_integer;
  qry_i               pls_integer := null;  -- iterator
  tab_i               pls_integer := null;  -- iterator
  commit_scn          number := null;
  dml_type            varchar2(10);
  dml_code            pls_integer;
  ridval              sys.anydata;

BEGIN
  print_trace('source_dml_handler()+');

  t := lcr_anydata.getObject(lcr);
  commit_scn  := lcr.get_commit_scn();
  ridval := lcr.get_extra_attribute('row_id');
  if (ridval is null) then
    raise_application_error(-20000, 
      'supplemental logging missing for rowid on ' ||
      lcr.get_object_owner || '.' || lcr.get_object_name);
  end if;

  print_trace('commit_scn='||commit_scn);

  --
  -- streams refresh codepath
  --
  while find_next_qry(lcr.get_object_owner, lcr.get_object_name, 
                      qry_i, tab_i) loop
    -- skip this query if commit scn of this LCR <= the query's instantiation 
    -- scn
    print_trace('query inst_scn=' || get_qrys()(qry_i).inst_scn);
    if (commit_scn > get_qrys()(qry_i).inst_scn) then

      dml_type := lcr.get_command_type;
      print_trace('command_type='||dml_type);

      if (dml_type = 'INSERT') then
        dml_code := streams_qry_refresh.insert_dml;
      elsif (dml_type = 'UPDATE') then
        dml_code := streams_qry_refresh.update_dml;
      elsif (dml_type = 'DELETE') then
        dml_code := streams_qry_refresh.delete_dml;
      else
        dml_code := 0;
      end if;

      -- add table to collection for commit-time processing
      streams_mv_refresh.add_to_table_coll(lcr.get_object_owner, 
                                           lcr.get_object_name, 
                                           dml_code, 
                                           ridval.AccessURowid,
                                           streams_qry_refresh.txn_tabs);
    end if;
  end loop;

  print_trace('source_dml_handler()-');
end;


--
-- pre-commit handler
--
procedure source_commit_hdlr(commit_scn number) is

  qry_i        pls_integer := null;  -- iterator
  tab_i        pls_integer := null;  -- iterator
  lcr          sys.lcr$_row_record;
  dml_type     pls_integer;
  tab_owner    varchar2(30);
  tab_name     varchar2(30);
  has_deletes  boolean;
  has_inserts  boolean;

begin
  print_trace('source_commit_hdlr()+');

  -- streams-based refresh
  if (streams_qry_refresh.txn_tabs.count > 0) then
    print_trace('commit_scn='||commit_scn);

    for i in streams_qry_refresh.txn_tabs.first..
             streams_qry_refresh.txn_tabs.last loop

      tab_owner   := streams_qry_refresh.txn_tabs(i).table_owner;
      tab_name    := streams_qry_refresh.txn_tabs(i).table_name;
      has_deletes := streams_qry_refresh.txn_tabs(i).delete_rowids.count > 0;
      has_inserts := streams_qry_refresh.txn_tabs(i).insert_rowids.count > 0;

      print_trace('table=' || tab_owner || '.' || tab_name);

      while find_next_qry(tab_owner, tab_name, qry_i, tab_i) loop
 
        print_trace('qry=' || get_qrys()(qry_i).query_owner || '.' ||
                    get_qrys()(qry_i).query_name);

        print_trace('qry inst_scn=' || get_qrys()(qry_i).inst_scn);
        if (commit_scn > get_qrys()(qry_i).inst_scn) then

          print_trace('delete phase:');
	  if (has_deletes) then
            execute_delta_query(qry_i, tab_i, 'DELETE', FALSE, commit_scn,
                                streams_qry_refresh.txn_tabs(i).delete_rowids);
          end if;
          if (has_inserts) then
            execute_delta_query(qry_i, tab_i, 'DELETE', FALSE, commit_scn,
                                streams_qry_refresh.txn_tabs(i).insert_rowids);
          end if;

          print_trace('insert phase:');
          if (has_deletes) then
            execute_delta_query(qry_i, tab_i, 'UPDATE', FALSE, commit_scn,
                                streams_qry_refresh.txn_tabs(i).delete_rowids);
          end if;
          if (has_inserts) then
            execute_delta_query(qry_i, tab_i, 'UPDATE', FALSE, commit_scn,
                                streams_qry_refresh.txn_tabs(i).insert_rowids);
          end if;

          if (get_qrys()(qry_i).base_tabs(tab_i).commit_query is not null) then
            print_trace('delete phase: commit_query');
            execute_delta_query(qry_i, tab_i, 'DELETE', TRUE, commit_scn,NULL);

            print_trace('insert phase: commit_query');
            execute_delta_query(qry_i, tab_i, 'UPDATE', TRUE, commit_scn,NULL);
          end if;
        end if;
      end loop;

    end loop;

    -- reset tab_owners, tab_names collections
    streams_qry_refresh.txn_tabs.delete;

  end if;

  print_trace('source_commit_hdlr()-');
end;

end streams_qry_refresh;
/

create or replace package body streams_qry_lcr as

--
-- apply the LCR
--
procedure process_lcr(lcr sys.lcr$_row_record) is

  -- new exception raised during apply to replace ora-1403
  row_does_not_exist exception;
  pragma exception_init(row_does_not_exist, -26787);

begin
  streams_qry_refresh.print_trace('process_lcr()+');

  -- this function may be customized to call a different procedure
  begin
    lcr.execute(true);
    streams_qry_refresh.print_trace('applied LCR');

  exception
    when DUP_VAL_ON_INDEX then
      streams_qry_refresh.print_trace('DUP_VAL_ON_INDEX, type='||
                                      lcr.get_command_type);

    when NO_DATA_FOUND or row_does_not_exist then
      streams_qry_refresh.print_trace('NO_DATA_FOUND, type='||
                                      lcr.get_command_type);
      streams_mv_lcr.do_an_insert(lcr);
    when others then
      raise;
  end;

  streams_qry_refresh.print_trace('process_lcr()-');
end;

end streams_qry_lcr;
/
