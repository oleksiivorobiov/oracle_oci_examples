Rem
Rem $Header: strmmvp2.sql 23-jun-2006.13:43:59 wesmith Exp $
Rem
Rem strmmvp2.sql
Rem
Rem Copyright (c) 2006, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      strmmvp2.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem      <short description of component this file declares/defines>
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    wesmith     06/23/06 - register_mv: add parameter queue_name
Rem    wesmith     06/12/06 - LRG 2247322: remove insert append hint 
Rem    wesmith     01/19/06 - Created
Rem

----------------------
-- supporting objects
----------------------
create or replace type stmv_num_list is table of number
/
create or replace type stmv_name_list is table of varchar2(30)
/
create or replace type stmv_anydata_list is table of anydata
/
create or replace type stmv_column_list is table of varchar2(30)
/

-- master table metadata
create or replace type stmv_table_t as object (
  table_owner    varchar2(30),
  table_name     varchar2(30),
  table_alias    varchar2(30),
  key_cols       stmv_column_list
)
/
create or replace type stmv_table_list is table of stmv_table_t
/

-- MV metadata
create or replace type stmv_mv_t as object (
  mv_owner        varchar2(30),                                  /* MV owner */
  mv_name         varchar2(30),                                   /* MV name */
  inst_scn        number,                            /* MV instantiation scn */
  use_str_refresh number,             /* streams-based refresh ? 1=yes, 0=no */
  mjv             number,                                        /* obsolete */
  flags           number,                                   /* MV properties */
  key_cols        stmv_column_list,                        /* MV key columns */
  select_list     varchar2(4000),                          /* MV select list */
  base_tabs       stmv_table_list,                         /* MV base tables */
  where_clause    varchar2(4000)                          /* MV where clause */
)
/
create or replace type stmv_mvs_t is table of stmv_mv_t
/

-- store streams MV metadata
create table stmv_reg of stmv_mv_t (primary key(mv_owner, mv_name))
  nested table key_cols store as stmv_mv_keycol_st
  nested table base_tabs store as stmv_tab_st
  (nested table key_cols store as stmv_tab_keycol_st);

-- temp table to store rowids for a transaction
create global temporary table stmv_rid 
(table_owner varchar2(30),
 table_name  varchar2(30),
 dml_type    number, 
 rid         rowid
)
on commit delete rows;

create index rid_i1 on stmv_rid(table_owner, table_name, dml_type);


------------
-- packages
------------
create or replace package streams_mv_refresh_adm AUTHID CURRENT_USER as
  type mycurtyp IS REF cursor;
  reg_mvs        stmv_mvs_t := stmv_mvs_t();

  -- constants
  source_dbname  constant varchar2(256) := dbms_standard.database_name; 

  -- constants for flags
  flg_mjv     constant pls_integer   := 1;
  flg_hasoj   constant pls_integer   := 2;

  -- returns primary key columns for a table
  function get_pkcols(tabowner in varchar2, tabname in varchar2)
    return stmv_column_list;

  -- returns key columns for a table 
  -- (PK or from the api dbms_apply_adm.set_key_columns)
  function get_keycols(tabowner in varchar2, tabname in varchar2)
    return stmv_column_list;

  -- sets up an MV for streams-driven refresh
  procedure register_mv(mv_owner             varchar2, 
                        mv_name              varchar2, 
                        capture_name         varchar2  := 'CAPTURE_MV_MASTER',
                        apply_name           varchar2  := 'APPLY_MV_MASTER',
                        queue_name           varchar2  := NULL,
                        instantiate          boolean   := FALSE, 
                        use_streams_refresh  boolean   := FALSE,
                        mv_dml_handler       varchar2  := NULL);

  -- removes an MV from streams-driven refresh
  procedure unregister_mv(mv_owner      varchar2, 
                          mv_name       varchar2,
                          capture_name  varchar2 := 'CAPTURE_MV_MASTER', 
                          apply_name    varchar2 := 'APPLY_MV_MASTER');

  -- removes streams configuration for streams-driven MV refresh
  procedure remove_streams_mv_refresh(
      capture_name varchar2 := 'CAPTURE_MV_MASTER', 
      apply_name   varchar2 := 'APPLY_MV_MASTER');

end streams_mv_refresh_adm;
/


create or replace package streams_mv_refresh AUTHID CURRENT_USER as
  type rowid_list is table of varchar2(18);
  type txn_tab_t is record (
    table_owner    varchar2(30),
    table_name     varchar2(30),
    dmltypes       pls_integer,
    delete_rowids  rowid_list,
    insert_rowids  rowid_list
  );
  type txn_tab_list is table of txn_tab_t;

  -- collection of MV owners, names
  txn_mvs   txn_tab_list := txn_tab_list();

  -- collection of table owners, names
  txn_tabs      txn_tab_list := txn_tab_list();

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

  procedure add_to_table_coll(tab_owner    IN varchar2, 
                              tab_name     IN varchar2, 
                              tab_dmltype  IN pls_integer,
                              tab_rowid    IN urowid,
                              tab_coll     IN OUT NOCOPY txn_tab_list);

  procedure define_columns(sql_cursor  pls_integer,
                           num_cols    pls_integer, 
                           desc_table  sys.dbms_sql.desc_tab);

  procedure get_column_values(sql_cursor  pls_integer,
                              num_cols    pls_integer, 
                              desc_table  sys.dbms_sql.desc_tab,
                              is_key_col  stmv_num_list,
                              lcr         in out nocopy sys.lcr$_row_record,
                              cmd_type    varchar2,
                              first_time  boolean);

  procedure print_trace(mesg varchar2);

end streams_mv_refresh;
/


create or replace package streams_mv_lcr AUTHID CURRENT_USER as
  -- applies an LCR to an MV during streams-based refresh
  procedure process_lcr(lcr sys.lcr$_row_record);

  procedure print_lcr_cols(cols sys.lcr$_row_list);
  procedure do_an_insert(lcr sys.lcr$_row_record);
end streams_mv_lcr;
/


create or replace package body streams_mv_refresh_adm as
  cursor mv_group is select sys_nc_rowinfo$ from stmv_reg;

--
-- returns primary key columns for a table, if they exist
--
function get_pkcols(tabowner in varchar2, tabname in varchar2)
  return stmv_column_list is

  pk_columns   stmv_column_list := stmv_column_list();
  pkcol_array  dbms_utility.uncl_array;    -- 1-based plsql table 
  pk_name      varchar2(30);
  pk_type      pls_integer;
  pk_cols      varchar2(32000);
  pk_idx_name  varchar2(30);
  idx_cols     varchar2(32000);
  dummy        pls_integer;
  canon_pkcol  varchar2(30);

begin
  streams_mv_refresh.print_trace('get_pkcols()+');

  -- get pk cols as comma-separated list
  sys.dbms_snapshot_utl.get_pk_constraint_info(
    tabowner, tabname, pk_name, pk_type, pk_cols, pk_idx_name, idx_cols);

  if (pk_cols is null) then
    return pk_columns;
  end if;

  -- convert comma-separated list to collection
  dbms_utility.comma_to_table(rtrim(pk_cols, ','), dummy, pkcol_array);

  -- move collection to collection of type stmv_column_list
  for i in 1 .. pkcol_array.last loop
    exit when pkcol_array(i) is null;
    -- strip double quotes in pkcol_array(i)
    dbms_utility.canonicalize(pkcol_array(i), canon_pkcol, 30);
    pk_columns.extend;
    pk_columns(pk_columns.last) := canon_pkcol;
  end loop;

  streams_mv_refresh.print_trace('get_pkcols()-');
  return pk_columns;
end;


--
-- get a table's key columns (PK or via api dbms_apply_adm.set_key_columns)
--
function get_keycols(tabowner in varchar2, tabname in varchar2)
  return stmv_column_list is

  key_cols  stmv_column_list := stmv_column_list();

  cursor key_columns(table_owner varchar2, table_name varchar2) is
    select column_name
    from dba_apply_key_columns
    where object_owner = table_owner
    and object_name = table_name
    and apply_database_link is null;

begin
  streams_mv_refresh.print_trace('get_keycols()+');

  -- first, see if the table has a PK constraint
  key_cols := get_pkcols(tabowner, tabname);

  if (key_cols.count = 0) then
    -- no PK, try and see if there are key columns defined
    -- (via dbms_apply_adm.set_key_columns)
    for rec in key_columns(tabowner, tabname) loop
      key_cols.extend;
      key_cols(key_cols.last) := rec.column_name;
    end loop;
  end if;

  streams_mv_refresh.print_trace('get_keycols()-');
  return key_cols;
end;

--
-- register an MV for streams-driven refresh
--
procedure register_mv(mv_owner             varchar2, 
                      mv_name              varchar2, 
                      capture_name         varchar2  := 'CAPTURE_MV_MASTER',
                      apply_name           varchar2  := 'APPLY_MV_MASTER',
                      queue_name           varchar2  := NULL,
                      instantiate          boolean   := FALSE, 
                      use_streams_refresh  boolean   := FALSE,
                      mv_dml_handler       varchar2  := NULL) is

  mycur           mycurtyp;
  mvinfo          stmv_mv_t;
  str_refresh     pls_integer;
  flags           pls_integer := 0;
  can_use_log     varchar2(3);
  updatable       varchar2(3);
  ref_method      varchar2(15);           -- rowid, pk, etc.
  ref_type        varchar2(10);           -- complete, fast, never, etc.
  ref_mode        varchar2(10);           -- demand, commit, never
  mv_query        varchar2(32000);
  mastabs         pls_integer;            -- # master tables
  has_obj         pls_integer;            -- MV with objects ? 1=yes, 0=no
  has_oj          pls_integer;            -- MV with outer joins ? 1=yes, 0=no
  cnt             pls_integer;
  select_pos      pls_integer := 0;
  select_start    pls_integer := 0;
  from_pos        pls_integer := 0;
  where_pos       pls_integer := 0;
  where_start     pls_integer := 0;
  tabowner        varchar2(30);
  tabname         varchar2(30);
  tabalias        varchar2(30);
  owner_name      varchar2(80);           -- table_owner.table_name
  inst_scn        number;                 -- instantiation SCN
  tablist         stmv_table_list := stmv_table_list();
  canon_mv_owner  varchar2(30);
  canon_mv_name   varchar2(30);
  canon_capture   varchar2(30);
  canon_apply     varchar2(30);
  canon_queue     varchar2(30);
  canon_hdlr      varchar2(98);
  key_cols        stmv_column_list;

  -- MV metadata
  check_mv  varchar2(32000) :=
'select refresh_method, can_use_log, updatable, type, refresh_mode, query
 from dba_snapshots
 where owner = :mv_owner
 and name = :mv_name';

  check_mv2  varchar2(32000) :=
'select mastabs, decode(bitand(flag, 268435456), 0, 0, 1) has_obj, 
                 decode(bitand(flag2, 256), 0, 0, 1) has_oj
 from sys.exu9snap 
 where owner = :mv_owner 
 and name =:mv_name';

  -- MV last refresh scn
  get_refscn varchar2(32000) :=
'select rscn
 from sys.exu9snap 
 where owner = :mv_owner 
 and name =:mv_name';

  -- MV master tables
  mv_master_tables varchar2(32000) :=
'select detail_owner, detail_relation, detail_alias
 from dba_summary_detail_tables
 where owner = :mv_owner
 and summary_name = :mv_name';

begin
  -- print param values
  streams_mv_refresh.print_trace('register_mv()+');
  streams_mv_refresh.print_trace('mv_owner='||mv_owner);
  streams_mv_refresh.print_trace('mv_name='||mv_name);
  streams_mv_refresh.print_trace('capture_name='||capture_name);
  streams_mv_refresh.print_trace('apply_name='||apply_name);
  streams_mv_refresh.print_trace('queue_name='||queue_name);
  if (instantiate) then
    streams_mv_refresh.print_trace('instantiate=TRUE');
  else
    streams_mv_refresh.print_trace('instantiate=FALSE');
  end if;
  if (use_streams_refresh) then
    streams_mv_refresh.print_trace('use_streams_refresh=TRUE');
  else
    streams_mv_refresh.print_trace('use_streams_refresh=FALSE');
  end if;
  streams_mv_refresh.print_trace('mv_dml_handler='||mv_dml_handler);

  -- canonicalize inputs
  dbms_utility.canonicalize(mv_owner, canon_mv_owner, 30);
  dbms_utility.canonicalize(mv_name, canon_mv_name, 30);
  dbms_utility.canonicalize(capture_name, canon_capture, 30);
  dbms_utility.canonicalize(apply_name, canon_apply, 30);
  dbms_utility.canonicalize(queue_name, canon_queue, 30);
  dbms_utility.canonicalize(mv_dml_handler, canon_hdlr, 30);

  -- make sure the apply process is down
  select count(*) into cnt from dba_apply 
  where apply_name = canon_apply 
  and status = 'ENABLED';
  if (cnt > 0) then
      raise_application_error(-20000, 
        'Apply process must be disabled');
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
  -- get MV metadata
  --
  begin
    execute immediate check_mv 
      into ref_method, can_use_log, updatable, ref_type, ref_mode, mv_query 
      using canon_mv_owner, canon_mv_name;

    execute immediate check_mv2
      into mastabs, has_obj, has_oj
      using canon_mv_owner, canon_mv_name;
  exception
    when NO_DATA_FOUND then
      raise_application_error(-20000, 'MV does not exist');
  end;

  streams_mv_refresh.print_trace('ref_method='||ref_method);
  streams_mv_refresh.print_trace('can_use_log='||can_use_log);
  streams_mv_refresh.print_trace('updatable='||updatable);
  streams_mv_refresh.print_trace('ref_type='||ref_type);
  streams_mv_refresh.print_trace('ref_mode='||ref_mode);
  streams_mv_refresh.print_trace('mastabs='||mastabs);
  streams_mv_refresh.print_trace('has_obj='||has_obj);

  -- for streams-based refresh, validate the MV
  if (use_streams_refresh = TRUE) then
    if (can_use_log = 'NO'  or   -- not fast refreshable
        updatable = 'YES'   or   -- updatable MV
        ref_mode = 'COMMIT' or   -- on-commit MV
        has_obj = 1         or   -- MV with objects
        -- not pk-based and not an MJV                                     
        (ref_method != 'PRIMARY KEY' and ref_method != 'JOIN VIEW') or
        (ref_method = 'PRIMARY KEY' and mastabs > 1)  or   -- pk MV w/ >1 table
        instr(upper(mv_query), 'SELECT', 1, 2) > 0    or   -- nested queries
        instr(upper(mv_query), 'FROM', 1, 2) > 0      or   -- nested queries
        instr(upper(mv_query), 'UNION') > 0                -- UNION clause
       ) then
      raise_application_error(-20000, 
        'MV not supported for streams-based refresh');
    end if;
  end if;

  -- get MV PK columns, else get key columns, else error
  key_cols := get_keycols(canon_mv_owner, canon_mv_name);
  if (key_cols.count = 0 and use_streams_refresh = TRUE) then
    raise_application_error(-20000, 
                  'MV must have a PK or use dbms_apply_adm.set_key_columns()');
  end if;
  if (key_cols.count > 0) then
    for i in key_cols.first .. key_cols.last loop
      streams_mv_refresh.print_trace('key_col='||key_cols(i));
    end loop;
  end if;

  if (use_streams_refresh = TRUE) then
    str_refresh := 1;
  else
    str_refresh := 0;
  end if;

  -- check if an MJV
  if (ref_method = 'JOIN VIEW') then
    flags := flags + flg_mjv;
  end if;

  -- MV has outerjoin predicates ?
  if (has_oj = 1) then
    flags := flags + flg_hasoj;
  end if;

  --
  -- initialize MV metadata
  --
  mvinfo := stmv_mv_t(canon_mv_owner, canon_mv_name, null, str_refresh, 0,
                      flags, stmv_column_list(), null, stmv_table_list(), 
                      null);

  -- parse mv_query for select list and where clause
  select_pos   := instr(upper(mv_query), 'SELECT');
  select_start := select_pos + length('SELECT') + 1;
  from_pos     := instr(upper(mv_query), 'FROM');

  -- copy select list to MV metadata object
  mvinfo.select_list := 
    substr(mv_query, select_start, from_pos - select_start);

  -- copy where clause to MV metadata object by parsing the MV query
  where_pos := instr(upper(mv_query), 'WHERE');
  if (where_pos != 0) then
    where_start := where_pos + length('WHERE') + 1;
    mvinfo.where_clause  := substr(mv_query, where_start);
  end if;

  -- get MV keycols
  mvinfo.key_cols := key_cols;

  --
  -- get MV base tables
  --
  open mycur for mv_master_tables using canon_mv_owner, canon_mv_name;

  streams_mv_refresh.print_trace('getting base tables');
  loop
    fetch mycur into tabowner, tabname, tabalias;
    exit when mycur%notfound;             -- exit loop when last row is fetched

    streams_mv_refresh.print_trace('tabowner='||tabowner);
    streams_mv_refresh.print_trace('tabname='||tabname);
    streams_mv_refresh.print_trace('tabalias='||tabalias);

    -- store table information
    tablist.extend;
    tablist(tablist.last) := stmv_table_t(tabowner, tabname, tabalias, 
                                          stmv_column_list());

    -- construct "<owner>"."<name>"
    owner_name := '"' || tabowner || '"."' || tabname || '"';

    -- add apply rule
    dbms_streams_adm.add_table_rules(
      table_name         => owner_name,
      streams_type       => 'apply', 
      streams_name       => canon_apply,
      queue_name         => canon_queue,
      include_tagged_lcr => true,
      include_dml        => true,
      include_ddl        => false,
      source_database    => streams_mv_refresh_adm.source_dbname);

    dbms_apply_adm.set_dml_handler(
      object_name         => owner_name,
      object_type         => 'TABLE',
      operation_name      => 'DEFAULT',
      error_handler       => false,
      user_procedure      => 'streams_mv_refresh.source_dml_handler',
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
  close mycur;

  -- capture rowids, too
  dbms_capture_adm.include_extra_attribute(canon_capture, 'row_id', true);

  mvinfo.base_tabs := tablist;

  --
  -- get instantiation scn by refreshing MV or using current scn 
  -- (must be done after setting capture and apply table rules)
  --
  if (instantiate = TRUE) then
    -- construct "<owner>"."<name>"
    owner_name := '"' || canon_mv_owner || '"."' || canon_mv_name || '"';

    -- refresh the MV
    dbms_mview.refresh(owner_name);

    -- get the refresh scn
    execute immediate get_refscn into inst_scn using 
      canon_mv_owner, canon_mv_name;

  else
    -- use the current scn for the instantiation scn
    inst_scn := dbms_flashback.get_system_change_number();
  end if;

  mvinfo.inst_scn := inst_scn;                      -- store instantiation scn

  -- for streams-based refresh, set MV to never refresh to prevent 
  -- user from refreshing MV using dbms_snapshot.refresh(), which
  -- could cause incorrect results 
  owner_name := '"' || canon_mv_owner || '"."' || canon_mv_name || '"';
  if (use_streams_refresh = TRUE) then
    execute immediate 
      'alter materialized view '|| owner_name || ' never refresh';
  end if;

  --
  -- set table instantiation scn for all master tables
  --
  open mycur for mv_master_tables using canon_mv_owner, canon_mv_name;

  loop
    fetch mycur into tabowner, tabname, tabalias;
    exit when mycur%notfound;             -- exit loop when last row is fetched

    -- construct "<owner>"."<name>"
    owner_name := '"' || tabowner || '"."' || tabname || '"';

    -- if table already has an instantiation scn registered, don't call
    -- dbms_apply_adm.set_table_instantiation_scn()
    select count(*) into cnt
    from dba_apply_instantiated_objects 
    where source_object_owner = tabowner
    and source_object_name = tabname
    and source_object_type = 'TABLE'
    and source_database = streams_mv_refresh_adm.source_dbname;

    if (cnt = 0) then
      dbms_apply_adm.set_table_instantiation_scn(
        source_object_name    => owner_name,
        source_database_name  => streams_mv_refresh_adm.source_dbname,
        instantiation_scn     => inst_scn);
    end if;
  end loop;

  -- if user passed in an mv_dml_handler, then set up the MV for capture/apply
  if (mv_dml_handler is not null) then

    -- construct "<owner>"."<name>"
    owner_name := '"' || canon_mv_owner || '"."' || canon_mv_name || '"';

    inst_scn := dbms_flashback.get_system_change_number();

    -- add apply rule
    dbms_streams_adm.add_table_rules(
      table_name         => owner_name,
      streams_type       => 'apply', 
      streams_name       => canon_apply,
      queue_name         => canon_queue,
      include_tagged_lcr => true,
      include_dml        => true,
      include_ddl        => false,
      source_database    => streams_mv_refresh_adm.source_dbname);

    dbms_apply_adm.set_dml_handler(
      object_name         => owner_name,
      object_type         => 'TABLE',
      operation_name      => 'DEFAULT',
      error_handler       => false,
      user_procedure      => canon_hdlr,
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

    -- if table already has an instantiation scn registered, don't call
    -- dbms_apply_adm.set_table_instantiation_scn()
    select count(*) into cnt
    from dba_apply_instantiated_objects 
    where source_object_owner = canon_mv_owner
    and source_object_name = canon_mv_name
    and source_object_type = 'TABLE'
    and source_database = streams_mv_refresh_adm.source_dbname;

    if (cnt = 0) then
      dbms_apply_adm.set_table_instantiation_scn(
        source_object_name    => owner_name,
        source_database_name  => streams_mv_refresh_adm.source_dbname,
        instantiation_scn     => inst_scn);
    end if;

  end if;

  --
  -- save MV registration metadata to disk
  --
  insert into stmv_reg values (mvinfo);
  commit;

  streams_mv_refresh.print_trace('register_mv()-');
exception when others then
  if mycur%isopen then
    close mycur;
  end if;
  raise;
end;


--
-- remove an MV from streams-driven refresh
--
procedure unregister_mv(mv_owner      varchar2, 
                        mv_name       varchar2,
                        capture_name  varchar2 := 'CAPTURE_MV_MASTER', 
                        apply_name    varchar2 := 'APPLY_MV_MASTER') is

  mycur           mycurtyp;
  tabowner        varchar2(30);
  tabname         varchar2(30);
  owner_name      varchar2(80);
  cnt             pls_integer;
  canon_mv_owner  varchar2(30);
  canon_mv_name   varchar2(30);
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

  -- select all master tables registered by this MV that is not used
  -- by other registered MVs
  mv_master_tables varchar2(32000) :=
'select mt.table_owner, mt.table_name
 from stmv_reg mv, table(mv.base_tabs) mt
 where mv.mv_owner = :mv_owner
 and   mv.mv_name  = :mv_name
 and not exists
 (select 1
  from stmv_reg mv2, table(mv2.base_tabs) mt2
  where not (mv2.mv_owner = mv.mv_owner 
             and mv2.mv_name = mv.mv_name)
  and   mt.table_owner = mt2.table_owner
  and   mt.table_name  = mt2.table_name)';

begin
  streams_mv_refresh.print_trace('unregister_mv()+');

  -- canonicalize inputs
  dbms_utility.canonicalize(mv_owner, canon_mv_owner, 30);
  dbms_utility.canonicalize(mv_name, canon_mv_name, 30);
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

  owner_name := '"' || canon_mv_owner || '"."' || canon_mv_name || '"';

  -- remove MV DML handlers
  dbms_apply_adm.set_dml_handler(
    object_name         => owner_name,
    object_type         => 'TABLE',
    operation_name      => 'DEFAULT',
    error_handler       => false,
    user_procedure      => NULL,
    apply_database_link => NULL,
    apply_name          => canon_apply);

  -- remove all MV rules, if they exist
  for rec in table_rules(canon_capture, canon_apply, 
                         canon_mv_owner, canon_mv_name) loop

    -- construct "<owner>"."<name>"
    owner_name := '"' || rec.rule_owner || '"."' || rec.rule_name || '"';

    dbms_streams_adm.remove_rule(
      rule_name     => owner_name,   
      streams_type  => rec.streams_type,
      streams_name  => rec.streams_name);
  end loop;

  --
  -- get MV base tables
  --
  open mycur for mv_master_tables using canon_mv_owner, canon_mv_name;

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

  -- delete MV from registration metadata
  delete from stmv_reg 
  where mv_owner = canon_mv_owner 
  and mv_name = canon_mv_name;

  commit;

  streams_mv_refresh.print_trace('unregister_mv()-');

exception when others then
  if mycur%isopen then
    close mycur;
  end if;
  raise;
end;


--
-- remove streams used for streams-driven refresh
--
procedure remove_streams_mv_refresh(
    capture_name varchar2 := 'CAPTURE_MV_MASTER', 
    apply_name   varchar2 := 'APPLY_MV_MASTER') is

  owner_name      varchar2(80);
  inst_scn        number;
  db              varchar2(128);
  cnt             pls_integer := 0;
  canon_capture   varchar2(30);
  canon_apply     varchar2(30);
  capture_queue   varchar2(30);
  apply_queue     varchar2(30);

  -- selects tables set up for MV refresh apply
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
  streams_mv_refresh.print_trace('remove_streams_mv_refresh()+');

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
        source_database_name  => streams_mv_refresh_adm.source_dbname,
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

  -- delete any MV registration metadata
  delete from stmv_reg;
  commit;

  streams_mv_refresh.print_trace('remove_streams_mv_refresh()-');

end;


--
-- package instantiation code
--
begin
  -- read in MV registration metadata
  for rec in mv_group loop
    reg_mvs.extend;
    reg_mvs(reg_mvs.last) := rec.sys_nc_rowinfo$;
  end loop;
end streams_mv_refresh_adm;
/


create or replace package body streams_mv_refresh as

first_in_session boolean := TRUE;  -- do once per apply session

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
-- returns list of registered MVs
--
function get_mvs return stmv_mvs_t is
begin
  return streams_mv_refresh_adm.reg_mvs;
end;


--
-- determines an anydata value's datatype and binds it appropriately
--
procedure bind_anydata(sql_cursor pls_integer,
                       name       varchar2,
                       value      sys.anydata) is

  tmp           sys.anytype;
  col_type      pls_integer; 
  col_max_len   pls_integer := 4000;
  haslob        boolean := FALSE;

begin
  print_trace('bind_anydata()+');

  col_type := value.getType(tmp);
  print_trace('col_type='||col_type);

  if col_type = 1 then
    dbms_sql.bind_variable(sql_cursor, name, value.AccessChar, col_max_len);
  elsif col_type = 2 then                                             -- number
    dbms_sql.bind_variable(sql_cursor, name, value.AccessNumber);
  elsif col_type =  DBMS_TYPES.TYPECODE_UROWID then                    -- rowid
    -- anydata doesn't support rowid in 9i
    dbms_sql.bind_variable(sql_cursor, name, value.AccessURowid);
  elsif col_type = 12 then                                              -- date
    dbms_sql.bind_variable(sql_cursor, name, value.Accessdate);
  elsif col_type = 23 then                                               -- raw
    dbms_sql.bind_variable_raw(sql_cursor, name, value.AccessRaw);
  elsif col_type = 96 then
    dbms_sql.bind_variable_char(sql_cursor, name, value.AccessChar, 
                                col_max_len);
  elsif col_type = DBMS_TYPES.TYPECODE_NCHAR then
    dbms_sql.bind_variable_char(sql_cursor, name, value.AccessNChar, 
                                col_max_len);
  elsif col_type = 112 then                                             -- clob
    dbms_sql.bind_variable(sql_cursor, name, value.Accessclob);
    haslob := true;
  elsif col_type = DBMS_TYPES.TYPECODE_NCLOB then
    dbms_sql.bind_variable(sql_cursor, name, value.Accessnclob);
    haslob := true;
  elsif col_type = 113 then                                             -- blob
    dbms_sql.bind_variable(sql_cursor, name, value.Accessblob);
    haslob := true;
  elsif col_type = 180 then                                        -- timestamp
    dbms_sql.bind_variable(sql_cursor, name, value.AccessTimestamp);
  elsif col_type = 181 then                         -- timestamp with time zone
    dbms_sql.bind_variable(sql_cursor, name, value.AccessTimestampTZ());
  elsif col_type=231 then                     -- timestamp with local time zone
    dbms_sql.bind_variable(sql_cursor, name, value.AccessTimestampLTZ());
  elsif col_type = 182 then                           -- interval year to month
    dbms_sql.bind_variable(sql_cursor, name, value.AccessIntervalYM());
  elsif col_type = 183 then                           -- interval day to second
    dbms_sql.bind_variable(sql_cursor, name, value.AccessIntervalDS());
  end if;

  print_trace('bind_anydata()-');
end;


--
-- Given an MV, returns a comma-separated list of tables with a flashback
-- clause. Will be used for the FROM clause in a refresh query.
--
function get_mvtab_list(mv_no   pls_integer,
                        old_new varchar2,
                        null_check boolean) return varchar2 is
  mvtab_list        varchar2(32000);
  flashback_clause  varchar2(20);
begin
  if (old_new = 'NEW') then
    flashback_clause := ' AS OF SCN(:scn) ';
  else
    flashback_clause := ' AS OF SCN(:scn-1) ';
  end if;

  for i in get_mvs()(mv_no).base_tabs.first ..
           get_mvs()(mv_no).base_tabs.last loop

    -- add comma separator
    if (i > get_mvs()(mv_no).base_tabs.first) then
      mvtab_list := mvtab_list || ', ';
    end if;

    mvtab_list := mvtab_list || '"' 
                  || get_mvs()(mv_no).base_tabs(i).table_owner
                  || '"."' || get_mvs()(mv_no).base_tabs(i).table_name || '"'
                  || flashback_clause;

    if (get_mvs()(mv_no).base_tabs(i).table_alias is not null) then
      mvtab_list := mvtab_list || '"' || 
                    get_mvs()(mv_no).base_tabs(i).table_alias || '"';
    end if;

  end loop;

  if (not null_check) then
    -- add rowid temp table
    mvtab_list := mvtab_list || ', stmv_rid';
  end if;

  return mvtab_list;
end; 


--
-- constructs selection predicates to uniquely identify a row in a master table
--
function append_tab_cond(mv_no      pls_integer,
                         tab_no     pls_integer,
                         delete_or_upsert varchar2,
                         null_check boolean := FALSE) return varchar2 is

  tab_cond  varchar2(32000);

begin
  print_trace('append_tab_cond()+');

  -- table alias or owner.table
  if (get_mvs()(mv_no).base_tabs(tab_no).table_alias is not null) then
    tab_cond := tab_cond || '"' || 
                get_mvs()(mv_no).base_tabs(tab_no).table_alias || '"';
  else
    tab_cond := tab_cond || '"' || 
                get_mvs()(mv_no).base_tabs(tab_no).table_owner || '"."' || 
                get_mvs()(mv_no).base_tabs(tab_no).table_name || '"';
  end if; 

  -- rowid column: check for null or rowid match
  if (null_check) then
    tab_cond := tab_cond ||'.' || streams_mv_refresh.rowid_col || ' is null ';
  else
    -- join with stmv_rid (rowid table) on rowid, table owner, table name,
    --   and dml_type
    tab_cond := tab_cond ||'.' || streams_mv_refresh.rowid_col || 
                  ' = stmv_rid.rid ';
    tab_cond := tab_cond || 
                'and stmv_rid.table_owner = ' || '''' || 
                   get_mvs()(mv_no).base_tabs(tab_no).table_owner || '''' ||
                ' and stmv_rid.table_name  = ' || '''' || 
                   get_mvs()(mv_no).base_tabs(tab_no).table_name || ''' ';

    if (delete_or_upsert = 'DELETE') then
      tab_cond := tab_cond || 'and stmv_rid.dml_type in (2,4) ';
    else
      tab_cond := tab_cond || 'and stmv_rid.dml_type in (1,2) ';
    end if;
  end if;

  print_trace('append_tab_cond()-');

  return tab_cond;
end;    


--
-- create flashback query relative to a base table in the MV query
--
function create_query(delete_or_upsert  varchar2,  
                      mv_no             pls_integer,
                      tab_no            pls_integer,
                      null_check        boolean := FALSE) return varchar2 is

  query      varchar2(32000);
  orig_where varchar2(32000);   -- MV where clause
  oldnew1    varchar2(5);
  oldnew2    varchar2(5);

begin
  print_trace('create_query()+');

  -- for deletes, get all MV rows removed due to the delete
  -- otherwise, get all MV rows added due to the update/insert dml
  if (delete_or_upsert = 'DELETE') then
    oldnew1 := 'OLD';
    oldnew2 := 'NEW';
  else
    oldnew1 := 'NEW';
    oldnew2 := 'OLD';
  end if;

  -- get original MV where clause 
  orig_where := get_mvs()(mv_no).where_clause;
  if (orig_where is not null) then
    orig_where := ' AND ' || orig_where;
  end if;

  query := 'SELECT /*+ leading(stmv_rid) */ ' ||
             get_mvs()(mv_no).select_list ||
           ' FROM '  || get_mvtab_list(mv_no, oldnew1, null_check) ||
           ' WHERE ' || append_tab_cond(mv_no, tab_no, delete_or_upsert, 
                                        null_check) || orig_where ||
          ' MINUS '  || 
           'SELECT /*+ leading(stmv_rid) */ ' || 
             get_mvs()(mv_no).select_list ||
           ' FROM '  || get_mvtab_list(mv_no, oldnew2, null_check) ||
           ' WHERE ' || append_tab_cond(mv_no,tab_no, delete_or_upsert, 
                                        null_check) || orig_where;

  print_trace('sql_stmt='||query);
  print_trace('create_query()-');
  return query;
end create_query;
                          

--
-- find the next MV that has owner.name as a base table
-- mv_no, tab_no keeps track of current position in MV registration metadata
--
function find_next_mv(owner           varchar2, 
                      name            varchar2,
                      streams_refresh boolean,
                      mv_no           in out nocopy pls_integer,
                      tab_no          in out nocopy pls_integer) 
return boolean is

  str_ref pls_integer;

begin
  print_trace('find_next_mv()+');

  if (streams_refresh) then
    str_ref := 1;
  else
    str_ref := 0;
  end if;

  -- MVs can be registered for streams-based refresh or traditional refresh.
  -- MV registration metadata keeps track of the refresh method registered.
  if mv_no is null then
    mv_no := get_mvs().first;
    tab_no := get_mvs()(mv_no).base_tabs.first;
  else
    tab_no := get_mvs()(mv_no).base_tabs.next(tab_no);
    if tab_no is null then
      mv_no := get_mvs().next(mv_no);
      if mv_no is not null then
        tab_no := get_mvs()(mv_no).base_tabs.first;
      end if;
    end if;
  end if;

  -- find next MV that has owner.name as a base table
  while mv_no is not null loop
    -- make sure MV stream_refresh flag = streams_refresh param
    if (get_mvs()(mv_no).use_str_refresh = str_ref) then
      while tab_no is not null loop
        if (get_mvs()(mv_no).base_tabs(tab_no).table_owner = owner 
            and get_mvs()(mv_no).base_tabs(tab_no).table_name = name) then

           print_trace('found MV: '||
             get_mvs()(mv_no).mv_owner || '.' || get_mvs()(mv_no).mv_name);
           print_trace('find_next_mv()-');
           return TRUE;
        end if;

        tab_no := get_mvs()(mv_no).base_tabs.next(tab_no);
      end loop;
    end if;

    mv_no := get_mvs().next(mv_no);
    if (mv_no is not null) then
      tab_no := get_mvs()(mv_no).base_tabs.first;
    end if;

  end loop;

  print_trace('find_next_mv()-');
  return FALSE;
end find_next_mv;


--
--
--
procedure define_columns(sql_cursor  pls_integer,
                         num_cols    pls_integer, 
                         desc_table  sys.dbms_sql.desc_tab) is

  col_type    pls_integer;
  vcval       varchar2(32000);
  dateval     date;
  numval      number;
  rowidval    rowid;
  nvcval      nvarchar2(15000);
  rawval      raw(2000);
  charval     char(2000);
  ncval       nchar(1000);
  clobval     clob;
  blobval     blob;
  nclobval    nclob;
  -- tmval    time(9);
  -- tmtzval  time(9) with time zone;
  tmstpval    timestamp(9);
  tmstptzval  timestamp(9) with time zone;
  tmstpltzval timestamp(9) with local time zone;
  ytomintval  interval year(9) to month;
  dtosintval  interval day(9) to second(9);
  haslob      boolean := false;

begin
  for ctr in 1..num_cols loop
    col_type := desc_table(ctr).col_type;

    if (col_type = 1) then
      if (desc_table(ctr).col_charsetform != 2) then                -- varchar2
        dbms_sql.define_column(sql_cursor,ctr,vcval,
                               desc_table(ctr).col_max_len);
      else                                                         -- nvarchar2
        dbms_sql.define_column(sql_cursor,ctr,vcval,
                               desc_table(ctr).col_max_len);
      end if;
    elsif (col_type = 2) then                                         -- number
      dbms_sql.define_column(sql_cursor,ctr,numval);
    elsif (col_type = 11) then                                 -- rowid
      dbms_sql.define_column_rowid(sql_cursor,ctr,rowidval);
    elsif (col_type = 12) then                                          -- date
      dbms_sql.define_column(sql_cursor,ctr,dateval);
    elsif (col_type = 23) then                                           -- raw
      dbms_sql.define_column_raw(sql_cursor,ctr,rawval,
                                 desc_table(ctr).col_max_len);
    elsif (col_type = 96) then
      if (desc_table(ctr).col_charsetform != 2) then                    -- char
        dbms_sql.define_column_char(sql_cursor,ctr,charval,
                                    desc_table(ctr).col_max_len);
      else                                                             -- nchar
        dbms_sql.define_column_char(sql_cursor,ctr,ncval,
                                    desc_table(ctr).col_max_len);
      end if;
    elsif (col_type = 112) then                                         -- clob
      if (desc_table(ctr).col_charsetform) != 2 then
        dbms_sql.define_column(sql_cursor,ctr,clobval);
      else 
        dbms_sql.define_column(sql_cursor,ctr,nclobval);
      end if;
      haslob := true;
    elsif (col_type = 113) then                                         -- blob
      dbms_sql.define_column(sql_cursor,ctr,blobval);
      haslob := true;
--     elsif (col_type = 178) then                                      -- time
--       dbms_sql.define_column(sql_cursor,ctr,tmval);
--     elsif (col_type = 179) then                       -- time with time zone
--       dbms_sql.define_column(sql_cursor,ctr,tmtzval);
    elsif (col_type = 180) then                                    -- timestamp
      dbms_sql.define_column(sql_cursor,ctr,tmstpval);
    elsif (col_type = 181) then                     -- timestamp with time zone
      dbms_sql.define_column(sql_cursor,ctr,tmstptzval);
    elsif (col_type = 231) then               -- timestamp with local time zone
      dbms_sql.define_column(sql_cursor,ctr,tmstpltzval);
    elsif (col_type = 182) then                       -- interval year to month
      dbms_sql.define_column(sql_cursor,ctr,ytomintval);
    elsif (col_type = 183) then                       -- interval day to second
      dbms_sql.define_column(sql_cursor,ctr,dtosintval);
    end if;
  end loop;

end;


--
--
--
procedure get_column_values(sql_cursor  pls_integer,
                            num_cols    pls_integer, 
                            desc_table  sys.dbms_sql.desc_tab,
                            is_key_col  stmv_num_list,
                            lcr         in out nocopy sys.lcr$_row_record,
                            cmd_type    varchar2,
                            first_time  boolean) is

  col_type    pls_integer;
  col_name    varchar2(32);
  vcval       varchar2(32000);
  dateval     date;
  numval      number;
  rowidval    rowid;
  nvcval      nvarchar2(15000);
  rawval      raw(2000);
  charval     char(2000);
  ncval       nchar(1000);
  clobval     clob;
  blobval     blob;
  nclobval    nclob;
  -- tmval    time(9);
  -- tmtzval  time(9) with time zone;
  tmstpval    timestamp(9);
  tmstptzval  timestamp(9) with time zone;
  tmstpltzval timestamp(9) with local time zone;
  ytomintval  interval year(9) to month;
  dtosintval  interval day(9) to second(9);
  myany       sys.anydata;
  is_key      boolean;

begin
  -- build the lcr based upon the data type of
  -- the column selected...
  for ctr in 1..num_cols loop
    col_type := desc_table(ctr).col_type;
    col_name := '"' || desc_table(ctr).col_name || '"';
    is_key := (is_key_col(ctr) = 1);

    -- DELETE: only process key columns
    if (cmd_type = 'DELETE' and not is_key) then
      goto next_col;
    end if;

    if (col_type = 1) then
      if (desc_table(ctr).col_charsetform != 2) then                -- varchar2
        dbms_sql.column_value(sql_cursor,ctr,vcval);        
        myany := Sys.AnyData.ConvertVarchar2(vcval);
      else                                                         -- nvarchar2
        dbms_sql.column_value(sql_cursor,ctr,vcval);
        myany := Sys.AnyData.ConvertNVarchar2(vcval);
      end if;
    elsif (col_type = 2) then                                         -- number
      dbms_sql.column_value(sql_cursor,ctr,numval);
      myany := Sys.AnyData.ConvertNumber(numval);
    elsif (col_type = 11) then                                         -- rowid
      dbms_sql.column_value_rowid(sql_cursor,ctr,rowidval);
      -- in 10i, rowid will be supported; for now put in a char
      print_trace('rowidval='||rowidval);
      vcval := rowidval;
      myany := Sys.AnyData.ConvertVarchar2(vcval);
    elsif (col_type = 12) then                                          -- date
      dbms_sql.column_value(sql_cursor,ctr,dateval);
      myany := Sys.AnyData.ConvertDate(dateval);
    elsif (col_type = 23) then                                           -- raw
      dbms_sql.column_value_raw(sql_cursor,ctr,rawval);
      myany := Sys.AnyData.ConvertRaw(rawval);
    elsif (col_type = 96) then
      if desc_table(ctr).col_charsetform != 2  then                     -- char
        dbms_sql.column_value_char(sql_cursor,ctr,charval);    
        myany := Sys.AnyData.ConvertChar(    -- trim charval to col_max_len
                       substr(charval, 1, desc_table(ctr).col_max_len));
      else                                                             -- nchar
        dbms_sql.column_value_char(sql_cursor,ctr,ncval);
        myany := Sys.AnyData.ConvertNchar(     -- trim ncval to col_max_len
                       substr(ncval, 1, desc_table(ctr).col_max_len));
      end if;
    elsif (col_type = 112) then                                         -- clob
      -- not supported for now
      null;
    elsif (col_type = 113) then                                         -- blob
      -- not supported for now
      null;
--  elsif col_type = 178 then                                           -- time
--    dbms_sql.column_value(sql_cursor,ctr,tmval);
--    -- anydata and time type???
--    myany := Sys.AnyData.ConvertTimestamp(tmval);
--  elsif col_type = 179 then                            -- time with time zone
--    -- time type???
--    dbms_sql.column_value(sql_cursor,ctr,tmtzval);
--    myany := Sys.AnyData.ConvertTimestampTZ(tmtzval);
    elsif (col_type = 180) then                                    -- timestamp
      dbms_sql.column_value(sql_cursor,ctr,tmstpval);
      myany := Sys.AnyData.ConvertTimestamp(tmstpval);
    elsif (col_type = 181) then                     -- timestamp with time zone
      dbms_sql.column_value(sql_cursor,ctr,tmstptzval);
      myany := Sys.AnyData.ConvertTimestampTZ(tmstptzval);
    elsif (col_type = 231) then                -- timestamp with local timezone
      dbms_sql.column_value(sql_cursor,ctr,tmstpltzval);
      myany := Sys.AnyData.ConvertTimestampLTZ(tmstpltzval);
    elsif (col_type = 182) then                       -- interval year to month
      dbms_sql.column_value(sql_cursor,ctr,ytomintval);
      myany := Sys.AnyData.ConvertIntervalYM(ytomintval);
    elsif (col_type = 183) then                       -- interval day to second
      dbms_sql.column_value(sql_cursor,ctr,dtosintval);
      myany := Sys.AnyData.ConvertIntervalDS(dtosintval);
    end if;

    if ((cmd_type = 'DELETE' or cmd_type = 'UPDATE')
        and is_key) then
      if (first_time) then
        lcr.add_column('OLD', col_name, myany);
      else
        lcr.set_value('OLD', col_name, myany);
      end if;
    end if;

    if (cmd_type = 'INSERT' or cmd_type = 'UPDATE') then
      if (first_time) then
        lcr.add_column('NEW', col_name, myany);
      else
        lcr.set_value('NEW', col_name, myany);
      end if;
    end if;

    <<next_col>>
    null;
  end loop;     
end;


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

  num_rows    pls_integer;
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
  print_trace('before execute');
  num_rows := dbms_sql.execute(sql_cursor);
  print_trace('after execute');

  lcr := sys.lcr$_row_record.construct(
           source_database_name=>source_dbname,
           command_type=>cmd_type,
           object_owner=>obj_owner,
           object_name=>obj_name,
           old_values=>NULL,
           new_values=>NULL); 

  -- loop until no more rows are returned
  while dbms_sql.fetch_rows(sql_cursor) > 0 loop
    print_trace('fetched row ');

    get_column_values(sql_cursor, num_cols, desc_table, is_key_col, 
                      lcr, cmd_type, first_time);

    first_time := FALSE;

    streams_mv_lcr.process_lcr(lcr);

  end loop;

  print_trace('build_and_process_lcrs()-');
end;
 

--
--
--
procedure execute_delta_query(mv_idx      pls_integer, 
                              tab_idx     pls_integer, 
                              dml_type    varchar2, 
                              null_check  boolean,
                              commit_scn  number) is

  sql_stmt    varchar2(32000);
  sql_cursor  pls_integer; 
  num_cols    pls_integer;
  desc_table  sys.dbms_sql.desc_tab;
  mv_owner    varchar2(30);
  mv_name     varchar2(30);
  key_cols    stmv_column_list;

begin
  print_trace('execute_delta_query()+');

  mv_owner := get_mvs()(mv_idx).mv_owner;
  mv_name  := get_mvs()(mv_idx).mv_name;
  key_cols := get_mvs()(mv_idx).key_cols;

  sql_stmt := create_query(dml_type, mv_idx, tab_idx, null_check);

  print_trace('start parse and describe');
  sql_cursor := dbms_sql.open_cursor;     
  dbms_sql.parse(sql_cursor, sql_stmt, sys.dbms_sql.v7); 
  dbms_sql.describe_columns(sql_cursor, num_cols, desc_table);      
  print_trace('end parse and describe');

  -- loop through the columns and do the define columns
  print_trace('start define_columns');
  define_columns(sql_cursor, num_cols, desc_table);
  print_trace('end define_columns');

  -- bind the scn variable once
  dbms_sql.bind_variable(sql_cursor, streams_mv_refresh.scn_bind, commit_scn);

  if (null_check) then
    build_and_process_lcrs(
      sql_cursor, 
      num_cols, 
      desc_table,
      streams_mv_refresh_adm.source_dbname,
      dml_type,
      mv_owner,
      mv_name, 
      key_cols);
  else
  print_trace('start process rowids');

    build_and_process_lcrs(
      sql_cursor, 
      num_cols, 
      desc_table,
      streams_mv_refresh_adm.source_dbname,
      dml_type,
      mv_owner,
      mv_name, 
      key_cols);

  print_trace('end process rowids');
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
-- adds rowid value to the rowid collection
--
procedure add_to_rowid_coll(tab_rowid    IN urowid,
                            rowid_coll   IN OUT NOCOPY rowid_list) is

  found   boolean := FALSE;

begin
  print_trace('add_to_rowid_coll()+');

  -- see if rowid already exists in collection
  if (rowid_coll.count > 0) then
    for i in rowid_coll.first..rowid_coll.last loop
      if (tab_rowid = rowid_coll(i)) then
        found := TRUE;
        exit;
      end if;
    end loop;
  end if;

  -- new rowid: add to collection
  if (found = FALSE) then
    print_trace('new rowid');
    rowid_coll.extend;
    rowid_coll(rowid_coll.last) := tab_rowid;
  end if;

  print_trace('add_to_rowid_coll()-');
end;


--
-- adds to table collection that keeps track of DMLs and associated rowids
-- done to a table during a transaction
--
procedure add_to_table_coll(tab_owner    IN varchar2, 
                            tab_name     IN varchar2, 
                            tab_dmltype  IN pls_integer,
                            tab_rowid    IN urowid,
                            tab_coll     IN OUT NOCOPY txn_tab_list) is

  found   boolean := FALSE;

begin
  print_trace('add_to_table_coll()+');

  -- see if name already exists in table collection
  if (tab_coll.count > 0) then
    for i in tab_coll.first..tab_coll.last loop
      if (tab_owner = tab_coll(i).table_owner and
          tab_name  = tab_coll(i).table_name) then
        found := TRUE;
        if (tab_dmltype is not null) then
          if (bitand(tab_coll(i).dmltypes, tab_dmltype) = 0) then
            tab_coll(i).dmltypes := tab_coll(i).dmltypes + tab_dmltype;
          end if;
          if (tab_rowid is not null) then
            if (tab_dmltype = streams_mv_refresh.delete_dml or
                tab_dmltype = streams_mv_refresh.update_dml) then

              add_to_rowid_coll(tab_rowid, tab_coll(i).delete_rowids);

            end if;
            if (tab_dmltype = streams_mv_refresh.insert_dml or
                tab_dmltype = streams_mv_refresh.update_dml) then

              add_to_rowid_coll(tab_rowid, tab_coll(i).insert_rowids);

            end if;
          end if;
        end if;
        exit;
      end if;
    end loop;
  end if;

  -- new table: add to collection
  if (found = FALSE) then
    print_trace('new table');
    tab_coll.extend;
    tab_coll(tab_coll.last).table_owner   := tab_owner;
    tab_coll(tab_coll.last).table_name    := tab_name;
    tab_coll(tab_coll.last).dmltypes      := tab_dmltype;
    tab_coll(tab_coll.last).delete_rowids := rowid_list();
    tab_coll(tab_coll.last).insert_rowids := rowid_list();
    -- add rowid
    if (tab_rowid is not null) then
      if (tab_dmltype = streams_mv_refresh.delete_dml or
          tab_dmltype = streams_mv_refresh.update_dml) then

        print_trace('adding delete rowid');
        tab_coll(tab_coll.last).delete_rowids:= rowid_list(tab_rowid);

      end if;
      if (tab_dmltype = streams_mv_refresh.insert_dml or
          tab_dmltype = streams_mv_refresh.update_dml) then

        print_trace('adding insert rowid');
        tab_coll(tab_coll.last).insert_rowids:= rowid_list(tab_rowid);

      end if;
    end if;
  end if;

  print_trace('add_to_table_coll()-');
end;


--
-- DML handler
--
procedure source_dml_handler(lcr_anydata sys.anydata) is

  lcr                 sys.lcr$_row_record;
  t                   pls_integer;
  stmv_i              pls_integer := null;  -- iterator
  tab_i               pls_integer := null;  -- iterator
  commit_scn          number := null;
  dml_type            varchar2(10);
  dml_code            pls_integer;
  ridany              sys.anydata;
  ridval              urowid;
  object_owner        varchar2(30);
  object_name         varchar2(30);

BEGIN
  print_trace('source_dml_handler()+');

  t := lcr_anydata.getObject(lcr);

  commit_scn  := lcr.get_commit_scn();
  print_trace('commit_scn='||commit_scn);

  -- get rowid from the lcr, if it exists
  ridany := lcr.get_extra_attribute('row_id');

  object_owner := lcr.get_object_owner;
  object_name  := lcr.get_object_name;

  print_trace('table=' || object_owner || '.' || object_name);

  if (first_in_session) then
    -- do_enq_message('SET CONSTRAINTS ALL DEFERRED;');
    first_in_session := FALSE;
  end if;

  --
  -- dbms_mview.refresh() codepath
  --
  print_trace('processing MVs for dbms_mview.refresh()');

  while (find_next_mv(object_owner, object_name, FALSE,
                      stmv_i, tab_i)) loop

    print_trace('MV=' || get_mvs()(stmv_i).mv_owner || '.' ||
                get_mvs()(stmv_i).mv_name);

    -- skip this MV if commit scn of this LCR <= the MV's instantiation scn
    if (commit_scn > get_mvs()(stmv_i).inst_scn) then

      add_to_table_coll(get_mvs()(stmv_i).mv_owner,
                        get_mvs()(stmv_i).mv_name, 
                        NULL, NULL, streams_mv_refresh.txn_mvs);
    end if;
  end loop;

  --
  -- streams refresh codepath
  --
  print_trace('processing MVs for streams-based refresh');

  stmv_i := null;
  tab_i  := null;
  -- store table and lcr rowid if there is some MV dependent on the table
  if find_next_mv(object_owner, object_name, TRUE,
                  stmv_i, tab_i) then

    -- need rowid for streams refresh
    if (ridany is null) then
      raise_application_error(-20000, 
        'supplemental logging missing for rowid on ' ||
        object_owner || '.' || object_name);
    end if;
    ridval := ridany.AccessURowid;
    print_trace('ridval='||ridval);

    dml_type := lcr.get_command_type;
    print_trace('command_type=' || dml_type);

    if (dml_type = 'INSERT') then
      dml_code := streams_mv_refresh.insert_dml;
    elsif (dml_type = 'UPDATE') then
      dml_code := streams_mv_refresh.update_dml;
    elsif (dml_type = 'DELETE') then
      dml_code := streams_mv_refresh.delete_dml;
    else
      dml_code := 0;
    end if;

    -- add table to collection for commit-time processing
    add_to_table_coll(object_owner, object_name, 
                      dml_code, NULL, streams_mv_refresh.txn_tabs);

    print_trace('inserting into stmv_rid: ');
    print_trace('table_owner='||object_owner);
    print_trace('table_name='||object_name);
    print_trace('dml_type='||dml_code);
    print_trace('rid='||ridval);

    -- add rowid to temporary table for later processing
    insert into stmv_rid (table_owner, table_name, dml_type, rid)
    values (object_owner, object_name, dml_code, ridval);
  end if;

  print_trace('source_dml_handler()-');

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

--
-- pre-commit handler
--
procedure source_commit_hdlr(commit_scn number) is
  l_jobnum     pls_integer;
  str          varchar2(32000);
  mvlist       varchar2(32000);
  is_first     boolean := TRUE;
  ref_scn      number;                                           -- refresh SCN
  stmv_i       pls_integer := null;                                 -- iterator
  tab_i        pls_integer := null;                                 -- iterator
  dml_type     varchar2(10);
  is_mjv       boolean;
  is_ojmjv     boolean;
  tab_owner    varchar2(30);
  tab_name     varchar2(30);
  has_deletes  boolean;
  has_inserts  boolean;

  -- MV last refresh scn
  get_refscn varchar2(32000) :=
'select rscn from sys.exu9snap 
 where owner = :mv_owner and name =:mv_name';

begin
  print_trace('source_commit_hdlr()+');
  print_trace('commit_scn='||commit_scn);

  --
  -- dbms_mview.refresh()
  --
  -- create comma-separated list of table owners and table names
  if (streams_mv_refresh.txn_mvs.count > 0) then

    print_trace('processing MVs for dbms_mview.refresh()');

    for i in streams_mv_refresh.txn_mvs.first..
             streams_mv_refresh.txn_mvs.last loop

      print_trace('MV=' || streams_mv_refresh.txn_mvs(i).table_owner || '.' ||
                  streams_mv_refresh.txn_mvs(i).table_name);

      -- get the refresh scn
      execute immediate get_refscn into ref_scn 
        using streams_mv_refresh.txn_mvs(i).table_owner, 
              streams_mv_refresh.txn_mvs(i).table_name;

      -- don't refresh the MV if it is current (this could happen if there
      -- were any interim refreshes to the MV)
      if (commit_scn >= ref_scn) then
        -- add table to the list
        if not is_first then
          mvlist := mvlist || ',';
        end if;
        mvlist := mvlist || '"' || 
                         streams_mv_refresh.txn_mvs(i).table_owner || '"."' || 
                         streams_mv_refresh.txn_mvs(i).table_name || '"';
        is_first := FALSE;
      end if;
    end loop;

    -- submit a refresh job
    str := 
      'begin 
         dbms_mview.refresh(list=>''' || mvlist || ''',' ||
                               'atomic_refresh=>FALSE);
       end;';

    create_scheduler_job(str);

    -- reset mv_owners, mv_names collections
    streams_mv_refresh.txn_mvs.delete;
  end if;

  --
  -- streams-based refresh
  --
  if (streams_mv_refresh.txn_tabs.count > 0) then
    print_trace('processing MVs for streams-based refresh');

    -- loop through all tables and DMLs done in the transaction
    for i in streams_mv_refresh.txn_tabs.first..
             streams_mv_refresh.txn_tabs.last loop

      tab_owner   := streams_mv_refresh.txn_tabs(i).table_owner;
      tab_name    := streams_mv_refresh.txn_tabs(i).table_name;

      -- determine if deletes or inserts were done (updates are considered
      -- a delete followed by and insert)
      has_deletes := (bitand(streams_mv_refresh.txn_tabs(i).dmltypes, 
                             streams_mv_refresh.delete_dml) = 
                               streams_mv_refresh.delete_dml 
                      or
                      bitand(streams_mv_refresh.txn_tabs(i).dmltypes, 
                             streams_mv_refresh.update_dml) = 
                               streams_mv_refresh.update_dml);

      has_inserts := (bitand(streams_mv_refresh.txn_tabs(i).dmltypes, 
                             streams_mv_refresh.insert_dml) = 
                               streams_mv_refresh.insert_dml 
                      or
                      bitand(streams_mv_refresh.txn_tabs(i).dmltypes, 
                             streams_mv_refresh.update_dml) = 
                               streams_mv_refresh.update_dml);

      print_trace('table=' || tab_owner || '.' || tab_name);

      while find_next_mv(tab_owner, tab_name, TRUE, stmv_i, tab_i) loop

        print_trace('MV=' || get_mvs()(stmv_i).mv_owner || '.' ||
                    get_mvs()(stmv_i).mv_name);

        print_trace('MV inst_scn=' || get_mvs()(stmv_i).inst_scn);
        if (commit_scn > get_mvs()(stmv_i).inst_scn) then

          print_trace('mv_flags='||get_mvs()(stmv_i).flags);
          -- determine if an MJV
          is_mjv := bitand(get_mvs()(stmv_i).flags, 
                             streams_mv_refresh_adm.flg_mjv) = 
                        streams_mv_refresh_adm.flg_mjv;

          -- determine if an outerjoin MJV
          is_ojmjv := bitand(get_mvs()(stmv_i).flags, 
                           streams_mv_refresh_adm.flg_mjv + 
                           streams_mv_refresh_adm.flg_hasoj) = 
                    streams_mv_refresh_adm.flg_mjv + 
                    streams_mv_refresh_adm.flg_hasoj;

          -- join row processing
          if (has_deletes) then
            print_trace('delete phase: joins');

            execute_delta_query(stmv_i, tab_i, 'DELETE', FALSE, commit_scn);
          end if;
    
          if (has_inserts) then
            print_trace('insert phase: joins');

            if (is_mjv) then
              dml_type := 'INSERT';
--              dml_type := 'UPDATE';
            else
              dml_type := 'INSERT';
            end if;

            execute_delta_query(stmv_i, tab_i, dml_type, FALSE, commit_scn);
          end if;      

          -- outer-join MJVs: take care of anti-join rows
          if (is_ojmjv) then

            print_trace('is_ojmjv=true');

            -- if INSERT or UPDATE was done, delete antijoins
            if (has_inserts) then
              print_trace('delete phase: antijoins');

              execute_delta_query(stmv_i, tab_i, 'DELETE', TRUE, commit_scn);
            end if;

            -- if DELETE or UPDATE was done, insert antijoins
            if (has_deletes) then
              print_trace('insert phase: antijoins');

              execute_delta_query(stmv_i, tab_i, 'UPDATE', TRUE, commit_scn);
            end if;
          end if;
        end if;
      end loop;

    end loop;

    -- reset tab_owners, tab_names collections
    streams_mv_refresh.txn_tabs.delete;

  end if;

  print_trace('source_commit_hdlr()-');
end;

end streams_mv_refresh;
/


create or replace package body streams_mv_lcr as

-- print LCR columns for debugging
procedure print_lcr_cols(cols sys.lcr$_row_list) is
  col_type      pls_integer; 
  tmp           sys.anytype;

begin
  streams_mv_refresh.print_trace('print_lcr_cols()+');

  -- make sure cols collection is non-empty
  if (cols.count = 0) then
    return;
  end if;

  for i in cols.first..cols.last loop
    streams_mv_refresh.print_trace('colname='||cols(i).column_name);

    col_type := cols(i).data.getType(tmp);

    if col_type = 1 then                                            -- varchar2
      streams_mv_refresh.print_trace('colval='||cols(i).data.accessvarchar2);
    elsif col_type = 2 then                                           -- number
      streams_mv_refresh.print_trace('colval='||cols(i).data.accessnumber);
    else
      streams_mv_refresh.print_trace('colval='||cols(i).data.accessvarchar2);
    end if;
  end loop;

  streams_mv_refresh.print_trace('print_lcr_cols()-');
end;


--
-- convert UPDATE LCR to an INSERT
--
procedure do_an_insert(lcr sys.lcr$_row_record) is
  tmp      sys.anydata;
  mv_no    pls_integer;
  i        pls_integer;
  found    boolean := FALSE;
  lcr_new  sys.lcr$_row_record;
  newvals  sys.lcr$_row_list := sys.lcr$_row_list();
begin
  streams_mv_refresh.print_trace('do_an_insert()+');

  if ( lcr.get_command_type = 'UPDATE' ) then
    newvals := lcr.get_values('NEW', 'N');

    -- double-quote all column names
    for i in newvals.first..newvals.last loop
      newvals(i).column_name := '"' || newvals(i).column_name || '"';
    end loop;

    lcr_new := sys.lcr$_row_record.construct(
                 source_database_name=>lcr.get_source_database_name, 
                 command_type=>'INSERT',
                 object_owner=>lcr.get_object_owner,
                 object_name=>lcr.get_object_name,
                 old_values=>NULL,
                 new_values=>newvals);

    streams_mv_refresh.print_trace('printing newvals:');
--    streams_mv_lcr.print_lcr_cols(newvals);

    lcr_new.execute(false);
  end if;

  streams_mv_refresh.print_trace('do_an_insert()-');
end;


--
-- apply the LCR
--
procedure process_lcr(lcr sys.lcr$_row_record) is

  -- new exception raised during apply to replace ora-1403
  row_does_not_exist exception;
  pragma exception_init(row_does_not_exist, -26787);

begin
  streams_mv_refresh.print_trace('process_lcr()+');

  -- this function may be customized to call a different procedure
  begin
    dbms_mview.set_i_am_a_refresh(true);  -- allows the MV to be updated
    lcr.execute(false);
    streams_mv_refresh.print_trace('applied LCR');
    dbms_mview.set_i_am_a_refresh(false);

  exception
    when DUP_VAL_ON_INDEX then
      streams_mv_refresh.print_trace('DUP_VAL_ON_INDEX, type='||
                                     lcr.get_command_type);

    when NO_DATA_FOUND or row_does_not_exist then
      streams_mv_refresh.print_trace('NO_DATA_FOUND, type='||
                                     lcr.get_command_type);
      do_an_insert(lcr);
    when others then
      raise;
  end;

  streams_mv_refresh.print_trace('process_lcr()-');
end;

end streams_mv_lcr;
/

-- Procedure for verifying results
create or replace procedure verify_tables(tab1 in varchar2, 
                                          tab2 in varchar2, 
                                          max_time in integer default  3600) 
  authid current_user as 

   tab2_sql varchar2(32000);
   tab1_sql varchar2(32000);
   diff_sql varchar2(32000);
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
