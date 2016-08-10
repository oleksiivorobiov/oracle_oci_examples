create or replace package async_trig_pkg AUTHID CURRENT_USER AS

  on_insert  constant binary_integer := 1;
  on_update  constant binary_integer := 2;
  on_delete  constant binary_integer := 4;

  /* set up catpure and apply parameters */
  procedure set_up_streams(capture_name IN varchar2,
                           apply_name   IN varchar2); 

  /* clean up capture and apply */
  procedure clean_up_streams;

  /*
   * After trigger creation, the following procedure will be created for
   * row triggers:
   * procedure <trigger_name> (lcr        IN lcr$_row_record, 
   *                           dml_events IN binary_integer) IS
   *   old <tab>%rowtype;
   *   new <tab>%rowtype;
   * BEGIN
   *   ... -- populate old and new values using lcr
   *   IF (<when_clause>) THEN 
   *     <action>
   *   END IF;
   * END;
   * 
   * The IF ... THEN caluse will be omitted if when_clause is NULL.
   * for table triggers, the following procedure will be created:
   * 
   * procedure <trigger_name> (dml_events IN binary_integer) IS
   * BEGIN
   *   <action>
   * END;
   *
   * Table triggers will be called once per txn. 
 */

  procedure create_trigger(trigger_owner IN varchar2,
                           trigger_name  IN varchar2,
                           table_owner   IN varchar2,
                           table_name    IN varchar2,
                           dml_events    IN binary_integer,
                           for_each_row  IN boolean DEFAULT TRUE,
                           when_clause   IN varchar2 DEFAULT NULL,
                           action        IN varchar2);

                                            /* drop the asynchronous trigger */
  procedure drop_trigger(owner          IN varchar2,
                         trigger_name   IN varchar2);
end;
/
show errors
/
grant execute on async_trig_pkg to public
/
create or replace public synonym async_trig_pkg for trgadm.async_trig_pkg
/

create or replace package async_trig_internal AS

  tabdml   table_dml_list := table_dml_list();

  procedure set_up_streams(capture_name IN varchar2,
                           apply_name   IN varchar2); 

  procedure clean_up_streams;


  procedure create_trigger(trig_owner   IN varchar2,
                           trig_name IN varchar2,
                           tab_owner    IN varchar2,
                           tab_name     IN varchar2,
                           dml_events   IN binary_integer,
                           for_each_row IN boolean DEFAULT TRUE,
                           when_clause  IN varchar2 DEFAULT NULL,
                           action       IN varchar2);


  procedure drop_trigger(trig_owner     IN varchar2,
                         trig_name      IN varchar2);

  procedure dml_handler(in_any IN ANYDATA);

  procedure commit_handler(scn IN number);

end;
/
show errors
/

create or replace package body async_trig_pkg as
  procedure set_up_streams(capture_name IN varchar2,
                           apply_name   IN varchar2) IS
    cnt     binary_integer;
  BEGIN
    select count(role) into cnt from session_roles where role = 'DBA';
    IF cnt > 0 THEN
      async_trig_internal.set_up_streams(capture_name, apply_name);

      dbms_rule_adm.create_rule_set(
          rule_set_name      => 'TRGADM.ASYNC_TRIG_ARS',
          evaluation_context => 'sys.streams$_evaluation_context');

      dbms_capture_adm.create_capture(queue_name   => 'TRGADM.ASYNC_TRIG_Q',
                                      capture_name => capture_name);

      dbms_apply_adm.create_apply(queue_name => 'TRGADM.ASYNC_TRIG_Q',
                                  apply_name => apply_name,
                                  rule_set_name => 'TRGADM.ASYNC_TRIG_ARS',
                                  apply_captured => TRUE);
      dbms_apply_adm.set_parameter(apply_name, 'COMMIT_SERIALIZATION', 'FULL');
    ELSE
      raise_application_error(-20002, 
            'DBA role is required to set up asynchronous trigger streams');
    END IF;
  END;

  procedure clean_up_streams IS
    cnt     binary_integer;
  BEGIN
    select count(role) into cnt from session_roles where role = 'DBA';
    IF cnt > 0 THEN
      async_trig_internal.clean_up_streams;
    ELSE
      raise_application_error(-20003, 
            'DBA role is required to clean up asynchronous trigger streams');
    END IF;
  END;

  procedure create_trigger(trigger_owner IN varchar2,
                           trigger_name  IN varchar2,
                           table_owner   IN varchar2,
                           table_name    IN varchar2,
                           dml_events    IN binary_integer,
                           for_each_row  IN boolean DEFAULT TRUE,
                           when_clause   IN varchar2 DEFAULT NULL,
                           action        IN varchar2) IS
    cnt        binary_integer;
    trig_owner VARCHAR2(30);
    tab_owner  VARCHAR2(30);
    tab_name   VARCHAR2(30);
    cur_user   VARCHAR2(30);
  BEGIN

    trig_owner := NVL(trigger_owner, USERENV('SCHEMAID'));
    tab_owner  := NVL(table_owner, USERENV('SCHEMAID'));
    tab_name := table_name;

    -- check the existence of the underlying table
    select count(1) into cnt from all_tables
    where owner = tab_owner and table_name = tab_name;

    IF cnt = 0 THEN
       raise_application_error(-20007, 
            'Table "' || tab_owner || '"."' || tab_name ||
            ' does not exist');
    END IF;

    select username into cur_user from user_users;

    -- check current user's privilege to create the trigger
    IF trig_owner = cur_user THEN
      select count(privilege) into cnt from session_privs 
      where  privilege = 'CREATE TRIGGER';
    ELSE
      select count(privilege) into cnt from session_privs 
      where  privilege = 'CREATE ANY TRIGGER';
    END IF;

    IF cnt > 0 THEN
      async_trig_internal.create_trigger(trig_owner, trigger_name, 
                          tab_owner, tab_name,
                          dml_events, for_each_row, when_clause, action);
    ELSE
      raise_application_error(-20004, 
            'Insufficient privilege to create trigger "' || trigger_owner || 
            '"."' || trigger_name || '"');
    END IF;
  END;


  procedure drop_trigger(owner          IN varchar2,
                         trigger_name   IN varchar2) IS
    cnt     binary_integer;
    cur_user   VARCHAR2(30);
  BEGIN
    select username into cur_user from user_users;

    IF owner = cur_user THEN
      cnt := 1;
    ELSE
      select count(privilege) into cnt from session_privs 
      where  privilege = 'DROP ANY TRIGGER';
    END IF;

    IF cnt > 0 THEN
      async_trig_internal.drop_trigger(owner, trigger_name);
    ELSE
      raise_application_error(-20005, 
            'Insufficient privilege to drop trigger "' || owner || '"."' ||
            trigger_name);
    END IF;
  END;
end async_trig_pkg;
/

show errors
/

create or replace package body async_trig_internal as

  CRLF       CONSTANT VARCHAR2(4) := '
';
  on_insert  constant binary_integer := async_trig_pkg.on_insert;
  on_update  constant binary_integer := async_trig_pkg.on_update;
  on_delete  constant binary_integer := async_trig_pkg.on_delete;

  procedure set_up_streams(capture_name IN varchar2,
                           apply_name   IN varchar2) IS
    cnt     NUMBER;
  BEGIN
    select count(1) into cnt FROM props$;
    IF cnt > 0 THEN
      raise_application_error(-20001, 
            'asynchronous trigger streams already exists.');
    END IF;
    insert into props$ values (capture_name, apply_name, 'NOT STARTED');
    commit;

    dbms_streams_adm.set_up_queue(
    queue_table  => 'TRGADM.ASYNC_TRIG_QT',
    queue_name   => 'TRGADM.ASYNC_TRIG_Q');

  END set_up_streams;

  PROCEDURE clean_up_streams IS
    db              varchar2(128);
    capture_name    VARCHAR2(30);
    apply_name      VARCHAR2(30);
    apply_status    VARCHAR2(30);
    cursor tab_c    is select unique table_owner, table_name 
                    from async_trigger$;
    cursor trig_c   is select trigger_owner, trigger_name from async_trigger$;
    owner_name      VARCHAR2(65);
    proc_does_not_exist EXCEPTION;
    PRAGMA          EXCEPTION_INIT(proc_does_not_exist, -26701);

  BEGIN
    select global_name into db from global_name;
    select trigger_capture, trigger_apply,  trigger_apply_status into 
           capture_name, apply_name, apply_status from props$;

    IF apply_status = 'STARTED' THEN 
      -- stop capture and apply
      dbms_capture_adm.stop_capture(capture_name);
      dbms_apply_adm.stop_apply(apply_name);

      for t_rec in tab_c loop
        owner_name := '"' || t_rec.table_owner || '"."' || t_rec.table_name ||
                      '"';

        -- remove dml handlers
        dbms_apply_adm.set_dml_handler(
          object_name         => owner_name,
          object_type         => 'TABLE',
          operation_name      => 'INSERT',
          error_handler       => false,
          user_procedure      => NULL,
          apply_database_link => NULL,
          apply_name          => apply_name);

        dbms_apply_adm.set_dml_handler(
          object_name         => owner_name,
          object_type         => 'TABLE',
          operation_name      => 'UPDATE',
          error_handler       => false,
          user_procedure      => NULL,
          apply_database_link => NULL,
          apply_name          => apply_name);

        dbms_apply_adm.set_dml_handler(
          object_name         => owner_name,
          object_type         => 'TABLE',
          operation_name      => 'DELETE',
          error_handler       => false,
          user_procedure      => NULL,
          apply_database_link => NULL,
          apply_name          => apply_name);

        dbms_apply_adm.set_table_instantiation_scn(
          source_object_name    => owner_name,
          source_database_name  => db,
          instantiation_scn     => null);
      end loop;

      -- remove capture and apply
      BEGIN 
        dbms_apply_adm.drop_apply(apply_name, true);
        dbms_capture_adm.drop_capture(capture_name, true);
      EXCEPTION
        WHEN proc_does_not_exist THEN
          NULL; 
        WHEN others THEN
          RAISE;
      END;

    END IF;

    FOR g_rec in trig_c LOOP
      execute immediate 'drop procedure "' || g_rec.trigger_owner || '"."' || 
                        g_rec.trigger_name || '"';
    END LOOP;

    DELETE FROM async_trigger$;
    DELETE FROM async_trigger_rule$;
    DELETE FROM props$;
    commit;
    -- remove the queue ASYNC_TRIG_Q
    dbms_streams_adm.remove_queue('ASYNC_TRIG_Q');

  END clean_up_streams;

  -- for simplicity, all the necessary error condition checks and security
  -- checks are omitted.

  PROCEDURE create_trigger(trig_owner   IN varchar2,
                           trig_name    IN varchar2,
                           tab_owner    IN varchar2,
                           tab_name     IN varchar2,
                           dml_events   IN binary_integer,
                           for_each_row IN boolean DEFAULT TRUE,
                           when_clause  IN varchar2 DEFAULT NULL,
                           action       IN varchar2) IS
    capture_name    VARCHAR2(30);
    apply_name      VARCHAR2(30);
    apply_status    VARCHAR2(30);
    stmt_buf        VARCHAR2(20000);
    flags           NUMBER;
    inst_scn        number;
    db              varchar2(128);
    cnt             binary_integer;
    full_name       VARCHAR2(65);  
    dml_rule_c      VARCHAR2(65);
    ddl_rule_c      VARCHAR2(65);
    dml_rule_a      VARCHAR2(65);
    ddl_rule_a      VARCHAR2(65);

    cursor col_c(tab_owner varchar2, tab_name varchar2) 
                    IS select column_name, data_type from dba_tab_columns 
                    where owner = tab_owner and table_name = tab_name;
  BEGIN

    -- check whether trigger already exists
    SELECT count(1) INTO cnt FROM async_trigger$ 
    WHERE  trigger_owner = trig_owner and trigger_name = trig_name;

    IF cnt > 0 THEN
       raise_application_error(-20009, 
            'trigger ' || trig_owner || '.' || trig_name || 'already exists');
    END IF;

    full_name := '"' || tab_owner || '"."' || tab_name || '"';
    stmt_buf := 'CREATE PROCEDURE "' || trig_owner || '"."' || 
                trig_name;

    IF (not for_each_row) and when_clause IS NOT NULL THEN
      raise_application_error(-20006, 
            'table triggers cannot specify WHEN clause');
    END IF;

    IF for_each_row THEN

      stmt_buf := stmt_buf || 
               '" (lcr IN sys.lcr$_row_record, dml_events IN binary_integer) '
               || 'IS'  || CRLF || '  old ' || full_name || '%rowtype; ' 
               || CRLF || '  new ' || full_name || '%rowtype; ' || CRLF ||
               '  rc NUMBER; ' || CRLF ||
               '  col_val SYS.ANYDATA; ' || CRLF || 'BEGIN' || CRLF ;

      -- now computes the old and new values for row triggers
      stmt_buf := stmt_buf || 
                  '  IF lcr.get_command_type() = ''INSERT'' THEN' || CRLF ||
                  '    old := NULL; ' || CRLF || '  ELSE' || CRLF;

      FOR c_rec IN col_c(tab_owner, tab_name) LOOP
        stmt_buf := stmt_buf || 
                    '    col_val := lcr.get_value(''OLD'', ''' ||
                    c_rec.column_name || ''');' || CRLF ||
                    '    IF col_val IS NULL THEN ' || CRLF ||
                    '      old.' || c_rec.column_name || ':= NULL; ' || CRLF ||
                    '    ELSE ' || CRLF;

        IF c_rec.data_type = 'NUMBER' or c_rec.data_type = 'FLOAT' THEN
          stmt_buf := stmt_buf || '      rc:= col_val.GETNUMBER(old.' ||  
                      c_rec.column_name ||'); ' || CRLF;
        ELSIF c_rec.data_type = 'VARCHAR2' THEN
          stmt_buf := stmt_buf || '      rc:= col_val.GETVARCHAR2(old.' ||  
                      c_rec.column_name ||'); ' || CRLF;
        ELSIF c_rec.data_type = 'RAW' THEN
          stmt_buf := stmt_buf || '      rc:= col_val.GETRAW(old.' ||  
                      c_rec.column_name ||'); ' || CRLF;
        ELSIF c_rec.data_type = 'DATE' THEN
          stmt_buf := stmt_buf || '      rc:= col_val.GETDATE(old.' ||  
                      c_rec.column_name ||'); ' || CRLF;
        ELSIF c_rec.data_type = 'TIMESTAMP' THEN
          stmt_buf := stmt_buf || '      rc:= col_val.GETTIMESTAMP(old.' ||  
                      c_rec.column_name ||'); ' || CRLF;
        ELSE
          raise_application_error(-20008, 'UNKNOWN data type');
        END IF;

        stmt_buf := stmt_buf || '    END IF; ' || CRLF;
      END LOOP;

      stmt_buf := stmt_buf || '  END IF; ' || CRLF || 
                  '  IF lcr.get_command_type() = ''DELETE'' THEN ' || CRLF ||
                  '    new := NULL; ' || CRLF || '  ELSE' || CRLF;

      FOR c_rec IN col_c(tab_owner, tab_name) LOOP
        stmt_buf := stmt_buf || 
                    '    col_val := lcr.get_value(''NEW'', ''' ||
                    c_rec.column_name || ''');' || CRLF ||
                    '    IF col_val IS NULL THEN ' || CRLF ||
                    '      new.' || c_rec.column_name || ':= NULL; ' || CRLF ||
                    '    ELSE ' || CRLF;

        IF c_rec.data_type = 'NUMBER' or c_rec.data_type = 'FLOAT' THEN
          stmt_buf := stmt_buf || '      rc:= col_val.GETNUMBER(new.' ||  
                      c_rec.column_name ||'); ' || CRLF;
        ELSIF c_rec.data_type = 'VARCHAR2' THEN
          stmt_buf := stmt_buf || '      rc:= col_val.GETVARCHAR2(new.' ||  
                      c_rec.column_name ||'); ' || CRLF;
        ELSIF c_rec.data_type = 'RAW' THEN
          stmt_buf := stmt_buf || '      rc:= col_val.GETRAW(new.' ||  
                      c_rec.column_name ||'); ' || CRLF;
        ELSIF c_rec.data_type = 'DATE' THEN
          stmt_buf := stmt_buf || '      rc:= col_val.GETDATE(new.' ||  
                      c_rec.column_name ||'); ' || CRLF;
        ELSIF c_rec.data_type = 'TIMESTAMP' THEN
          stmt_buf := stmt_buf || '      rc:= col_val.GETTIMESTAMP(new.' ||  
                      c_rec.column_name ||'); ' || CRLF;
        ELSE
          raise_application_error(-20008, 'UNKNOWN data type');
        END IF;

        stmt_buf := stmt_buf || '    END IF; ' || CRLF;
      END LOOP;

      stmt_buf := stmt_buf || '  END IF; ' || CRLF;

    ELSE
      stmt_buf := stmt_buf || 
                  '" (dml_events IN binary_integer) IS' || CRLF || 'BEGIN' || 
                  CRLF ;
    END IF;

    IF (when_clause IS NOT NULL) THEN
      stmt_buf := stmt_buf || '  IF ' || when_clause || ' THEN' || CRLF ||
                  action || CRLF || '  END IF; ' || CRLF || 'END; ';
    ELSE
      stmt_buf := stmt_buf || action || CRLF || 'END; ';
    END IF;

    -- create the trigger
    execute immediate stmt_buf;
    
    -- insert into trigger table
    IF for_each_row THEN
      flags := 1;
    ELSE
      flags := 0;
    END IF;

    insert into async_trigger$ values (tab_owner, tab_name, trig_owner, 
                                       trig_name, dml_events, flags);
    commit;

    select trigger_capture, trigger_apply,  trigger_apply_status into 
           capture_name, apply_name, apply_status from props$;
    select global_name into db from global_name;
    select count(*) into cnt from dba_apply_instantiated_objects 
    where source_object_owner = tab_owner and source_object_name = tab_name and
          source_object_type = 'TABLE' and source_database = db;

    -- if table already has an instantiation scn registered, don't call
    -- dbms_apply_adm.set_table_instantiation_scn()

    IF (cnt = 0) THEN
      inst_scn := dbms_flashback.get_system_change_number();
      dbms_apply_adm.set_table_instantiation_scn(
        source_object_name    => full_name,
        source_database_name  => db,
        instantiation_scn     => inst_scn);
    END IF;

    -- as a simplification, add one rule per-table for all triggers
    -- on the table
    select count(1) into cnt from async_trigger_rule$ 
    where owner = tab_owner and table_name = tab_name;

    IF cnt = 0 THEN
      dbms_streams_adm.add_table_rules(
      table_name         => full_name,
      streams_type       => 'apply', 
      streams_name       => apply_name,
      queue_name         => 'TRGADM.ASYNC_TRIG_Q',
      include_tagged_lcr => true,
      include_dml        => true,
      include_ddl        => false,
      source_database    => db,
      dml_rule_name      => dml_rule_a,
      ddl_rule_name      => ddl_rule_a);

      dbms_apply_adm.set_dml_handler(
        object_name         => full_name,
        object_type         => 'TABLE',
        operation_name      => 'INSERT',
        error_handler       => false,
        user_procedure      => 'trgadm.async_trig_internal.dml_handler',
        apply_database_link => NULL,
        apply_name          => apply_name);

      dbms_apply_adm.set_dml_handler(
        object_name         => full_name,
        object_type         => 'TABLE',
        operation_name      => 'UPDATE',
        error_handler       => false,
        user_procedure      => 'trgadm.async_trig_internal.dml_handler',
        apply_database_link => NULL,
        apply_name          => apply_name);

      dbms_apply_adm.set_dml_handler(
        object_name         => full_name,
        object_type         => 'TABLE',
        operation_name      => 'DELETE',
        error_handler       => false,
        user_procedure      => 'trgadm.async_trig_internal.dml_handler',
        apply_database_link => NULL,
        apply_name          => apply_name);

     dbms_streams_adm.add_table_rules(
      table_name         => full_name,   
      streams_type       => 'capture',
      streams_name       => capture_name,
      queue_name         => 'TRGADM.ASYNC_TRIG_Q',
      include_tagged_lcr => true,
      include_dml        => true,
      include_ddl        => false,
      dml_rule_name      => dml_rule_c,
      ddl_rule_name      => ddl_rule_c,
      inclusion_rule     => true);

    insert into async_trigger_rule$ values (tab_owner, tab_name, dml_rule_c,
                                            dml_rule_a);
    commit;
    END IF;
 
    IF (apply_status = 'NOT STARTED') THEN
      -- start capture and apply
      dbms_apply_adm.set_parameter(apply_name  => apply_name, 
                                   parameter   => 'disable_on_error', 
                                   value       => 'n');
      dbms_apply_adm.alter_apply(
      apply_name        => apply_name,
      precommit_handler => 'trgadm.async_trig_internal.commit_handler');

      dbms_apply_adm.start_apply(apply_name  => apply_name);
      dbms_capture_adm.start_capture(capture_name  => capture_name);

      UPDATE props$ set trigger_apply_status = 'STARTED' 
      where trigger_capture = capture_name;
      COMMIT;
    END IF;

  END;

  procedure drop_trigger(trig_owner     IN varchar2,
                         trig_name      IN varchar2) IS
    d_table         VARCHAR2(30);
    dml_evts        binary_integer;
    cnt             NUMBER;
    capture_name    VARCHAR2(30);
    apply_name      VARCHAR2(30);
    dml_rule_c      VARCHAR2(65);
    dml_rule_a      VARCHAR2(65);
    full_name       VARCHAR2(65);
    tab_owner       VARCHAR2(30);
    ob_does_not_exist    EXCEPTION;
    PRAGMA          EXCEPTION_INIT(ob_does_not_exist, -4043);
  BEGIN

    select table_owner, table_name, dml_events 
           into tab_owner, d_table, dml_evts 
    from   async_trigger$ 
    where  trigger_owner = trig_owner and trigger_name = trig_name;

    delete from async_trigger$ 
    where trigger_owner = trig_owner and trigger_name = trig_name;

    select count(1) into cnt from async_trigger$ 
    where table_owner = tab_owner and table_name = d_table;
    commit;

    BEGIN
    execute immediate 'drop procedure "' || 
                      trig_owner || '"."' ||trig_name|| '"';
    EXCEPTION
      WHEN ob_does_not_exist THEN
        NULL;
      WHEN others THEN
        RAISE;
    END;

    -- cleanup DML handler when necessary
    select trigger_capture, trigger_apply into capture_name, apply_name 
    from props$;

    IF cnt = 0 THEN
    BEGIN
      select capture_rule, apply_rule into dml_rule_c, dml_rule_a 
      from async_trigger_rule$ 
      where  owner = tab_owner and table_name = d_table;

      dbms_streams_adm.remove_rule(rule_name => dml_rule_c, 
                                   streams_type => 'CAPTURE',
                                   streams_name => capture_name);

      dbms_streams_adm.remove_rule(rule_name => dml_rule_a, 
                                   streams_type => 'APPLY',
                                   streams_name => apply_name);
      full_name := '"' || tab_owner || '"."' || d_table || '"';
      dbms_apply_adm.set_dml_handler(
             object_name         => full_name,
             object_type         => 'TABLE',
             operation_name      => 'INSERT',
             error_handler       => false,
             user_procedure      => NULL,
             apply_database_link => NULL,
             apply_name          => apply_name);
 
      dbms_apply_adm.set_dml_handler(
             object_name         => full_name,
             object_type         => 'TABLE',
             operation_name      => 'UPDATE',
             error_handler       => false,
             user_procedure      => NULL,
             apply_database_link => NULL,
             apply_name          => apply_name);

      dbms_apply_adm.set_dml_handler(
             object_name         => full_name,
             object_type         => 'TABLE',
             operation_name      => 'DELETE',
             error_handler       => false,
             user_procedure      => NULL,
             apply_database_link => NULL,
             apply_name          => apply_name);
    EXCEPTION
      WHEN no_data_found THEN
        NULL;
      WHEN OTHERS THEN
        RAISE;
    END;
    END IF;
  END;

  PROCEDURE add_table_dml(tab_owner   IN varchar2,
                          tab_name    IN varchar2,
                          dml_evts    IN binary_integer) IS
    tab_found   boolean;
  BEGIN
    tab_found := FALSE;
    FOR i IN 1 .. tabdml.count LOOP
      IF tabdml(i).owner = tab_owner and tabdml(i).table_name = tab_name and
         tabdml(i).dml = dml_evts THEN
        tab_found := TRUE;
        EXIT;
      END IF;
    END LOOP;
    
    IF NOT tab_found THEN
      tabdml.extend(1);
      tabdml(tabdml.count) := table_dml(tab_owner, tab_name, dml_evts);
    END IF;   
  END;

  PROCEDURE dml_handler(in_any IN ANYDATA) IS
    lcr          SYS.LCR$_ROW_RECORD;
    rc           PLS_INTEGER;
    commnd       varchar2(30);
    dml_evts     binary_integer;
    flags        binary_integer;
    cursor       trg_c(tab_owner varchar2, tab_name varchar2, evts number) 
                 IS select trigger_owner, trigger_name 
                 from trgadm.async_trigger$ 
                 where table_owner = tab_owner and table_name = tab_name and 
                       bitand(dml_events, evts) > 0 and flags = 1;
    tab_owner    VARCHAR2(30);
    tab_name     VARCHAR2(30);
  BEGIN
    -- get LCR info
    rc        := in_any.getobject(lcr);

    -- Fire row triggers 
    commnd := lcr.get_command_type();
    IF commnd = 'INSERT' THEN
      dml_evts := on_insert;
    ELSIF commnd = 'UPDATE' THEN
      dml_evts := on_update;
    ELSIF commnd = 'DELETE' THEN
      dml_evts := on_delete;
    ELSE
      dml_evts := 0;
    END IF;

    IF dml_evts > 0 THEN
      tab_owner := lcr.get_object_owner();
      tab_name  := lcr.get_object_name();
  
      -- remember to fire table triggers in the commit handler
      add_table_dml(tab_owner, tab_name, dml_evts);

      FOR t_rec IN trg_c(tab_owner, tab_name, dml_evts) LOOP
        execute immediate 'begin "' || t_rec.trigger_owner || '"."' ||
                t_rec.trigger_name || '"(:1, :2); end;' using 
                lcr, dml_evts;
      END LOOP;
   END IF;
  END;
 
  PROCEDURE commit_handler(scn IN number) IS
    cursor       trg_c(tab_owner varchar2, tab_name varchar2, evts number) 
                 IS select trigger_owner, trigger_name 
                 from trgadm.async_trigger$ 
                 where table_owner = tab_owner and table_name = tab_name and 
                       bitand(dml_events, evts) > 0 and flags = 0;
  BEGIN

    -- fire table triggers here
    FOR i in 1 .. tabdml.count LOOP
      FOR t_rec IN trg_c(tabdml(i).owner, tabdml(i).table_name,
                         tabdml(i).dml) LOOP
        execute immediate 'begin "' || t_rec.trigger_owner || '"."' ||
                t_rec.trigger_name || '"(:1); end;' using tabdml(i).dml;
      END LOOP;
    END LOOP;
   
    tabdml.delete;
  END;

end;
/
