/*
 ************* asynchronous commit-time trigger using Streams *************

 This script creates two row triggers and a table trigger on 
 table u1.accounts, recording any changes to user Alice's balance.

*/

SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100

SET ECHO ON
connect sys/knl_test7 as sysdba;
grant connect, resource to u1 identified by u1;

connect trgadm/trgadm
exec async_trig_pkg.set_up_streams('ACCOUNT_CAPTURE', 'ACCOUNT_APPLY');

connect u1/u1
create table accounts(name varchar2(30), balance number);
create table account_history (event_name  varchar2(30), 
                              event_time  timestamp,
                              name        varchar2(30), 
                              old_balance number, 
                              new_balance number);

DECLARE
  trigger_action VARCHAR2(1000);
BEGIN

  -- first, create an insert trigger, for each row
  trigger_action := 'INSERT INTO u1.account_history values (''INSERT'', ' ||
                    'SYSTIMESTAMP, new.name, NULL, new.balance); ';
  async_trig_pkg.create_trigger(trigger_owner => 'U1', 
                                trigger_name => 'ACCOUNT_TRIG1', 
                                table_owner => 'U1',
                                table_name => 'ACCOUNTS', 
                                dml_events => async_trig_pkg.on_insert, 
                                for_each_row => TRUE, 
                                when_clause => 'new.name = ''ALICE''', 
                                action => trigger_action);

  -- then, create an update/delete trigger, for each row
  trigger_action := 'IF bitand(dml_events, async_trig_pkg.on_update) = ' ||
                    'async_trig_pkg.on_update THEN ' ||
                    'INSERT INTO u1.account_history values (''UPDATE'', ' ||
                    'SYSTIMESTAMP, old.name, old.balance, new.balance); ' ||
                    ' ELSE ' ||
                    'INSERT INTO u1.account_history values (''DELETE'', ' ||
                    'SYSTIMESTAMP, old.name, old.balance, NULL); ' ||
                    ' END IF; ';
  async_trig_pkg.create_trigger(trigger_owner => 'U1', 
                                trigger_name => 'ACCOUNT_TRIG2', 
                                table_owner => 'U1',
                                table_name => 'ACCOUNTS', 
                                dml_events => async_trig_pkg.on_update + 
                                              async_trig_pkg.on_delete, 
                                for_each_row => TRUE, 
                                when_clause => 'old.name = ''ALICE''', 
                                action => trigger_action);

  -- last, create an insert/update/delete table trigger
  trigger_action := 'INSERT INTO u1.account_history values (''DML'', ' ||
                    'SYSTIMESTAMP, NULL, NULL, NULL); ';
  async_trig_pkg.create_trigger(trigger_owner => 'U1', 
                                trigger_name => 'ACCOUNT_TRIG3',
                                table_owner => 'U1', 
                                table_name => 'ACCOUNTS', 
                                dml_events => async_trig_pkg.on_update + 
                                              async_trig_pkg.on_insert +
                                              async_trig_pkg.on_delete, 
                                for_each_row => FALSE, 
                                action => trigger_action);

END;
/

insert into accounts values ('ALICE', 1000);
commit;

update accounts set balance = 2000 where name = 'ALICE';
commit;

delete from accounts where name= 'ALICE';
commit;

insert into accounts values ('BOB', 500);
commit;

connect system/manager
declare
scn     number;
cscn    number;
slept   number := 0;
begin

  -- Get current scn
  scn := dbms_flashback.get_system_change_number;
  while (slept < 1200) loop
    dbms_lock.sleep(15);
    slept := slept + 15;
    begin  
      select lwm_message_number into cscn from gv$streams_apply_coordinator
      where apply_name = 'ACCOUNT_APPLY';
    exception 
      when no_data_found 
        then cscn := 0;
      when others then raise;
    end;
    if cscn >= scn then
      exit;
    end if;
  end loop;
end;
/

connect u1/u1
select * from account_history order by event_name, event_time;

connect trgadm/trgadm
exec async_trig_pkg.clean_up_streams;
