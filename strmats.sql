connect sys/knl_test7 as sysdba
drop user trgadm cascade
/
grant dba to trgadm identified by trgadm
/
begin
  dbms_streams_auth.grant_admin_privilege(
    grantee          => 'trgadm',    
    grant_privileges => true);
end;
/
grant create any procedure to trgadm
/
grant drop any procedure to trgadm
/
grant execute any procedure to trgadm
/
grant select on dba_tab_columns to trgadm
/

connect trgadm/trgadm

create table async_trigger$ (
  table_owner     VARCHAR2(30),
  table_name      VARCHAR2(30),
  trigger_owner   VARCHAR2(30),
  trigger_name    VARCHAR2(30),
  dml_events      NUMBER,
  flags           NUMBER)
/

create table async_trigger_rule$(
  owner           VARCHAR2(30),
  table_name      VARCHAR2(30),
  capture_rule    VARCHAR2(65),
  apply_rule      VARCHAR2(65))
  /

create table props$ (
  trigger_capture            VARCHAR2(30),
  trigger_apply              VARCHAR2(30),
  trigger_apply_status       VARCHAR2(30))
/

create index async_trigger_i1 on async_trigger$(table_owner, table_name)
/
create unique index async_trigger_i2 on async_trigger$(trigger_owner, 
trigger_name)
/

create type table_dml IS object (
    owner      varchar2(30),
    table_name varchar2(30), 
    dml        number)
/

create type table_dml_list is varray(1024) of table_dml
/

@@strmatp.sql
