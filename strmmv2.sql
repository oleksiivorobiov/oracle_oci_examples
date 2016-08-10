Rem
Rem $Header: strmmv2.sql 27-sep-2006.15:36:22 davzhang Exp $
Rem
Rem strmmv2.sql
Rem
Rem Copyright (c) 2006, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      strmmv2.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem      <short description of component this file declares/defines>
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    davzhang    09/27/06 - grant create view explicitly
Rem    davzhang    08/29/06 - where clause when selecting from
Rem                           dba_apply_instantiated_objects
Rem    wesmith     06/23/06 - register_mv: add parameter queue_name
Rem    wesmith     06/12/06 - remove wait_for_jobq() call and misc cleanup
Rem    liwong      05/16/06 - sync capture cleanup 
Rem    davzhang    04/18/06 - wait for job queue before drop user
Rem    wesmith     01/19/06 - Created
Rem

SET ECHO ON
SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 10000
SET SERVEROUTPUT ON

COLUMN SRC_LONG_CNAME FORMAT A15
COLUMN SCHEMA_NAME FORMAT A15
COLUMN OBJECT_NAME FORMAT A15
COLUMN STREAMS_TYPE HEADING 'STREAMS_TYPE' FORMAT A19

variable site1 varchar2(80);

-- grant necessary privileges for streams admin
connect system/manager

drop user stradm cascade;
grant dba to stradm identified by stradm;
grant execute on SYS.DBMS_SYSTEM to stradm;
begin
  dbms_streams_auth.grant_admin_privilege(
    grantee          => 'stradm',    
    grant_privileges => true);
end;
/

-- create user for MVs
drop user sqst cascade;
grant connect, resource to sqst identified by sqst;
grant create materialized view, create view to sqst;

connect sqst/sqst

-- create base tables and load
set echo off
@strmmv2s
set echo on

-- create views to aid data verification
create view orders_v as
select o_id, orders.c_id c_id, ol_num, 
       orders.rowid o_rid, customer.rowid c_rid 
from orders, customer
where orders.c_id = customer.c_id
and customer.zip >= 19555;

create view oline_v as
select ol_id, order_line.o_id, i_id, 
       order_line.rowid ol_rid, orders.rowid o_rid, customer.rowid c_rid 
from order_line, orders, customer
where order_line.o_id = orders.o_id
and orders.c_id = customer.c_id
and customer.zip >= 19555;

-- MV logs
--create materialized view log on orders with primary key, rowid;
--create materialized view log on order_line with rowid;
--create materialized view log on customer with primary key, rowid(zip);

-- MJV
create materialized view orders_mv refresh force as
select o_id, orders.c_id, ol_num, orders.rowid o_rid, customer.rowid c_rid 
from orders, customer
where orders.c_id = customer.c_id
and customer.zip >= 19555;

-- another MJV
create materialized view oline_mv refresh force as
select ol_id, order_line.o_id, i_id, 
       order_line.rowid ol_rid, orders.rowid o_rid, customer.rowid c_rid 
from order_line, orders, customer
where order_line.o_id = orders.o_id
and orders.c_id = customer.c_id
and customer.zip >= 19555;

create unique index oline_mv_u1 on oline_mv(ol_rid, o_rid, c_rid);

-- pk-based MV (single table)
create materialized view customer_mv refresh force as select * from customer;

--
-- create unsupported MVs 
--

-- rowid MV
create materialized view bad_rowid refresh force with rowid as 
select * from customer;

-- on-commit MV
create materialized view bad_oncmt refresh force with rowid on commit as 
select * from customer;

-- updatable MV
create materialized view bad_upd refresh force for update as 
select * from customer;

-- subquery MV
create materialized view bad_sq refresh force as
select o_id, orders.c_id, ol_num from orders 
  where exists
  (select c_id from customer 
   where zip >= 19555 and orders.c_id = customer.c_id);

-- union all MJV
create materialized view bad_mjv_union_all refresh force as
select '1' mark, o_id, orders.c_id, ol_num, orders.rowid o_rid, 
  customer.rowid c_rid 
from orders, customer
where orders.c_id = customer.c_id
and customer.zip >= 19555
union all
select '2' mark, o_id, orders.c_id, ol_num, orders.rowid o_rid, 
  customer.rowid c_rid 
from orders, customer
where orders.c_id = customer.c_id
and customer.zip <= 11005;

-- MV with objects
create or replace type t1 as object (a1 number, a2 date)
/
create table objreltab (c1 number primary key, c2 t1, c3 varchar2(10));
create snapshot log on objreltab;
create materialized view bad_obj refresh force as select * from objreltab;


-- create supporting objects and packages for streams refresh metadata
connect stradm/stradm
set echo off
@strmmvp2
set echo on

--
-- streams setup
--
begin
  dbms_streams_adm.set_up_queue 
      (queue_user => 'stradm', queue_name => 'mv_master_q',
       queue_table => 'mv_master_qt');
end;
/

-- setting key columns is mandatory if a primary key does not exist on the MV
exec dbms_apply_adm.set_key_columns('SQST.ORDERS_MV', 'o_rid, c_rid');
exec dbms_apply_adm.set_key_columns('SQST.OLINE_MV', 'ol_rid, o_rid, c_rid');

-- register orders_mv for dbms_mview.refresh()
begin
  streams_mv_refresh_adm.register_mv(
    mv_owner=>'"SQST"', mv_name=>'"ORDERS_MV"', 
    capture_name=>'"CAPTURE"', apply_name=>'"APPLY_MV_MASTER"', 
    queue_name=>'mv_master_q',
    instantiate=>FALSE, use_streams_refresh=>FALSE);
end;
/

-- register oline_mv, customer_mv for streams-based refresh
begin
  streams_mv_refresh_adm.register_mv(
    mv_owner=>'sqst', mv_name=>'oline_mv', 
    capture_name=>'capture', apply_name=>'apply_mv_master', 
    instantiate=>TRUE, use_streams_refresh=>TRUE);
end;
/

begin
  streams_mv_refresh_adm.register_mv(
    mv_owner=>'SQST', mv_name=>'CUSTOMER_MV', 
    capture_name=>'CAPTURE', apply_name=>'APPLY_MV_MASTER', 
    instantiate=>FALSE, use_streams_refresh=>TRUE);
end;
/

-- FAIL: non-existent MV
begin
  streams_mv_refresh_adm.register_mv(
    mv_owner=>'SQST', mv_name=>'no_such_mv', 
    capture_name=>'CAPTURE', apply_name=>'APPLY_MV_MASTER', 
    instantiate=>FALSE, use_streams_refresh=>TRUE);
end;
/

--
-- unsupported registrations for streams-based refresh (neg testcases)
--
begin
  streams_mv_refresh_adm.register_mv(
    mv_owner=>'SQST', mv_name=>'BAD_ROWID', 
    capture_name=>'CAPTURE', apply_name=>'APPLY_MV_MASTER', 
    instantiate=>FALSE, use_streams_refresh=>TRUE);
end;
/
begin
  streams_mv_refresh_adm.register_mv(
    mv_owner=>'SQST', mv_name=>'BAD_ONCMT', 
    capture_name=>'CAPTURE', apply_name=>'APPLY_MV_MASTER', 
    instantiate=>FALSE, use_streams_refresh=>TRUE);
end;
/
begin
  streams_mv_refresh_adm.register_mv(
    mv_owner=>'SQST', mv_name=>'BAD_UPD', 
    capture_name=>'CAPTURE', apply_name=>'APPLY_MV_MASTER', 
    instantiate=>FALSE, use_streams_refresh=>TRUE);
end;
/
begin
  streams_mv_refresh_adm.register_mv(
    mv_owner=>'SQST', mv_name=>'BAD_SQ', 
    capture_name=>'CAPTURE', apply_name=>'APPLY_MV_MASTER', 
    instantiate=>FALSE, use_streams_refresh=>TRUE);
end;
/
begin
  streams_mv_refresh_adm.register_mv(
    mv_owner=>'SQST', mv_name=>'BAD_MJV_UNION_ALL', 
    capture_name=>'CAPTURE', apply_name=>'APPLY_MV_MASTER', 
    instantiate=>FALSE, use_streams_refresh=>TRUE);
end;
/


connect stradm/stradm
exec dbms_capture_adm.start_capture('CAPTURE');

exec dbms_apply_adm.set_parameter('APPLY_MV_MASTER','DISABLE_ON_ERROR','N');

-- assign precommit handler to apply process 
begin
  dbms_apply_adm.alter_apply(
    apply_name        => 'APPLY_MV_MASTER',
    precommit_handler => 'streams_mv_refresh.source_commit_hdlr');
end;
/
exec dbms_apply_adm.start_apply('APPLY_MV_MASTER');

-- check streams metadata
select apply_name, status from dba_apply order by apply_name;
select capture_name, status from dba_capture order by 1,2;

select streams_type, streams_name, table_owner, table_name, 
  rule_owner, rule_name
from dba_streams_table_rules
where streams_name in ('CAPTURE', 'APPLY_MV_MASTER')
order by streams_type, streams_name, table_owner, table_name;

select source_object_owner, source_object_name, source_database, 
       instantiation_scn
from dba_apply_instantiated_objects 
where source_object_owner='SQST'
order by source_database, source_object_owner, source_object_name;

select object_owner, object_name, operation_name, user_procedure
from dba_apply_dml_handlers
where apply_name = 'APPLY_MV_MASTER'
order by object_owner, object_name, operation_name;

-- check MV registration metadata
select mv_owner, mv_name from stmv_reg order by mv_owner, mv_name;

-- do some DMLs
connect sqst/sqst
set serveroutput on
update customer set zip = 0;
update order_line set i_id = 101 where ol_id < 800;
commit;

-- wait for streams to finish
set timing on
exec verify_tables('orders_v','orders_mv');
exec verify_tables('oline_v','oline_mv');
exec verify_tables('customer','customer_mv');
set timing off

-- 0 rows expected
select o_id, c_id, ol_num from orders_mv order by o_id;
select ol_id, o_id, i_id from oline_mv order by ol_id;

update customer set zip = 19555 where c_id > 5;
update order_line set i_id = 61 where ol_id >= 800;
-- update primary key
update order_line set ol_id = 599 where ol_id = 522;
commit;

-- wait for streams to finish
set timing on
exec verify_tables('orders_v','orders_mv');
exec verify_tables('oline_v','oline_mv');
exec verify_tables('customer','customer_mv');
set timing off

select o_id, c_id, ol_num from orders_mv order by o_id;
select ol_id, o_id, i_id from oline_mv order by ol_id;

update customer set zip = -19555 where zip > 19455;
commit;

-- wait for streams to finish
set timing on
exec verify_tables('orders_v','orders_mv');
exec verify_tables('oline_v','oline_mv');
exec verify_tables('customer','customer_mv');
set timing off

--0 rows
select o_id, c_id, ol_num from orders_mv order by o_id;
--0 rows
select ol_id, o_id, i_id from oline_mv order by ol_id;


update customer set zip = 19556 where zip =  -19555;
commit;

-- wait for streams to finish
set timing on
exec verify_tables('orders_v','orders_mv');
exec verify_tables('oline_v','oline_mv');
exec verify_tables('customer','customer_mv');
set timing off

select o_id, c_id, ol_num from orders_mv order by o_id;
select ol_id, o_id, i_id from oline_mv order by ol_id;

delete from order_line where o_id = 71;
delete from orders where c_id = 6;
delete from customer where c_id = 9;
commit;

-- wait for streams to finish
set timing on
exec verify_tables('orders_v','orders_mv');
exec verify_tables('oline_v','oline_mv');
exec verify_tables('customer','customer_mv');
set timing off

select o_id, c_id, ol_num from orders_mv order by o_id;
select ol_id, o_id, i_id from oline_mv order by ol_id;

insert into customer (c_id, zip) values (101, 29555);
insert into orders (o_id, c_id, ol_num) values (1001, 101, 11);
insert into orders (o_id, c_id, ol_num) values (1002, 101, 11);
delete from orders where o_id < 22;
update orders set c_id = 7 where c_id  = 5;
update order_line set o_id = 2 where o_id = 82;
commit;

-- wait for streams to finish
set timing on
exec verify_tables('orders_v','orders_mv');
exec verify_tables('oline_v','oline_mv');
exec verify_tables('customer','customer_mv');
set timing off

select o_id, c_id, ol_num from orders_mv order by o_id;
select ol_id, o_id, i_id from oline_mv order by ol_id;

-- Cleanup
connect stradm/stradm

-- FAIL: apply must be stopped before you can unregister an MV
begin
  streams_mv_refresh_adm.unregister_mv(
    mv_owner=>'sqst', mv_name=>'"OLINE_MV"', 
    capture_name=>'CAPTURE', apply_name=>'apply_mv_master');
end;
/

exec dbms_apply_adm.stop_apply('APPLY_MV_MASTER');

-- unregister one MV
begin
  streams_mv_refresh_adm.unregister_mv(
    mv_owner=>'sqst', mv_name=>'"OLINE_MV"', 
    capture_name=>'CAPTURE', apply_name=>'apply_mv_master');
end;
/

select streams_type, streams_name, table_owner, table_name, 
  rule_owner, rule_name
from dba_streams_table_rules
where streams_name in ('CAPTURE', 'APPLY_MV_MASTER')
order by streams_type, streams_name, table_owner, table_name;

select source_object_owner, source_object_name, source_database, 
       instantiation_scn
from dba_apply_instantiated_objects 
where source_object_owner='SQST'
order by source_database, source_object_owner, source_object_name;

select object_owner, object_name, operation_name, user_procedure
from dba_apply_dml_handlers
where apply_name = 'APPLY_MV_MASTER'
order by object_owner, object_name, operation_name;

select mv_owner, mv_name from stmv_reg order by mv_owner, mv_name;

-- unregister the remaining MVs
begin
  streams_mv_refresh_adm.unregister_mv(
    mv_owner=>'SQST', mv_name=>'ORDERS_MV', 
    capture_name=>'CAPTURE', apply_name=>'APPLY_MV_MASTER');
end;
/

begin
  streams_mv_refresh_adm.unregister_mv(
    mv_owner=>'sqst', mv_name=>'customer_mv', 
    capture_name=>'capture', apply_name=>'apply_mv_master');
end;
/

select streams_type, streams_name, table_owner, table_name, 
  rule_owner, rule_name
from dba_streams_table_rules
where streams_name in ('CAPTURE', 'APPLY_MV_MASTER')
order by streams_type, streams_name, table_owner, table_name;

select source_object_owner, source_object_name, source_database, 
       instantiation_scn
from dba_apply_instantiated_objects 
where source_object_owner='SQST'
order by source_database, source_object_owner, source_object_name;

select object_owner, object_name, operation_name, user_procedure
from dba_apply_dml_handlers
where apply_name = 'APPLY_MV_MASTER'
order by object_owner, object_name, operation_name;

select mv_owner, mv_name from stmv_reg order by mv_owner, mv_name;

-- remove streams configuration
begin
  streams_mv_refresh_adm.remove_streams_mv_refresh(
     'CAPTURE', 'apply_mv_master');
end;
/

select 1 from dba_capture where capture_name = 'CAPTURE';
select 1 from dba_apply where apply_name = 'APPLY_MV_MASTER';

select streams_type, streams_name, table_owner, table_name, 
  rule_owner, rule_name
from dba_streams_table_rules
where streams_name in ('CAPTURE', 'APPLY_MV_MASTER');

select source_object_owner, source_object_name, source_database,
       instantiation_scn
from dba_apply_instantiated_objects 
where source_object_owner='SQST'
order by source_database, source_object_owner, source_object_name;

select object_owner, object_name, operation_name, user_procedure
from dba_apply_dml_handlers
where apply_name = 'APPLY_MV_MASTER'
order by object_owner, object_name, operation_name;

-- check mvs
select name, refresh_method, type, refresh_mode 
from dba_snapshots where owner = 'SQST' order by name;

-- cleanup
connect system/manager
drop user stradm cascade;
drop user sqst cascade;
