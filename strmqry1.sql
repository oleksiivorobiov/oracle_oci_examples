Rem
Rem $Header: rdbms/demo/strmqry1.sql /main/6 2009/06/30 03:03:52 snalla Exp $
Rem
Rem strmqry1.sql
Rem
Rem Copyright (c) 2006, 2009, Oracle and/or its affiliates. 
Rem All rights reserved. 
Rem
Rem    NAME
Rem      strmqry1.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem      <short description of component this file declares/defines>
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    snalla      06/23/09 - fix ORA-01940: cannot drop a user that is
Rem                           currently connected
Rem    davzhang    09/27/06 - grant create view explicitly
Rem    wesmith     06/23/06 - register_query: add parameter queue_name
Rem    wesmith     06/12/06 - move include_extra_attribute() call
Rem    liwong      05/15/06 - sync capture cleanup 
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

-- create user for queries
drop user sqst cascade;
grant connect, resource to sqst identified by sqst;
grant create materialized view, create view to sqst;

connect sqst/sqst

-- create base tables and load
set echo off
@strmmv2s.sql
set echo on

-- subquery (1 level)
create view orders_v  (o_id, c_id, ol_num) as
select o.o_id, o.c_id, o.ol_num from sqst.orders o
  where exists
  (select c.c_id from sqst.customer c
   where c.zip >= 19555 and o.c_id = c.c_id);

create table orders_mv as
select o.o_id, o.c_id, o.ol_num from sqst.orders o
  where exists
  (select c.c_id from sqst.customer c
   where c.zip >= 19555 and o.c_id = c.c_id);

alter table orders_mv add primary key (o_id);

-- another subquery (2-levels)
create view oline_v (ol_id, o_id, i_id) as
  select ol.ol_id, ol.o_id, ol.i_id from sqst.order_line ol
  where exists
  (select o.o_id from sqst.orders o
   where ol.o_id = o.o_id 
   and exists
   (select c.c_id from sqst.customer c
    where c.zip >= 19555 and o.c_id = c.c_id));

create table oline_mv as
  select ol.ol_id, ol.o_id, ol.i_id from sqst.order_line ol
  where exists
  (select o.o_id from sqst.orders o
   where ol.o_id = o.o_id 
   and exists
   (select c.c_id from sqst.customer c
    where c.zip >= 19555 and o.c_id = c.c_id));

alter table oline_mv add primary key (ol_id);

-- join view
create view orders_v2 as
select o.o_id, o.c_id, o.ol_num, o.rowid o_rid, c.zip, c.rowid c_rid 
from sqst.orders o, sqst.customer c
where o.c_id = c.c_id(+) and c.zip(+) >= 19555;

create table orders_mv2 as
select o.o_id, o.c_id, o.ol_num, o.rowid o_rid, c.zip, c.rowid c_rid 
from sqst.orders o, sqst.customer c
where o.c_id = c.c_id(+) and c.zip(+) >= 19555;

-- aggregate 
create view sales_region_v as
select s.region, sum(s.amt) sales
from sqst.sales s
group by s.region;

create table sales_region_mv as
select s.region, sum(s.amt) sales
from sqst.sales s
group by s.region;


-- create supporting objects and packages for streams refresh metadata
connect stradm/stradm
set echo off
@strmmvp2
@strmqp1
set echo on

--
-- streams setup
--
begin
  dbms_streams_adm.set_up_queue 
      (queue_user => 'stradm', queue_name => 'QRY_MASTER_Q',
       queue_table => 'QRY_MASTER_QT');
end;
/

begin
  streams_qry_refresh_adm.register_query(
    query_owner=>'SQST', query_name=>'ORDERS_MV', 
    capture_name=>'CAPTURE', apply_name=>'APPLY_MV_MASTER',
    queue_name=>'QRY_MASTER_Q',
    base_tables=>
stq_table_list(
  stq_table_t('SQST', 'ORDERS',
    'select o.o_id, o.c_id, o.ol_num from sqst.orders as of scn(:scn) o
     where exists
     (select c.c_id from sqst.customer as of scn(:scn) c
      where c.zip >= 19555 and o.c_id = c.c_id)
     and o.rowid = :rid', null),
  stq_table_t('SQST', 'customer',
    'select o.o_id, o.c_id, o.ol_num from sqst.orders as of scn(:scn) o
     where exists
     (select c.c_id from sqst.customer as of scn(:scn) c
      where c.zip >= 19555 
      and o.c_id = c.c_id
      and c.rowid = :rid)', null)));
end;
/

-- need set_key_columns for this query table since there is no pk
exec dbms_apply_adm.set_key_columns('SQST.ORDERS_MV2', 'o_rid, c_rid');

-- register queries for streams refresh
begin
  streams_qry_refresh_adm.register_query(
    query_owner=>'SQST', query_name=>'ORDERS_MV2', 
    capture_name=>'CAPTURE', apply_name=>'APPLY_MV_MASTER',
    base_tables=>
stq_table_list(
  stq_table_t('SQST', 'ORDERS',
    'select o.o_id, o.c_id, o.ol_num, o.rowid o_rid, c.zip, c.rowid c_rid 
     from sqst.orders as of scn(:scn) o, sqst.customer as of scn(:scn) c
     where o.c_id = c.c_id(+) and c.zip(+) >= 19555
     and o.rowid = :rid', null),
  stq_table_t('SQST', 'customer',
    'select o.o_id, o.c_id, o.ol_num, o.rowid o_rid, c.zip, c.rowid c_rid 
     from sqst.orders as of scn(:scn) o, sqst.customer as of scn(:scn) c
     where o.c_id = c.c_id(+) and c.zip(+) >= 19555
     and c.rowid = :rid', 
    'select o.o_id, o.c_id, o.ol_num, o.rowid o_rid, c.zip, c.rowid c_rid 
     from sqst.orders as of scn(:scn) o, sqst.customer as of scn(:scn) c
     where not exists (select 1 from sqst.customer as of scn(:scn) c
                       where o.c_id = c.c_id and c.zip >= 19555)
     and o.c_id = c.c_id(+) and c.zip(+) >= 19555
     and c.rowid is null')));
end;
/

begin
  streams_qry_refresh_adm.register_query(
    query_owner=>'SQST', query_name=>'OLINE_MV', 
    capture_name=>'CAPTURE', apply_name=>'APPLY_MV_MASTER',
    base_tables=>
stq_table_list(
  stq_table_t('SQST', 'ORDER_LINE',
    'select ol.ol_id, ol.o_id, ol.i_id from sqst.order_line as of scn(:scn) ol
     where exists
     (select o.o_id from sqst.orders as of scn(:scn) o
      where ol.o_id = o.o_id 
      and exists
      (select c.c_id from sqst.customer as of scn(:scn) c
       where c.zip >= 19555 and o.c_id = c.c_id))
     and ol.rowid = :rid', null),
  stq_table_t('SQST', 'orders',
    'select ol.ol_id, ol.o_id, ol.i_id from sqst.order_line as of scn(:scn) ol
     where exists
     (select o.o_id from sqst.orders as of scn(:scn) o
      where ol.o_id = o.o_id 
      and exists
      (select c.c_id from sqst.customer as of scn(:scn) c
       where c.zip >= 19555 and o.c_id = c.c_id)
      and o.rowid = :rid)', null),
  stq_table_t('SQST', 'CUSTOMER',
    'select ol.ol_id, ol.o_id, ol.i_id from sqst.order_line as of scn(:scn) ol
     where exists
     (select o.o_id from sqst.orders as of scn(:scn) o
      where ol.o_id = o.o_id 
      and exists
      (select c.c_id from sqst.customer as of scn(:scn) c
       where c.zip >= 19555 
       and o.c_id = c.c_id
       and c.rowid = :rid))', null)));
end;
/

-- need set_key_columns for this query table since there is no pk
exec dbms_apply_adm.set_key_columns('SQST.sales_region_mv', 'region');

begin
  streams_qry_refresh_adm.register_query(
    query_owner=>'SQST', query_name=>'sales_region_mv', 
    capture_name=>'CAPTURE', apply_name=>'APPLY_MV_MASTER',
    base_tables=>
stq_table_list(
  stq_table_t('SQST', 'sales',
    'select s.region, sum(s.amt) sales
     from sqst.sales as of scn(:scn) s, sqst.sales as of scn(:scn) s2
     where s.region = s2.region and s2.rowid = :rid
     group by s.region',
    null)));
end;
/


connect stradm/stradm
exec dbms_capture_adm.start_capture('CAPTURE');

exec dbms_apply_adm.set_parameter('APPLY_MV_MASTER','DISABLE_ON_ERROR','N');

-- assign precommit handler to apply process 
begin
  dbms_apply_adm.alter_apply(
    apply_name        => 'APPLY_MV_MASTER',
    precommit_handler => 'streams_qry_refresh.source_commit_hdlr');
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
order by source_database, source_object_owner, source_object_name;

select object_owner, object_name, operation_name, user_procedure
from dba_apply_dml_handlers
where apply_name = 'APPLY_MV_MASTER'
order by object_owner, object_name, operation_name;

-- check query registration metadata
select query_owner, query_name from stq_reg order by query_owner, query_name;

-- do some DMLs
connect sqst/sqst
set serveroutput on
update customer set zip = 0;
update order_line set i_id = 101 where ol_id < 800;
insert into sales values('West', 100);
insert into sales values('East', 50);
commit;

-- wait for streams to finish
set timing on
exec verify_tables('orders_v','orders_mv');
exec verify_tables('orders_v2','orders_mv2');
exec verify_tables('oline_v','oline_mv');
exec verify_tables('sales_region_v','sales_region_mv');
set timing off

-- 0 rows expected
select o_id, c_id, ol_num from orders_mv order by o_id;
select ol_id, o_id, i_id from oline_mv order by ol_id;

select region, sales from sales_region_mv order by region;

update customer set zip = 19555 where c_id > 5;
update order_line set i_id = 61 where ol_id >= 800;
-- update primary key
update order_line set ol_id = 599 where ol_id = 522;
commit;
update sales set amt=null where region='West' and amt = 100;
insert into sales values('East', 150);
commit;

-- wait for streams to finish
set timing on
exec verify_tables('orders_v','orders_mv');
exec verify_tables('orders_v2','orders_mv2');
exec verify_tables('oline_v','oline_mv');
exec verify_tables('sales_region_v','sales_region_mv');
set timing off

select o_id, c_id, ol_num from orders_mv order by o_id;
select ol_id, o_id, i_id from oline_mv order by ol_id;
select region, sales from sales_region_mv order by region;

update customer set zip = -19555 where zip > 19455;
commit;
update sales set amt=250 where region='East' and amt=150;
commit;

-- wait for streams to finish
set timing on
exec verify_tables('orders_v','orders_mv');
exec verify_tables('orders_v2','orders_mv2');
exec verify_tables('oline_v','oline_mv');
exec verify_tables('sales_region_v','sales_region_mv');
set timing off

--0 rows
select o_id, c_id, ol_num from orders_mv order by o_id;
--0 rows
select ol_id, o_id, i_id from oline_mv order by ol_id;

select region, sales from sales_region_mv order by region;

update customer set zip = 19556 where zip =  -19555;
commit;
delete from sales;
commit;

-- wait for streams to finish
set timing on
exec verify_tables('orders_v','orders_mv');
exec verify_tables('orders_v2','orders_mv2');
exec verify_tables('oline_v','oline_mv');
exec verify_tables('sales_region_v','sales_region_mv');
set timing off

select o_id, c_id, ol_num from orders_mv order by o_id;
select ol_id, o_id, i_id from oline_mv order by ol_id;
select region, sales from sales_region_mv order by region;

delete from order_line where o_id = 71;
delete from orders where c_id = 6;
delete from customer where c_id = 9;
commit;

-- wait for streams to finish
set timing on
exec verify_tables('orders_v','orders_mv');
exec verify_tables('orders_v2','orders_mv2');
exec verify_tables('oline_v','oline_mv');
set timing off

select o_id, c_id, ol_num from orders_mv order by o_id;
select ol_id, o_id, i_id from oline_mv order by ol_id;
select region, sales from sales_region_mv order by region;

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
exec verify_tables('orders_v2','orders_mv2');
exec verify_tables('oline_v','oline_mv');
set timing off

select o_id, c_id, ol_num from orders_mv order by o_id;
select ol_id, o_id, i_id from oline_mv order by ol_id;
select region, sales from sales_region_mv order by region;

-- Cleanup
connect stradm/stradm
exec dbms_apply_adm.stop_apply('APPLY_MV_MASTER');

-- unregister one query
begin
  streams_qry_refresh_adm.unregister_query(
    query_owner=>'SQST', query_name=>'OLINE_MV', 
    capture_name=>'CAPTURE', apply_name=>'APPLY_MV_MASTER');
end;
/

-- check streams metadata
select streams_type, streams_name, table_owner, table_name, 
  rule_owner, rule_name
from dba_streams_table_rules
where streams_name in ('CAPTURE', 'APPLY_MV_MASTER')
order by streams_type, streams_name, table_owner, table_name;

select source_object_owner, source_object_name, source_database, 
       instantiation_scn
from dba_apply_instantiated_objects 
order by source_database, source_object_owner, source_object_name;

select object_owner, object_name, operation_name, user_procedure
from dba_apply_dml_handlers
where apply_name = 'APPLY_MV_MASTER'
order by object_owner, object_name, operation_name;

select query_owner, query_name from stq_reg order by query_owner, query_name;

-- unregister more queries
begin
  streams_qry_refresh_adm.unregister_query(
    query_owner=>'SQST', query_name=>'ORDERS_MV', 
    capture_name=>'CAPTURE', apply_name=>'APPLY_MV_MASTER');
end;
/
begin
  streams_qry_refresh_adm.unregister_query(
    query_owner=>'SQST', query_name=>'ORDERS_MV2', 
    capture_name=>'CAPTURE', apply_name=>'APPLY_MV_MASTER');
end;
/
begin
  streams_qry_refresh_adm.unregister_query(
    query_owner=>'SQST', query_name=>'sales_region_mv', 
    capture_name=>'CAPTURE', apply_name=>'APPLY_MV_MASTER');
end;
/

-- check streams metadata
select streams_type, streams_name, table_owner, table_name, 
  rule_owner, rule_name
from dba_streams_table_rules
where streams_name in ('CAPTURE', 'APPLY_MV_MASTER')
order by streams_type, streams_name, table_owner, table_name;

select source_object_owner, source_object_name, source_database, 
       instantiation_scn
from dba_apply_instantiated_objects 
order by source_database, source_object_owner, source_object_name;

select object_owner, object_name, operation_name, user_procedure
from dba_apply_dml_handlers
where apply_name = 'APPLY_MV_MASTER'
order by object_owner, object_name, operation_name;

select query_owner, query_name from stq_reg order by query_owner, query_name;

-- remove streams configuration
begin
  streams_qry_refresh_adm.remove_streams_qry_refresh(
     'CAPTURE', 'APPLY_MV_MASTER');
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
order by source_database, source_object_owner, source_object_name;

select object_owner, object_name, operation_name, user_procedure
from dba_apply_dml_handlers
where apply_name = 'APPLY_MV_MASTER'
order by object_owner, object_name, operation_name;

-- cleanup
connect system/manager
set serveroutput on
exec wait_session ('stradm');
drop user stradm cascade ;
exec wait_session ('sqst');
drop user sqst cascade ;
