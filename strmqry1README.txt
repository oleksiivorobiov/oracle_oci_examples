/
/ $Header: strmqry1README.txt 08-sep-2006.14:04:24 wesmith Exp $
/
/ strmqry1README.txt
/
/ Copyright (c) 2006, Oracle. All Rights Reserved.
/
/   NAME
/     strmqry1README.txt - <one-line expansion of the name>
/
/   DESCRIPTION
/     <short description of component this file declares/defines>
/
/   NOTES
/     <other useful comments, qualifications, etc.>
/
/   MODIFIED   (MM/DD/YY)
/   wesmith     06/23/06 - register_query: add parameter queue_name
/   wesmith     01/24/06 - Creation
/

register_query
--------------
for streams-based refresh of arbitrary queries that may not be 
fast-refreshable as an MV.


APIs
----

-- sets up a query for streams-driven refresh
procedure register_query(query_owner         varchar2, 
                         query_name          varchar2, 
                         base_tables         stq_table_list,
                         capture_name        varchar2 := 'CAPTURE_QRY_MASTER',
                         apply_name          varchar2 := 'APPLY_QRY_MASTER',
                         queue_name          varchar2 := NULL) 



Parameters
----------
query_owner, query_name: query to register (actually the container table
  for the query, which must exist)

base_tables: collection of base tables referenced in the query, along with
  the queries required to incrementally maintain the query with respect
  to each base table. This parameter is of type stq_table_list which is
  a nested table of the type stq_table_t:

create or replace type stq_table_list is table of stq_table_t
/

create or replace type stq_table_t as object (
  table_owner    varchar2(30),                           /* base table owner */
  table_name     varchar2(30),                            /* base table name */
  refresh_query  varchar2(4000),     /* query used by the pre-commit handler */
  commit_query   varchar2(4000)      /* query used by the pre-commit handler */
)
/

For each base table in the query, create a stq_table_t element. 
refresh_query and commit_query will be used by the pre-commit handler to 
incrementally maintain the query.

capture_name: 
  name of the capture process to use. It is recommended to specify a capture
  that does not already exist.

apply_name: 
  name of the apply process to use. It is recommended to specify an apply
  that does not already exist.

queue_name:
  name of the streams queue associated with the capture and apply process.
  If NULL, then the queue_name is derived from the capture process
  metadata, if it exists.


Notes:

- base_tables collection element:
refresh_query and commit_query are both derived from the orignal query.
Both should have a flashback expression, 'as of scn(:scn)', for each table 
in the original query. 

In addition, refresh_query should contain an additional rowid match
predicate 'and <table_owner>.<table_name>.rowid = :rid', where 
<table_owner>.<table_name> is the current base table. In some cases, 
the base table must be added again to the original query in order to 
match the rowid (ex: query with aggregates).

refresh_query is used to incrementally maintain the registered query 
on a per-LCR basis.

commit_query is used to incrementally maintain the registered query
on a per-transaction basis. A commit_query is required for certain
types of queries that cannot be properly maintained with just 
a refresh_query. For example, a query with outer joins will need to
delete anti-join rows no longer in the query, and/or insert anti-join rows 
that are new to the query. These queries require a 'rowid is null' 
predicate instead of a rowid match predicate.

- refresh algorithm
If a user transaction modifies a table that a registered query references,
then for each LCR for that table, the pre-commit handler constructs the 
following queries:

a.  refresh_query (binds: scn=<commit_scn>-1, rid=<LCR rowid>) 
    minus
    refresh_query (binds: scn=<commit_scn>, rid=<LCR rowid>)

These are rows that no longer exist in the query. DELETE LCRs will be 
constructed and executed on the query table.

b.  refresh_query (binds: scn=<commit_scn>, rid=<LCR rowid>) 
    minus
    refresh_query (binds: scn=<commit_scn>-1, rid=<LCR rowid>)

These are rows that are new to the query. UPDATE LCRs will be 
constructed and executed (upsert performed) on the query table.

If a commit_query was registered for a table that was modified in the 
transaction, the pre-commit handler constructs the following
queries:

a.  commit_query (binds: scn=<commit_scn>-1) 
    minus
    commit_query (binds: scn=<commit_scn>)

These are rows that no longer exist in the query. DELETE LCRs will be 
constructed and executed on the query table.

b.  commit_query (binds: scn=<commit_scn>) 
    minus
    commit_query (binds: scn=<commit_scn>-1)

These are rows that are new to the query. UPDATE LCRs will be 
constructed and executed (upsert performed) on the query table.


Examples:

1. subquery 
create table customer(
  c_id  number primary key,
  zip   number,
  c_name varchar(30));

create table orders(
  o_id  number primary key,
  c_id  number,
  ol_num number default 0);

-- query table
create table orders_mv as
select o.o_id, o.c_id, o.ol_num from sqst.orders o
  where exists
  (select c.c_id from sqst.customer c
   where c.zip >= 19555 and o.c_id = c.c_id);

alter table orders_mv add primary key (o_id);

begin
  streams_qry_refresh_adm.register_query(
    query_owner=>'SQST', query_name=>'ORDERS_MV', 
    capture_name=>'CAPTURE', apply_name=>'APPLY_MV_MASTER',
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

Notes:
- no commit_query needed for this query


2. join 

-- query table
create table orders_mv2 as
select o.o_id, o.c_id, o.ol_num, o.rowid o_rid, c.zip, c.rowid c_rid 
from sqst.orders o, sqst.customer c
where o.c_id = c.c_id(+) and c.zip(+) >= 19555;

-- need set_key_columns for this query table since there is no pk
exec dbms_apply_adm.set_key_columns('SQST.ORDERS_MV2', 'o_rid, c_rid');

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
     and o.rowid = :rid', 
    'select o.o_id, o.c_id, o.ol_num, o.rowid o_rid, c.zip, c.rowid c_rid 
     from sqst.orders as of scn(:scn) o, sqst.customer as of scn(:scn) c
     where o.c_id = c.c_id(+) and c.zip(+) >= 19555
     and o.rowid is null'),
  stq_table_t('SQST', 'customer',
    'select o.o_id, o.c_id, o.ol_num, o.rowid o_rid, c.zip, c.rowid c_rid 
     from sqst.orders as of scn(:scn) o, sqst.customer as of scn(:scn) c
     where o.c_id = c.c_id(+) and c.zip(+) >= 19555
     and c.rowid = :rid', 
    'select o.o_id, o.c_id, o.ol_num, o.rowid o_rid, c.zip, c.rowid c_rid 
     from sqst.orders as of scn(:scn) o, sqst.customer as of scn(:scn) c
     where o.c_id = c.c_id(+) and c.zip(+) >= 19555
     and c.rowid is null')));
end;
/

Notes:
- a commit query is required for this join query with outer joins to 
  deal with new/old antijoin rows in the query. For example, if there is
  no customer row with matching c_id for an order row, there will be an
  antijoin row for that o_id. If a row with matching c_id is now inserted
  into customer, the antijoin row must be replaced with a join row. 
  Since c_rid is null for the antijoin row, it cannot be handled by
  refresh_query (which uses rowid match predicate) and therefore must be
  done by commit_query.


3. aggregate
create table sales (region varchar2(10), amt number);

-- query table
create table sales_region_mv as
select s.region, sum(s.amt) sales
from sqst.sales s
group by s.region;

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

Notes:
- another join of sales is required to add a rowid match predicate, 
  where the join clause is on the grouping columns for sales (region)
- no additional aggregates is required (count(*), count(sales)) as would
  be if an MV is created using the query.
- no commit_query is required for this query


