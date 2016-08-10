rem
rem $Header: extdemo5.sql 30-apr-2001.16:05:05 hdnguyen Exp $
rem
rem extdemo5.sql
rem
rem Copyright (c) Oracle Corporation 2001. All Rights Reserved.
rem
rem    NAME
rem      extdemo5.sql - An extensible indexing example implemented 
rem                     as C routines for local domain index on 
rem                     varchar2 column of a range partitioned table.
rem
rem    DESCRIPTION
rem      This file demonstrates the definition and usage of a simple
rem      btree indextype whose routines are implemented as C callouts.
rem      The C routines are in the file extdemo5.c
rem      The header file is extdemo5.h
rem
rem      The following steps should have been done before running
rem      this script.
rem      1. Compile the C file (i.e make -f demo_rdbms.mk demos)
rem      2. Create a user named extdemo5 with password extdemo5
rem           with all the necessary privileges. 
rem      3. Create a library in extdemo5 schema called extdemo5l
rem         which points to the compiled extdemo5.c
rem
rem      The design of the indextype is as follows :
rem
rem      The sbtree indextype implemented here will support the evaluation
rem      of three user-defined operators : gt(Greater Than), lt(Less Than)
rem      and eq(EQuals). These operators can operate on the operands of
rem      VARCHAR2 datatype.
rem      To simplify the implementation of the indextype, we will store
rem      the index data in a regular table.
rem      Thus, our code merely translates operations on the SB-tree into
rem      operations on the table storing the index data.
rem      When a user creates a SB-tree index, we will create a table
rem      consisting of the indexed column and a rowid column. Inserts into
rem      the base table will cause appropriate insertions into the index table.
rem      Deletes and updates are handled similarly.
rem      When the SB-tree is queried based on a user-defined operator (one
rem      of gt, lt and eq), we will fire off an appropriate query against
rem      the index table to retrieve all the satisfying rows and return them.
rem
rem    NOTE: 
rem      The database has to be at compatible = 9.0.0 or above
rem
rem    MODIFIED   (MM/DD/YY)
rem    hdnguyen    04/30/01 - modified MergePartition.
rem    hdnguyen    04/27/01 - misc. modification
rem    spsundar    04/25/01 - Created

---------------------------------------------------------------------
--    SIMPLE B-TREE Index Method  Implemented as C Callouts  --
---------------------------------------------------------------------

connect extdemo5/extdemo5
set echo off
@'?/rdbms/admin/utlxplan.sql'
set echo on

-- CREATE FUNCTIONAL IMPLEMENTATIONS for operators

create function bt_eq(a varchar2, b varchar2) return number as
begin
  if a = b then
    return 1;
  else
    return 0;
  end if;
end;
/
show errors

create function bt_lt(a varchar2, b varchar2) return number as
begin
  if a < b then
    return 1;
  else
   return 0;
  end if;
end;
/
show errors

create function bt_gt(a varchar2, b varchar2) return number as
begin
  if a > b then
    return 1;
  else
    return 0;
  end if;
end;
/
show errors

-- CREATE BTREE OPERATORS

create operator eq binding (varchar2, varchar2) 
  return number using bt_eq;

create operator lt binding (varchar2, varchar2) 
  return number using bt_lt;

create operator gt binding (varchar2, varchar2) 
  return number using bt_gt;


-- CREATE INDEXTYPE IMPLEMENTATION TYPE
create type psbtree_im as object
(
  scanctx RAW(4),
  static function ODCIGetInterfaces(ifclist OUT sys.ODCIObjectList)
    return NUMBER,

  static function ODCIIndexCreate (ia sys.ODCIIndexInfo, 
    parms varchar2, env sys.ODCIEnv) return NUMBER, 

  static function ODCIIndexDrop(ia sys.ODCIIndexInfo, 
    env sys.ODCIEnv) return NUMBER,

  static function ODCIIndexInsert(ia sys.ODCIIndexInfo, rid VARCHAR2,
    newval VARCHAR2, env sys.ODCIEnv) return NUMBER, 

  static function ODCIIndexDelete(ia sys.ODCIIndexInfo, rid VARCHAR2,
    oldval VARCHAR2, env sys.ODCIEnv) return NUMBER, 

  static function ODCIIndexUpdate(ia sys.ODCIIndexInfo, rid VARCHAR2,
    oldval VARCHAR2, newval VARCHAR2, env sys.ODCIEnv)  
    return NUMBER,  

  static function ODCIIndexTruncate(ia sys.ODCIIndexInfo,
    env sys.ODCIEnv) return NUMBER,

  static function ODCIIndexExchangePartition (ia sys.ODCIIndexInfo,
    ia1 sys.ODCIIndexInfo, env sys.ODCIEnv) return NUMBER,

  static function ODCIIndexMergePartition (ia sys.ODCIIndexInfo,
    part_name1 sys.ODCIPartInfo, part_name2 sys.ODCIPartInfo,
    parms varchar2, env sys.ODCIEnv) return NUMBER, 

  static function ODCIIndexSplitPartition (ia sys.ODCIIndexInfo,
    part_name1 sys.ODCIPartInfo, part_name2 sys.ODCIPartInfo,
    parms varchar2, env sys.ODCIEnv) return NUMBER,

  static function ODCIIndexStart(sctx IN OUT psbtree_im, 
    ia sys.ODCIIndexInfo, op sys.ODCIPredInfo,  
    qi sys.ODCIQueryInfo, strt number, stop number, 
    cmpval varchar2, env sys.ODCIEnv) 
    return NUMBER,

  member function ODCIIndexFetch(nrows number, 
    rids OUT sys.ODCIRidList, env sys.ODCIEnv)  
    return NUMBER,

  member function ODCIIndexClose(env sys.ODCIEnv) return NUMBER
);
/
show errors

---------------------------------
--  CREATE IMPLEMENTATION UNIT --
---------------------------------

-- CREATE TYPE BODY
create or replace type body psbtree_im
is
  static function ODCIGetInterfaces(ifclist OUT sys.ODCIObjectList)
    return number is
  begin
    ifclist := sys.ODCIObjectList(sys.ODCIObject('SYS','ODCIINDEX2'));
    return ODCIConst.Success;
  end ODCIGetInterfaces;

  static function ODCIIndexCreate (ia sys.ODCIIndexInfo, 
    parms varchar2, env sys.ODCIEnv) return number  
  is
   i integer;
   stmt varchar2(1000);
   cnum integer;
   junk integer;
  begin
   stmt := '';

   if ((env.CallProperty is null) and (ia.IndexPartition is null ))  then
     stmt := 'create table ' ||ia.IndexSchema || '.' || ia.IndexName ||
             '_sbtree(f1, f2) as select '  ||
             ia.IndexCols(1).Colname || ', ROWID from ' ||
             ia.IndexCols(1).TableSchema || '.' || 
             ia.IndexCols(1).TableName;
   end if;

   if ((env.CallProperty is not null) and (ia.IndexPartition is not null)) then
     stmt := 'create table ' || ia.IndexSchema || '.' || ia.IndexName ||
             '_' || ia.IndexPartition || '_sbtree'  ||
             '(f1, f2) as select ' ||
             ia.IndexCols(1).Colname || ', ROWID from ' ||
             ia.IndexCols(1).TableSchema || '.' ||
             ia.IndexCols(1).TableName || ' partition (' ||
             ia.IndexCols(1).tablepartition || ')';
   end if;

   if ((env.CallProperty is null) and (ia.IndexPartition is not null)) then
     stmt := 'create table ' || ia.IndexSchema || '.' || ia.IndexName ||
             '_' || ia.IndexPartition || '_sbtree'  ||
             '(f1, f2) as select ' ||
             ia.IndexCols(1).Colname || ', ROWID from ' ||
             ia.IndexCols(1).TableSchema || '.' ||
             ia.IndexCols(1).TableName || ' partition (' ||
             ia.IndexCols(1).TablePartition || ')';
   end if;
 
   dbms_output.put_line('Create');
   dbms_output.put_line(stmt);

   -- execute the statement
   if ( (env.CallProperty is null) or
        (env.CallProperty = sys.ODCIConst.IntermediateCall) ) then
     cnum := dbms_sql.open_cursor;
     dbms_sql.parse(cnum, stmt, dbms_sql.native);
     junk := dbms_sql.execute(cnum);
     dbms_sql.close_cursor(cnum);
   end if;

   return ODCIConst.Success;
  end;

  static function ODCIIndexDrop(ia sys.ODCIIndexInfo, env sys.ODCIEnv) 
    return number is
   stmt varchar2(1000);
   cnum integer;
   junk integer;
  begin
    -- construct the sql statement
   stmt := '';
   if ((env.CallProperty is null) and (ia.IndexPartition is null) ) then
     stmt := 'drop table ' || ia.IndexSchema || '.' || ia.IndexName ||
             '_sbtree';
   else
     if (ia.IndexPartition is not null) then
       stmt := 'drop table ' || ia.IndexSchema || '.' || ia.IndexName ||
               '_' || ia.IndexPartition || '_sbtree';
     end if;
   end if;

   dbms_output.put_line('Drop');
   dbms_output.put_line(stmt);

   -- execute the statement
   if ( (env.CallProperty is null) or
        (env.CallProperty = sys.ODCIConst.IntermediateCall) ) then
     cnum := dbms_sql.open_cursor;
     dbms_sql.parse(cnum, stmt, dbms_sql.native);
     junk := dbms_sql.execute(cnum);
     dbms_sql.close_cursor(cnum);
   end if;

   return ODCIConst.Success;
  end;

  static function ODCIIndexTruncate(ia sys.ODCIIndexInfo, env sys.ODCIEnv)
    return number is
   stmt varchar2(2000);
   cnum integer;
   junk integer;
  begin
    -- construct the sql statement
   stmt := '';
   if ((env.CallProperty is null) and (ia.IndexPartition is null) ) then
     stmt := 'truncate table ' || ia.IndexSchema || '.' || ia.IndexName ||
             '_sbtree';
   else
     if (ia.IndexPartition is not null) then
       stmt := 'truncate table ' || ia.IndexSchema || '.' || ia.IndexName ||
               '_' || ia.IndexPartition || '_sbtree';
     end if;
   end if;

   dbms_output.put_line('Truncate');
   dbms_output.put_line(stmt);

   -- execute the statement
   if ( (env.CallProperty is null) or
        (env.CallProperty = sys.ODCIConst.IntermediateCall) ) then
     cnum := dbms_sql.open_cursor;
     dbms_sql.parse(cnum, stmt, dbms_sql.native);
     junk := dbms_sql.execute(cnum);
     dbms_sql.close_cursor(cnum);
   end if;

   return ODCIConst.Success;
  end;

  static function ODCIIndexExchangePartition(ia sys.ODCIIndexInfo,
    ia1 sys.ODCIIndexInfo, env sys.ODCIEnv)
   return number
  is
   stmt varchar2(2000);
   cnum integer;
   junk integer;
  begin
   stmt := '';
   dbms_output.put_line('Exchange Partitions');

    -- construct the sql statement
   stmt := 'alter table temp exchange partition p1 with table ' ||
        ia1.IndexSchema || '.' || ia1.IndexName || '_sbtree';
   cnum := dbms_sql.open_cursor;
   dbms_sql.parse(cnum, stmt, dbms_sql.native);
   junk := dbms_sql.execute(cnum);
   dbms_sql.close_cursor(cnum);

   stmt := 'alter table temp exchange partition p1 with table ' ||
         ia.IndexSchema || '.' || ia.IndexName ||
         '_' || ia.IndexPartition || '_sbtree';

   cnum := dbms_sql.open_cursor;
   dbms_sql.parse(cnum, stmt, dbms_sql.native);
   junk := dbms_sql.execute(cnum);
   dbms_sql.close_cursor(cnum);

   stmt := 'alter table temp exchange partition p1 with table ' ||
        ia1.IndexSchema || '.' || ia1.IndexName || '_sbtree';
   cnum := dbms_sql.open_cursor;
   dbms_sql.parse(cnum, stmt, dbms_sql.native);
   junk := dbms_sql.execute(cnum);
   dbms_sql.close_cursor(cnum);

   return ODCIConst.Success;
  end;

  static function ODCIIndexMergePartition(ia sys.ODCIIndexInfo,
    part_name1 sys.ODCIPartInfo, part_name2 sys.ODCIPartInfo,
    parms varchar2, env sys.ODCIEnv)
    return number
  is
   stmt varchar2(2000);
   cnum integer;
   junk integer;
  begin
   dbms_output.put_line('Merge Partitions');
   stmt := '';

   if ( part_name2 is not null) then
      stmt := 'create table ' || ia.IndexSchema || '.' || ia.IndexName ||
           '_' || part_name2.IndexPartition ||  '_sbtree'  ||
           '(f1 ' || ia.IndexCols(1).ColTypeName ||
           '(30), f2 ROWID)';

     dbms_output.put_line('create');
     dbms_output.put_line('Parameter string : ' || parms);

     dbms_output.put_line(stmt);

     -- execute the statement
     cnum := dbms_sql.open_cursor;
     dbms_sql.parse(cnum, stmt, dbms_sql.native);
     junk := dbms_sql.execute(cnum);
     dbms_sql.close_cursor(cnum);
   end if;

   if ( part_name1 is not null) then
      stmt := 'drop table ' || ia.IndexSchema || '.' || ia.IndexName ||
               '_' || part_name1.IndexPartition || '_sbtree';

      dbms_output.put_line('drop');
      dbms_output.put_line(stmt);

      -- execute the statement
      cnum := dbms_sql.open_cursor;
      dbms_sql.parse(cnum, stmt, dbms_sql.native);
      junk := dbms_sql.execute(cnum);
      dbms_sql.close_cursor(cnum);
   end if;

   if (ia.IndexPartition is not null) then
      stmt := 'drop table ' || ia.IndexSchema || '.' || ia.IndexName ||
               '_' || ia.IndexPartition || '_sbtree';

      dbms_output.put_line('drop');
      dbms_output.put_line(stmt);

      -- execute the statement
      cnum := dbms_sql.open_cursor;
      dbms_sql.parse(cnum, stmt, dbms_sql.native);
      junk := dbms_sql.execute(cnum);
      dbms_sql.close_cursor(cnum);
   end if;

   return ODCIConst.Success;
  end;

  static function ODCIIndexSplitPartition(ia sys.ODCIIndexInfo,
    part_name1 sys.ODCIPartInfo, part_name2 sys.ODCIPartInfo,
    parms varchar2, env sys.ODCIEnv)
    return number
  is
   stmt varchar2(2000);
   cnum integer;
   junk integer;
  begin
   dbms_output.put_line('Split Partition');
   stmt := '';

   if (ia.IndexPartition is not null) then
      stmt := 'drop table ' || ia.IndexSchema || '.' || ia.IndexName ||
               '_' || ia.IndexPartition || '_sbtree';

      dbms_output.put_line('drop');
      dbms_output.put_line(stmt);

      -- execute the statement
      cnum := dbms_sql.open_cursor;
      dbms_sql.parse(cnum, stmt, dbms_sql.native);
      junk := dbms_sql.execute(cnum);
      dbms_sql.close_cursor(cnum);
    end if;

   if ( part_name1 is not null) then
      stmt := 'create table ' || ia.IndexSchema || '.' || ia.IndexName ||
           '_' || part_name1.IndexPartition ||  '_sbtree'  ||
           '( f1 ' || ia.IndexCols(1).ColTypeName ||
           '(30), f2 ROWID)';

      dbms_output.put_line('create');
      dbms_output.put_line('Parameter string : ' || parms);
      dbms_output.put_line(stmt);

      -- execute the statement
      cnum := dbms_sql.open_cursor;
      dbms_sql.parse(cnum, stmt, dbms_sql.native);
      junk := dbms_sql.execute(cnum);
      dbms_sql.close_cursor(cnum);
    end if;

   if ( part_name2 is not null) then
      stmt := 'create table ' || ia.IndexSchema || '.' || ia.IndexName ||
           '_' || part_name2.IndexPartition ||  '_sbtree'  ||
           '( f1 ' || ia.IndexCols(1).ColTypeName ||
           '(30), f2 ROWID)';

      dbms_output.put_line('create');
      dbms_output.put_line('Parameter string : ' || parms);
      dbms_output.put_line(stmt);

      -- execute the statement
      cnum := dbms_sql.open_cursor;
      dbms_sql.parse(cnum, stmt, dbms_sql.native);
      junk := dbms_sql.execute(cnum);
      dbms_sql.close_cursor(cnum);
    end if;

    return ODCIConst.Success;
  end;

  static function ODCIIndexInsert(ia sys.ODCIIndexInfo, rid VARCHAR2,
    newval VARCHAR2, env sys.ODCIEnv)  
    RETURN NUMBER AS external
    name "qxiqtbpi"
    library extdemo5l
    WITH context
    parameters (
      context,
      ia,
      ia indicator struct,
      rid,
      rid indicator,
      newval,
      newval indicator,
      env,
      env indicator struct,
      return ocinumber
               );

  static function ODCIIndexDelete(ia sys.ODCIIndexInfo, rid VARCHAR2,
    oldval VARCHAR2, env sys.ODCIEnv)  
    RETURN NUMBER AS external
    name "qxiqtbpd"
    library extdemo5l
    WITH context
    parameters (
      context,
      ia,
      ia indicator struct,
      rid,
      rid indicator,
      oldval,
      oldval indicator,
      env,
      env indicator struct,
      return ocinumber
               );

  static function ODCIIndexUpdate(ia sys.ODCIIndexInfo, rid VARCHAR2,
    oldval VARCHAR2, newval VARCHAR2, env sys.ODCIEnv)  
    RETURN NUMBER AS external
    name "qxiqtbpu"
    library extdemo5l
    WITH context
    parameters (
      context,
      ia,
      ia indicator struct,
      rid,
      rid indicator,
      oldval,
      oldval indicator,
      newval,
      newval indicator,
      env,
      env indicator struct,
      return ocinumber
               );

  static function ODCIIndexStart(sctx in out psbtree_im, 
    ia sys.ODCIIndexInfo, op sys.ODCIPredInfo,   
    qi sys.ODCIQueryInfo, strt number, stop number,  
    cmpval varchar2, env sys.ODCIEnv)  
    RETURN NUMBER AS external
    name "qxiqtbps"
    library extdemo5l
    WITH context
    parameters (
      context,
      sctx,
      sctx indicator struct,
      ia,
      ia indicator struct,
      op,
      op indicator struct,
      qi,
      qi indicator struct,
      strt,
      strt indicator,
      stop,
      stop indicator,
      cmpval,
      cmpval indicator,
      env,
      env indicator struct,
      return OCINumber
              );

  member function ODCIIndexFetch(nrows number, 
    rids OUT sys.ODCIRidList, env sys.ODCIEnv)  
   RETURN NUMBER AS external
   name "qxiqtbpf"
   library extdemo5l
   WITH context
   parameters (
     context,
     self,
     self indicator struct,
     nrows,
     nrows indicator,
     rids,
     rids indicator,
     env,
     env indicator struct,
     return OCINumber
             );

  member function ODCIIndexClose (env sys.ODCIEnv) 
   ReTURN NUMBEr AS external
   name "qxiqtbpc"
   library extdemo5l
   WITH context
   parameters (
     context,
     self,
     self indicator struct,
     env,
     env indicator struct,
     return OCINumber
              );

end;
/
show errors

---------------------
-- CREATE INDEXTYPE
---------------------

create indextype psbtree
for
eq(varchar2, varchar2),
lt(varchar2, varchar2),
gt(varchar2, varchar2)
using psbtree_im with local range partition;

--------------------------
--    USAGE EXAMPLES    --
--------------------------
set serveroutput on size 20000

----------------
-- CREATE TABLE
----------------

create table t1 (f1 number, f2 varchar2(200))
partition by range(f1)
(
  partition p1 values less than (101),
  partition p2 values less than (201),
  partition p3 values less than (301),
  partition p4 values less than (401)
 );
insert into t1 values (10, 'aaaa');
insert into t1 values (200, 'bbbb');
insert into t1 values (100, 'cccc');
insert into t1 values (300, 'dddd');
insert into t1 values (400, 'eeee');
commit;

-----------------------------
-- CREATE LOCAL DOMAIN INDEX
-----------------------------

create index it1 on t1(f2) indextype is psbtree local   
(partition pe1 parameters('test1'), partition pe2,
 partition pe3, partition pe4 parameters('test4'))
parameters('test');

-----------
-- INSERTS
-----------

insert into t1 values (11, 'gggg');
insert into t1 values (325, 'hhhh');
insert into t1 values (327, 'iiii');
select * from t1 order by f1;

commit;

-----------
-- DELETES
-----------

delete from t1 where f1 = 325;
select * from t1 order by f1;

rollback;

select * from t1 where eq(f2, 'hhhh') = 1;

delete from t1 where eq(f2, 'hhhh') = 1;
select * from t1 order by f1;

select * from t1 where eq(f2, 'hhhh') = 1;

commit;

-----------
-- UPDATES
-----------
update t1 set f2 = '####' where f1=327;
select * from t1 order by f1;

commit;

update t1 set f1 = 328 where eq(f2, '####') = 1;

select * from t1 where eq(f2, '####') = 1;

-----------
-- QUERIES
-----------
-- partition extended table_name
explain plan for 
 select * from t1  partition(p1) where eq(f2, 'gggg') = 1;
set echo off
@@extdemo0
set echo on

select * from t1 partition(p1) where eq(f2, 'gggg') = 1;

-- entire table
explain plan for 
 select * from t1 where eq(f2, 'gggg') = 1;
set echo off
@@extdemo0
set echo on

select * from t1 where eq(f2, 'gggg') = 1;

-- subset of table
explain plan for 
 select * from t1 where eq(f2, 'dddd') = 1 and f1>101 ;
set echo off
@@extdemo0
set echo on

select * from t1 where eq(f2, 'dddd') = 1 and f1>101 ;

-- single partition
explain plan for 
 select * from t1 where eq(f2, 'dddd') = 1 and f1 =300 ;
set echo off
@@extdemo0
set echo on

select * from t1 where eq(f2, 'dddd') = 1 and f1 = 300;

select * from t1 where lt(f2, 'zzzz') = 1 order by f1;

select * from t1 where gt(f2, 'aaaa') = 1 order by f1;

--------------------------
-- ALTER TABLE OPERATIONS
--------------------------
--Alter Table Add Partition
alter table t1 add partition pp2 values less than (501);
insert into t1 values (500, 'ffff');

--Alter Table Drop Partition
alter table t1 drop partition pp2;

--Alter Table Split Partition
alter table t1 split partition p2 at (150) into
(partition p21, partition p22);

--Alter Table Merge Partition
alter table t1 merge partitions p22, p3 into partition pp2;

--Create A Non-Partitioned Table
create table ext (f1 number, f2 varchar2(200));

insert into ext values (310, 'aaaa');
insert into ext values (320, 'bbbb');
insert into ext values (330, 'dddd');

-- NOW, CREATE DOMAIN INDEXES
create index it2 on ext(f2) indextype is psbtree;

create table temp ( f1 varchar2(200) , f2 ROWID)  
partition by range (f1)
( partition p1 values less than (maxvalue));

--Alter Table Exchange Partition

alter table t1 exchange partition p4 with table ext including indexes;

--Alter Table Modify Unusable
alter table t1 modify partition p4 unusable local indexes;

--Alter Table Truncate Partition
alter table t1 truncate partition p1;

------------
-- CLEANUPS
------------

drop index it1;
drop index it2;
drop table t1;
drop table temp;
drop table ext;
drop indextype psbtree;
drop type psbtree_im;
drop operator eq;
drop operator lt;
drop operator gt;
drop function bt_eq;
drop function bt_lt;
drop function bt_gt;


