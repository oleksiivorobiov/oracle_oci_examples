Rem
Rem $Header: extdemo6.sql 06-jun-2008.09:31:29 yhu      Exp $
Rem
Rem extdemo6.sql
Rem
Rem Copyright (c) 2008, Oracle.  All rights reserved.  
Rem
Rem    NAME
Rem      extdemo6.sql - An extensible indexing example implemented 
Rem                     as C routines for system-managed local domain index 
Rem                     on varchar2 column of a range partitioned table.
Rem
Rem    DESCRIPTION
Rem      This file demonstrates the definition and usage of a simple
Rem      btree indextype whose routines are implemented as C callouts.
Rem      The C routines are in the file extdemo6.c
Rem      The header file is extdemo6.h
Rem
Rem      The following steps should have been done before running
Rem      this script.
Rem      1. Compile the C file (i.e make -f demo_rdbms.mk demos)
Rem      2. Create a user named extdemo6 with password extdemo6
Rem           with all the necessary privileges. 
Rem      3. Create a library in extdemo6 schema called extdemo6l
Rem         which points to the compiled extdemo6.c
Rem
Rem      The design of the indextype is as follows :
Rem
Rem      The sbtree indextype implemented here will support the evaluation
Rem      of three user-defined operators : gt(Greater Than), lt(Less Than)
Rem      and eq(EQuals). These operators can operate on the operands of
Rem      VARCHAR2 datatype.
Rem      To simplify the implementation of the indextype, we will store
Rem      the index data in a regular table.
Rem      Thus, our code merely translates operations on the SB-tree into
Rem      operations on the table storing the index data.
Rem      When a user creates a SB-tree index, we will create a table
Rem      consisting of the indexed column and a rowid column. Inserts into
Rem      the base table will cause appropriate insertions into the index table.
Rem      Deletes and updates are handled similarly.
Rem      When the SB-tree is queried based on a user-defined operator (one
Rem      of gt, lt and eq), we will fire off an appropriate query against
Rem      the index table to retrieve all the satisfying rows and return them.
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    yhu         06/06/08 - Created
Rem

SET ECHO ON
SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100

---------------------------------------------------------------------
--    SIMPLE B-TREE Index Method  Implemented as C Callouts  --
---------------------------------------------------------------------

connect extdemo6/extdemo6
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

create table md_table(iname varchar2(30), ipname varchar2(30),
                      params varchar2(1000), ipobj# number);
                        
create or replace package md  authid definer as
  procedure insertmd(iname varchar2, ipname varchar2,
         ipobj# number, params  varchar2);
  procedure updateobjnum(iname varchar2, ipname varchar2, ipobj# number);
  procedure updateidxnam(oldiname varchar2, newiname varchar2);
  procedure updateidxpart(iname varchar2,oldpname varchar2, newpname varchar2);
  procedure updateparams(iname varchar2, ipname varchar2, params varchar2);
  procedure deletemd(iname varchar2, ipname varchar2);
  procedure deleteindexmd(iname varchar2); 
end md;
/

show errors

create or replace package body md as
   procedure insertmd (iname varchar2, ipname varchar2,
         ipobj# number, params  varchar2)
     is
     stmt1 varchar2(1000);
   begin
     stmt1 := 'insert into md_table values(:1, :2, :3, :4)';
     execute immediate stmt1 using iname, ipname, params, ipobj#;

   end;

   procedure updateobjnum(iname varchar2, ipname varchar2, ipobj# number)
    is
    stmt1 varchar2(1000);
   begin
    stmt1 := 'update md_table set ipobj# = :1 where iname = :2 and ipname= :3';
     execute immediate stmt1 using ipobj#, iname, ipname;

   end;

   procedure updateidxnam(oldiname varchar2, newiname varchar2)
    is
     stmt1 varchar2(1000);
   begin
     stmt1 := 'update md_table set iname = :1 where iname = :2';
     execute immediate stmt1 using newiname, oldiname;
   end;

   procedure updateidxpart(iname varchar2,oldpname varchar2, newpname varchar2)
    is
     stmt1 varchar2(1000);
   begin
     stmt1 := 'update md_table set ipname =:1 where iname =:2 and ipname =:3';
     execute immediate stmt1 using newpname, iname, oldpname;
   end;

   procedure updateparams(iname varchar2, ipname varchar2, params varchar2)
   is
    stmt1   varchar2(1000);
   begin
    stmt1 := 'update md_table set params = :1 where iname =:2 and ipname=:3';
    execute immediate stmt1 using params, iname, ipname;
   end;

   procedure deletemd(iname varchar2, ipname varchar2)
     is
    stmt1  varchar2(1000);
   begin
    stmt1 := 'delete from md_table where iname = :1 and ipname = :2';
    execute immediate stmt1 using iname, ipname;

   end;

   procedure deleteindexmd(iname varchar2)
     is
    stmt1  varchar2(1000);
   begin
    stmt1 := 'delete from md_table where iname = :1';
    execute immediate stmt1 using iname;

   end;
 end;
/

show errors;

-- CREATE INDEXTYPE IMPLEMENTATION TYPE
create type psbtree_im as object
(
  scanctx RAW(4),
  static function ODCIGetInterfaces(ifclist OUT sys.ODCIObjectList)
    return NUMBER,

  static function ODCIIndexCreate (ia sys.ODCIIndexInfo, 
    parms varchar2, env sys.ODCIEnv) return NUMBER, 

  static function ODCIIndexAlter (ia sys.ODCIIndexInfo, 
    parms IN OUT varchar2, altopt number, env sys.ODCIEnv) return NUMBER, 

  static function ODCIIndexDrop(ia sys.ODCIIndexInfo, 
    env sys.ODCIEnv) return NUMBER,

  static function ODCIIndexInsert(ia sys.ODCIIndexInfo, rid VARCHAR2,
    newval VARCHAR2, env sys.ODCIEnv) return NUMBER, 

  static function ODCIIndexDelete(ia sys.ODCIIndexInfo, rid VARCHAR2,
    oldval VARCHAR2, env sys.ODCIEnv) return NUMBER, 

  static function ODCIIndexUpdate(ia sys.ODCIIndexInfo, rid VARCHAR2,
    oldval VARCHAR2, newval VARCHAR2, env sys.ODCIEnv)  
    return NUMBER,  

  static function ODCIIndexUpdPartMetadata(ia sys.ODCIIndexInfo, 
    palist sys.ODCIPartInfoList, env sys.ODCIEnv) 
    return NUMBER,

  static function ODCIIndexExchangePartition (ia sys.ODCIIndexInfo,
    ia1 sys.ODCIIndexInfo, env sys.ODCIEnv) return NUMBER,

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
   stmt varchar2(2000);
   cursor cur1(ianame varchar2) is
      select partition_name, parameters from user_ind_partitions 
            where index_name = ianame order by partition_name;
  begin
   stmt := '';

   if (env.CallProperty is null)  then
     stmt := 'create table ' ||ia.IndexSchema || '.' || ia.IndexName ||
             '_sbtree(f1 varchar2(1000), f2 rowid)';
   elsif (env.callproperty = sys.ODCIConst.FirstCall) then
     stmt := '';
     i := 1;

     for c1 in cur1(ia.indexname) loop
        if (i >1) then
          stmt := stmt || ',';
        end if;
        stmt := stmt || 'partition ' || c1.partition_name;
        md.insertmd(ia.indexname, c1.partition_name, 0, c1.parameters);
        i := i+1;
     end loop;
     stmt := 'create table ' || ia.indexschema || '.' || ia.indexname ||
             '_sbtree (f1 varchar2(1000), f2 rowid) partition by system ' ||
              '( ' || stmt || ')';
   end if;
 
   dbms_output.put_line('Create');
   dbms_output.put_line(stmt);

   -- execute the statement
   if ( (env.CallProperty is null) or
        (env.CallProperty = sys.ODCIConst.FirstCall) ) then
     execute immediate stmt;
     if (env.CallProperty is null) then
       execute immediate 'insert into ' ||ia.IndexSchema || '.' 
             || ia.IndexName || '_sbtree select '  ||
             ia.IndexCols(1).Colname || ', ROWID from ' ||
             ia.IndexCols(1).TableSchema || '.' || 
             ia.IndexCols(1).TableName;
     end if;
   end if;

   return ODCIConst.Success;
  end;

 static function ODCIIndexAlter (ia sys.ODCIIndexInfo, 
    parms IN OUT varchar2, altopt number, env sys.ODCIEnv) 
    return number is
   stmt    varchar2(2000);

 begin
   stmt := '';
   if (altopt = ODCIConst.AlterIndexRebuild) then
     if (env.callproperty = ODCIConst.IntermediateCall) then
       md.updateobjnum(ia.IndexName, ia.IndexPartition, ia.IndexPartitionIden);
       stmt := 'insert into ' || ia.indexschema || '.' || ia.indexname ||
             '_sbtree partition (' || ia.indexpartition || ') select ' || 
             ia.indexcols(1).colname || ', rowid from ' ||
             ia.indexcols(1).tableschema || '.' || ia.indexcols(1).tablename ||
             ' partition (' || ia.indexcols(1).tablepartition || ')';
       dbms_output.put_line(stmt);
       execute immediate stmt;
     end if;
   elsif (altopt = ODCIConst.AlterIndexRename) then
     if (ia.IndexPartition is not null) then
       md.updateidxpart(ia.IndexName, ia.IndexPartition, parms);
       stmt := 'alter table ' || ia.indexschema || '.' ||
               ia.indexname || '_sbtree rename partition ' || 
               ia.indexpartition || ' to ' || parms;
       dbms_output.put_line(stmt);
       execute immediate stmt;
     else
       md.updateidxnam(ia.IndexName, parms);
       stmt := 'alter table ' || ia.indexschema || '.' ||
               ia.indexname || '_sbtree rename to ' || parms || '_sbtree';
       dbms_output.put_line(stmt);
       execute immediate stmt;
     end if;
   end if;

   dbms_output.put_line('Alter');
   return ODCIConst.Success;
  end ODCIIndexAlter;

  static function ODCIIndexDrop(ia sys.ODCIIndexInfo, env sys.ODCIEnv) 
    return number is
   stmt varchar2(1000);
   cnum integer;
   junk integer;
  begin
    -- construct the sql statement
   stmt := '';
   if (env.CallProperty is null) then
     md.deleteindexmd(ia.indexname);
     stmt := 'drop table ' || ia.IndexSchema || '.' || ia.IndexName ||
             '_sbtree';

     dbms_output.put_line('Drop');
     dbms_output.put_line(stmt);
      
     execute immediate stmt;
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
   stmt := 'alter table ' || ia.IndexSchema || '.' || ia.IndexName ||
        '_sbtree exchange partition ' ||   ia.IndexPartition || 
        ' with table ' || ia1.IndexSchema || '.' || ia1.IndexName || '_sbtree';

   dbms_output.put_line(stmt);
   execute immediate stmt;

   return ODCIConst.Success;
  end;

  static function ODCIIndexUpdPartMetadata(ia sys.ODCIIndexInfo, 
    palist sys.ODCIPartInfoList, env sys.ODCIEnv) 
    return number is
   col  number;
  begin

   dbms_output.put_line('ODCIUpdPartMetadata');
   SYS.ODCIINDEXINFODUMP(ia);
   SYS.ODCIPARTINFOLISTDUMP(palist);

   FOR col IN palist.FIRST..palist.LAST LOOP
     IF (palist(col).PartOp = ODCIConst.AddPartition) THEN  
       md.insertmd(ia.indexname, palist(col).indexpartition,
                   palist(col).indexpartitioniden, null);
     ELSIF (palist(col).PartOp = ODCIConst.DropPartition) THEN  
       md.deletemd(ia.indexname, palist(col).indexpartition);
     END IF;
   END LOOP; 
   return ODCIConst.Success;
  END;

  static function ODCIIndexInsert(ia sys.ODCIIndexInfo, rid VARCHAR2,
    newval VARCHAR2, env sys.ODCIEnv)  
    RETURN NUMBER AS external
    name "qxiqtbspi"
    library extdemo6l
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
    name "qxiqtbspd"
    library extdemo6l
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
    name "qxiqtbspu"
    library extdemo6l
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
    name "qxiqtbsps"
    library extdemo6l
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
   name "qxiqtbspf"
   library extdemo6l
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
   name "qxiqtbspc"
   library extdemo6l
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
using psbtree_im with local range partition
with system managed storage tables;

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
drop table ext;
drop indextype psbtree;
drop type psbtree_im;
drop operator eq;
drop operator lt;
drop operator gt;
drop function bt_eq;
drop function bt_lt;
drop function bt_gt;


