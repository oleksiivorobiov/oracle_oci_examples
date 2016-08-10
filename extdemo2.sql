rem
rem $Header: extdemo2.sql 09-mar-2001.14:56:24 rmurthy Exp $
rem
rem extdemo2.sql
rem
rem Copyright (c) 1998, 1999,, 2000 Oracle Corporation.  All rights reserved.
rem
rem    NAME
rem      extdemo2.sql - An extensible indexing example
rem                     implemented as C routines
rem
rem    DESCRIPTION
rem      This file demonstrates the definition and usage of a simple
rem      btree indextype whose routines are implemented as C callouts.
rem      The C routines are in the file extdemo2.c
rem      The header file is extdemo2.h
rem
rem      The foll. steps should have been done before running
rem      this script.
rem      1. Compile the C file (i.e make -f demo_rdbms.mk demos)
rem      2. Create a user named extdemo2 with password extdemo2
rem      3. Create a library in extdemo2 schema called extdemo2l
rem         which points to the compiled extdemo2.c
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
rem    MODIFIED   (MM/DD/YY)
rem    rmurthy     03/09/01 - bug 1676437 - change call to extdemo0
rem    rmurthy     01/13/00 - add path name while calling utlxplan
rem    mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem    hdnguyen    07/26/99 - sqlplus conversion
rem    hdnguyen    06/15/99 - modified step 1 to add compile instruction
rem    rmurthy     09/25/98 - Created

---------------------------------------------------------------------
--    SIMPLE B-TREE Index Method  Implemented as C Callouts  --
---------------------------------------------------------------------

connect extdemo2/extdemo2
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

create operator eq binding (varchar2, varchar2) return number using bt_eq;

create operator lt binding (varchar2, varchar2) return number using bt_lt;

create operator gt binding (varchar2, varchar2) return number using bt_gt;


-- CREATE INDEXTYPE IMPLEMENTATION TYPE
create type sbtree_im as object
(
  scanctx RAW(4),
  static function ODCIGetInterfaces(ifclist OUT sys.ODCIObjectList)
  return NUMBER,
  static function ODCIIndexCreate (ia sys.odciindexinfo, parms varchar2)
  return number,
  static function ODCIIndexDrop(ia sys.odciindexinfo) return number,
  STATIC FUNCTION odciindexinsert(ia sys.odciindexinfo, rid VARCHAR2,
                                                    newval VARCHAR2)
                                  RETURN NUMBER,
  STATIC FUNCTION odciindexdelete(ia sys.odciindexinfo, rid VARCHAR2,
                                                    oldval VARCHAR2)
                                  RETURN NUMBER,
  STATIC FUNCTION odciindexupdate(ia sys.odciindexinfo, rid VARCHAR2,
                                  oldval VARCHAR2, newval VARCHAR2)
                                  RETURN NUMBER,
  static function ODCIIndexStart(sctx IN OUT sbtree_im, ia sys.odciindexinfo,
                         op sys.odciPredInfo, qi sys.ODCIQueryInfo,
                         strt number, stop number,
                         cmpval varchar2) return number,
  member function ODCIIndexFetch(nrows number, rids OUT sys.odciridlist)
      return number,
  member function ODCIIndexClose return number
);
/
show errors


---------------------------------
--  CREATE IMPLEMENTATION UNIT --
---------------------------------

-- CREATE TYPE BODY
create or replace type body sbtree_im
is
   static function ODCIGetInterfaces(ifclist OUT sys.ODCIObjectList)
       return number is
   begin
       ifclist := sys.ODCIObjectList(sys.ODCIObject('SYS','ODCIINDEX1'));
       return ODCIConst.Success;
   end ODCIGetInterfaces;

  static function ODCIIndexCreate (ia sys.odciindexinfo, parms varchar2)
    return number
  is
   i integer;
   stmt varchar2(1000);
   cnum integer;
   junk integer;
  begin
   -- construct the sql statement
     stmt := 'create table ' || ia.IndexSchema || '.' ||
       ia.IndexName || '_sbtree'  ||
       '( f1 , f2 ) as select ' ||
       ia.IndexCols(1).ColName || ', ROWID from ' ||
       ia.IndexCols(1).TableSchema || '.' || ia.IndexCols(1).TableName;

   dbms_output.put_line('CREATE');
   dbms_output.put_line(stmt);

   -- execute the statement
   cnum := dbms_sql.open_cursor;
   dbms_sql.parse(cnum, stmt, dbms_sql.native);
   junk := dbms_sql.execute(cnum);
   dbms_sql.close_cursor(cnum);

   return ODCIConst.Success;
  end;

  static function ODCIIndexDrop(ia sys.odciindexinfo) return number is
   stmt varchar2(1000);
   cnum integer;
   junk integer;
  begin
    -- construct the sql statement
   stmt := 'drop table ' || ia.IndexSchema || '.' || ia.IndexName || '_sbtree';

   dbms_output.put_line('DROP');
   dbms_output.put_line(stmt);

   -- execute the statement
   cnum := dbms_sql.open_cursor;
   dbms_sql.parse(cnum, stmt, dbms_sql.native);
   junk := dbms_sql.execute(cnum);
   dbms_sql.close_cursor(cnum);

   return ODCIConst.Success;
  end;

  STATIC FUNCTION odciindexinsert(ia sys.odciindexinfo, rid VARCHAR2,
                                                    newval VARCHAR2)
                                  RETURN NUMBER AS external
    name "qxiqtbi"
    library extdemo2l
    WITH context
    parameters (
    context,
    ia,
    ia indicator struct,
    rid,
    rid indicator,
    newval,
    newval indicator,
    RETURN ocinumber
               );

  STATIC FUNCTION odciindexdelete(ia sys.odciindexinfo, rid VARCHAR2,
                                                    oldval VARCHAR2)
                                  RETURN NUMBER AS external
    name "qxiqtbd"
    library extdemo2l
    WITH context
    parameters (
    context,
    ia,
    ia indicator struct,
    rid,
    rid indicator,
    oldval,
    oldval indicator,
    RETURN ocinumber
               );

  STATIC FUNCTION odciindexupdate(ia sys.odciindexinfo, rid VARCHAR2,
                                  oldval VARCHAR2, newval VARCHAR2)
                                  RETURN NUMBER AS external
    name "qxiqtbu"
    library extdemo2l
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
    RETURN ocinumber
               );

  static function ODCIIndexStart(sctx in out sbtree_im, ia sys.odciindexinfo,
                         op sys.odciPredInfo,
                         qi sys.ODCIQueryInfo,
                         strt number,
                         stop number,
                         cmpval varchar2)
     return number as external
     name "qxiqtbs"
     library extdemo2l
     with context
     parameters (
       context,
       sctx,
       sctx INDICATOR STRUCT,
       ia,
       ia INDICATOR STRUCT,
       op,
       op INDICATOR STRUCT,
       qi,
       qi INDICATOR STRUCT,
       strt,
       strt INDICATOR,
       stop,
       stop INDICATOR,
       cmpval,
       cmpval INDICATOR,
       return OCINumber
    );

  member function ODCIIndexFetch(nrows number, rids OUT sys.odciridlist)
   return number as external
   name "qxiqtbf"
   library extdemo2l
   with context
   parameters (
     context,
     self,
     self INDICATOR STRUCT,
     nrows,
     nrows INDICATOR,
     rids,
     rids INDICATOR,
     return OCINumber
   );

  member function ODCIIndexClose return number as external
   name "qxiqtbc"
   library extdemo2l
   with context
   parameters (
     context,
     self,
     self INDICATOR STRUCT,
     return OCINumber
   );

end;
/
show errors

---------------------
-- CREATE INDEXTYPE
---------------------

create indextype sbtree
for
eq(varchar2, varchar2),
lt(varchar2, varchar2),
gt(varchar2, varchar2)
using sbtree_im;

--------------------------
--    USAGE EXAMPLES    --
--------------------------
set serveroutput on size 20000

----------------
-- CREATE TABLE
----------------

create table t1 (f1 number, f2 varchar2(200));
insert into t1 values (1, 'ravi');
insert into t1 values (3, 'murthy');
commit;

-----------------
-- CREATE INDEX
-----------------

create index it1 on t1(f2) indextype is sbtree parameters('test');

------------
-- QUERIES
------------

explain plan for
select * from t1 where eq(f2, 'ravi') = 1;
set echo off
@@extdemo0
set echo on
select * from t1 where eq(f2, 'ravi') = 1;

explain plan for
select * from t1 where gt(f2, 'aaa') = 1;
set echo off
@@extdemo0
set echo on
select * from t1 where gt(f2, 'aaa') = 1;

explain plan for
select * from t1 where lt(f2, 'aaa') = 1;
set echo off
@@extdemo0
set echo on
select * from t1 where lt(f2, 'aaa') = 1;

explain plan for
select * from t1 where lt(f2, 'aaa') = 0;
set echo off
@@extdemo0
set echo on
select * from t1 where lt(f2, 'aaa') = 0;

-----------
-- INSERTS
-----------

INSERT INTO t1 VALUES (6, 'cheuk');
INSERT INTO t1 VALUES (7, 'chau');
explain plan for
select * from t1 where eq(f2, 'cheuk') = 1;
set echo off
@@extdemo0
set echo on
select * from t1 where eq(f2, 'cheuk') = 1;

-----------
-- DELETES
-----------

DELETE FROM t1 WHERE f2 = 'ravi';
explain plan for
select * from t1 where eq(f2, 'ravi') = 1;
set echo off
@@extdemo0
set echo on
select * from t1 where eq(f2, 'ravi') = 1;

-----------
-- UPDATES
-----------

UPDATE t1 SET f2 = 'Nipun' WHERE f1 = 3;
explain plan for
select * from t1 where eq(f2, 'Nipun') = 1;
set echo off
@@extdemo0
set echo on
select * from t1 where eq(f2, 'Nipun') = 1;

------------
-- CLEANUPS
------------

drop index it1;
drop table t1;
drop indextype sbtree;
drop type sbtree_im;
drop operator eq;
drop operator lt;
drop operator gt;
drop function bt_eq;
drop function bt_lt;
drop function bt_gt;
