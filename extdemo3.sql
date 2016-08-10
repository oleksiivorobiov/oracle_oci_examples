Rem
Rem $Header: extdemo3.sql 09-mar-2001.14:56:30 rmurthy Exp $
Rem
Rem extdemo3.sql
Rem
Rem  Copyright (c) Oracle Corporation 1999, 2000. All Rights Reserved.
Rem
rem    NAME
rem      extdemo3.sql - An extensible indexing example
rem                     implemented as Java callouts.
rem
rem    DESCRIPTION
rem      This file demonstrates the definition and usage of a simple
rem      btree indextype whose routines are implemented as Java callouts.
rem
rem      The Java methods are in the file extdemo3.java
rem      The associated context class is in extdemo3a.java
rem
rem      The foll. steps should have been done before running
rem      this script.
rem      1. Compile the Java files 
rem          (i.e javac extdemo3a.java, javac extdemo3.java .  Make
rem           sure that the ORACLE_HOME/rdbms/jlib/ODCI.jar and 
rem           ORACLE_HOME/rdbms/jlib/CartridgeServices.jar files are in 
rem           your classpath and that these files have been loaded into the
rem 	      database in SYS schema)
rem      2. Create a user named extdemo with password extdemo
rem      3. Grant create any directory privilege to extdemo
rem      3. Create a directory in extdemo schema called vmtestdir
rem         which points to the directory containing the compiled
rem         extdemo3.class and extdemo3a.class
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
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    rmurthy     03/09/01 - bug 1676437 - change call to extdemo0
Rem    hdnguyen    11/14/00 - fixed connect internal
Rem    rmurthy     01/12/00 - add path name while calling utlxplan
Rem    hdnguyen    09/16/99 - Use extdemo0
Rem    rshaikh     09/13/99 - Created
Rem

---------------------------------------------------------------------
--    SIMPLE B-TREE Index Method  Implemented as Trusted Callouts  --
---------------------------------------------------------------------
connect extdemo/extdemo
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
create or replace type extdemo3 as object
(
   scanctx integer,
   static function ODCIGetInterfaces(ifclist OUT sys.ODCIObjectList) return NUMBER,  
  static function ODCIIndexCreate (ia sys.odciindexinfo, parms varchar2) return number,
  static function ODCIIndexDrop(ia sys.odciindexinfo) return number,

  STATIC FUNCTION odciindexinsert(ia sys.odciindexinfo, rid VARCHAR2, 
                                                    newval VARCHAR2)
                                  RETURN NUMBER AS LANGUAGE JAVA NAME 
	'extdemo3.ODCIInsert(oracle.ODCI.ODCIIndexInfo, java.lang.String, 
		java.lang.String) return java.math.BigDecimal',
  
  STATIC FUNCTION odciindexdelete(ia sys.odciindexinfo, rid VARCHAR2, 
                                                    oldval VARCHAR2)
                                  RETURN NUMBER AS LANGUAGE JAVA NAME
	'extdemo3.ODCIDelete(oracle.ODCI.ODCIIndexInfo, java.lang.String, 
		java.lang.String) return java.math.BigDecimal',
  
  STATIC FUNCTION odciindexupdate(ia sys.odciindexinfo, rid VARCHAR2, 
                                  oldval VARCHAR2, newval VARCHAR2)
                                  RETURN NUMBER AS LANGUAGE JAVA NAME
	'extdemo3.ODCIUpdate(oracle.ODCI.ODCIIndexInfo, java.lang.String, 
		java.lang.String, java.lang.String) return 
		java.math.BigDecimal',
  
  static function ODCIIndexStart(sctx in out extdemo3, ia sys.odciindexinfo,
                         op sys.odciPredInfo, 
                         qi sys.ODCIQueryInfo, 
                         strt number, 
                         stop number,
                         cmpval varchar2) 
     RETURN NUMBER AS LANGUAGE JAVA NAME
	'extdemo3.ODCIStart(extdemo3[], oracle.ODCI.ODCIIndexInfo, 
		oracle.ODCI.ODCIPredInfo, 
		oracle.ODCI.ODCIQueryInfo, java.math.BigDecimal, 
		java.math.BigDecimal, 
                java.lang.String) return java.math.BigDecimal',

  member function ODCIIndexFetch(nrows number, rids OUT sys.odciridlist)
   return number as LANGUAGE JAVA NAME
	'extdemo3.ODCIFetch(java.math.BigDecimal, 
	oracle.ODCI.ODCIRidList[]) return java.math.BigDecimal',

  member function ODCIIndexClose return number as LANGUAGE JAVA NAME
	'extdemo3.ODCIClose() return java.math.BigDecimal'
  
);
/
show errors


CREATE OR REPLACE JAVA CLASS USING BFILE (vmtestdir, 'extdemo3a.class')
/
CREATE OR REPLACE JAVA CLASS USING BFILE (vmtestdir, 'extdemo3.class')
/

ALTER JAVA CLASS "extdemo3a" RESOLVE;
ALTER JAVA CLASS "extdemo3"  RESOLVE;




---------------------------------
--  CREATE IMPLEMENTATION UNIT --
---------------------------------

-- CREATE TYPE BODY
create or replace type body extdemo3 
is
   static function ODCIGetInterfaces(ifclist OUT sys.ODCIObjectList) 
       return number is
   begin
       ifclist := sys.ODCIObjectList(sys.ODCIObject('SYS','ODCIINDEX1'));
       return ODCIConst.Success;
   end ODCIGetInterfaces;

  static function ODCIIndexCreate (ia sys.odciindexinfo, parms varchar2) return number
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
  
end;
/
show errors

--------------------------
--    USAGE EXAMPLES    --
--------------------------

-- CREATE INDEXTYPE
connect / as sysdba
grant create library to EXTDEMO;
grant create any directory to EXTDEMO;
grant create any operator,  create indextype, create table to EXTDEMO;
connect extdemo/extdemo
set serveroutput on

create indextype sbtree
for
eq(varchar2, varchar2),
lt(varchar2, varchar2),
gt(varchar2, varchar2)
using extdemo3;

create table t1 (f1 number, f2 varchar2(200));
insert into t1 values (1, 'ravi');
insert into t1 values (3, 'murthy');
commit;

create index it1 on t1(f2) indextype is sbtree parameters('test');

-- query
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

-- INSERT TESTS
-- BUG 687446
INSERT INTO t1 VALUES (6, 'cheuk');
INSERT INTO t1 VALUES (7, 'chau');

-- query from index table
SELECT f1 FROM it1_sbtree;

-- DELETE TEST
DELETE FROM t1 WHERE f2 = 'ravi';

-- query from index table
SELECT f1 FROM it1_sbtree;

-- UPDATE TEST
UPDATE t1 SET f2 = 'Nipun' WHERE f1 = 3;

-- query from index table
SELECT f1 FROM it1_sbtree;

-- DROP TEST
drop table t1;

describe t1;
describe it1;
describe it1_sbtree;

drop index it1;
describe it1;
describe it1_sbtree;

	
