Rem
Rem $Header: mddemo2.sql 16-apr-2001.16:44:33 nmeng Exp $
Rem
Rem mddemo2.sql
Rem
Rem  Copyright (c) Oracle Corporation 2001. All Rights Reserved.
Rem
Rem    NAME
Rem      mddemo2.sql - MetaData API Demo Part II 
Rem
Rem    DESCRIPTION
Rem      <short description of component this file declares/defines>
Rem
Rem    NOTES
Rem
Rem       Oracle 9i demo of Metadata API  Part II
Rem       This script continues to demonstrate how to use the Metadata API.
Rem       It first establishes a schema (mddemo) with associated grants.
Rem       It then creates various objects for the demo purpose, including
Rem       tables, indexes, fuctions and procedures.
Rem
Rem       This demo shows the usage of Metadata API's browsing and
Rem       programmatic APIs.  Instead of writing the retrieved DDLs
Rem       to a table, this script creates a procedure PRINT_DDL to
Rem       display them.
Rem
Rem       To run the demo, first cd to rdbms/demo directory.  Then,
Rem       % sqlplus /nolog
Rem       SQL> @mddemo2
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    nmeng       04/16/01 - Merged nmeng_metadata_api_demo_010412
Rem    nmeng       04/13/01 - Created
Rem

REM 
REM initialize environment
REMARK >>>> Set System Variables For Current SQLPlus Session <<<<
SET FEEDBACK 1
SET NUMWIDTH 10
SET PAGESIZE 0
SET LINESIZE 200
SET TRIMSPOOL ON
SET TAB OFF
SET DEFINE '^'

set echo on
connect system/manager
drop user mddemo cascade;

REM create new user: mddemo
grant connect, resource, select_catalog_role to mddemo identified by mddemo;

REM grant select on scott.emp to mddemo
connect scott/tiger
grant select on emp to mddemo;

REM start session as new user
connect mddemo/mddemo
set long 200000
set serveroutput on size 1000000

REM create objects for demo
REM  TABLE 
CREATE TABLE EMP AS SELECT * FROM SCOTT.EMP;

CREATE TABLE DEPT (
  DEPTNO    NUMBER CONSTRAINT dept_pk PRIMARY KEY,
  DNAME      VARCHAR2(20),
  LOCATION  VARCHAR2(80)
);

CREATE TABLE NEWEMP(
  ENO       NUMBER,
  ENAME     VARCHAR2(40),
  DEPTNO    NUMBER CONSTRAINT emp_fk REFERENCES DEPT
);


REM COMPOSITE INDEX
CREATE UNIQUE INDEX dept_uk1 ON DEPT(DNAME,LOCATION)
pctfree 40
initrans 10
maxtrans 20
logging
compute statistics
compress 1
nosort
STORAGE(INITIAL 8K NEXT 12K MINEXTENTS 1 MAXEXTENTS 120 PCTINCREASE 40
FREELISTS 1 FREELIST GROUPS 4 BUFFER_POOL DEFAULT) TABLESPACE "SYSTEM"
parallel ( degree 5 instances 2);

REM BITMAP INDEX
CREATE BITMAP INDEX dept_bitmap ON DEPT(LOCATION, DNAME) PARALLEL; 

REM  FUNCTION 
CREATE OR REPLACE FUNCTION f1(
 arg1 NUMBER,
 arg2 IN INTEGER,
 arg3 OUT NOCOPY FLOAT,
 arg4 IN OUT VARCHAR2
) RETURN BLOB DETERMINISTIC PARALLEL_ENABLE
AUTHID CURRENT_USER IS
  BEGIN
    /* function body
       .
       .
       .
    */
    null;
  END;
/


REM  PACKAGE 
CREATE OR REPLACE PACKAGE MyPkg IS
  FUNCTION sqr(x IN NUMBER) RETURN NUMBER;
  PROCEDURE hello(name IN VARCHAR2);
END;
/

CREATE OR REPLACE PACKAGE BODY MyPkg AS

  FUNCTION sqr(x IN NUMBER) RETURN NUMBER IS
  BEGIN
    return x*x;
  END;

  PROCEDURE hello(name IN VARCHAR2) IS
  BEGIN
    dbms_output.put_line('Hello ' || name);
  END;
END;
/

CREATE OR REPLACE PACKAGE emp_mgmt AS
  FUNCTION hire(ename VARCHAR2,
                job VARCHAR2,
                mgr NUMBER,
                sal NUMBER,
                comm NUMBER,
                deptno NUMBER)
    RETURN NUMBER;
  FUNCTION create_dept(dname VARCHAR2,loc VARCHAR2)
    RETURN NUMBER;
  PROCEDURE remove_emp(empno NUMBER);
  PROCEDURE remove_dept(deptno NUMBER);
  PROCEDURE increase_sal(empno NUMBER, sal_incr NUMBER);
  PROCEDURE increase_comm(empno NUMBER, comm_incr NUMBER);
    no_comm EXCEPTION;
    no_sal EXCEPTION;
END emp_mgmt;
/

CREATE OR REPLACE PACKAGE BODY emp_mgmt AS
   tot_emps NUMBER;
   tot_depts NUMBER;

  FUNCTION hire(
    ename VARCHAR2,
    job VARCHAR2,
    mgr NUMBER,
    sal NUMBER,
    comm NUMBER,
    deptno NUMBER)
  RETURN NUMBER IS
    new_empno NUMBER(4);
  BEGIN
    new_empno:=1;
    RETURN(new_empno);
  END;

  FUNCTION create_dept(dname VARCHAR2, loc VARCHAR2)
    RETURN NUMBER IS
      new_deptno NUMBER(4);
    BEGIN
      return 101;
    END;

  PROCEDURE remove_emp(empno NUMBER) IS
    BEGIN
      null;
    END;

  PROCEDURE remove_dept(deptno NUMBER) IS
    BEGIN
      null;
    END;

  PROCEDURE increase_sal(empno NUMBER, sal_incr NUMBER) IS
    curr_sal NUMBER(7,2);
    BEGIN
      null;
    END;

  PROCEDURE increase_comm(empno NUMBER, comm_incr NUMBER) IS
    curr_comm NUMBER(7,2);
    BEGIN
      null;
    END;

END emp_mgmt;
/




REM ***********
REM *  DEMOS  *
REM ***********

REM CREATE procedure PRINT_DDL to display fetched DDLs
CREATE OR REPLACE PROCEDURE Print_DDL(DDLString IN CLOB)
IS 
  str varchar2(32767);
  tmpstr varchar2(32767);
  len integer;
  idx integer := 1;
  nlidx integer;
  newline character := chr(10);
  BEGIN
    str := substr(DDLString,1,31*1024);

    len := length(str);
    loop
      exit when idx >len;
      nlidx := instr(str,newline,idx);
      if (nlidx = 0) then
         nlidx := len+1;
      end if;
      tmpstr := substr(str,idx,nlidx-idx);

      if(length(tmpstr) > 250) then
        --print line or 250 bytes per line
        tmpstr := '| '||tmpstr;
        loop
          exit when tmpstr is null;
          dbms_output.put_line(substr(tmpstr,1,250));
          tmpstr := substr(tmpstr,251);
        end loop;
      else
        dbms_output.put_line('| '||tmpstr);
      end if;

      idx := nlidx+1;
    end loop;
  END Print_DDL;
/

REM create procedure PRINT_XML to print fetched XML
CREATE OR REPLACE PROCEDURE PRINT_XML (XMLString IN CLOB) IS
BEGIN
  PRINT_DDL(XMLString);
END PRINT_XML;
/

REM BROWSING API: GET_XML, GET_DDL

REM EXAMPLE 1 
REM Fetch the XML representation of EMP ( object in same schema)
SELECT DBMS_METADATA.GET_XML('TABLE','EMP') FROM DUAL; 

REM EXAMPLE 2
REM Fetch the DDL of SCOTT.EMP (object in different schema)
-- first, get the complete DDL by not setting session transform parameter
SELECT DBMS_METADATA.GET_DDL('TABLE','EMP','SCOTT') FROM DUAL;

-- then, get the ddl without the segment attributes.  
-- we need to call SET_TRANSFORM_PARAM using SESSION_TRANSFORM to
-- set transform parameters for the whole session.
EXEC DBMS_METADATA.SET_TRANSFORM_PARAM(  - 
        dbms_metadata.SESSION_TRANSFORM, -
        'SEGMENT_ATTRIBUTES', false);
SELECT DBMS_METADATA.GET_DDL('TABLE','EMP','SCOTT') FROM DUAL;
-- we now reset the transform parameters to default
EXEC DBMS_METADATA.SET_TRANSFORM_PARAM(  - 
        dbms_metadata.SESSION_TRANSFORM, -
        'DEFAULT', true);


REM PROGRAMMATIC API

REM EXAMPLE 3
REM Retrieve all tables in the current schema as XML documents
DECLARE
  handle       NUMBER;
  MyTable      SYS.XMLType;
  XMLString    CLOB;
BEGIN
  handle := DBMS_METADATA.OPEN('TABLE');
  LOOP
    MyTable := DBMS_METADATA.FETCH_XML(handle);
    EXIT WHEN MyTable IS NULL;
    -- get the CLOB value of the XML document and print it
    XMLString := MyTable.getClobVal;
    PRINT_XML(XMLString); 
  END LOOP;
  DBMS_METADATA.CLOSE(handle);
END;
/

REM  You can also fetch the XML as CLOB directly
REM  by using DBMS_METADATA.FETCH_CLOB
DECLARE
  handle       NUMBER;
  XMLString    CLOB;
BEGIN
  handle := DBMS_METADATA.OPEN('TABLE');
  DBMS_METADATA.SET_FILTER(handle, 'NAME', 'NEWEMP');
  LOOP
    XMLString := dbms_metadata.fetch_clob(handle);
    EXIT when XmlString is NULL;
    PRINT_XML(XMLString);
  END LOOP;
  DBMS_METADATA.CLOSE(handle);
END;
/



REM EXAMPLE 4 
REM Retrieve a package and package body as DDL
DECLARE
  handle   NUMBER;
  tHandle  NUMBER;
  PkgTable SYS.KU$_DDLS;
  PkgSpec  CLOB;
  PkgBody  CLOB;
BEGIN
  handle := DBMS_METADATA.OPEN('PACKAGE');
  DBMS_METADATA.SET_FILTER(handle,'NAME','MYPKG');
  -- setting SCHEMA filter to MDDEMO is Optional because 
  -- it is the current Schema
  DBMS_METADATA.SET_FILTER(handle,'SCHEMA','MDDEMO');
  tHandle := DBMS_METADATA.ADD_TRANSFORM(handle, 'DDL');
  PkgTable := DBMS_METADATA.FETCH_DDL(Handle);
  if (PkgTable is NOT NULL) then
    -- Extract DDL text from the PkgTable
    -- A package has two parts: specification and body
    -- which are stored as two rows in the ddl table PkgTable
    -- To retrieve the complete DDL for a package,
    -- we need to get both PkgSpec and PkgBody ddls.  
    PkgSpec := PkgTable(1).ddlText;
    PkgBody := PkgTable(2).ddlText;
    Print_DDL(PkgSpec);
    Print_DDL(PkgBody);
  else
    dbms_output.put_line('OBJECT NOT FOUND IN SCHEMA');
  end if;
  DBMS_METADATA.CLOSE(handle);
END;
/

REM EXAMPLE 5
REM Using dbms_metadata.set_filter to set filters
declare
  h1 number;
  th1 number;
  ddls sys.ku$_ddls;
  ddlstring clob;
begin
  h1 := dbms_metadata.open('PACKAGE');
  -- Besides providing exact name and schema, we can also using
  -- expressions: NAME_EXPR, and SCHEMA_EXPR for more flexiblity
  dbms_metadata.set_filter(h1,'NAME_EXPR','IN (''EMP_MGMT'',''MYPKG'')');
  dbms_metadata.set_filter(h1,'SCHEMA_EXPR', 'like ''MDDEMO'' ');

  th1 := dbms_metadata.add_transform(h1, 'DDL');
  -- using default tranformation parameters
  dbms_metadata.set_transform_param(th1,'DEFAULT', true);
  ddls := dbms_metadata.fetch_ddl(h1);

  -- get all the fetched ddls by using a for loop 
  for i in ddls.FIRST..ddls.LAST loop
  ddlstring :=ddls(i).ddlText;
  Print_DDL(ddlstring);
  end loop;

  dbms_metadata.close(h1);
end;
/

REM EXAMPLE 6
REM Retrieving DDL for a function is the same as other objects.
REM This example shows how to use dbms_metadata.set_transform_param
declare
  h1 number;
  th1 number;
  functionDDLs sys.ku$_ddls;
  ddlstring  clob;
begin
  h1 := dbms_metadata.open('FUNCTION');

  dbms_metadata.set_filter(h1,'NAME', 'F1');

  th1 := dbms_metadata.add_transform(h1, 'DDL');

  dbms_metadata.set_transform_param(th1,'PRETTY', true);
  dbms_metadata.set_transform_param(th1,'SQLTERMINATOR', true);
  functionDDLs := dbms_metadata.fetch_ddl(h1);
  ddlstring := functionDDLs(1).ddlText;
  Print_DDL(ddlstring);
  dbms_metadata.close(h1);
end;
/

REM EXAMPLE 7
REM A more complicated example 
REM Example: Retrieve all tables in the current schema.  For each table
REM          return its indexes.  This example shows the use of SET_PARSE_ITEM
REM          to enable output parsing.

DECLARE
  handle1        NUMBER;
  handle2        NUMBER;
  tHandle1       NUMBER;
  tHandle2       NUMBER;
  SchemaName     VARCHAR2(30);
  TableName      VARCHAR2(30);
  TableDDLs      sys.ku$_ddls;
  TableDDL       sys.ku$_ddl;
  TableDDLtext   CLOB;
  parsedItems    sys.ku$_parsed_items;
  IndexDDLtext   CLOB;
BEGIN
  handle1 := dbms_metadata.open('TABLE');
  tHandle1 := dbms_metadata.add_transform(handle1,'DDL');
  dbms_metadata.set_parse_item(handle1,'NAME');
  dbms_metadata.set_parse_item(handle1,'SCHEMA');
  LOOP
    TableDDLs := dbms_metadata.fetch_ddl(handle1);
    EXIT WHEN TableDDLs IS NULL;
    TableDDL := TableDDLs(1);
    TableDDLtext := TableDDL.ddltext;
    parsedItems := TableDDL.parsedItems;

    FOR i IN parsedItems.FIRST..parsedItems.LAST LOOP
      IF parsedItems(i).item = 'SCHEMA' THEN
        SchemaName := parsedItems(i).value;
      ELSE
        TableName := parsedItems(i).value;
      END IF;
    END LOOP;
    -- open a second stream to fetch the table's indexes.
    -- use the parsed schema and table names to construct
    -- the filters
    handle2 := dbms_metadata.open('INDEX');
    dbms_metadata.set_filter(handle2, 'BASE_OBJECT_SCHEMA',
                           SchemaName);
    dbms_metadata.set_filter(handle2, 'BASE_OBJECT_NAME',
                           TableName);
    tHandle2 := dbms_metadata.add_transform(handle2, 'DDL');
    -- do not fetch segment attributes
    dbms_metadata.set_transform_param(tHandle2,'SEGMENT_ATTRIBUTES',false);
    LOOP
      IndexDDLtext := dbms_metadata.fetch_clob(handle2);
      EXIT WHEN IndexDDLtext IS NULL;
      -- print fetched DDLs
      Print_DDL(IndexDDLtext);
    END LOOP;
    dbms_metadata.close(handle2);
  END LOOP;
  dbms_metadata.close(handle1);
END;
/

    
REM cleanup
connect system/manager
drop user mddemo cascade;
