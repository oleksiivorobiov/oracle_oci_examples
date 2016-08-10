rem
rem $Header: mddemo.sql 01-nov-00.16:13:33 gclaborn Exp $
rem
rem Copyright (c) 1996, 1999, 2000 Oracle Corporation.  All rights reserved.
rem
rem NAME
rem   mddemo.sql - Oracle 9i demo of the Metadata API
rem
rem DESCRIPTON
rem   This demo shows how to programmatically extract database object
rem   definitions using the DBMS_METADATA package.
rem
rem MODIFIED   (MM/DD/YY)
rem     gclaborn    11/01/00 - Created

-- This script demonstrates how to use the Metadata API. It first
-- establishes a schema (MDDEMO) and some payroll users, then creates three
-- payroll-like tables within it along with associated indexes, triggers
-- and grants.

-- It then creates a package PAYROLL_DEMO that shows common usage of the
-- Metadata API. The procedure GET_PAYROLL_TABLES retrieves the DDL for the
-- two tables in this schema that start with 'PAYROLL' then for each one,
-- retrieves the DDL for its associate dependent objects; indexes, grants
-- and triggers. All the DDL is written to a table named "MDDEMO"."DDL".

-- First, Install the demo. cd to rdbms/demo:
-- > sqlplus system/manager
-- SQL> @mddemo

-- Then, run it.
-- > sqlplus mddemo/mddemo
-- SQL> set long 40000
-- SQL> set pages 0
-- SQL> call payroll_demo.get_payroll_tables();
-- SQL> select ddl from DDL order by seqno;

Rem Set up schema for demo pkg. PAYROLL_DEMO.

connect system/manager
drop user mddemo cascade;
drop user mddemo_clerk cascade;
drop user mddemo_mgr cascade;

create user mddemo identified by mddemo;
GRANT resource, connect, create session
     , create table
     , create procedure 
     , create sequence
     , create trigger
     , create view
     , create synonym
     , alter session
TO mddemo;

create user mddemo_clerk identified by clerk;
create user mddemo_mgr identified by mgr;

connect mddemo/mddemo

Rem Create some payroll-like tables...

create table payroll_emps
(	lastname varchar2(60) not null,
	firstname varchar2(20) not null,
	mi varchar2(2),
	suffix varchar2(10),
	DOB date not null,
	badge_no number(6) primary key,
	exempt varchar(1) not null,
	salary number (9,2),
	hourly_rate number (7,2)
)
/
create table payroll_timecards
(	badge_no number(6) references payroll_emps (badge_no),
	week number(2),
	job_id number(5),
	hours_worked number(4,2)
)
/
-- This is a dummy table used only to show that tables NOT starting with
-- 'PAYROLL' are NOT retrieved by payroll_demo.get_payroll_tables

create table audit_trail
(	action_time	DATE,
	lastname	VARCHAR2(60),
	action		LONG
)
/

Rem Then, create some grants...

grant update (salary,hourly_rate) on payroll_emps to mddemo_clerk;
grant ALL on payroll_emps to mddemo_mgr with grant option;

grant insert,update on payroll_timecards to mddemo_clerk;
grant ALL on payroll_timecards to mddemo_mgr with grant option;

Rem Then, create some indexes...

create index i_payroll_emps_name on payroll_emps(lastname);
create index i_payroll_emps_dob on payroll_emps(DOB);

create index i_payroll_timecards_badge on payroll_timecards(badge_no);

Rem Then, create some triggers (and required procedure)...

create or replace procedure check_sal( salary in number) as
begin
  return;  -- Fairly loose security here...
end;
/

create or replace trigger salary_trigger before insert or update of salary on payroll_emps
for each row when (new.salary > 150000)
call check_sal(:new.salary)
/

create or replace trigger hourly_trigger before update of hourly_rate on payroll_emps
for each row
begin :new.hourly_rate:=:old.hourly_rate;end;
/

--
-- Set up a table to hold the generated DDL
--
CREATE TABLE ddl (ddl CLOB, seqno NUMBER);

Rem Finally, create the PAYROLL_DEMO package itself.

CREATE OR REPLACE PACKAGE payroll_demo AS

   PROCEDURE get_payroll_tables;
END;
/
CREATE OR REPLACE PACKAGE BODY payroll_demo AS

-- GET_PAYROLL_TABLES: Fetch DDL for payroll tables and their dependent objects

PROCEDURE  get_payroll_tables IS

tableOpenHandle		NUMBER;
depObjOpenHandle	NUMBER;
tableTransHandle	NUMBER;
indexTransHandle	NUMBER;
schemaName		VARCHAR2(30);
tableName		VARCHAR2(30);
tableDDLs		sys.ku$_ddls;
tableDDL		sys.ku$_ddl;
parsedItems		sys.ku$_parsed_items;
depObjDDL		CLOB;
seqNo			NUMBER := 1;

TYPE obj_array_t IS VARRAY(3) OF VARCHAR2(30);

-- Load this array with the dependent object classes to be retrieved...
obj_array obj_array_t := obj_array_t('OBJECT_GRANT', 'INDEX', 'TRIGGER');

BEGIN

-- Open a handle for tables in the current schema.
  tableOpenHandle := dbms_metadata.open('TABLE');

-- Tell mdAPI to retrieve one table at a time. This call is not actually
-- necessary since 1 is the default... just showing the call.
  dbms_metadata.set_count(tableOpenHandle, 1);

-- Retrieve tables whose name starts with 'PAYROLL'. When the filter is
-- 'NAME_EXPR', the filter value string must include the SQL operator. This
-- gives the caller flexibility to use LIKE, IN, NOT IN, subqueries, etc.
  dbms_metadata.set_filter(tableOpenHandle, 'NAME_EXPR', 'LIKE ''PAYROLL%''');

-- There are no index-organized tables in the MDDEMO schema, so tell the API.
-- This eliminates one of the views it'll need to look in.
  dbms_metadata.set_filter(tableOpenHandle, 'IOT', FALSE);

-- Tell the mdAPI to parse out each table's schema and name separately so we
-- can use them to set up the calls to retrieve its dependent objects.
  dbms_metadata.set_parse_item(tableOpenHandle, 'SCHEMA');
  dbms_metadata.set_parse_item(tableOpenHandle, 'NAME');

-- Add the DDL transform so we get SQL creation DDL
  tableTransHandle := dbms_metadata.add_transform(tableOpenHandle, 'DDL');

-- Tell the XSL stylesheet we don't want physical storage information (storage,
-- tablespace, etc), and that we want a SQL terminator on each DDL. Notice that
-- these calls use the transform handle, not the open handle.
  dbms_metadata.set_transform_param(tableTransHandle,
				    'SEGMENT_ATTRIBUTES', FALSE);
  dbms_metadata.set_transform_param(tableTransHandle,
				    'SQLTERMINATOR', TRUE);

-- Ready to start fetching tables. We use the FETCH_DDL interface (rather than
-- FETCH_XML or FETCH_CLOB). This interface returns a SYS.KU$_DDLS; a table of
-- SYS.KU$_DDL objects. This is a table because some object types return
-- multiple DDL statements (like types / pkgs which have create header and 
-- body statements). Each KU$_DDL has a CLOB containing the 'CREATE foo'
-- statement plus a nested table of the parse items specified. In our case,
-- we asked for two parse items; Schema and Name. (NOTE: See admin/dbmsmeta.sql
-- for a more detailed description of these types)

  LOOP
    tableDDLs := dbms_metadata.fetch_ddl(tableOpenHandle);
    EXIT WHEN tableDDLs IS NULL;	-- Get out when no more payroll tables

-- In our case, we know there is only one row in tableDDLs (a KU$_DDLS tbl obj)
-- for the current table. Sometimes tables have multiple DDL statements;
-- eg, if constraints are applied as ALTER TABLE statements, but we didn't ask
-- for that option. So, rather than writing code to loop through tableDDLs,
-- we'll just work with the 1st row.
--
-- First, write the CREATE TABLE text to our output table then retrieve the
-- parsed schema and table names.
    tableDDL := tableDDLs(1);
    INSERT INTO ddl VALUES(tableDDL.ddltext, seqNo);
    seqNo := seqNo + 1;
    parsedItems := tableDDL.parsedItems;

-- Must check the name of the returned parse items as ordering isn't guaranteed
    FOR i IN 1..2 LOOP
      IF parsedItems(i).item = 'SCHEMA'
      THEN
        schemaName := parsedItems(i).value;
      ELSE
        tableName  := parsedItems(i).value;
      END IF;
    END LOOP;
 
-- Now, we want to retrieve all the dependent objects defined on the current
-- table: indexes, triggers and grants. Since all 'dependent' object types
-- have BASE_OBJECT_NAME and BASE_OBJECT_SCHEMA in common as filter criteria,
-- we'll set up a loop to get all objects of the 3 types, just changing the
-- OPEN context in each pass through the loop. Transform parameters are
-- different for each object type, so we'll only use one that's common to all;
-- SQLTERMINATOR.

    FOR i IN 1..3 LOOP
      depObjOpenHandle := dbms_metadata.open(obj_array(i));
      dbms_metadata.set_filter(depObjOpenHandle,'BASE_OBJECT_SCHEMA',
			       schemaName);
      dbms_metadata.set_filter(depObjOpenHandle,'BASE_OBJECT_NAME',tableName);

-- Add the DDL transform and say we want a SQL terminator
      indexTransHandle := dbms_metadata.add_transform(depObjOpenHandle, 'DDL');
      dbms_metadata.set_transform_param(indexTransHandle,
				        'SQLTERMINATOR', TRUE);

-- Retrieve dependent object DDLs as CLOBs and write them to table DDL.
      LOOP
        depObjDDL := dbms_metadata.fetch_clob(depObjOpenHandle);
        EXIT WHEN depObjDDL IS NULL;
        INSERT INTO ddl VALUES(depObjDDL, seqNo);
        seqNo := seqNo + 1;
      END LOOP;

-- Free resources allocated for current dependent object stream.
      dbms_metadata.close(depObjOpenHandle);

    END LOOP; -- End of fetch dependent objects loop

  END LOOP;   -- End of fetch table loop

-- Free resources allocated for table stream and close output file.
  dbms_metadata.close(tableOpenHandle);
  RETURN;

END;  -- of procedure get_payroll_tables

END payroll_demo;
/
