Rem
Rem $Header: anytype.sql 31-oct-2006.13:41:17 ytsai Exp $
Rem
Rem anytype.sql
Rem
Rem Copyright (c) 2001, 2006, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      anytype.sql - Demonstrates Sys.AnyType feature .
Rem
Rem    DESCRIPTION
Rem      Sys.AnyType stores Type descriptions and this demo 
Rem	 shows its usage . 
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    ytsai       10/31/06 - fix connect
Rem    sjanardh    08/03/01 - Fix CSID part. 
Rem    sjanardh    05/02/01 - Merged sjanardh_trans_adddemo
Rem    sjanardh    05/02/01 - 
Rem    sjanardh    04/30/01 - Created
Rem

SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100
SET ECHO ON

CONNECT system/manager;
DROP USER ANYTYPE_USER CASCADE ;

GRANT CONNECT,RESOURCE,DBA TO ANYTYPE_USER IDENTIFIED BY ANYTYPE_USER ;
CONNECT ANYTYPE_USER/ANYTYPE_USER

SET ECHO ON
SET SERVEROUTPUT ON SIZE 20000 

--***********************************************************
--    This  demonstrates  the usage of Sys.AnyType 
--***********************************************************

--Creating types required  for the demo.

create type object_type1  as object (
col1	number ,
col2	varchar2(30) ,
col3	clob  ,
col4	raw(10)
);
/

create type object_type2 as object (
col1	number ,
col2	blob
);
/

create type object_type3 as object (
col1	char(20),
col2	date 
);
/

create type object_type4 as object (    
number_col	number ,        
object_col	object_type1
);
/

create type varray_type1  as varray(5) of date ;
/
create type varray_type2  as varray(5) of object_type2 ;
/

create type nstd_type1 as table of number ;
/
create type nstd_type2 as table of object_type4 ;
/

--***********************************************************
--Procedures to display AnyType information.
--***********************************************************

create procedure display_anytype_info(an in sys.anytype) as
rtn	number ;
pr	pls_integer ;
sc	pls_integer ;
ln	pls_integer ;
cs	pls_integer ;
cf	pls_integer ;
sch	varchar2(30) ;
tn	varchar2(30) ;
vr	varchar2(30) ;
cn	pls_integer  ;
csid	pls_integer  ;
begin
	--Get the character set id of database character set .
	select nls_charset_id(value) into csid from nls_database_parameters
			where parameter='NLS_CHARACTERSET' ;

	rtn := an.GetInfo(pr,sc,ln,cs,cf,sch,tn,vr,cn) ;

	dbms_output.put_line('-         Precision ' || pr ) ;
	dbms_output.put_line('-         Scale     ' || sc ) ;
	dbms_output.put_line('-         Length    ' || ln ) ;
	if (cs is not null ) then 
	begin
		if (cs = csid ) then
			dbms_output.put_line('-         CSID      is same as db charset id' ) ;
		else
			dbms_output.put_line('-         CSID      is not same as db charset id') ;	
		end if ;
	end ;
	else
		dbms_output.put_line('-         CSID      is null ' ) ;
	end if ;
	dbms_output.put_line('-         CSFRM     ' || cf ) ;
	dbms_output.put_line('-         Schema    ' || sch) ;
	dbms_output.put_line('-         Type name ' || tn ) ;
	dbms_output.put_line('-         Version   ' || vr ) ;
	dbms_output.put_line('-         Count     ' || cn ) ;
end ;
/

create or replace procedure display_attribute_element_info(an1 in sys.anytype) as
an	sys.anytype ;
rtn	number ;
pos	pls_integer ;
pr	pls_integer ;
sc	pls_integer ;
ln	pls_integer ;
cs	pls_integer ;
cf	pls_integer ;
sch	varchar2(20) ;
tn	varchar2(20) ;
vr	varchar2(30) ;
cn	pls_integer ;
name	varchar2(30) ;
j	number ;
csid	pls_integer  ;
begin
	--Get the character set id of database character set .
	select nls_charset_id(value) into csid from nls_database_parameters
			where parameter='NLS_CHARACTERSET' ;

	--Get the count , number of elements.
	rtn := an1.GetInfo(pr,sc,ln,cs,cf,sch,tn,vr,cn) ;

	--For nested table type count is null.
	if (cn is null ) then 
		cn := 1 ;
	end if ;

	for j in 1..cn loop
	begin

		--Get attribute element information.
		rtn := an1.GetAttreleminfo(j,pr,sc,ln,cs,cf,an,name) ;

		dbms_output.put_line(' --------------------  ');
		dbms_output.put_line(' COLUMN      ' || name );

		--Attribute is returned as an AnyType in variable an.
		--If it is not null then we want to describe it .
		if (an is not null) then
			dbms_output.put_line(' - ');
			dbms_output.put_line(' desc ' || name );
			--Display information of attribute 
			display_anytype_info(an) ;
			dbms_output.put_line(' - ');
		end if ;

		--Rest of the information .
		dbms_output.put_line(' Precision ' || pr ) ;
		dbms_output.put_line(' Scale     ' || sc ) ;
		dbms_output.put_line(' Length    ' || ln ) ;
		if (cs is not null ) then 
		begin
	        	if (cs = csid ) then
       				dbms_output.put_line('-         CSID      is same as db charset id' ) ;
	        	else
                		dbms_output.put_line('-         CSID      is not same as db charset id') ;
	        	end if ;
		end ;
	        else
                	dbms_output.put_line('-         CSID      is null ' ) ;
		end if ;
		dbms_output.put_line(' CSFRM     ' || cf ) ;
		dbms_output.put_line(' --------------------  ');

	exception
		when others then
			 dbms_output.put_line(SQLERRM);
	end ;
	end loop ;
end ;
/


--***********************************************************
--  The  following blocks show how to create AnyTypes from
--      from persistent database types .
--***********************************************************

--For object_type1
declare
any_type1	Sys.AnyType ; 
begin
	any_type1 := Sys.AnyType.GetPersistent('ANYTYPE_USER','OBJECT_TYPE1');
	display_anytype_info(any_type1);
end;
/
declare
any_type1       Sys.AnyType ;
begin
        any_type1 := Sys.AnyType.GetPersistent('ANYTYPE_USER','OBJECT_TYPE1');
	display_attribute_element_info(any_type1);
end ;
/

--For object_type2
declare
any_type1       Sys.AnyType ;
begin
        any_type1 := Sys.AnyType.GetPersistent('ANYTYPE_USER','OBJECT_TYPE2');
        display_anytype_info(any_type1);
end;
/
declare
any_type1       Sys.AnyType ;
begin
        any_type1 := Sys.AnyType.GetPersistent('ANYTYPE_USER','OBJECT_TYPE2');
        display_attribute_element_info(any_type1);
end ;
/

--For object_type3
declare
any_type1       Sys.AnyType ;
begin
        any_type1 := Sys.AnyType.GetPersistent('ANYTYPE_USER','OBJECT_TYPE3');
        display_anytype_info(any_type1);
end;
/
declare
any_type1       Sys.AnyType ;
begin
        any_type1 := Sys.AnyType.GetPersistent('ANYTYPE_USER','OBJECT_TYPE3');
        display_attribute_element_info(any_type1);
end ;
/

--For object_type4
declare
any_type1       Sys.AnyType ;
begin
        any_type1 := Sys.AnyType.GetPersistent('ANYTYPE_USER','OBJECT_TYPE4');
        display_anytype_info(any_type1);
end;
/
declare
any_type1       Sys.AnyType ;
begin
        any_type1 := Sys.AnyType.GetPersistent('ANYTYPE_USER','OBJECT_TYPE4');
        display_attribute_element_info(any_type1);
end ;
/

--For varray_type1
declare
any_type1       Sys.AnyType ;
begin
        any_type1 := Sys.AnyType.GetPersistent('ANYTYPE_USER','VARRAY_TYPE1');
        display_anytype_info(any_type1);
end;
/
declare
any_type1       Sys.AnyType ;
begin
        any_type1 := Sys.AnyType.GetPersistent('ANYTYPE_USER','VARRAY_TYPE1');
        display_attribute_element_info(any_type1);
end ;
/

--For varray_type2
declare
any_type1       Sys.AnyType ;
begin
        any_type1 := Sys.AnyType.GetPersistent('ANYTYPE_USER','VARRAY_TYPE2');
        display_anytype_info(any_type1);
end;
/
declare
any_type1       Sys.AnyType ;
begin
        any_type1 := Sys.AnyType.GetPersistent('ANYTYPE_USER','VARRAY_TYPE2');
        display_attribute_element_info(any_type1);
end ;
/

--For nstd_type1
declare
any_type1       Sys.AnyType ;
begin
        any_type1 := Sys.AnyType.GetPersistent('ANYTYPE_USER','NSTD_TYPE1');
        display_anytype_info(any_type1);
end;
/
declare
any_type1       Sys.AnyType ;
begin
        any_type1 := Sys.AnyType.GetPersistent('ANYTYPE_USER','NSTD_TYPE1');
        display_attribute_element_info(any_type1);
end ;
/

--For nstd_type1
declare
any_type1       Sys.AnyType ;
begin
        any_type1 := Sys.AnyType.GetPersistent('ANYTYPE_USER','NSTD_TYPE2');
        display_anytype_info(any_type1);
end;
/
declare
any_type1       Sys.AnyType ;
begin
        any_type1 := Sys.AnyType.GetPersistent('ANYTYPE_USER','NSTD_TYPE2');
        display_attribute_element_info(any_type1);
end ;
/

--***********************************************************
--  Create Transient AnyTypes in PL/SQL blocks .
--***********************************************************

--Transient AnytType of Date .
declare
an1  sys.anytype ;
begin
	Sys.AnyType.BeginCreate(DBMS_TYPES.TYPECODE_DATE,an1) ;
	an1.SetInfo(null,null,null,null,null,null,null);
	an1.EndCreate() ;
	display_anytype_info(an1);
end ;
/

--Transient AnytType of Varchar.
declare
an1  sys.anytype ;
begin
	Sys.AnyType.BeginCreate(DBMS_TYPES.TYPECODE_VARCHAR,an1) ;
	an1.SetInfo(null,null,20,null,null,null,null);
	an1.EndCreate() ;
	display_anytype_info(an1);
end ;
/

--Transient AnytType of Object type.
declare
an2  Sys.AnyType ;
begin
	Sys.AnyType.BeginCreate(DBMS_TYPES.TYPECODE_OBJECT,an2) ;
	an2.AddAttr('COL1',DBMS_TYPES.TYPECODE_NUMBER,null,null,null,null,null);
	an2.AddAttr('COL2',DBMS_TYPES.TYPECODE_CLOB,null,null,null,null,null);
	an2.AddAttr('COL3',DBMS_TYPES.TYPECODE_DATE,null,null,null,null,null);
	an2.EndCreate() ;
	display_anytype_info(an2);
	display_attribute_element_info(an2);
end ;
/

--Transient AnytType of Collection type.
declare
an1  Sys.AnyType ;
begin
	Sys.AnyType.BeginCreate(DBMS_TYPES.TYPECODE_NAMEDCOLLECTION,an1);
	an1.SetInfo(null,null,null,null,null,null,DBMS_TYPES.TYPECODE_RAW,0) ;
	an1.EndCreate() ;
	display_anytype_info(an1);
end ;
/

--Transient AnytType of Persistent types.
declare
an1  Sys.AnyType ;
an2  Sys.AnyType ;
begin
	an1 := Sys.AnyType.GetPersistent('ANYTYPE_USER','OBJECT_TYPE1');
	Sys.AnyType.BeginCreate(DBMS_TYPES.TYPECODE_NAMEDCOLLECTION,an2);
	an2.SetInfo(null,null,null,null,null,an1,DBMS_TYPES.TYPECODE_OBJECT,0);
	an2.EndCreate() ;
        display_anytype_info(an2);
end ;
/

--***********************************************************
--    Drop all objects created for the demo.
--***********************************************************

drop type varray_type1 ;
drop type varray_type2 ;
drop type nstd_type1   ;
drop type nstd_type2   ;
drop type object_type4 ;
drop type object_type3 ;
drop type object_type2 ;
drop type object_type1 ;

CONNECT system/manager;
DROP USER ANYTYPE_USER CASCADE ;

set serveroutput off  
set echo off 


