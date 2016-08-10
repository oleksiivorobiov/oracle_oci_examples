Rem
Rem $Header: anydset.sql 31-oct-2006.13:41:39 ytsai Exp $
Rem
Rem anydset.sql
Rem
Rem Copyright (c) 2006, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      anydset.sql - Demonstrates Sys.AnyDataSet feature .
Rem
Rem    DESCRIPTION
Rem      Sys.AnyDataSet contains Type description as well as a 
Rem	 set of values . This demo shows its usage.
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    ytsai       10/31/06 - fix connect
Rem    sjanardh    05/02/01 - Merged sjanardh_trans_adddemo
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
DROP USER ANYDSET_USER CASCADE ;
GRANT CONNECT,RESOURCE,DBA TO ANYDSET_USER IDENTIFIED BY ANYDSET_USER ;
CONNECT ANYDSET_USER/ANYDSET_USER

SET ECHO ON
SET SERVEROUTPUT ON SIZE 200000

--***********************************************************
--    This  demonstrates  the usage of Sys.AnyDataSet
--***********************************************************

create type person_type as object (
ssn     number ,
name    varchar2(20),
job     clob ,
salary  number(10,4)
);
/
create type phone_list as varray(100) of number ;
/
create type address_type as object (
apt	integer ,
street	char(30) ,
state	char(2) ,
zip	number(5)
);
/
create type address_list as table of address_type ;
/

--***********************************************************
--    Creates Procedure to display AnyDataSet value
--***********************************************************

create or replace procedure anydataset_display_value
(anyset in Sys.AnyDataSet ) as
as1     Sys.AnyDataSet ;
tn      varchar2(30) ;
tp      varchar2(30) ;
ct      number ;
rtn_val pls_integer ;
rw1     raw(30);
cl1     clob ;
o1	person_type ;
o2	address_type ;
vr	phone_list ;
nt	address_list ;
begin

as1     := anyset ;
tn      := as1.GetTypeName() ;
ct      := as1.GetCount() ;

if (tn = 'SYS.RAW' ) then
begin
	for i in 1..ct loop
		rtn_val := as1.GetInstance();
		rtn_val := as1.GetRaw(rw1);
		dbms_output.put_line(rw1);
	end loop ;
end ;
elsif (tn = 'SYS.CLOB' ) then
begin
	for i in 1..ct loop
		rtn_val := as1.GetInstance();
		rtn_val := as1.GetClob(cl1);
		if (cl1 is null) then
			dbms_output.put_line(' CLOB is NULL ' );
		else
			dbms_output.put_line(dbms_lob.substr(cl1));
		end if ;
	end loop ;
end ;
elsif (tn like '%PERSON_TYPE%'  ) then
begin
	for i in 1..ct loop
		rtn_val := as1.GetInstance();
		rtn_val := as1.GetObject(o1);
		dbms_output.put_line ('PERSON_TYPE  ' || o1.ssn || '  ' || o1.name
		|| '  '  || dbms_lob.substr(o1.job) || '  ' || o1.salary ) ;
	end loop ;
end ;
elsif (tn like '%PHONE_LIST%' ) then
begin
	for i in 1..ct loop
		rtn_val := as1.GetInstance();
		rtn_val := as1.GetCollection(vr);
		for j in 1..vr.count loop
			dbms_output.put_line ('PHONE_LIST(' || j || ')  ' || vr(j) ) ;
		end loop ;
	end loop ;
end ;
elsif (tn like '%ADDRESS_LIST%' ) then 
begin
	for i in 1..ct loop
		rtn_val := as1.GetInstance();
		rtn_val := as1.GetCollection(nt);
		for j in 1..nt.count loop
			dbms_output.put_line ('ADDRESS_TYPE(' || j || ')  ' || nt(j).apt || ' ' ||
			dbms_lob.substr(nt(j).street) || ' ' || nt(j).state || ' ' || nt(j).zip ) ;
		end loop ;
	end loop ;
end ;

end if ; 
end ;
/
show errors

--***********************************************************
--    Creates AnyDataSet,sets value and displays the value
--***********************************************************

declare
as1     Sys.AnyDataSet;
at1     Sys.AnyType ;
rw1	raw(200)  ;
begin
Sys.AnyDataSet.BeginCreate(DBMS_TYPES.TYPECODE_RAW,at1,as1);
for i in 1..30 loop
	as1.AddInstance();
	rw1 :=  utl_raw.cast_to_raw(rpad('0123',i,'0123')) ;
	as1.SetRaw(rw1);
end loop ; 
as1.EndCreate();
anydataset_display_value(as1);
end ;
/

declare
as1     Sys.AnyDataSet;
at1     Sys.AnyType ;
cl1     clob ;
begin 
Sys.AnyDataSet.BeginCreate(DBMS_TYPES.TYPECODE_CLOB,at1,as1);
for i in 1..10 loop 
	as1.AddInstance();
	cl1 := to_clob(rpad('Clob',i*10,'Clob')) ;
	as1.SetClob(cl1) ;
end loop ; 
as1.EndCreate();
anydataset_display_value(as1);
end ;
/

declare
as1	Sys.AnyDataSet;
at1     Sys.AnyType ;
begin
at1  := Sys.AnyType.GetPersistent('ANYDSET_USER','PERSON_TYPE') ;
Sys.AnyDataSet.BeginCreate(DBMS_TYPES.TYPECODE_OBJECT,at1,as1);
as1.AddInstance();
as1.SetObject(Person_type(1237659012,'Betty','HR Manager',80999.00));
as1.AddInstance();
as1.SetObject(Person_type(8097659100,'Clark',null,null));
as1.AddInstance();
as1.PieceWise() ;
as1.SetNumber(1023040078);
as1.SetVarchar2('James');
as1.SetClob('ADMINISTRAIVE ASSIST');
as1.SetNumber(60000.00);
as1.EndCreate();
anydataset_display_value(as1);
end ;
/

declare
as1     Sys.AnyDataSet;
at1     Sys.AnyType ;
begin
at1  := Sys.AnyType.GetPersistent('ANYDSET_USER','PHONE_LIST');
Sys.AnyDataSet.BeginCreate(DBMS_TYPES.TYPECODE_NAMEDCOLLECTION,at1,as1);
as1.AddInstance();
as1.SetCollection(PHONE_LIST(8095067000,0012034511,null));
as1.AddInstance();
as1.SetCollection(PHONE_LIST(3014556780,9012331099,1003073087,1105067890));
as1.EndCreate();
anydataset_display_value(as1);
end ;
/

declare
as1     Sys.AnyDataSet;
at1     Sys.AnyType ;
begin
at1  := Sys.AnyType.GetPersistent('ANYDSET_USER','ADDRESS_LIST');
Sys.AnyDataSet.BeginCreate(DBMS_TYPES.TYPECODE_NAMEDCOLLECTION,at1,as1);
as1.AddInstance();
as1.SetCollection(ADDRESS_LIST(ADDRESS_TYPE(113,'ABC','MA',90878),ADDRESS_TYPE(45,'DFG','MI',80912)) );
as1.AddInstance();
as1.PieceWise();
as1.SetObject(ADDRESS_TYPE(12,'NBC','TX',10764));
as1.SetObject(ADDRESS_TYPE(70,'MNS','CA',94067),TRUE) ;
as1.EndCreate();
anydataset_display_value(as1);
end ;
/


--***********************************************************
--  Drop objects created , user etc.
--***********************************************************

drop type address_list ; 
drop type phone_list ; 
drop type address_type ; 
drop type person_type ; 

CONNECT system/manager;
DROP USER ANYDSET_USER CASCADE ; 

SET SERVEROUTPUT OFF
SET ECHO OFF

