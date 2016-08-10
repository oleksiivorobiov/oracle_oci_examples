Rem
Rem $Header: anydata.sql 31-oct-2006.13:41:28 ytsai Exp $
Rem
Rem anydata.sql
Rem
Rem Copyright (c) 2006, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      anydata.sql - Demonstrates Sys.AnyData feature.
Rem
Rem    DESCRIPTION
Rem      Sys.AnyData stores contains Type description as wells as 
Rem	 a value . This demo shows its usage .
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
GRANT CONNECT,RESOURCE,DBA TO ANYDATA_USER IDENTIFIED BY ANYDATA_USER ;
GRANT READ ON DIRECTORY GPFSD1 TO ANYDATA_USER WITH GRANT OPTION ;
CONNECT ANYDATA_USER/ANYDATA_USER

SET ECHO ON
SET SERVEROUTPUT ON SIZE 200000

--***********************************************************
--    This  demonstrates  the usage of Sys.AnyData
--***********************************************************

create type person_type as object (
ssn	number ,
name	varchar2(20),
job	char(10) ,
salary	number(10,4)
);
/
create type phone_list as varray(100) of number ;
/
create type date_list as table of date;
/
create table anydata_tab (col1  number , col2 Sys.AnyData ) ; 

--***********************************************************
--    Creating AnyData type at one go using Convert functions
--***********************************************************

insert into anydata_tab values (10,Sys.AnyData.ConvertNumber(100.101)) ;
insert into anydata_tab values (20,Sys.AnyData.ConvertDate('01-JAN-1901'));
insert into anydata_tab values (30,Sys.AnyData.ConvertChar('thirty')) ;
insert into anydata_tab values (40,Sys.AnyData.ConvertVarchar('forty')) ;
insert into anydata_tab values (50,Sys.AnyData.ConvertVarchar2('fifty'));
insert into anydata_tab values (60,Sys.AnyData.ConvertRaw(utl_raw.cast_to_raw('0908FFGGA')) );

--Cannot insert an AnyData of lob type into a table .
insert into anydata_tab values (70,Sys.AnyData.ConvertBlob(to_blob('AABB98076'))) ;
insert into anydata_tab values (80,Sys.AnyData.ConvertClob('eighty') ) ;


insert into anydata_tab values (90,Sys.AnyData.ConvertBfile(BFILENAME('GPFSD1', 'gpfdf1.dat')));
insert into anydata_tab values (100,
	Sys.AnyData.ConvertObject(Person_Type(123456767,'Harry','Manager',1500.0909)));

insert into anydata_tab values (110,
	Sys.AnyData.ConvertCollection(phone_list(9774567878,4075689000,1238761020)));

insert into anydata_tab values (120,
	Sys.AnyData.ConvertCollection(date_list('01-JAN-1910','02-FEB-1920','03-MAR-1930')) );

commit ;

--***********************************************************
--    Procedures to display AnyData information.
--***********************************************************

--Procedure takes in two parameters , first is an AnyData , second 
--is a number to indicate whether it has to be described piecewise
--or not .

create or replace procedure display_anydata_value( ad in sys.AnyData,piece_wise in number ) as
an           sys.AnyType ;
ad2          sys.AnyData ;
type_code    pls_integer ;
type_name    varchar2(30);
rtn_val      pls_integer ;
n1           number ;
fl1	     number(10,4) ;
vc1          varchar(10) ;
vc2          varchar2(30);
ch1          char(10) ;
d1           date ;
rw1          raw(10) ;
bl1          blob ;
cl1          clob ;
bf1          bfile ;
ob1          person_type ;
vry1         phone_list ;
nstd1        date_list ;
begin

type_code   := ad.GetType(an) ;
type_name   := ad.GetTypeName() ;

if (type_code = DBMS_TYPES.TYPECODE_NUMBER) then
begin
        rtn_val     := ad.GetNumber(n1) ;
        dbms_output.put_line (' NUMBER : ' || n1) ;
end ;

elsif (type_code = DBMS_TYPES.TYPECODE_VARCHAR) then
begin
         rtn_val     := ad.GetVarchar(vc1) ;
         dbms_output.put_line (' VARCHAR : ' || vc1) ;
end ;

elsif (type_code = DBMS_TYPES.TYPECODE_VARCHAR2) then
begin
        rtn_val     := ad.GetVarchar2(vc2) ;
        dbms_output.put_line (' VARCHAR2 : ' || vc2) ;
end ;

elsif (type_code = DBMS_TYPES.TYPECODE_CHAR) then
begin
        rtn_val     := ad.GetChar(ch1) ;
        dbms_output.put_line (' CHAR : ' || ch1) ;
end ;

elsif (type_code = DBMS_TYPES.TYPECODE_DATE) then
begin
        rtn_val     := ad.GetDate(d1) ;
        dbms_output.put_line (' DATE : ' || d1 ) ;
end ;

elsif (type_code = DBMS_TYPES.TYPECODE_RAW) then
begin
        rtn_val     := ad.GetRaw(rw1);
        dbms_output.put_line (' RAW : ' || rw1 ) ;
end ;

elsif (type_code = DBMS_TYPES.TYPECODE_BLOB) then
begin
        rtn_val     := ad.GetBlob(bl1);
        dbms_output.put_line (' BLOB : ' || dbms_lob.substr(bl1) );
end ;

elsif (type_code = DBMS_TYPES.TYPECODE_CLOB) then
begin
        rtn_val     := ad.GetClob(cl1);
        dbms_output.put_line (' CLOB : ' || dbms_lob.substr(cl1) );
end ;

elsif (type_code = DBMS_TYPES.TYPECODE_BFILE) then
declare
	amt     pls_integer ;
	offset  pls_integer := 1 ;
	bff     raw(100) ;
begin

        rtn_val     := ad.GetBfile(bf1);
        dbms_lob.fileopen(bf1,dbms_lob.file_readonly);
        amt         := dbms_lob.getlength(bf1) ;
        dbms_lob.read(bf1,amt,offset,bff);
        dbms_output.put_line (' BFILE : ' || utl_raw.cast_to_varchar2(bff) );
        dbms_lob.filecloseall;
end ;

elsif (type_code = DBMS_TYPES.TYPECODE_OBJECT) then
begin
        type_name   := ad.GetTypeName() ;
        if(type_name  like '%PERSON_TYPE%' ) then
        begin
                if(piece_wise = 0 ) then
                begin
                        dbms_output.put_line(' Accessing the whole ADT1 ' ) ;
                        rtn_val := ad.GetObject(ob1) ;
                        dbms_output.put_line('ADT1: ' || ob1.ssn || ' ' || ob1.name ||
                        ' ' || ob1.job || ' ' || ob1.salary ) ;
                end ;
                elsif (piece_wise =1 ) then
                begin
                        dbms_output.put_line(' Accessing ADT1 Piecewise ' );
                        ad2 := ad ;
                        ad2.PieceWise();
                        rtn_val := ad2.GetNumber(n1);
                        rtn_val := ad2.GetVarChar2(vc2);
                        rtn_val := ad2.GetChar(ch1);
			rtn_val := ad2.GetNumber(fl1);
                        dbms_output.put_line('ADT1: ' || n1 || ' ' || vc2 || ' ' ||
                        ch1  || ' ' || n1 );
                end ;
                end if ;
        end ;
        end if ;
end ;
elsif (type_code = DBMS_TYPES.TYPECODE_NAMEDCOLLECTION) then
begin
        type_name   := ad.GetTypeName() ;
        if(type_name like '%PHONE_LIST%') then
        begin
                dbms_output.put_line(' Accessing the whole VARRAY ' );
                rtn_val := ad.GetCollection(vry1);
                for i in 1..vry1.count loop
                        dbms_output.put_line('phone_list(' || i || ') ' || vry1(i) ) ; 
                end loop ;
        end ;
        elsif (type_name like '%DATE_LIST%' ) then
        begin
                dbms_output.put_line(' Accessing the whole Nested Table ' );
                rtn_val := ad.GetCollection(nstd1) ;
                for i in 1..nstd1.count loop
			dbms_output.put_line(' NSTD1(' ||  i || ')  ' || nstd1(i) ) ;
                end loop ;
        end ;
        end if ;
end ;
end if ;
end ;
/

--***********************************************************
--  Dsiplay AnyData stored in table 
--***********************************************************

declare 
ad	Sys.AnyData ; 
begin
for i in 1..12 loop  
	begin
		select col2 into ad from anydata_tab where col1=i*10 ;
		dbms_output.put_line('************************************') ;
		dbms_output.put_line('    Display row ' || i*10 ) ;
		dbms_output.put_line('************************************') ;
		display_anydata_value(ad,0) ;
	exception when others then
		dbms_output.put_line(SQLERRM) ;
	end ;
end loop ;
end ;
/

declare
ad      Sys.AnyData ;
begin
select col2 into ad from anydata_tab where col1=100 ;
dbms_output.put_line('************************************') ;
dbms_output.put_line('    Display ADT piecewise  ') ;
dbms_output.put_line('************************************') ;
display_anydata_value(ad,1) ;
end ;
/
--***********************************************************
--  Create AnyData of LOBs in PL/SQL blocks and display data 
--***********************************************************

declare
ad      sys.anydata ;
begin
ad := Sys.AnyData.ConvertBlob(to_blob('AABB98076')) ;
display_anydata_value(ad,0) ;
end ;
/

declare
ad      sys.anydata ;
begin
ad := Sys.AnyData.ConvertClob('eighty') ;
display_anydata_value(ad,0) ;
end ;
/

--***********************************************************
--  Create transient AnyData and display its value 
--***********************************************************

declare
a1      SYS.AnyType;
a2      SYS.AnyType;
ad1     Sys.AnyData;
ad2     Sys.AnyData;
begin
a1  := SYS.AnyType.GetPersistent('ANYDATA_USER','PERSON_TYPE') ;
Sys.AnyData.BeginCreate(a1,ad1) ;
ad1.SetNumber(678905435,FALSE);
ad1.SetVarChar2('Emily',FALSE);
ad1.SetChar('DIRECTOR',FALSE) ;
ad1.SetNumber(1020.20,FALSE);
ad1.EndCreate();
begin
	--Inserting transient type into table not supported.
	--This will raise an error .
	insert into anydata_tab values (130,ad1) ; 
exception when others then
	dbms_output.put_line(SQLERRM);
end ;
display_anydata_value(ad1,1) ;
end ;
/

CONNECT system/manager;
DROP USER ANYDATA_USER CASCADE ;

SET SERVEROUTPUT OFF
SET ECHO OFF 

