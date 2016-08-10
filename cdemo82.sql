rem
rem $Header: cdemo82.sql 18-jun-2004.14:31:26 stsun Exp $
rem
rem cdemo82.sql
rem
rem Copyright (c) 1996, 2004, Oracle. All rights reserved.  
rem
rem    NAME
rem      cdemo82.sql - sql to be executed before cdemo82
rem
rem    DESCRIPTION
rem      <short description of component this file declares/defines>
rem
rem    NOTES
rem      <other useful comments, qualifications, etc.>
rem
rem    MODIFIED   (MM/DD/YY)
rem    stsun       06/18/04 - system/manager instead of sysdba 
rem    mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem    svedala     09/11/98 - a "/" required after create type - bug 717842
rem    cchau       08/18/97 - enable dictionary protection
rem    echen       06/03/97 - fix name resolution problem
rem    azhao       04/02/97 - add as object for create type
rem    dchatter    07/19/96 - scott/tiger to cdemo82/cdemo82
rem    slari       07/15/96 - Created
rem

REMARK >>>> Set System Variables For Current SQLPlus Session <<<<
SET FEEDBACK 1
SET NUMWIDTH 10
SET PAGESIZE 100
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET DEFINE '^'

set echo on;

connect system/manager;

drop user cdemo82 cascade;

create user cdemo82 identified by cdemo82;

grant connect, resource to cdemo82;

connect cdemo82/cdemo82;

drop table customerval;

drop table customerobj;

drop table extaddr;

drop table embextaddr;

drop type embedded_address;

drop type address_object;

drop type person;

drop table emp;

create type address_object as object (state char(2), zip char(10));
/

create type embedded_address as object (state char(2), zip char(10),
                                        preaddr REF address_object);
/

drop type address_value;

create type address_value as object (state char(2), zip char(10));
/

create table customerval (custno number, addr address_value);

insert into customerval values(100, address_value('CA', '94065'));

create table extaddr of address_object;

create table customerobj (custno number, addr REF address_object);

insert into extaddr values (address_object('CA', '94065'));

insert into customerobj values(1000, null);

update customerobj
  set addr = (select ref(e) from extaddr e where e.zip='94065');

insert into extaddr values (address_object('CA', '98765'));

insert into extaddr values (address_object('CA', '95117'));

select REFTOHEX(ref(e)) from extaddr e;

create table embextaddr of embedded_address;

insert into embextaddr values (embedded_address('CA', '95117', NULL));

drop table extper;

drop table empref;

drop table emp;

drop type person;

create type person as object ( name char(20), age number,
                               address address_object );
/

create table emp (emp_id number, emp_info person);

create table empref (emp_id number, emp_info REF person);

create table extper of person;

create or replace procedure upd_addr(addr IN OUT address_object) is
begin
   addr.state := 'CA';
   addr.zip := '95117';
end;
/

commit;

set echo off;



