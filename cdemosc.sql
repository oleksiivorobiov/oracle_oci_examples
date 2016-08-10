Rem
Rem $Header: cdemosc.sql 03-may-2001.17:06:41 jchai Exp $
Rem
Rem cdemosc.sql
Rem
Rem  Copyright (c) Oracle Corporation 2001. All Rights Reserved.
Rem
Rem    NAME
Rem      cdemosc.sql - Demo program for scrollable cursor.
Rem
Rem    DESCRIPTION
Rem      SQL script to prepare table empo and data in the table.
Rem
Rem    NOTES
Rem      Neet to run before cdemosc.
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    jchai       05/03/01 - connect as scott/tiger
Rem    ani         04/30/01 - Merged ani_ocidemo
Rem    ani         04/24/01 - Created
Rem

connect scott/tiger;
SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100
SET ECHO ON

drop table empo 
/
drop type evarray
/
drop type empaddr
/

create or replace type empaddr as object ( 
       state char(2) ,
       zip number ) 
/

create or replace type evarray is VARRAY(2) of number 
/

create table empo (empno number, 
       ename char(5), 
       addr empaddr, 
       ecoll evarray)
/

insert into empo values (1, 'abc1', empaddr('ca', 94061), evarray(13,145))
/
insert into empo values (2, 'abc2', empaddr('ca', 94062), evarray(23,245))
/
insert into empo values (3, 'abc3', empaddr('ca', 94063), evarray(33,345))
/
insert into empo values (4, 'abc4', empaddr('ca', 94064), evarray(43,445))
/
insert into empo values (5, 'abc5', empaddr('ca', 94065), evarray(53,545))
/
insert into empo values (6, 'abc6', empaddr('ca', 94066), evarray(63,645))
/
insert into empo values (7, 'abc7', empaddr('ca', 94067), evarray(73,745))
/
insert into empo values (8, 'abc8', empaddr('ca', 94068), evarray(83,85))
/
insert into empo values (9, 'abc9', empaddr('ca', 94069), evarray(93,95))
/
insert into empo values (10, 'abc10', empaddr('ca', 94060), evarray(103,1045))
/
insert into empo values (11, 'abc11', empaddr('ca', 94070), evarray(113,1045))
/
insert into empo values (12, 'abc12', empaddr('ca', 94071), evarray(123,1045))
/
insert into empo values (13, 'abc13', empaddr('ca', 94072), evarray(133,1045))
/
insert into empo values (14, 'abc14', empaddr('ca', 94073), evarray(143,1045))
/
