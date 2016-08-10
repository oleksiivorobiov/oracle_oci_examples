Rem
Rem $Header: cdemoanydata1.sql 04-may-2001.16:09:54 jchai Exp $
Rem
Rem cdemoAnyData1.sql
Rem
Rem  Copyright (c) Oracle Corporation 2001. All Rights Reserved.
Rem
Rem    NAME
Rem      cdemoAnyData1.sql - Demo program for ANYDATA
Rem
Rem    DESCRIPTION
Rem      SQL script to prepare table anydatatab and address_object type
Rem
Rem    NOTES
Rem      Neet to run before cdemoAnyData.
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    jchai       05/04/01 - Merged jchai_add_oci_demos_to_shiphome
Rem    ani         04/30/01 - Merged ani_ocidemo
Rem    ani         04/24/01 - Created
Rem

SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100
SET ECHO ON

connect scott/tiger
/
drop table anydatatab
/
drop table addr_objtab
/
drop type addr_tab
/
drop type address_object
/

CREATE OR REPLACE TYPE address_object AS OBJECT
(
  state varchar2(3),
  zip varchar2(13)
)
/

create or replace type addr_tab is table of address_object
/

create table addr_objtab of address_object
/
insert into addr_objtab values(address_object('CA','94065'))
/

CREATE TABLE anydatatab(col1 number, col2 Sys.AnyData)
/

