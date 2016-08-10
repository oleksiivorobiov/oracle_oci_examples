Rem
Rem $Header: cdemoanydata2.sql 04-may-2001.16:09:55 jchai Exp $
Rem
Rem cdemoAnyData1.sql
Rem
Rem  Copyright (c) Oracle Corporation 2001. All Rights Reserved.
Rem
Rem    NAME
Rem      cdemoAnyData2.sql - Demo program for ANYDATA
Rem
Rem    DESCRIPTION
Rem      SQL script to prepare table anydatatab and address_object type
Rem
Rem    NOTES
Rem      Neet to run before cdemoAnyData2.
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

drop type foo
/
drop type basic_object
/

CREATE OR REPLACE TYPE foo as OBJECT
(
   a DATE,
   b VARCHAR2(50),
   c CLOB
)
/

create or replace type basic_object as object (state char(2), zip char(10),
                a3 raw(10), a4 date, a5 number)
/

