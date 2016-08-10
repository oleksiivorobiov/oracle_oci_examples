Rem
Rem $Header: nlsdemo5.sql 23-jul-2003.11:34:27 chli Exp $
Rem
Rem nlsdemo5.sql
Rem
Rem Copyright (c) 2001, 2003, Oracle Corporation.  All rights reserved.  
Rem
Rem    NAME
Rem      nlsdemo5.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem      <Clean extra nls data for nls demo>
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    huwang      02/22/01 - Merged huwang_nlsdemo
Rem    huwang      02/22/01 - Created
Rem

SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100
SET ECHO ON

REM Clean the indexes and Policy for the demo

Prompt Please enter the password for User SYS

connect sys/change_on_install as sysdba

BEGIN
  DBMS_RLS.DROP_POLICY('OE','PRODUCT_DESCRIPTIONS','NCHARFEATURE');
END;
/

CONNECT OE/OE

DROP FUNCTION secure_person;

Delete from product_descriptions where language_id in ('D','ZHS') and product_id in (2728,2729,2730,2731,2732);

Delete from product_information where product_id in (2728,2729,2730,2731,2732);

DROP INDEX NLS_GENERIC;

DROP INDEX NLS_ZHSSTROKE;

DROP INDEX NLS_ZHSPINYIN;

DROP TABLE PLAN_TABLE;


