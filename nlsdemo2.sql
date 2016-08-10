Rem
Rem $Header: nlsdemo2.sql 23-jul-2003.11:28:40 chli Exp $
Rem
Rem nlsdemo2.sql
Rem
Rem Copyright (c) 2001, 2003, Oracle Corporation.  All rights reserved.  
Rem
Rem    NAME
Rem      nlsdemo2.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem      <Create Policy for fine-gain access control>
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    chli        07/23/03 - 
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

REM  Create Policy Function

CONNECT OE/OE 

DROP FUNCTION secure_person;

CREATE OR REPLACE FUNCTION secure_person ( object_schema  IN VARCHAR2, object_name IN VARCHAR2) return  VARCHAR2 IS
BEGIN
    RETURN( 'language_id=SYS_CONTEXT(''USERENV'',''LANG'')');
END;
/

SHOW ERRORS

REM Add and Enable The Policy

Prompt Please enter the password for User SYS 

connect sys/change_on_install as sysdba

BEGIN
  DBMS_RLS.DROP_POLICY('OE','PRODUCT_DESCRIPTIONS','NCHARFEATURE');
END;
/

BEGIN
  DBMS_RLS.ADD_POLICY('OE','PRODUCT_DESCRIPTIONS','NCHARFEATURE','OE','SECURE_PERSON','SELECT');
END;
/

