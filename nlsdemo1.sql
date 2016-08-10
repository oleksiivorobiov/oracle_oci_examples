Rem
Rem $Header: nlsdemo1.sql 23-jul-2003.11:27:27 chli Exp $
Rem
Rem nlsdemo1.sql
Rem
Rem Copyright (c) 2001, 2003, Oracle Corporation.  All rights reserved.  
Rem
Rem    NAME
Rem      nlsdemo1.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem      <Show all multi-language data in two different sorting : default(binary and generic_m.
Rem
Rem    NOTES
Rem      Before you run the sql file, please set NLS_LANG to .utf8 so that you can see all kind of chars in different languages
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

CONNECT OE/OE

SET AUTOTRACE ON EXPLAIN

COL LANGUANE_ID FORMAT A3
COL TRANSLATED_NAME FORMAT A20
COL TRANSLATED_DESCRIPTION FORMAT A20

rem pause  You may see the default sorting result for Translated_name NVARCHAR2 Column, Please Press <ENTER> to continue 
SELECT * FROM PRODUCT_DESCRIPTIONS
ORDER BY TRANSLATED_NAME;

rem pause  The following three ALTER statements change the sort setting to Generic_M linguistic sort 

ALTER SESSION SET NLS_COMP=ANSI;

ALTER SESSION SET NLS_SORT='GENERIC_M';

ALTER SESSION SET QUERY_REWRITE_ENABLED=TRUE;

rem pause  Now you will see the generic_m sorting result for Translated_name NVARCHAR2 Column, Please press<ENTER> to continue 

SELECT * FROM PRODUCT_DESCRIPTIONS
WHERE TRANSLATED_NAME > UNISTR('\0000')
ORDER BY TRANSLATED_NAME;





