Rem
Rem $Header: nlsdemo3.sql 23-jul-2003.11:29:13 chli Exp $
Rem
Rem nlsdemo3.sql
Rem
Rem Copyright (c) 2001, 2003, Oracle Corporation.  All rights reserved.  
Rem
Rem    NAME
Rem      nlsdemo3.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem 	 Using fine gained access control to show only chinese data in 3 different sorting mode: Generic_m, stroke and Pinyin
Rem    NOTES
Rem      We suppose to show chinese data , please set NLS_LANG to 'Simplified Chinese_China.zhs16gbk'
Rem      Before you run this sql file, please run nlsdemo2.sql so that you can see the fine gained access control demo
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


REM 
REM Before you run the sql file, you must run nlsdemo2.sql
REM Please don't Forget to set NLS_LANG to 'Simplified Chinese_China.zhs16gbk' 
REM 

CONNECT OE/OE

COL LANGUANE_ID FORMAT A3
COL TRANSLATED_NAME FORMAT A20
COL TRANSLATED_DESCRIPTION FORMAT A20

SET AUTOTRACE ON EXPLAIN

ALTER SESSION SET NLS_COMP=ANSI;

ALTER SESSION SET QUERY_REWRITE_ENABLED=TRUE;

rem pause You will see the Generic_M sorting result for only Chinese translated name 

ALTER SESSION SET NLS_SORT=GENERIC_M;

SELECT * FROM Product_descriptions WHERE translated_name > UNISTR('\0000') ORDER BY translated_name;  

rem pause You will see the Storke sorting result for only Chinese translated name 

ALTER SESSION SET NLS_SORT=SCHINESE_STROKE_M;

SELECT * FROM Product_descriptions WHERE translated_name > UNISTR('\0000') ORDER BY translated_name;  

rem pause You will see the Pinyin sorting result for only Chinese translated name 

ALTER SESSION SET NLS_SORT=SCHINESE_PINYIN_M;

SELECT * FROM Product_descriptions WHERE translated_name > UNISTR('\0000') ORDER BY translated_name;  



