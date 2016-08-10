Rem
Rem $Header: nlsdemo4.sql 23-jul-2003.11:29:59 chli Exp $
Rem
Rem nlsdemo4.sql
Rem
Rem Copyright (c) 2001, 2003, Oracle Corporation.  All rights reserved.  
Rem
Rem    NAME
Rem      nlsdemo3.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem 	 Using fine gained access control to show only one language data which is depending on your NLS_LANG setting
Rem	Say, if you set NLS_LANG to japanese_japan.ja16sjis, only japanese data is shown, if you set NLS_LANG to 'Simplified Chinese
Rem	_china.zhs16cgb231280', only chinese data is shown
Rem    NOTES
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
REM Please don't Forget to set NLS_LANG to what you want
REM 

CONNECT OE/OE

COL LANGUANE_ID FORMAT A3
COL TRANSLATED_NAME FORMAT A20
COL TRANSLATED_DESCRIPTION FORMAT A20

SET AUTOTRACE ON EXPLAIN

rem pause You will see only one language data

SELECT * FROM Product_descriptions;

