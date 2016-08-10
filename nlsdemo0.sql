Rem
Rem $Header: nlsdemo0.sql 23-jul-2003.12:21:16 chli Exp $
Rem
Rem nlsdemo0.sql
Rem
Rem Copyright (c) 2001, 2003, Oracle Corporation.  All rights reserved.  
Rem
Rem    NAME
Rem      nlsdemo0.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem	 This is the first sql that must be run before run NLS demo 
Rem      Insert extra multibyte data for nls linguistic index test
Rem	 create 3 linguistic index, plustrace role and plan_table  
Rem
Rem    NOTES
Rem      <Before you run this sql file, please set NLS_LANG to .we8dec>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    tnallath    05/09/02 - bug 2368367:.
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

insert into product_information(product_id) values(2728);
insert into product_information(product_id) values(2729); 
insert into product_information(product_id) values(2730); 
insert into product_information(product_id) values(2731); 
insert into product_information(product_id) values(2732); 
insert into product_information(product_id) values(2734); 

insert into product_descriptions values (2728,'D','abcd','abcd');
insert into product_descriptions values (2729,'D','Abcd','Abcd');
insert into product_descriptions values (2730,'D',Unistr('\00e4bcd'),'bcd');
insert into product_descriptions values (2731,'D',Unistr('\00c4bcd'),'bcd');


insert into product_descriptions values (2728
, 'ZHS'
, UNISTR('\6db2\6676\663e\793a\5668')
, 'LED DISPLAYER' 
);

insert into product_descriptions values (2729
, 'ZHS'
, UNISTR('\7535\89c6\673a')
, 'TV set'
);

insert into product_descriptions values(2730
,'ZHS'
,UNISTR('\6fc0\5149\5531\76d8')
,'Laser CD'
);

insert into product_descriptions values(2731
,'ZHS'
,UNISTR('\79fb\52a8\7535\8bdd')
,'Celluar phone'
);

insert into product_descriptions values(2732 
,'ZHS'
,UNISTR('\7535\51b0\7bb1')
,'Refrigerator');

Commit;


REM  Create Linguistic Index on Product_Descriptions Table's Translated_Description 

CONNECT OE/OE

ALTER SESSION SET NLS_COMP=ANSI;

ALTER SESSION SET QUERY_REWRITE_ENABLED=TRUE;

rem pause Create Generic_M Index on Translated_name NVARCHAR2 Column, Please Press < Enter> to continue...

ALTER SESSION SET NLS_SORT=GENERIC_M;

DROP INDEX NLS_GENERIC;

CREATE INDEX NLS_GENERIC ON product_descriptions( NLSSORT(translated_name, 'NLS_SORT=GENERIC_M'));

rem pause Create Simplified Chinese Storke sorting Index on Translated_name NVARCHAR2 Column, Please Press <enter> to continue

ALTER SESSION SET NLS_SORT=SCHINESE_STROKE_M;

DROP INDEX NLS_ZHSSTROKE;

CREATE INDEX NLS_ZHSSTROKE ON product_descriptions( NLSSORT(translated_name, 'NLS_SORT=SCHINESE_STROKE_M'));

rem pause Create Simplified Chinese Pinyin sorting Index on Translated_name NVARCHAR2 Column, Please Press <enter> to continue

ALTER SESSION SET NLS_SORT=SCHINESE_PINYIN_M;

DROP INDEX NLS_ZHSPINYIN;

CREATE INDEX NLS_ZHSPINYIN ON product_descriptions( NLSSORT(translated_name, 'NLS_SORT=SCHINESE_PINYIN_M'));

ANALYZE TABLE Product_descriptions COMPUTE STATISTICS;

Rem Set Autotrace On to see the execution plan

Prompt Please Input the password for User sys

CONNECT SYS/change_on_install AS SYSDBA

Rem Run ?/sqlplus/admin/plustrce.sql to Create role

@?/sqlplus/admin/plustrce.sql

GRANT plustrace To OE;

CONNECT OE/OE

Rem Run ?/rdbms/admin/utlxplan.sql to Create PLAN_TABLE

DROP TABLE PLAN_TABLE;

@?/rdbms/admin/utlxplan.sql

