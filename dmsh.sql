--------------------------------------------------------------------------------
--
-- $Header: dmsh.sql 11-jul-2005.15:09:03 ktaylor Exp $
--
-- dmsh.sql
--
--  Copyright (c) 2001, 2005, Oracle. All rights reserved.
--
--    NAME
--    dmsh.sql
--
--    DESCRIPTION
--      This script creates views and tables using SH data
--      in the schema of the data mining user. These tables/views
--      are the datasets used by the Oracle Data Mining demo programs.
--      This script also creates an index and index preference for
--      text mining.
--    NOTES
--       The script assumes that the full SH schema is already created and the
--       necessary SELECTs have been granted (See dmshgrants.sql). This script runs in 
--       the schema of the data mining user.
--       mining_data_*_str_v views : Used for OC
--       mining_data_*_v views : Used for mining (no text)
--       market_basket_v view  : Used for association rules
--       mining_*_text tables: Used for Text mining in ODM Java API
--       mining_*_nested_text tables: Used for Text mining in DBMS_DATA_MINING API
--
--    MODIFIED   (MM/DD/YY)
--       ktaylor     07/11/05 - minor edits to comments
--       xbarr       03/14/05 - add purge for object drop 
--       cbhagwat    11/09/04 - Using collect 
--       bmilenov    10/20/04 - Add one-class demo view 
--       cbhagwat    09/17/04 - Bug 3881118 
--       xbarr       08/17/04 - create view for OC
--       xbarr       06/25/04 - xbarr_dm_rdbms_migration
--       cbhagwat    11/11/03 - Text NMF_CLUSTERING => SVM_CLASSIFIER
--       cbhagwat    10/10/03 - Remove DROPs
--       cbhagwat    10/10/03 - Remove Sh grants
--       cbhagwat    10/09/03 - Add nested table creatin using Text tf
--       cbhagwat    10/08/03 - cbhagwat_txn109150
--       cbhagwat    10/08/03 - creation
--
--------------------------------------------------------------------------------
--
-- Creates data mining views on SH data
--
-- Build, apply views for OC
CREATE VIEW mining_data_apply_str_v(
CUST_ID,
CUST_GENDER,
AGE,
CUST_MARITAL_STATUS,
COUNTRY_NAME,
CUST_INCOME_LEVEL, 
EDUCATION,
OCCUPATION,
HOUSEHOLD_SIZE,
YRS_RESIDENCE,
AFFINITY_CARD,
BULK_PACK_DISKETTES,
FLAT_PANEL_MONITOR,
HOME_THEATER_PACKAGE,
BOOKKEEPING_APPLICATION,
PRINTER_SUPPLIES,
Y_BOX_GAMES,
OS_DOC_SET_KANJI)
AS SELECT
 a.CUST_ID,
 a.CUST_GENDER,
 2003-a.CUST_YEAR_OF_BIRTH AGE,
 a.CUST_MARITAL_STATUS,
 c.COUNTRY_NAME,
 a.CUST_INCOME_LEVEL,
 b.EDUCATION,
 b.OCCUPATION,
 b.HOUSEHOLD_SIZE,
 b.YRS_RESIDENCE,
 SUBSTR(TO_CHAR(b.AFFINITY_CARD),1,1),
 SUBSTR(TO_CHAR(b.BULK_PACK_DISKETTES),1,1),
 SUBSTR(TO_CHAR(b.FLAT_PANEL_MONITOR),1,1),
 SUBSTR(TO_CHAR(b.HOME_THEATER_PACKAGE),1,1),
 SUBSTR(TO_CHAR(b.BOOKKEEPING_APPLICATION),1,1),
 SUBSTR(TO_CHAR(b.PRINTER_SUPPLIES),1,1),
 SUBSTR(TO_CHAR(b.Y_BOX_GAMES),1,1),
 SUBSTR(TO_CHAR(b.OS_DOC_SET_KANJI),1,1)
FROM
 sh.customers a,
 sh.supplementary_demographics b,
 sh.countries c
WHERE
 a.CUST_ID = b.CUST_ID
 AND a.country_id  = c.country_id
 AND a.cust_id between 100001 and 101500;

CREATE VIEW mining_data_build_str_v(
CUST_ID,
CUST_GENDER,
AGE,
CUST_MARITAL_STATUS,
COUNTRY_NAME,
CUST_INCOME_LEVEL,
EDUCATION,
OCCUPATION,
HOUSEHOLD_SIZE,
YRS_RESIDENCE,
AFFINITY_CARD,
BULK_PACK_DISKETTES,
FLAT_PANEL_MONITOR,
HOME_THEATER_PACKAGE,
BOOKKEEPING_APPLICATION,
PRINTER_SUPPLIES,
Y_BOX_GAMES,
OS_DOC_SET_KANJI)
AS SELECT
 a.CUST_ID,
 a.CUST_GENDER,
 2003-a.CUST_YEAR_OF_BIRTH AGE,
 a.CUST_MARITAL_STATUS,
 c.COUNTRY_NAME,
 a.CUST_INCOME_LEVEL,
 b.EDUCATION,
 b.OCCUPATION,
 b.HOUSEHOLD_SIZE,
 b.YRS_RESIDENCE,
 SUBSTR(TO_CHAR(b.AFFINITY_CARD),1,1),
 SUBSTR(TO_CHAR(b.BULK_PACK_DISKETTES),1,1),
 SUBSTR(TO_CHAR(b.FLAT_PANEL_MONITOR),1,1),
 SUBSTR(TO_CHAR(b.HOME_THEATER_PACKAGE),1,1),
 SUBSTR(TO_CHAR(b.BOOKKEEPING_APPLICATION),1,1),
 SUBSTR(TO_CHAR(b.PRINTER_SUPPLIES),1,1),
 SUBSTR(TO_CHAR(b.Y_BOX_GAMES),1,1),
 SUBSTR(TO_CHAR(b.OS_DOC_SET_KANJI),1,1)
FROM
 sh.customers a,
 sh.supplementary_demographics b,
 sh.countries c
WHERE
 a.CUST_ID = b.CUST_ID
 AND a.country_id  = c.country_id
 AND a.cust_id between 101501 and 103000;

-- Build , test and apply views, no text
CREATE VIEW mining_data_apply_v AS
SELECT
 a.CUST_ID,
 a.CUST_GENDER,
 2003-a.CUST_YEAR_OF_BIRTH AGE,
 a.CUST_MARITAL_STATUS,
 c.COUNTRY_NAME,
 a.CUST_INCOME_LEVEL,
 b.EDUCATION,
 b.OCCUPATION,
 b.HOUSEHOLD_SIZE,
 b.YRS_RESIDENCE,
 b.AFFINITY_CARD,
 b.BULK_PACK_DISKETTES,
 b.FLAT_PANEL_MONITOR,
 b.HOME_THEATER_PACKAGE,
 b.BOOKKEEPING_APPLICATION,
 b.PRINTER_SUPPLIES,
 b.Y_BOX_GAMES,
 b.OS_DOC_SET_KANJI
FROM
 sh.customers a,
 sh.supplementary_demographics b,
 sh.countries c
WHERE
 a.CUST_ID = b.CUST_ID
 AND a.country_id  = c.country_id
 AND a.cust_id between 100001 and 101500;
 
CREATE VIEW mining_data_build_v AS
SELECT
 a.CUST_ID,
 a.CUST_GENDER,
 2003-a.CUST_YEAR_OF_BIRTH AGE,
 a.CUST_MARITAL_STATUS,
 c.COUNTRY_NAME,
 a.CUST_INCOME_LEVEL,
 b.EDUCATION,
 b.OCCUPATION,
 b.HOUSEHOLD_SIZE,
 b.YRS_RESIDENCE,
 b.AFFINITY_CARD,
 b.BULK_PACK_DISKETTES,
 b.FLAT_PANEL_MONITOR,
 b.HOME_THEATER_PACKAGE,
 b.BOOKKEEPING_APPLICATION,
 b.PRINTER_SUPPLIES,
 b.Y_BOX_GAMES,
 b.OS_DOC_SET_KANJI
FROM
 sh.customers a,
 sh.supplementary_demographics b,
 sh.countries c
WHERE
 a.CUST_ID = b.CUST_ID
 AND a.country_id  = c.country_id
 AND a.cust_id between 101501 and 103000;
 
CREATE VIEW mining_data_test_v AS
SELECT
 a.CUST_ID,
 a.CUST_GENDER,
 2003-a.CUST_YEAR_OF_BIRTH AGE,
 a.CUST_MARITAL_STATUS,
 c.COUNTRY_NAME,
 a.CUST_INCOME_LEVEL,
 b.EDUCATION,
 b.OCCUPATION,
 b.HOUSEHOLD_SIZE,
 b.YRS_RESIDENCE,
 b.AFFINITY_CARD,
 b.BULK_PACK_DISKETTES,
 b.FLAT_PANEL_MONITOR,
 b.HOME_THEATER_PACKAGE,
 b.BOOKKEEPING_APPLICATION,
 b.PRINTER_SUPPLIES,
 b.Y_BOX_GAMES,
 b.OS_DOC_SET_KANJI
FROM
 sh.customers a,
 sh.supplementary_demographics b,
 sh.countries c
WHERE
 a.CUST_ID = b.CUST_ID
 AND a.country_id  = c.country_id
 AND a.cust_id between 103001 and 104500;

CREATE VIEW market_basket_v AS
SELECT a.cust_id,
 MAX((CASE WHEN b.PROD_NAME = 
  'Extension Cable' THEN a.QUANTITY_SOLD else null end)) EXTENSION_CABLE,
 MAX((CASE WHEN b.PROD_NAME = 
  '18" Flat Panel Graphics Monitor' THEN a.QUANTITY_SOLD else NULL END)) 
	FLAT_PANEL_MONITOR ,
 MAX((CASE WHEN b.PROD_NAME = 
  'CD-RW, High Speed Pack of 5' THEN a.QUANTITY_SOLD else NULL END)) 
       CD_RW_HIGH_SPEED_5_PACK,
 MAX((CASE WHEN b.PROD_NAME = 
  'Envoy 256MB - 40GB' THEN a.QUANTITY_SOLD else NULL END)) 
       ENVOY_256MB_40GB,
 MAX((CASE WHEN b.PROD_NAME = 
  'Envoy Ambassador' THEN a.QUANTITY_SOLD else NULL END)) 
       ENVOY_AMBASSADOR,
 MAX((CASE WHEN b.PROD_NAME = 
  'External 8X CD-ROM' THEN a.QUANTITY_SOLD else NULL END)) 
       EXTERNAL_8X_CD_ROM,
 MAX((CASE WHEN b.PROD_NAME = 
  'Keyboard Wrist Rest' THEN a.QUANTITY_SOLD else NULL END)) 
       KEYBOARD_WRIST_REST,
 MAX((CASE WHEN b.PROD_NAME = 
  'Model SM26273 Black Ink Cartridge' THEN a.QUANTITY_SOLD else NULL END)) 
       SM26273_BLACK_INK_CARTRIDGE,
 MAX((CASE WHEN b.PROD_NAME = 
  'Mouse Pad' THEN a.QUANTITY_SOLD else NULL END)) 
       MOUSE_PAD,
 MAX((CASE WHEN b.PROD_NAME = 
  'Multimedia speakers- 3" cones' THEN a.QUANTITY_SOLD else NULL END)) 
       MULTIMEDIA_SPEAKERS_3INCH,
 MAX((CASE WHEN b.PROD_NAME = 
  'O/S Documentation Set - English' THEN a.QUANTITY_SOLD else NULL END)) 
       OS_DOC_SET_ENGLISH,
 MAX((CASE WHEN b.PROD_NAME = 
  'SIMM- 16MB PCMCIAII card' THEN a.QUANTITY_SOLD else NULL END)) 
       SIMM_16MB_PCMCIAII_CARD,
 MAX((CASE WHEN b.PROD_NAME = 
  'Standard Mouse' THEN a.QUANTITY_SOLD else NULL END)) 
       STANDARD_MOUSE
FROM sh.sales a,
  sh.products b
    where a.prod_id = b.prod_id
    AND a.cust_id BETWEEN 100001 AND 104500
  group by cust_id;

-- Build, test and apply views with Text
  
CREATE TABLE mining_apply_text AS
SELECT
 a.CUST_ID,
 a.CUST_GENDER,
 2003-a.CUST_YEAR_OF_BIRTH AGE,
 a.CUST_MARITAL_STATUS,
 c.COUNTRY_NAME,
 a.CUST_INCOME_LEVEL,
 b.EDUCATION,
 b.OCCUPATION,
 b.HOUSEHOLD_SIZE,
 b.YRS_RESIDENCE,
 b.AFFINITY_CARD,
 b.BULK_PACK_DISKETTES,
 b.FLAT_PANEL_MONITOR,
 b.HOME_THEATER_PACKAGE,
 b.BOOKKEEPING_APPLICATION,
 b.PRINTER_SUPPLIES,
 b.Y_BOX_GAMES,
 b.os_doc_set_kanji,
 b.comments 
FROM
 sh.customers a,
 sh.supplementary_demographics b,
 sh.countries c
WHERE
 a.CUST_ID = b.CUST_ID
 AND a.country_id  = c.country_id
  AND a.cust_id between 100001 and 101500;

-- CREATE TEXT INDEX
CREATE INDEX apply_text_idx ON mining_apply_text(comments)
  INDEXTYPE IS ctxsys.context PARAMETERS('nopopulate')
/
  
CREATE TABLE mining_build_text AS
SELECT
 a.CUST_ID,
 a.CUST_GENDER,
 2003-a.CUST_YEAR_OF_BIRTH AGE,
 a.CUST_MARITAL_STATUS,
 c.COUNTRY_NAME,
 a.CUST_INCOME_LEVEL,
 b.EDUCATION,
 b.OCCUPATION,
 b.HOUSEHOLD_SIZE,
 b.YRS_RESIDENCE,
 b.AFFINITY_CARD,
 b.BULK_PACK_DISKETTES,
 b.FLAT_PANEL_MONITOR,
 b.HOME_THEATER_PACKAGE,
 b.BOOKKEEPING_APPLICATION,
 b.PRINTER_SUPPLIES,
 b.Y_BOX_GAMES,
 b.os_doc_set_kanji,
 b.comments 
FROM
 sh.customers a,
 sh.supplementary_demographics b,
 sh.countries c
WHERE
 a.CUST_ID = b.CUST_ID
 AND a.country_id  = c.country_id
 AND a.cust_id between 101501 and 103000;
 
-- CREATE TEXT INDEX
CREATE INDEX build_text_idx ON mining_build_text(comments)
  INDEXTYPE IS ctxsys.context PARAMETERS('nopopulate')
/
    
CREATE TABLE mining_test_text AS
SELECT
 a.CUST_ID,
 a.CUST_GENDER,
 2003-a.CUST_YEAR_OF_BIRTH AGE,
 a.CUST_MARITAL_STATUS,
 c.COUNTRY_NAME,
 a.CUST_INCOME_LEVEL,
 b.EDUCATION,
 b.OCCUPATION,
 b.HOUSEHOLD_SIZE,
 b.YRS_RESIDENCE,
 b.AFFINITY_CARD,
 b.BULK_PACK_DISKETTES,
 b.FLAT_PANEL_MONITOR,
 b.HOME_THEATER_PACKAGE,
 b.BOOKKEEPING_APPLICATION,
 b.PRINTER_SUPPLIES,
 b.Y_BOX_GAMES,
 b.os_doc_set_kanji,
 b.comments 
FROM
 sh.customers a,
 sh.supplementary_demographics b,
 sh.countries c
WHERE
 a.CUST_ID = b.CUST_ID
 AND a.country_id  = c.country_id
 AND a.cust_id between 103001 and 104500;

-- CREATE TEXT INDEX
CREATE INDEX test_text_idx ON mining_test_text(comments)
  INDEXTYPE IS ctxsys.context PARAMETERS('nopopulate')
/

--
-- Use the text table functions to create nested data
-- Using dm$ to avoid conflicts with user tables
EXECUTE ctx_ddl.create_preference('dm$temp_text_pref','SVM_CLASSIFIER');  

-- SVM_CLASSIFIER needs a table
CREATE TABLE
  dm$temp_text_pref_cat (
			 id NUMBER,
			 cat NUMBER)
/
    
-- Extract features from the build table
CREATE TABLE dm$build_tf_out AS
SELECT *
  FROM
  TABLE(ctxsys.drvodm.feature_prep
        ('BUILD_TEXT_IDX', -- Text index name
         'CUST_ID', -- case-id
         'dm$temp_text_pref_cat',
         'id',
         'cat',
         'dm$build_prep_features', -- Features table
         'dm$temp_text_pref')) -- Text preference name
/
CREATE TABLE dm$build_explain_out AS
SELECT a.sequence_id,
  b.text,
  a.value
  FROM dm$build_tf_out a,
  TABLE(ctxsys.drvodm.feature_explain('dm$build_prep_features')) b
  WHERE a.attribute_id = b.id
/  
CREATE TABLE mining_build_nested_text
   NESTED TABLE comments store AS build_comments 
  AS
SELECT non_text.cust_id,
  non_text.cust_gender,
  non_text.age,
  non_text.cust_marital_status,
  non_text.country_name,
  non_text.cust_income_level,
  non_text.education,
  non_text.occupation,
  non_text.household_size,
  non_text.yrs_residence,
  non_text.affinity_card,
  non_text.bulk_pack_diskettes,
  non_text.flat_panel_monitor,
  non_text.home_theater_package,
  non_text.bookkeeping_application,
  non_text.printer_supplies,
  non_text.y_box_games,
  non_text.os_doc_set_kanji,
  txt.comments
  FROM 
  mining_build_text non_text,
  ( SELECT features.sequence_id,
           cast(COLLECT(dm_nested_numerical(features.text,features.value)) 
                        as dm_nested_numericals)  comments
    FROM dm$build_explain_out features
    group by features.sequence_id) txt
  WHERE non_text.cust_id = txt.sequence_id(+)
/

DROP TABLE dm$build_tf_out purge;
DROP TABLE dm$build_explain_out purge;

--
-- Extract features, Cast into dm_nested_numericals
-- Make a TEST view with non-text and text data
-- Use featres table from build data
CREATE TABLE dm$test_tf_out AS
SELECT *
  FROM
  TABLE(ctxsys.drvodm.feature_prep
        ('TEST_TEXT_IDX', -- Text index name
         'CUST_ID', -- case-id
         'dm$build_prep_features')) -- restab   
/
CREATE TABLE dm$test_explain_out AS
SELECT a.sequence_id,
  b.text,
  a.value
  FROM dm$test_tf_out a,
  TABLE(ctxsys.drvodm.feature_explain('dm$build_prep_features')) b
  WHERE a.attribute_id = b.id
/  
CREATE TABLE  mining_test_nested_text
   NESTED TABLE comments store AS test_comments AS
SELECT non_text.cust_id,
  non_text.cust_gender,
  non_text.age,
  non_text.cust_marital_status,
  non_text.country_name,
  non_text.cust_income_level,
  non_text.education,
  non_text.occupation,
  non_text.household_size,
  non_text.yrs_residence,
  non_text.affinity_card,
  non_text.bulk_pack_diskettes,
  non_text.flat_panel_monitor,
  non_text.home_theater_package,
  non_text.bookkeeping_application,
  non_text.printer_supplies,
  non_text.y_box_games,
  non_text.os_doc_set_kanji,
  txt.comments
  FROM 
  mining_test_text non_text,
  ( SELECT features.sequence_id,
           cast(COLLECT(dm_nested_numerical(features.text,features.value)) 
                        as dm_nested_numericals)  comments
    FROM dm$test_explain_out features
    group by features.sequence_id) txt
  WHERE non_text.cust_id = txt.sequence_id(+)
/

DROP TABLE dm$test_tf_out purge;
DROP TABLE dm$test_explain_out purge;


-- Extract features from the build table
-- Use features from build data
CREATE TABLE dm$apply_tf_out AS
SELECT *
  FROM
  TABLE(ctxsys.drvodm.feature_prep
        ('APPLY_TEXT_IDX', -- Text index name
         'CUST_ID', -- case-id
         'dm$build_prep_features')) -- restab
/
CREATE TABLE dm$apply_explain_out AS
SELECT a.sequence_id,
  b.text,
  a.value
  FROM dm$apply_tf_out a,
  TABLE(ctxsys.drvodm.feature_explain('dm$build_prep_features')) b
  WHERE a.attribute_id = b.id
/
  
CREATE TABLE  mining_apply_nested_text 
  NESTED TABLE comments store AS apply_comments AS 
SELECT non_text.cust_id,
  non_text.cust_gender,
  non_text.age,
  non_text.cust_marital_status,
  non_text.country_name,
  non_text.cust_income_level,
  non_text.education,
  non_text.occupation,
  non_text.household_size,
  non_text.yrs_residence,
  non_text.affinity_card,
  non_text.bulk_pack_diskettes,
  non_text.flat_panel_monitor,
  non_text.home_theater_package,
  non_text.bookkeeping_application,
  non_text.printer_supplies,
  non_text.y_box_games,
  non_text.os_doc_set_kanji,
  txt.comments
  FROM
  mining_apply_text non_text,
  ( SELECT features.sequence_id,
           cast(COLLECT(dm_nested_numerical(features.text,features.value)) 
                        as dm_nested_numericals)  comments
    FROM dm$apply_explain_out features
    group by features.sequence_id) txt
  WHERE non_text.cust_id = txt.sequence_id(+)   
/

CREATE VIEW mining_data_one_class_v AS
SELECT
 a.CUST_ID,
 a.CUST_GENDER,
 2003-a.CUST_YEAR_OF_BIRTH AGE,
 a.CUST_MARITAL_STATUS,
 c.COUNTRY_NAME,
 a.CUST_INCOME_LEVEL,
 b.EDUCATION,
 b.OCCUPATION,
 b.HOUSEHOLD_SIZE,
 b.YRS_RESIDENCE
FROM
 sh.customers a,
 sh.supplementary_demographics b,
 sh.countries c
WHERE
 a.CUST_ID = b.CUST_ID
 AND a.country_id  = c.country_id
 AND a.cust_id between 101501 and 103000
 AND affinity_card=1;

DROP TABLE dm$build_prep_features purge;
DROP TABLE dm$apply_tf_out purge;
DROP TABLE dm$apply_explain_out purge;
-- Clean up the temp preference
EXECUTE ctx_ddl.drop_preference('dm$temp_text_pref');
-- Clean up temp cat table created
DROP TABLE dm$temp_text_pref_cat purge;
         







  
