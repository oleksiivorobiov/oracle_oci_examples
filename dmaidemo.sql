Rem
Rem $Header: dmaidemo.sql 11-jul-2005.12:07:01 ktaylor Exp $
Rem
Rem dmabdemo.sql
Rem
Rem Copyright (c) 2003, 2005, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      dmaidemo.sql - Sample program for the DBMS_DATA_MINING package.
Rem
Rem    DESCRIPTION
Rem      This script creates an attribute importance model
Rem      using the MDL algorithm
Rem      and data in the SH (Sales History) schema in the RDBMS. 
Rem
Rem    NOTES
Rem     
Rem
Rem    MODIFIED   (MM/DD/YY) 
Rem    ktaylor     07/11/05 - minor edits to comments
Rem    jcjeon      01/18/05 - add column format 
Rem    ramkrish    10/26/04 - add data analysis and comments/cleanup 
Rem    ramkrish    10/02/03 - Creation
  
SET serveroutput ON
SET trimspool ON  
SET pages 10000
SET echo ON

-----------------------------------------------------------------------
--                            SAMPLE PROBLEM
-----------------------------------------------------------------------
-- Given a target attribute affinity_card, find the importance of
-- independent attributes.

-----------------------------------------------------------------------
--                            SET UP AND ANALYZE THE DATA
-----------------------------------------------------------------------

-- See the corresponding section in dmabdemo.sql - Classification
-- using ABN. The analysis and preparation steps are very similar.

-----------------------------------------------------------------------
--                            BUILD THE MODEL
-----------------------------------------------------------------------

-- Cleanup old build data preparation objects for repeat runs
BEGIN EXECUTE IMMEDIATE 'DROP TABLE ai_sh_sample_num';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP TABLE ai_sh_sample_cat';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP VIEW ai_sh_sample_build_prepared';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP VIEW ai_sh_sample_build_cat';
EXCEPTION WHEN OTHERS THEN NULL; END;
/

--------------------------------
-- PREPARE BUILD (TRAINING) DATA
--
-- 1. Missing Value treatment for all Predictors
--    Skipped - see dmsvcdem.sql
--
-- 2. Outlier Treatment
--    See notes in dmabdemo.sql and dmsvcdem.sql on outlier
--    treatment with equi-width binning.
--    Skipped - see dmsvcdem.sql for example.
--
-- 3. Binning
--
BEGIN
  -- Bin categorical attributes: OCCUPATION
  DBMS_DATA_MINING_TRANSFORM.CREATE_BIN_CAT (
    bin_table_name => 'ai_sh_sample_cat');        

  DBMS_DATA_MINING_TRANSFORM.INSERT_BIN_CAT_FREQ (
    bin_table_name  => 'ai_sh_sample_cat',
    data_table_name => 'mining_data_build_v',
    bin_num         => 7,
    exclude_list    => DBMS_DATA_MINING_TRANSFORM.COLUMN_LIST (
                       'cust_gender',
                       'cust_marital_status',
                       'country_name',
                       'cust_income_level',
                       'education',
                       'household_size')
  );

  -- Bin numerical attributes: AGE
  DBMS_DATA_MINING_TRANSFORM.CREATE_BIN_NUM (
    bin_table_name => 'ai_sh_sample_num');
                         
  DBMS_DATA_MINING_TRANSFORM.INSERT_BIN_NUM_QTILE (
    bin_table_name  => 'ai_sh_sample_num',
    data_table_name => 'mining_data_build_v',
    bin_num         => 5,
    exclude_list    => DBMS_DATA_MINING_TRANSFORM.COLUMN_LIST (
                       'affinity_card',
                       'bookkeeping_application',
                       'bulk_pack_diskettes',
                       'cust_id',
                       'flat_panel_monitor',
                       'home_theater_package',
                       'os_doc_set_kanji',
                       'printer_supplies',
                       'y_box_games',
                       'yrs_residence')
  );

  -- Create the transformed view
  -- Execute the first transformation (categorical binning)
  DBMS_DATA_MINING_TRANSFORM.XFORM_BIN_CAT (
    bin_table_name  => 'ai_sh_sample_cat',
    data_table_name => 'mining_data_build_v',
    xform_view_name => 'ai_sh_sample_build_cat');    

  -- Provide the result (ai_sh_sample_build_cat)
  -- to the next transformation (numerical binning)
  DBMS_DATA_MINING_TRANSFORM.XFORM_BIN_NUM (
    bin_table_name  => 'ai_sh_sample_num',
    data_table_name => 'ai_sh_sample_build_cat',
    xform_view_name => 'ai_sh_sample_build_prepared');
END;
/
-- BUILD DATA PREPARATION OBJECTS:
-- ------------------------------
-- 1. Categorical Bin Table:        ai_sh_sample_cat
-- 2. Numerical Bin Table:          ai_sh_sample_num
-- 3. Input (view) to CREATE_MODEL: ai_sh_sample_build_prepared

-------------------
-- SPECIFY SETTINGS
--
-- There are no explicit settings for attribute importance.
--
---------------------
-- CREATE A NEW MODEL
--
-- Cleanup old model with the same name for repeat runs
BEGIN DBMS_DATA_MINING.DROP_MODEL('AI_SH_sample');
EXCEPTION WHEN OTHERS THEN NULL; END;
/

-- Build a new AI model
BEGIN
  DBMS_DATA_MINING.CREATE_MODEL(
    model_name          => 'AI_SH_sample',
    mining_function     => DBMS_DATA_MINING.ATTRIBUTE_IMPORTANCE,
    data_table_name     => 'ai_sh_sample_build_prepared',
    case_id_column_name => 'cust_id',
    target_column_name  => 'affinity_card');
END;
/

-------------------------
-- DISPLAY MODEL SETTINGS
--
-- Skip. This routine is not applicable for this function.

--------------------------
-- DISPLAY MODEL SIGNATURE
--
-- Skip. This routine is not applicable for this function.

------------------------
-- DISPLAY MODEL DETAILS
--
-- list of attribute names ranked by their importance value.
--
-- NOTE: The attributes that show negative importance values
--       are not important attributes, similar to attributes
--       with importance value of 0. From MDL principles, the
--       negative importance value indicates that the given
--       attribute simply adds to the size of the model -
--       in this case, the list of attributes most relevant
--       to the target - without adding any additional knowledge.
--
column attribute_name   format a40
SELECT attribute_name, importance_value, rank
  FROM TABLE(DBMS_DATA_MINING.GET_MODEL_DETAILS_AI('AI_SH_sample')) 
ORDER BY RANK;
