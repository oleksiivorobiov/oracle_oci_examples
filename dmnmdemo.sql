Rem
Rem $Header: dmnmdemo.sql 17-jan-2008.13:35:55 jiawang Exp $
Rem
Rem dmnmdemo.sql
Rem
Rem Copyright (c) 2003, 2007, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      dmnmdemo.sql - Sample program for the DBMS_DATA_MINING package.
Rem
Rem    DESCRIPTION
Rem      This script creates a feature extraction model
Rem      using the NMF algorithm
Rem      and data in the SH (Sales History) schema in the RDBMS. 
Rem
Rem    NOTES
Rem     
Rem
Rem    MODIFIED   (MM/DD/YY) 
Rem    ramkrish    06/14/07 - remove commit after settings
Rem    jiawang     01/17/08 - Correct comments
Rem    ramkrish    10/25/07 - replace deprecated get_model calls with catalog
Rem                           queries
Rem    jiawang     03/06/07 - Add order by to fix bug5381396
Rem    xbarr       02/22/07 - add force drop for type objects
Rem    amozes      05/09/06 - repeatable ordering 
Rem    ktaylor     07/11/05 - Minor edits to comments
Rem    ramkrish    02/01/05 - remove rownum clause in model signature 
Rem    jcjeon      01/18/05 - add column format 
Rem    ramkrish    10/27/04 - add data analysis and comments/cleanup
Rem    jiawang     07/22/04 - Add order by to fix sorting dif 
Rem    xbarr       06/25/04 - xbarr_dm_rdbms_migration
Rem    ramkrish    10/20/03 - ramkrish_txn109085
Rem    pstengar    10/17/03 - added denormalization of model details
Rem    cbhagwat    10/17/03 - feature_extraction
Rem    ramkrish    10/02/03 - Creation
  
SET serveroutput ON
SET trimspool ON  
SET pages 10000
SET linesize 100
SET echo ON

-----------------------------------------------------------------------
--                            SAMPLE PROBLEM
-----------------------------------------------------------------------
-- Given demographic data about a set of customers, extract features
-- from the given dataset.
--

-----------------------------------------------------------------------
--                            SET UP AND ANALYZE THE DATA
-----------------------------------------------------------------------

-- See the corresponding section in dmsvcdem.sql
--

-----------------------------------------------------------------------
--                            BUILD THE MODEL
-----------------------------------------------------------------------

-- Cleanup old build data preparation objects for repeat runs
BEGIN EXECUTE IMMEDIATE 'DROP TABLE nmf_sh_sample_norm';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP VIEW nmf_sh_sample_build_prepared';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
-- Cleanup old model with same name for repeat runs
BEGIN DBMS_DATA_MINING.DROP_MODEL('NMF_SH_sample');
EXCEPTION WHEN OTHERS THEN NULL; END;
/

--------------------------------
-- PREPARE BUILD (TRAINING) DATA
--
-- This step is required only if the data is not prepared/transformed
-- to enable creation of an efficient model using the NMF algorithm.
-- In this case, the data for attribute 'age' requires preparation.
--
BEGIN
  -- Normalize numerical attributes: age
  DBMS_DATA_MINING_TRANSFORM.CREATE_NORM_LIN(
    norm_table_name => 'nmf_sh_sample_norm');

  DBMS_DATA_MINING_TRANSFORM.INSERT_NORM_LIN_MINMAX (
    norm_table_name  => 'nmf_sh_sample_norm',
    data_table_name  => 'mining_data_build_v',
    exclude_list     => dbms_data_mining_transform.column_list (
                        'yrs_residence',
                        'affinity_card',
                        'bulk_pack_diskettes',
                        'flat_panel_monitor',
                        'home_theater_package',
                        'bookkeeping_application',
                        'printer_supplies',
                        'y_box_games',
                        'os_doc_set_kanji',
                        'cust_id')
  );

  -- Create the transformed view
  DBMS_DATA_MINING_TRANSFORM.XFORM_NORM_LIN (
    norm_table_name => 'nmf_sh_sample_norm',
    data_table_name => 'mining_data_build_v',
    xform_view_name => 'nmf_sh_sample_build_prepared');
END;
/

-- BUILD DATA PREPARATION OBJECTS:
-- ------------------------------
-- 1. Missing Value Treatment for all predictors
--    skipped. See dmsvcdem.sql for an example.
-- 2. Normalization Table:          nmf_sh_sample_norm
-- 3. Input (view) to CREATE_MODEL: nmf_sh_sample_build_prepared

-------------------
-- SPECIFY SETTINGS
--
-- Cleanup old settings table objects for repeat runs
BEGIN EXECUTE IMMEDIATE 'DROP TABLE nmf_sh_sample_settings';
EXCEPTION WHEN OTHERS THEN NULL; END;
/

-- CREATE A SETTINGS TABLE
--
-- NMF is the default Feature Extraction algorithm. For this sample,
-- we skip specification of any overrides to defaults
--
-- Uncomment the appropriate sections of the code below if you
-- want to override any defaults.
-- 
set echo off
-- CREATE TABLE nmf_sh_sample_settings (
--   setting_name  VARCHAR2(30),
--   setting_value VARCHAR2(30));
set echo on
 
-- BEGIN       
  -- Populate settings table
  -- INSERT INTO nmf_sh_sample_settings (setting_name, setting_value) VALUES
  -- Examples of possible overrides are:
  -- (dbms_data_mining.feat_num_features, 10);
  -- (dbms_data_mining.nmfs_conv_tolerance,0.05);
  -- (dbms_data_mining.nmfs_num_iterations,50);
  -- (dbms_data_mining.nmfs_random_seed,-1);
-- END;
-- /

---------------------
-- CREATE A NEW MODEL
--
-- Build NMF model
BEGIN
  DBMS_DATA_MINING.CREATE_MODEL(
    model_name          => 'NMF_SH_sample',
    mining_function     => dbms_data_mining.feature_extraction,
    data_table_name     => 'nmf_sh_sample_build_prepared',
    case_id_column_name => 'cust_id');
END;
/

-------------------------
-- DISPLAY MODEL SETTINGS
--
column setting_name format a30
column setting_value format a30
SELECT setting_name, setting_value
  FROM user_mining_model_settings
 WHERE model_name = 'NMF_SH_SAMPLE'
ORDER BY setting_name;

--------------------------
-- DISPLAY MODEL SIGNATURE
--
column attribute_name format a40
column attribute_type format a20
SELECT attribute_name, attribute_type
  FROM user_mining_model_attributes
 WHERE model_name = 'NMF_SH_SAMPLE'
ORDER BY attribute_name;

------------------------
-- DISPLAY MODEL DETAILS
--
-- Each feature is a linear combination of the original attribute set; 
-- the coefficients of these linear combinations are non-negative.
-- The model details return for each feature the coefficients
-- associated with each one of the original attributes. Categorical 
-- attributes are described by (attribute_name, attribute_value) pairs.
-- That is, for a given feature, each distinct value of a categorical 
-- attribute has its own coefficient.
--
column attribute_name format a40;
column attribute_value format a20;
SELECT * FROM (
SELECT F.feature_id,
       A.attribute_name,
       A.attribute_value,
       A.coefficient
  FROM TABLE(DBMS_DATA_MINING.GET_MODEL_DETAILS_NMF('NMF_SH_Sample')) F,
       TABLE(F.attribute_set) A
ORDER BY feature_id,attribute_name,attribute_value
) WHERE ROWNUM < 11;

-----------------------------------------------------------------------
--                               TEST THE MODEL
-----------------------------------------------------------------------
-- There is no specific set of testing parameters for feature extraction.
-- Examination and analysis of features is the main method to prove
-- the efficacy of an NMF model.
--

-----------------------------------------------------------------------
--                               APPLY THE MODEL
-----------------------------------------------------------------------
--
-- For a descriptive mining function like feature extraction, "Scoring"
-- involves providing the probability values for each feature.
-- During model apply, an NMF model maps the original data into the 
-- new set of attributes (features) discovered by the model.
-- 
-- Cleanup scoring data preparation objects for repeat runs
BEGIN EXECUTE IMMEDIATE 'DROP VIEW nmf_sh_sample_apply_prepared';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP TYPE Featattrs FORCE';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP TYPE Featattr FORCE';
EXCEPTION WHEN OTHERS THEN NULL; END;
/

-----------------------
-- PREPARE SCORING DATA
--
-- 1. Missing Value Treatment for all predictors
--    skipped. See dmsvcdem.sql for an example.
--
-- 2. Outlier Treatment
--    Skipped. See dmsvcdem.sql for an example.
--
-- 3. Normalization
--
BEGIN
  DBMS_DATA_MINING_TRANSFORM.XFORM_NORM_LIN (
    norm_table_name => 'nmf_sh_sample_norm',
    data_table_name => 'mining_data_apply_v',
    xform_view_name => 'nmf_sh_sample_apply_prepared');
END;
/

-------------------------------------------------
-- SCORE NEW DATA USING SQL DATA MINING FUNCTIONS
--
------------------
-- BUSINESS CASE 1
-- List the features that correspond to customers in this dataset.
--
SELECT FEATURE_ID(nmf_sh_sample USING *) AS feat, COUNT(*) AS cnt
  FROM nmf_sh_sample_apply_prepared
GROUP BY FEATURE_ID(nmf_sh_sample USING *)
ORDER BY cnt DESC,FEAT DESC;

------------------
-- BUSINESS CASE 2
-- List customers that correspond to feature 3 ordered by match quality
--
SELECT *
  FROM (SELECT cust_id, FEATURE_VALUE(nmf_sh_sample, 3 USING *) match_quality
          FROM nmf_sh_sample_apply_prepared
        ORDER BY match_quality DESC)
 WHERE ROWNUM < 11;

column value format 99.9999
column fid format 999
column attr format a25
column val format a20
column coeff format 9.9999

------------------
-- BUSINESS CASE 3
-- List top 10 features corresponding to a given customer
-- record (based on match_quality), and find out the top
-- attributes for each feature (based on coefficient > 0.25)
--
set echo off
CREATE TYPE Featattr AS OBJECT (
  attr VARCHAR2(30),
  val  VARCHAR2(30),
  coeff NUMBER)
/
set echo on
CREATE TYPE Featattrs AS TABLE OF Featattr
/

column val format a50
WITH
feat_tab AS (
SELECT F.feature_id fid,
       A.attribute_name attr,
       TO_CHAR(A.attribute_value) val,
       A.coefficient coeff
  FROM TABLE(DBMS_DATA_MINING.GET_MODEL_DETAILS_NMF('nmf_sh_sample')) F,
       TABLE(F.attribute_set) A
 WHERE A.coefficient > 0.25
),
feat AS (
SELECT fid,
       CAST(COLLECT(Featattr(attr, val, coeff))
         AS Featattrs) f_attrs
  FROM feat_tab
GROUP BY fid
),
cust_10_features AS (
SELECT T.cust_id, S.feature_id, S.value
  FROM (SELECT cust_id, FEATURE_SET(nmf_sh_sample, 10 USING *) pset
          FROM nmf_sh_sample_apply_prepared
         WHERE cust_id = 100002) T,
       TABLE(T.pset) S
)
SELECT A.value, A.feature_id fid,
       B.attr, B.val, B.coeff
  FROM cust_10_features A,
       (SELECT T.fid, F.*
          FROM feat T,
               TABLE(T.f_attrs) F) B
 WHERE A.feature_id = B.fid
ORDER BY A.value DESC, A.feature_id ASC, coeff DESC, attr ASC, val ASC;
