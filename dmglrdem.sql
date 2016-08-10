Rem
Rem $Header: dmglrdem.sql 22-jan-2008.14:06:12 ramkrish Exp $
Rem
Rem dmglrdem.sql
Rem
Rem Copyright (c) 2003, 2008, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      dmglrdem.sql - Sample program for the DBMS_DATA_MINING package.
Rem
Rem    DESCRIPTION
Rem      This script creates a regression model using GLM based on
Rem      data in the SH (Sales History) schema in the RDBMS and
Rem      demonstrates the use of SQL prediction functions to test
Rem      the model and apply it to new data.
Rem
Rem    NOTES
Rem     
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    ramkrish    06/14/07 - remove commit after settings
Rem    ramkrish    01/22/08 - add prediction_bounds
Rem    ramkrish    10/25/07 - replace deprecated get_model calls with catalog
Rem                           queries
Rem    bmilenov    05/08/07 - Change case_id column in row diagnostics table
Rem    bmilenov    12/13/06 - Turn ADP on
Rem    bmilenov    12/05/06 - Change row diagnostic query
Rem    jyarmus     11/21/06 - Change ridge regression constant
Rem    jyarmus     10/13/06 - drop diagnostics table prior to second model
Rem                           build
Rem    jyarmus     10/12/06 - build with and without columns causing 
Rem                           a singularity in covariance matrix    
Rem    bmilenov    10/04/06 - Remove multicolinear columns
Rem    bmilenov    08/22/06 - Remove filtered view
Rem    bmilenov    08/18/06 - Cleanup
Rem    ramkrish    10/02/03 - Creation
  
SET serveroutput ON
SET trimspool ON  
SET pages 10000
SET echo ON

-----------------------------------------------------------------------
--                            SAMPLE PROBLEM
-----------------------------------------------------------------------
-- Given demographic, purchase, and affinity card membership data for a 
-- set of customers, predict customer's age. Since age is a continuous 
-- variable, this is a regression problem.

-----------------------------------------------------------------------
--                            SET UP AND ANALYZE DATA
-----------------------------------------------------------------------

-- The data for this sample is composed from base tables in the SH Schema
-- (See Sample Schema Documentation) and presented through these views:
-- mining_data_build_v (build data)
-- mining_data_test_v  (test data)
-- mining_data_apply_v (apply data)
-- (See dmsh.sql for view definitions).
--
-----------
-- ANALYSIS
-----------
-- Data preparation for GLM is performed internally

-----------------------------------------------------------------------
--                            BUILD THE MODEL
-----------------------------------------------------------------------

-- Cleanup old model with same name (if any)
BEGIN
  DBMS_DATA_MINING.DROP_MODEL('GLMR_SH_Regr_sample');
EXCEPTION WHEN OTHERS THEN NULL; END;
/

-------------------
-- SPECIFY SETTINGS
--
-- Cleanup old settings table for repeat runs
BEGIN EXECUTE IMMEDIATE 'DROP TABLE glmr_sh_sample_settings';
EXCEPTION WHEN OTHERS THEN NULL; END;
/

-- Cleanup diagnostic table
BEGIN EXECUTE IMMEDIATE 'DROP TABLE glmr_sh_sample_diag';
EXCEPTION WHEN OTHERS THEN NULL; END;
/

CREATE TABLE glmr_sh_sample_settings (
  setting_name  VARCHAR2(30),
  setting_value VARCHAR2(30));

BEGIN 
-- Populate settings table
  INSERT INTO glmr_sh_sample_settings (setting_name, setting_value) VALUES
  (dbms_data_mining.algo_name, dbms_data_mining.algo_generalized_linear_model);
  INSERT INTO  glmr_sh_sample_settings (setting_name, setting_value) VALUES
    (dbms_data_mining.glms_diagnostics_table_name, 'GLMR_SH_SAMPLE_DIAG');  
  INSERT INTO  glmr_sh_sample_settings (setting_name, setting_value) VALUES
    (dbms_data_mining.prep_auto, dbms_data_mining.prep_auto_on);  
  -- Examples of other possible overrides are:
  --(dbms_data_mining.glms_ridge_regression,
  -- dbms_data_mining.glms_ridge_reg_enable);
END;
/ 

---------------------
-- CREATE A NEW MODEL
--
BEGIN
  DBMS_DATA_MINING.CREATE_MODEL(
    model_name          => 'GLMR_SH_Regr_sample',
    mining_function     => dbms_data_mining.regression,
    data_table_name     => 'mining_data_build_v',
    case_id_column_name => 'cust_id',
    target_column_name  => 'age',
    settings_table_name => 'glmr_sh_sample_settings');
END;
/

-------------------------
-- DISPLAY MODEL SETTINGS
--
column setting_name format a30
column setting_value format a30
SELECT setting_name, setting_value
  FROM user_mining_model_settings
 WHERE model_name = 'GLMR_SH_REGR_SAMPLE'
ORDER BY setting_name;

--------------------------
-- DISPLAY MODEL SIGNATURE
--
column attribute_name format a40
column attribute_type format a20
SELECT attribute_name, attribute_type
  FROM user_mining_model_attributes
 WHERE model_name = 'GLMR_SH_REGR_SAMPLE'
ORDER BY attribute_name;

------------------------
-- DISPLAY MODEL DETAILS
--
-- Global statistics
SELECT *
  FROM TABLE(dbms_data_mining.get_model_details_global('GLMR_SH_Regr_sample'))
ORDER BY global_detail_name;

-- We see from the global details that the covariance matrix was invalid:
--   VALID_COVARIANCE_MATRIX is 0
-- As a result we only get a limited set of diagnostics, a sample of which
-- are shown below

-- Coefficient statistics
SET line 120
column class format a20
column attribute_name format a20  
column attribute_subname format a20
column attribute_value format a20  
  
SELECT *
  FROM (SELECT *
          FROM TABLE(dbms_data_mining.get_model_details_glm(
                       'GLMR_SH_Regr_sample')) 
        ORDER BY class, attribute_name, attribute_value)
 WHERE ROWNUM < 11;  
    
-- Row diagnostics
SELECT *
  FROM (SELECT *
          FROM glmr_sh_sample_diag 
        ORDER BY case_id)
 WHERE ROWNUM < 11;

-----------------------------------------------------------------------
--                               TEST THE MODEL
-----------------------------------------------------------------------

------------------------------------
-- COMPUTE METRICS TO TEST THE MODEL
--

-- 1. Root Mean Square Error - Sqrt(Mean((x - x')^2))
-- 2. Mean Absolute Error - Mean(|(x - x')|)
--
column rmse format 9999.99
column mae format 9999.99  
SELECT SQRT(AVG((A.pred - B.age) * (A.pred - B.age))) rmse,
       AVG(ABS(a.pred - B.age)) mae
  FROM (SELECT cust_id, prediction(GLMR_SH_Regr_sample using *) pred
          FROM mining_data_test_v) A,
       mining_data_test_v B
 WHERE A.cust_id = B.cust_id;

-- Further analysis showed that excluding the columns 
-- PRINTER_SUPPLIES, CUST_INCOME_level and BOOKKEEPING_APPLICATION 
-- avoid singularities in the data
-- that lead to the invalid covariance matrix. Below we create a new view
-- that excludes the problematic columns, thereby enabling computation of a 
-- full set of diagnostics
-- (See notes in dmglcdem.sql)

-----------------------------------------------------------------------
--                            BUILD A NEW MODEL
-----------------------------------------------------------------------

-- Cleanup old build view
BEGIN EXECUTE IMMEDIATE 'DROP VIEW mining_data_build_v2';
EXCEPTION WHEN OTHERS THEN NULL; END;
/

-- CREATE a view that excludes columns PRINTER_SUPPLIES, CUST_INCOME_level 
-- and BOOKKEEPING_APPLICATION
CREATE VIEW mining_data_build_v2 AS
SELECT CUST_id, CUST_gender, age, CUST_MARITAL_status, COUNTRY_name,
       education, occupation, HOUSEHOLD_size, YRS_residence, 
       AFFINITY_card, BULK_PACK_diskettes, FLAT_PANEL_monitor,
       HOME_THEATER_package, Y_BOX_games, OS_DOC_SET_kanji
  FROM mining_data_build_v;

-- Cleanup diagnostic table
BEGIN EXECUTE IMMEDIATE 'DROP TABLE glmr_sh_sample_diag';
EXCEPTION WHEN OTHERS THEN NULL; END;
/

---------------------
-- CREATE A NEW MODEL
--
-- Cleanup old model with same name (if any)
BEGIN
  DBMS_DATA_MINING.DROP_MODEL('GLMR_SH_Regr_sample');
EXCEPTION WHEN OTHERS THEN
  NULL;
END;
/

BEGIN
  DBMS_DATA_MINING.CREATE_MODEL(
    model_name          => 'GLMR_SH_Regr_sample',
    mining_function     => dbms_data_mining.regression,
    data_table_name     => 'mining_data_build_v2',
    case_id_column_name => 'cust_id',
    target_column_name  => 'age',
    settings_table_name => 'glmr_sh_sample_settings');
END;
/

-------------------------
-- DISPLAY MODEL SETTINGS
--
column setting_name format a30
column setting_value format a30
SELECT setting_name, setting_value
  FROM user_mining_model_settings
 WHERE model_name = 'GLMR_SH_REGR_SAMPLE'
ORDER BY setting_name;

--------------------------
-- DISPLAY MODEL SIGNATURE
--
column attribute_name format a40
column attribute_type format a20
SELECT attribute_name, attribute_type
  FROM user_mining_model_attributes
 WHERE model_name = 'GLMR_SH_REGR_SAMPLE'
ORDER BY attribute_name;

------------------------
-- DISPLAY MODEL DETAILS
--
-- Global statistics
SELECT *
  FROM TABLE(dbms_data_mining.get_model_details_global(
              'GLMR_SH_Regr_sample'))
ORDER BY global_detail_name;

-- Coefficient statistics
SET line 120
column class format a20
column attribute_name format a20  
column attribute_subname format a20
column attribute_value format a20  
  
SELECT *
  FROM (SELECT *
          FROM TABLE(dbms_data_mining.get_model_details_glm(
                       'GLMR_SH_Regr_sample')) 
        ORDER BY class, attribute_name, attribute_value)
 WHERE ROWNUM < 11;  
    
-- Row diagnostics
SELECT *
  FROM (SELECT *
          FROM glmr_sh_sample_diag 
        ORDER BY case_id)
 WHERE ROWNUM < 11;

-----------------------------------------------------------------------
--                               TEST THE MODEL
-----------------------------------------------------------------------

------------------------------------
-- COMPUTE METRICS TO TEST THE MODEL
--

-- 1. Root Mean Square Error - Sqrt(Mean((x - x')^2))
-- 2. Mean Absolute Error - Mean(|(x - x')|)
--
column rmse format 9999.99
column mae format 9999.99  
SELECT SQRT(AVG((A.pred - B.age) * (A.pred - B.age))) rmse,
       AVG(ABS(a.pred - B.age)) mae
  FROM (SELECT cust_id, prediction(GLMR_SH_Regr_sample using *) pred
          FROM mining_data_test_v) A,
       mining_data_test_v B
 WHERE A.cust_id = B.cust_id;

-- 3. Residuals
--    If the residuals show substantial variance between
--    the predicted value and the actual, you can consider
--    changing the algorithm parameters.
--
SELECT TO_CHAR(ROUND(pred, 4)) prediction, residual
  FROM (SELECT A.pred, (A.pred - B.age) residual
          FROM (SELECT cust_id, prediction(GLMR_SH_Regr_sample using *) pred
                  FROM mining_data_test_v) A,
               mining_data_test_v B
         WHERE A.cust_id = B.cust_id
        ORDER BY A.pred ASC)
 WHERE ROWNUM < 11;

-----------------------------------------------------------------------
--                               SCORE NEW DATA
-----------------------------------------------------------------------

-- now that the model has a valid covariance matrix, it is possible
-- to obtain confidence bounds.
SELECT *
  FROM (SELECT CUST_ID,
               PREDICTION(GLMR_SH_Regr_sample USING *) pr,
               PREDICTION_BOUNDS(GLMR_SH_Regr_sample USING *).lower pl,
               PREDICTION_BOUNDS(GLMR_SH_Regr_sample USING *).upper pu
          FROM mining_data_apply_v
        ORDER BY CUST_ID)
 WHERE ROWNUM < 11
ORDER BY CUST_ID;
