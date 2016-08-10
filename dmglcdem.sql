Rem
Rem $Header: dmglcdem.sql 22-jan-2008.14:06:04 ramkrish Exp $
Rem
Rem dmglcdem.sql
Rem
Rem Copyright (c) 2003, 2008, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      dmglcdem.sql - Sample program for the DBMS_DATA_MINING package.
Rem
Rem    DESCRIPTION
Rem      This script creates a classification model using GLM based on
Rem      data in the SH (Sales History) schema in the RDBMS and
Rem      demonstrates the use of SQL prediction functions to test
Rem      the model and apply it to new data.
Rem
Rem    NOTES
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
Rem    jyarmus     10/18/06 - reduce precision of accuracy and AUC
Rem    jyarmus     10/13/06 - drop diagnostics table prior to second model
Rem                           build
Rem    jyarmus     10/12/06 - Run with and without ridge
Rem    bmilenov    10/04/06 - Remove multicolinear columns
Rem    bmilenov    08/22/06 - Drop class weight table
Rem    bmilenov    08/18/06 - Cleanup
Rem    ramkrish    10/02/03 - Creation
  
SET serveroutput ON
SET trimspool ON  
SET pages 10000
SET echo ON

-----------------------------------------------------------------------
--                            SAMPLE PROBLEM
-----------------------------------------------------------------------
-- Given demographic and purchase data about a set of customers, predict
-- customer's response to an affinity card program using a GLM classifier.
--

-----------------------------------------------------------------------
--                            SET UP AND ANALYZE THE DATA
-----------------------------------------------------------------------

-------
-- DATA
-------
-- The data for this sample is composed from base tables in SH Schema
-- (See Sample Schema Documentation) and presented through these views:
-- mining_data_build_v (build data)
-- mining_data_test_v  (test data)
-- mining_data_apply_v (apply data)
-- (See dmsh.sql for view definitions).
--

-----------
-- ANALYSIS
-----------
-- Data preparation in GLM is performed internally
--

-----------------------------------------------------------------------
--                            BUILD THE MODEL
-----------------------------------------------------------------------

-- Cleanup old model with the same name for repeat runs
BEGIN DBMS_DATA_MINING.DROP_MODEL('GLMC_SH_Clas_sample');
EXCEPTION WHEN OTHERS THEN NULL; END;
/

------------------
-- SPECIFY SETTINGS
--
-- Cleanup old settings table for repeat runs
BEGIN EXECUTE IMMEDIATE 'DROP TABLE glmc_sh_sample_settings';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
-- Cleanup class weights
BEGIN EXECUTE IMMEDIATE 'DROP TABLE glmc_sh_sample_clas_weights';
EXCEPTION WHEN OTHERS THEN NULL; END;
/

-- Cleanup diagnostics table
BEGIN EXECUTE IMMEDIATE 'DROP TABLE glmc_sh_sample_diag';
EXCEPTION WHEN OTHERS THEN NULL; END;
/

-- CREATE AND POPULATE A CLASS WEIGHTS TABLE
--
-- A class weights table is used to influence the weighting of target classes
-- during model creation.
--
CREATE TABLE glmc_sh_sample_clas_weights (
  target_value      NUMBER,
  prior_probability NUMBER);
INSERT INTO glmc_sh_sample_clas_weights VALUES (0,0.35);
INSERT INTO glmc_sh_sample_clas_weights VALUES (1,0.65);

-- CREATE AND POPULATE A SETTINGS TABLE
--
set echo off
CREATE TABLE glmc_sh_sample_settings (
  setting_name  VARCHAR2(30),
  setting_value VARCHAR2(30));
set echo on

-- The default classification algorithm is Naive Bayes. So override
-- this choice to GLM logistic regression using a settings table. 
--    
BEGIN 
-- Populate settings table
  INSERT INTO glmc_sh_sample_settings (setting_name, setting_value) VALUES
    (dbms_data_mining.algo_name, dbms_data_mining.algo_generalized_linear_model);
  INSERT INTO  glmc_sh_sample_settings (setting_name, setting_value) VALUES
    (dbms_data_mining.glms_diagnostics_table_name, 'GLMC_SH_SAMPLE_DIAG');  
  INSERT INTO  glmc_sh_sample_settings (setting_name, setting_value) VALUES
    (dbms_data_mining.prep_auto, dbms_data_mining.prep_auto_on);
  -- Examples of other possible overrides are:
  --(dbms_data_mining.glms_ridge_regression, dbms_data_mining.glms_ridge_reg_enable);
END;
/

---------------------
-- CREATE A NEW MODEL
--
-- Build a new GLM Model
BEGIN
  DBMS_DATA_MINING.CREATE_MODEL(
    model_name          => 'GLMC_SH_Clas_sample',
    mining_function     => dbms_data_mining.classification,
    data_table_name     => 'mining_data_build_v',
    case_id_column_name => 'cust_id',
    target_column_name  => 'affinity_card',
    settings_table_name => 'glmc_sh_sample_settings');
END;
/

-------------------------
-- DISPLAY MODEL SETTINGS
--
column setting_name format a30
column setting_value format a30
SELECT setting_name, setting_value
  FROM user_mining_model_settings
 WHERE model_name = 'GLMC_SH_CLAS_SAMPLE'
ORDER BY setting_name;

--------------------------
-- DISPLAY MODEL SIGNATURE
--
column attribute_name format a40
column attribute_type format a20
SELECT attribute_name, attribute_type
  FROM user_mining_model_attributes
 WHERE model_name = 'GLMC_SH_CLAS_SAMPLE'
ORDER BY attribute_name;

------------------------
-- DISPLAY MODEL DETAILS
--
-- Global statistics
SELECT *
  FROM TABLE(dbms_data_mining.get_model_details_global('GLMC_SH_Clas_sample'))
ORDER BY global_detail_name;

-- We see from the global details that the covariance matrix was invalid:
--   VALID_COVARIANCE_MATRIX is 0
-- As a result we only get a limited set of diagnostics, a sample of which
-- are shown below.
--
-- Another important consequence of an invalid covariance matrix is that
-- the model cannot predict confidence bounds - i.e. the result of
-- PREDICTION_BOUNDS function in a SQL query is NULL.
--
-- We also note that RIDGE REGRESSION has been enabled. It may happen that
-- the ridge regression model built on multicollinear predictors is better
-- than a non-ridge model built on a subset of the predictors where the
-- multicollinearity has been reduced sufficiently to produce a valid
-- covariance matrix.
--

-- Coefficient statistics
SET line 120
column class format a20
column attribute_name format a20  
column attribute_subname format a20
column attribute_value format a20  
  
SELECT *
  FROM (SELECT *
          FROM TABLE(dbms_data_mining.get_model_details_glm(
                       'GLMC_SH_Clas_sample'))
        ORDER BY class, attribute_name, attribute_value)
 WHERE ROWNUM < 11;  
    
-- Row diagnostics
SELECT *
  FROM (SELECT *
          FROM glmc_sh_sample_diag 
        ORDER BY case_id)
 WHERE ROWNUM < 11;

-----------------------------------------------------------------------
--                               TEST THE MODEL
-----------------------------------------------------------------------
------------------------------------
-- COMPUTE METRICS TO TEST THE MODEL
--
-- The queries shown below demonstrate the use of new SQL data mining functions
-- along with analytic functions to compute the various test metrics.
--
-- Modelname:             glmc_sh_clas_sample
-- Target attribute:      affinity_card
-- Positive target value: 1
-- (Change as appropriate for a different example)

-- Compute CONFUSION MATRIX
--
-- This query demonstates how to generate a confusion matrix using the new
-- SQL prediction functions for scoring. The returned columns match the
-- schema of the table generated by COMPUTE_CONFUSION_MATRIX procedure.
--
SELECT affinity_card AS actual_target_value,
       PREDICTION(glmc_sh_clas_sample USING *) AS predicted_target_value,
       COUNT(*) AS value
  FROM mining_data_test_v
 GROUP BY affinity_card, PREDICTION(glmc_sh_clas_sample USING *)
 ORDER BY 1, 2;

-- Compute ACCURACY
--
column accuracy format 9.99

SELECT SUM(correct)/COUNT(*) AS accuracy
  FROM (SELECT DECODE(affinity_card,
                 PREDICTION(glmc_sh_clas_sample USING *), 1, 0) AS correct
          FROM mining_data_test_v);
  
-- Compute AUC (Area Under the roc Curve)
-- (See notes on ROC Curve and AUC computation in dmsvcdem.sql)
--
column auc format 9.99
WITH
pos_prob_and_counts AS (
SELECT PREDICTION_PROBABILITY(glmc_sh_clas_sample, 1 USING *) pos_prob,
       DECODE(affinity_card, 1, 1, 0) pos_cnt
  FROM mining_data_test_v
),
tpf_fpf AS (
SELECT  pos_cnt,
       SUM(pos_cnt) OVER (ORDER BY pos_prob DESC) /
         SUM(pos_cnt) OVER () tpf,
       SUM(1 - pos_cnt) OVER (ORDER BY pos_prob DESC) /
         SUM(1 - pos_cnt) OVER () fpf
  FROM pos_prob_and_counts
),
trapezoid_areas AS (
SELECT 0.5 * (fpf - LAG(fpf, 1, 0) OVER (ORDER BY fpf, tpf)) *
        (tpf + LAG(tpf, 1, 0) OVER (ORDER BY fpf, tpf)) area
  FROM tpf_fpf
 WHERE pos_cnt = 1
    OR (tpf = 1 AND fpf = 1)
)
SELECT SUM(area) auc
  FROM trapezoid_areas;

-----------------------------------------------------------------------
--                            BUILD THE NEW MODEL
-----------------------------------------------------------------------

-- Further analysis (i.e. inspecting data for constants, correlations)
-- showed that excluding the columns PRINTER_SUPPLIES, CUST_INCOME_level and
-- BOOKKEEPING_APPLICATION avoid singularities in the data that lead to
-- the invalid covariance matrix. For example, PRINTER_SUPPLIES has a
-- constant 1 as the value in all rows, and the other two columns were
-- found to be strongly correlated with yet another column.

-- So, in order to demo a full set of diagnostics, we create a new view
-- that excludes the problematic columns, and build a new model against
-- this training data.
  
-- Cleanup diagnostics table
BEGIN EXECUTE IMMEDIATE 'DROP TABLE glmc_sh_sample_diag';
EXCEPTION WHEN OTHERS THEN NULL; END;
/

-- Cleanup old build view
BEGIN EXECUTE IMMEDIATE 'DROP VIEW mining_data_build_v2';
EXCEPTION WHEN OTHERS THEN NULL; END;
/

-- CREATE a view that excludes columns PRINTER_SUPPLIES, CUST_INCOME_level 
-- and BOOKKEEPING_APPLICATION
CREATE VIEW mining_data_build_v2 AS
SELECT CUST_id, CUST_gender, age, CUST_MARITAL_status, COUNTRY_name,
       education, occupation, HOUSEHOLD_size, YRS_residence, AFFINITY_card,
       BULK_PACK_diskettes, FLAT_PANEL_monitor, HOME_THEATER_package,
       Y_BOX_games, OS_DOC_SET_kanji
  FROM mining_data_build_v;

---------------------
-- CREATE A NEW MODEL
--
-- Cleanup old model with the same name for repeat runs
BEGIN DBMS_DATA_MINING.DROP_MODEL('GLMC_SH_Clas_sample');
EXCEPTION WHEN OTHERS THEN NULL; END;
/

-- Build a new GLM Model
BEGIN
  DBMS_DATA_MINING.CREATE_MODEL(
    model_name          => 'GLMC_SH_Clas_sample',
    mining_function     => dbms_data_mining.classification,
    data_table_name     => 'mining_data_build_v2',
    case_id_column_name => 'cust_id',
    target_column_name  => 'affinity_card',
    settings_table_name => 'glmc_sh_sample_settings');
END;
/

-------------------------
-- DISPLAY MODEL SETTINGS
--
column setting_name format a30
column setting_value format a30
SELECT setting_name, setting_value
  FROM user_mining_model_settings
 WHERE model_name = 'GLMC_SH_CLAS_SAMPLE'
ORDER BY setting_name;

--------------------------
-- DISPLAY MODEL SIGNATURE
--
column attribute_name format a40
column attribute_type format a20
SELECT attribute_name, attribute_type
  FROM user_mining_model_attributes
 WHERE model_name = 'GLMC_SH_CLAS_SAMPLE'
ORDER BY attribute_name;

------------------------
-- DISPLAY MODEL DETAILS
--
-- Global statistics
SELECT *
  FROM TABLE(dbms_data_mining.get_model_details_global('GLMC_SH_Clas_sample'))
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
                       'GLMC_SH_Clas_sample')) 
        ORDER BY class, attribute_name, attribute_value)
 WHERE ROWNUM < 11;  
    
-- Row diagnostics
SELECT *
  FROM (SELECT *
         FROM glmc_sh_sample_diag 
        ORDER BY case_id)
 WHERE ROWNUM < 11;

-----------------------------------------------------------------------
--                               TEST THE NEW MODEL
-----------------------------------------------------------------------
------------------------------------
-- COMPUTE METRICS TO TEST THE MODEL
--
-- The queries shown below demonstrate the use of new SQL data mining functions
-- along with analytic functions to compute various test metrics. 
--
-- Modelname:             glmc_sh_clas_sample
-- Target attribute:      affinity_card
-- Positive target value: 1
-- (Change these as appropriate for a different example)

-- Compute CONFUSION MATRIX
--
-- This query demonstates how to generate a confusion matrix using the new
-- SQL prediction functions for scoring. The returned columns match the
-- schema of the table generated by COMPUTE_CONFUSION_MATRIX procedure.
--
SELECT affinity_card AS actual_target_value,
       PREDICTION(glmc_sh_clas_sample USING *) AS predicted_target_value,
       COUNT(*) AS value
  FROM mining_data_test_v
 GROUP BY affinity_card, PREDICTION(glmc_sh_clas_sample USING *)
 ORDER BY 1, 2;

-- Compute ACCURACY
--
column accuracy format 9.99

SELECT SUM(correct)/COUNT(*) AS accuracy
  FROM (SELECT DECODE(affinity_card,
                 PREDICTION(glmc_sh_clas_sample USING *), 1, 0) AS correct
          FROM mining_data_test_v);

-- Compute AUC (Area Under the roc Curve)
--
-- See notes on ROC Curve and AUC computation above
--
column auc format 9.99
WITH
pos_prob_and_counts AS (
SELECT PREDICTION_PROBABILITY(glmc_sh_clas_sample, 1 USING *) pos_prob,
       DECODE(affinity_card, 1, 1, 0) pos_cnt
  FROM mining_data_test_v
),
tpf_fpf AS (
SELECT  pos_cnt,
       SUM(pos_cnt) OVER (ORDER BY pos_prob DESC) /
         SUM(pos_cnt) OVER () tpf,
       SUM(1 - pos_cnt) OVER (ORDER BY pos_prob DESC) /
         SUM(1 - pos_cnt) OVER () fpf
  FROM pos_prob_and_counts
),
trapezoid_areas AS (
SELECT 0.5 * (fpf - LAG(fpf, 1, 0) OVER (ORDER BY fpf, tpf)) *
        (tpf + LAG(tpf, 1, 0) OVER (ORDER BY fpf, tpf)) area
  FROM tpf_fpf
 WHERE pos_cnt = 1
    OR (tpf = 1 AND fpf = 1)
)
SELECT SUM(area) auc
  FROM trapezoid_areas;

-- Judging from the accuracy and AUC, the ridge regression model is 
-- marginally superior to the model produced on the reduced predictor set.

--------------------------------------------------------------------------
--                               SCORE DATA
--------------------------------------------------------------------------

-- now that the model has a valid covariance matrix, it is possible
-- to obtain confidence bounds.
SELECT *
  FROM (SELECT CUST_ID,
               PREDICTION(GLMC_SH_Clas_sample USING *) pr,
               PREDICTION_PROBABILITY(GLMC_SH_Clas_sample USING *) pb,
               PREDICTION_BOUNDS(GLMC_SH_Clas_sample USING *).lower pl,
               PREDICTION_BOUNDS(GLMC_SH_Clas_sample USING *).upper pu
          FROM mining_data_apply_v
        ORDER BY CUST_ID)
 WHERE ROWNUM < 11
ORDER BY CUST_ID;
