Rem
Rem $Header: dmsvrdem.sql 25-oct-2007.11:34:45 ramkrish Exp $
Rem
Rem dmsvrdem.sql
Rem
Rem Copyright (c) 2003, 2007, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      dmsvrdem.sql - Sample program for the DBMS_DATA_MINING package.
Rem
Rem    DESCRIPTION
Rem      This script creates a regression model
Rem      using the SVM algorithm
Rem      and data in the SH (Sales History) schema in the RDBMS. 
Rem
Rem    NOTES
Rem     
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    ramkrish    06/14/07 - remove commit after settings
Rem    ramkrish    10/25/07 - replace deprecated get_model calls with catalog
Rem                           queries
Rem    jiawang     08/02/06 - ENABLE ADP
Rem    ktaylor     07/11/05 - minor edits to comments
Rem    ramkrish    12/09/04 - fix incorrect table names
Rem    bmilenov    11/04/04 - Edit comments 
Rem    ramkrish    09/28/04 - add data analysis, comments, SQL functions
Rem    bmilenov    05/18/04 - Change zscore to minmax normalization
Rem    pstengar    12/11/03 - modified apply selection query
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
-- For regression using SVM, perform the following on mining data.
--
-- 1. Use Auto Data Preparation
--

-----------------------------------------------------------------------
--                            BUILD THE MODEL
-----------------------------------------------------------------------

-- Cleanup old model with same name (if any)
BEGIN DBMS_DATA_MINING.DROP_MODEL('SVMR_SH_Regr_sample');
EXCEPTION WHEN OTHERS THEN NULL; END;
/

-------------------
-- SPECIFY SETTINGS
--
-- Cleanup old settings table for repeat runs
BEGIN EXECUTE IMMEDIATE 'DROP TABLE svmr_sh_sample_settings';
EXCEPTION WHEN OTHERS THEN NULL; END;
/

-- The default algorithm for regression is SVM.
-- see dmsvcdem.sql on choice of kernel function.
-- 
CREATE TABLE svmr_sh_sample_settings (
  setting_name  VARCHAR2(30),
  setting_value VARCHAR2(30));
 
BEGIN       
  -- Populate settings table
  INSERT INTO svmr_sh_sample_settings (setting_name, setting_value) VALUES
  (dbms_data_mining.svms_kernel_function, dbms_data_mining.svms_gaussian);

  INSERT INTO svmr_sh_sample_settings (setting_name, setting_value) VALUES 
  (dbms_data_mining.prep_auto,dbms_data_mining.prep_auto_on);
  
  -- Examples of other possible overrides are:
  --(dbms_data_mining.svms_conv_tolerance,0.01);
  --(dbms_data_mining.svms_epsilon,0.1);
  --(dbms_data_mining.svms_kernel_cache_size,50000000);
  --(dbms_data_mining.svms_kernel_function,dbms_data_mining.svms_linear);
END;
/

---------------------
-- CREATE A NEW MODEL
--
BEGIN
  DBMS_DATA_MINING.CREATE_MODEL(
    model_name          => 'SVMR_SH_Regr_sample',
    mining_function     => dbms_data_mining.regression,
    data_table_name     => 'mining_data_build_v',
    case_id_column_name => 'cust_id',
    target_column_name  => 'age',
    settings_table_name => 'svmr_sh_sample_settings');
END;
/

-------------------------
-- DISPLAY MODEL SETTINGS
--
column setting_name format a30
column setting_value format a30
SELECT setting_name, setting_value
  FROM user_mining_model_settings
 WHERE model_name = 'SVMR_SH_REGR_SAMPLE'
ORDER BY setting_name;

--------------------------
-- DISPLAY MODEL SIGNATURE
--
column attribute_type format a20
SELECT attribute_name, attribute_type
  FROM user_mining_model_attributes
 WHERE model_name = 'SVMR_SH_REGR_SAMPLE'
ORDER BY attribute_name;

------------------------
-- DISPLAY MODEL DETAILS
--
-- Skip. GET_MODEL_DETAILS_SVM is supported only for Linear Kernels.
-- The current model is built using a Gaussian Kernel (see dmsvcdem.sql).
--

-----------------------------------------------------------------------
--                               TEST THE MODEL
-----------------------------------------------------------------------

------------------------------------
-- COMPUTE METRICS TO TEST THE MODEL
--

CREATE VIEW svmr_sh_sample_test AS
SELECT A.cust_id,
       PREDICTION(svmr_sh_regr_sample USING *) prediction
  FROM mining_data_test_v A;


-- COMPUTE TEST METRICS
--
-- 1. Root Mean Square Error - Sqrt(Mean((x - x')^2))
--
column rmse format 9999.99
SELECT SQRT(AVG((A.prediction - B.age) * (A.prediction - B.age))) rmse
  FROM svmr_sh_sample_test A,
       mining_data_test_v B
 WHERE A.cust_id = B.cust_id;



-- 2. Mean Absolute Error - Mean(|(x - x')|)
--
column mae format 9999.99
SELECT AVG(ABS(a.prediction - B.age)) mae
  FROM svmr_sh_sample_test A,
       mining_data_test_v B
  WHERE A.cust_id = B.cust_id;

-- 3. Residuals
--    If the residuals show substantial variance between
--    the predicted value and the actual, you can consider
--    changing the algorithm parameters.
--
SELECT TO_CHAR(ROUND(prediction, 4)) prediction, residual
  FROM (SELECT A.prediction, (A.prediction - B.age) residual
          FROM svmr_sh_sample_test A,
               mining_data_test_v B
         WHERE A.cust_id = B.cust_id
        ORDER BY A.prediction ASC)
 WHERE ROWNUM < 11;

-----------------------------------------------------------------------
--                               APPLY THE MODEL
-----------------------------------------------------------------------
-------------------------------------------------
-- SCORE NEW DATA USING SQL DATA MINING FUNCTIONS
--
------------------
-- BUSINESS CASE 1
-- Predict the average age of customers, broken out by gender.
--
SELECT A.cust_gender,
       COUNT(*) AS cnt,
       ROUND(
       AVG(PREDICTION(svmr_sh_regr_sample USING A.*)),4)
       AS avg_age
  FROM mining_data_apply_v A
GROUP BY cust_gender
ORDER BY cust_gender;


column pred_age format 999.99
------------------
-- BUSINESS CASE 2
-- Create a 10 bucket histogram of customers from Italy based on their age
-- and return each customer's age group.
--
WITH
cust_italy AS (
SELECT *
  FROM mining_data_apply_v
 WHERE country_name = 'Italy'
)
SELECT cust_id,
       PREDICTION(svmr_sh_regr_sample USING A.*) pred_age,
       WIDTH_BUCKET(
        PREDICTION(svmr_sh_regr_sample USING A.*), 10, 100, 10) "Age Group"
  FROM cust_italy A
ORDER BY pred_age;
