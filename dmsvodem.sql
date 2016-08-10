Rem
Rem $Header: dmsvodem.sql 25-oct-2007.11:34:45 ramkrish Exp $
Rem
Rem dmsvodem.sql
Rem
Rem Copyright (c) 2004, 2007, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      dmsvodem.sql - Sample program for the DBMS_DATA_MINING package.
Rem
Rem    DESCRIPTION
Rem      This script creates an anomaly detection model
Rem      for data analysis and outlier identification using the 
Rem      one-class SVM algorithm 
Rem      and data in the SH (Sales History)schema in the RDBMS. 
Rem
Rem    NOTES
Rem    
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    ramkrish    06/14/07 - remove commit after settings
Rem    ramkrish    10/25/07 - replace deprecated get_model calls with catalog
Rem                           queries
Rem    ktaylor     07/11/05 - minor edits to comments
Rem    jcjeon      01/18/05 - add column format 
Rem    bmilenov    10/28/04 - bmilenov_oneclass_demo
Rem    bmilenov    10/25/04 - Remove dbms_output statements 
Rem    bmilenov    10/22/04 - Comment revision 
Rem    bmilenov    10/20/04 - Created
Rem

SET serveroutput ON
SET trimspool ON  
SET pages 10000
SET echo ON

-----------------------------------------------------------------------
--                            SAMPLE PROBLEM
-----------------------------------------------------------------------
-- Given demographics about a set of customers that are known to have 
-- an affinity card, 1) find the most atypical members of this group 
-- (outlier identification), 2) discover the common demographic 
-- characteristics of the most typical customers with affinity  card, 
-- and 3) compute how typical a given new/hypothetical customer is.
--
-------
-- DATA
-------
-- The data for this sample is composed from base tables in the SH schema
-- (See Sample Schema Documentation) and presented through a view:
-- mining_data_one_class_v
-- (See dmsh.sql for view definition).
--
--

-----------------------------------------------------------------------
--                            BUILD THE MODEL
-----------------------------------------------------------------------

-- Cleanup old build data preparation objects, if any
BEGIN
  EXECUTE IMMEDIATE 'DROP TABLE svmo_sh_sample_norm';
  EXECUTE IMMEDIATE 'DROP VIEW svmo_sh_sample_prepared';
EXCEPTION WHEN OTHERS THEN
  NULL;
END;
/
-- Cleanup old model with the same name (if any)
BEGIN DBMS_DATA_MINING.DROP_MODEL('SVMO_SH_Clas_sample');
EXCEPTION WHEN OTHERS THEN NULL; END;
/

--------------------------------
-- PREPARE DATA
--

-- 1. Missing Value treatment for all Predictors
-- This is an optional step. Missing value imputation is recommended
-- when the fraction of missing at random values is high. Sparse data
-- representation where missing values encode 0 (non-present) should 
-- be left as it is.
-- For numerical attributes, if z-score normalization is applied, the
-- missing value treatment step can be skipped.
-- For missing value treatment sample code see dmsvcdem.sql.
--
-- 2. Outlier Treatment
-- This is an optional step. Large outlier values in individual attributes
-- can result in sub-optimal normalization ouput and sub-optimal models.
-- For outlier treatment sample code see dmsvcdem.sql.
--
-- 3. Normalization
-- Normalization brings all numeric attributes on a similar scale. Unless 
-- there are reasons for weighting individual attributes differently, 
-- this step is strongly recommended. In the presence of numeric only
-- attributes either min_max or z-score normalization can be used. If the
-- dataset has both numeric and categorical attributes, min_max 
-- normalization is preferred.
--
BEGIN
  -- Normalize numerical attributes: AGE, YRS_RESIDENCE
  -- CUST_ID is the record unique identifier and therefore excluded 
  -- from normalization 
  -- Create normalization specification 
  DBMS_DATA_MINING_TRANSFORM.CREATE_NORM_LIN (
    norm_table_name => 'svmo_sh_sample_norm');        

  DBMS_DATA_MINING_TRANSFORM.INSERT_NORM_LIN_MINMAX (
    norm_table_name => 'svmo_sh_sample_norm',
    data_table_name => 'mining_data_one_class_v',
    exclude_list    => DBMS_DATA_MINING_TRANSFORM.COLUMN_LIST (
                       'cust_id'));

  -- Create the transformed view
  DBMS_DATA_MINING_TRANSFORM.XFORM_NORM_LIN (
    norm_table_name => 'svmo_sh_sample_norm',
    data_table_name => 'mining_data_one_class_v',
    xform_view_name => 'svmo_sh_sample_prepared');

END;
/

-------------------
-- SPECIFY SETTINGS
--
-- Cleanup old settings table (if any)
BEGIN
  EXECUTE IMMEDIATE 'DROP TABLE svmo_sh_sample_settings';
EXCEPTION WHEN OTHERS THEN
  NULL;
END;
/

-- CREATE AND POPULATE A SETTINGS TABLE
--
set echo off
CREATE TABLE svmo_sh_sample_settings (
  setting_name  VARCHAR2(30),
  setting_value VARCHAR2(30));
set echo on

BEGIN       
  -- Populate settings table
  -- SVM needs to be selected explicitly (default classifier: Naive Bayes)
   
  -- Examples of other possible overrides are:
  -- select a different rate of outliers in the data (default 0.1)
  -- (dbms_data_mining.svms_outlier_rate, ,0.05);
  -- select a kernel type (default kernel: selected by the algorithm)
  -- (dbms_data_mining.svms_kernel_function, dbms_data_mining.svms_linear);
  -- (dbms_data_mining.svms_kernel_function, dbms_data_mining.svms_gaussian);
  -- turn off active learning (enabled by default)
  -- (dbms_data_mining.svms_active_learning, dbms_data_mining.svms_al_disable);
   
  INSERT INTO svmo_sh_sample_settings (setting_name, setting_value) VALUES
  (dbms_data_mining.algo_name, dbms_data_mining.algo_support_vector_machines);  
END;
/

---------------------
-- CREATE A MODEL
--
-- Build a new one-class SVM Model
-- Note the NULL sprecification for target column name
BEGIN
  DBMS_DATA_MINING.CREATE_MODEL(
    model_name          => 'SVMO_SH_Clas_sample',
    mining_function     => dbms_data_mining.classification,
    data_table_name     => 'svmo_sh_sample_prepared',
    case_id_column_name => 'cust_id',
    target_column_name  => NULL,
    settings_table_name => 'svmo_sh_sample_settings');
END;
/

-------------------------
-- DISPLAY MODEL SETTINGS
--
column setting_name format a30
column setting_value format a30
SELECT setting_name, setting_value
  FROM user_mining_model_settings
 WHERE model_name = 'SVMO_SH_CLAS_SAMPLE'
ORDER BY setting_name;

--------------------------
-- DISPLAY MODEL SIGNATURE
--
-- For sample code displaying SVM signature see dmsvcdem.sql.

------------------------
-- DISPLAY MODEL DETAILS
--
-- Model details are available only for SVM models with linear kernel.
-- For SVM model details sample code see dmsvcdem.sql.
--

-----------------------------------------------------------------------
--                               APPLY THE MODEL
-----------------------------------------------------------------------

-- Depending on the business case, the model can be scored against the
-- build data (e.g, business cases 1 and 2) or against new, previously
-- unseen data (e.g., business case 3). New apply data needs to undergo 
-- the same transformations as the build data (see business case 3).

------------------
-- BUSINESS CASE 1
-- Find the top 10 outliers - customers that differ the most from 
-- the rest of the population. Depending on the application, such 
-- atypical customers can be removed from the data (data cleansing).
--

SELECT cust_id FROM (
 SELECT cust_id
 FROM svmo_sh_sample_prepared
  ORDER BY prediction_probability(SVMO_SH_Clas_sample, 0 using *) DESC, 1)
WHERE rownum < 11;


------------------
-- BUSINESS CASE 2
-- Find demographic characteristics of the typical affinity card members.
-- These statistics will not be influenced by outliers and are likely to
-- to provide a more truthful picture of the population of interest than
-- statistics computed on the entire group of affinity members.
-- Statistics are computed on the original (non-transformed) data.
column cust_gender format a12
SELECT a.cust_gender, round(avg(a.age)) age, 
       round(avg(a.yrs_residence)) yrs_residence,
       count(*) cnt
FROM mining_data_one_class_v a,
     svmo_sh_sample_prepared b
WHERE prediction(SVMO_SH_Clas_sample using b.*) = 1
AND a.cust_id=b.cust_id  
GROUP BY a.cust_gender
ORDER BY a.cust_gender;  


------------------
-- BUSINESS CASE 3
-- 
-- Compute probability of a new/hypothetical customer being a typical  
-- affinity card holder.
-- Normalization of the numerical attributes is performed on-the-fly.
--
column prob_typical format 9.99
  
WITH age_norm AS (
SELECT shift, scale FROM svmo_sh_sample_norm WHERE col = 'AGE'),
yrs_residence_norm AS (
SELECT shift, scale FROM svmo_sh_sample_norm WHERE col = 'YRS_RESIDENCE')
SELECT prediction_probability(SVMO_SH_Clas_sample, 1 using 
                             (44 - a.shift)/a.scale AS age,
                             (6 - b.shift)/b.scale AS yrs_residence,
                             'Bach.' AS education,
                             'Married' AS cust_marital_status,
                             'Exec.' AS occupation,
                             'United States of America' AS country_name,
                             'M' AS cust_gender,
                             'L: 300,000 and above' AS cust_income_level,
                             '3' AS houshold_size
                             ) prob_typical
FROM age_norm a, yrs_residence_norm b;

