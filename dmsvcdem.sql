Rem
Rem $Header: dmsvcdem.sql 24-mar-2008.12:41:40 ramkrish Exp $
Rem
Rem dmsvcdem.sql
Rem
Rem Copyright (c) 2003, 2008, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      dmsvcdem.sql - Sample program for the DBMS_DATA_MINING package.
Rem
Rem    DESCRIPTION
Rem      This script creates a classification model
Rem      using the SVM algorithm
Rem      and data in the SH (Sales History) schema in the RDBMS. 
Rem
Rem    NOTES
Rem      This script demonstrates the use of the new Oracle10gR2
Rem      SQL functions for scoring models against new data, and
Rem      the computation of various test metrics based on these
Rem      new SQL functions.
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    ramkrish    03/06/08 - bug 6820838 - clas_weights instead of priors
Rem    ramkrish    06/14/07 - remove commit after settings
Rem    ramkrish    10/25/07 - replace deprecated get_model calls with catalog
Rem                           queries
Rem    jyarmus     10/18/06 - reduce precision of accuracy and AUC
Rem    ktaylor     07/11/05 - minor edits to comments 
Rem    jcjeon      01/18/05 - add column format 
Rem    dmukhin     01/12/05 - bug 4053211: missing value treatment
Rem    dmukhin     12/07/04 - 4053822 - lift and roc computation 
Rem    bmilenov    11/04/04 - Edit comments, add priors, remove costs
Rem    ramkrish    09/27/04 - add data analysis and comments/cleanup
Rem    bmilenov    05/18/04 - Change zscore to minmax normalization
Rem    ramkrish    10/02/03 - Creation
  
SET serveroutput ON
SET trimspool ON  
SET pages 10000
SET echo ON

-----------------------------------------------------------------------
--                            SAMPLE PROBLEM
-----------------------------------------------------------------------
-- Given demographic and purchase data about a set of customers, predict
-- customer's response to an affinity card program using an SVM classifier.
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

-- For classification using SVM, perform the following on mining data.
--
-- 1. Missing Value treatment for Predictors in Build, Test and Apply data
--
--    "Missing Value" here implies "Missing at Random" values, that is, 
--    there is no pattern to the missing data, a NULL value does not have a
--    special meaning, and there is no correlation either direct or indirect
--    with the target or other predictor values.
--
--    Missing value imputation is recommended when the fraction of missing 
--    at random values is high compared to the overall attribute value set.
--
--    Contrast this with a "sparse" dataset - where a NULL value for
--    an attribute encodes a zero/non-present. Typical examples of domains
--    with sparse data are market basket and text mining. Missing value
--    imputation should not be used on sparse data. By default, SVM
--    interprets all NULL values for a given attribute as "sparse".
--
--    Based on the business problem, one should designate the NULL value
--    for a given attribute to be "missing at random" or "sparse". For
--    example, a NULL value for AGE in demographic data can be missing
--    at random if omitting this information is a random occurrence.
--    In contrast, a shopping cart record with NULL values for certain
--    items is usually considered sparse - because a customer cannot
--    buy the entire inventory of items in a store.
--
--    
--
--    Here we are showing a simple approach to missing value treatment:
--
--    1.1. Compute the mean for numerical attributes, and mode for
--         categoricals. Store them away in a table for use with
--         Test and Apply data.
--
--    1.2. Replace the NULLs in the build, test, and apply data
--         with the computed mean (for numerical), mode (for categorical).
--
--
-- 2. Outlier Treatment for Predictors for Build data
--
--    For SVM, we recommended that the data be normalized (individual 
--    attributes need to be on a similar scale) to achieve faster
--    convergence of model builds and more accurate models. Large outliers 
--    in individual attributes can result in sub-optimal normalization
--    output and sub-optimal models. Note that outlier treatment would be also 
--    beneficial for algorithms that use equi-width-binning (e.g., O-Cluster) 
--    since outliers in the data would produce sub-optimal bin boundaries.
--
--    Here we winsorize the tail (see documentation on DBMS_DATA_MINING_TRANSFORM
--    for details) elements of the data as a preparatory step to normalization.
--    We recommend the use of winsorizing to reduce the influence of outliers on
--    the normalization computation. The normalization parameters are computed 
--    on the winsorized data. Once the normalization parameters are computed,
--    they can be used directly on the original data. Winsorizing is not needed
--    at test and apply time - it only affects the specification of the
--    normalization parameters.
--
-- 3. Normalize Predictors in Build, Test, and Apply data
--
--    NOTE: that unlike Regression using SVM, the target attribute is
--          NOT normalized.


-----------------------------------------------------------------------
--                            BUILD THE MODEL
-----------------------------------------------------------------------

-- Cleanup old build data preparation objects for repeat runs
BEGIN EXECUTE IMMEDIATE 'DROP TABLE svmc_sh_sample_miss_num';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP TABLE svmc_sh_sample_miss_cat';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP TABLE svmc_sh_sample_clip';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP TABLE svmc_sh_sample_norm';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP VIEW mining_data_build_miss';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP VIEW mining_data_build';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP VIEW svmc_sh_sample_winsor';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP VIEW svmc_sh_sample_build_prepared';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
-- Cleanup old model with the same name for repeat runs
BEGIN DBMS_DATA_MINING.DROP_MODEL('SVMC_SH_Clas_sample');
EXCEPTION WHEN OTHERS THEN NULL; END;
/

--------------------------------
-- PREPARE BUILD (TRAINING) DATA
--

-- 1. Missing Value treatment for all Predictors and
-- 2. Outlier Treatment and
-- 3. Normalization are performed below.
--    NOTE: that unlike SVM regression, the classification target is NOT
--    normalized here.
--    NOTE: AGE is the only predictor with continuous values in this dataset.
--
BEGIN
  -- Perform missing value treatment for all predictors
  -- create miss tables
  DBMS_DATA_MINING_TRANSFORM.CREATE_MISS_NUM (
    miss_table_name => 'svmc_sh_sample_miss_num');
  DBMS_DATA_MINING_TRANSFORM.CREATE_MISS_CAT (
    miss_table_name => 'svmc_sh_sample_miss_cat');

  -- populate miss tables
  DBMS_DATA_MINING_TRANSFORM.INSERT_MISS_NUM_MEAN (
    miss_table_name => 'svmc_sh_sample_miss_num',
    data_table_name => 'mining_data_build_v',
    exclude_list    => DBMS_DATA_MINING_TRANSFORM.COLUMN_LIST (
                       'affinity_card',
                       'cust_id'));
  DBMS_DATA_MINING_TRANSFORM.INSERT_MISS_CAT_MODE (
    miss_table_name => 'svmc_sh_sample_miss_cat',
    data_table_name => 'mining_data_build_v',
    exclude_list    => DBMS_DATA_MINING_TRANSFORM.COLUMN_LIST (
                       'affinity_card',
                       'cust_id'));

  -- xform input data to replace missing values
  DBMS_DATA_MINING_TRANSFORM.XFORM_MISS_NUM(
    miss_table_name => 'svmc_sh_sample_miss_num',
    data_table_name => 'mining_data_build_v',
    xform_view_name => 'mining_data_build_miss');
  DBMS_DATA_MINING_TRANSFORM.XFORM_MISS_CAT(
    miss_table_name => 'svmc_sh_sample_miss_cat',
    data_table_name => 'mining_data_build_miss',
    xform_view_name => 'mining_data_build');

  -- Perform outlier treatment for: AGE
  -- create clip table
  DBMS_DATA_MINING_TRANSFORM.CREATE_CLIP (
    clip_table_name => 'svmc_sh_sample_clip');

  -- populate clip table
  DBMS_DATA_MINING_TRANSFORM.INSERT_CLIP_WINSOR_TAIL (
    clip_table_name => 'svmc_sh_sample_clip',
    data_table_name => 'mining_data_build',
    tail_frac       => 0.025,
    exclude_list    => DBMS_DATA_MINING_TRANSFORM.COLUMN_LIST (
                       'affinity_card',
                       'bookkeeping_application',
                       'bulk_pack_diskettes',
                       'cust_id',
                       'flat_panel_monitor',
                       'home_theater_package',
                       'os_doc_set_kanji',
                       'printer_supplies',
                       'y_box_games'));

  -- xform input data to winsorized data
  DBMS_DATA_MINING_TRANSFORM.XFORM_CLIP(
    clip_table_name => 'svmc_sh_sample_clip',
    data_table_name => 'mining_data_build',
    xform_view_name => 'svmc_sh_sample_winsor');

  -- normalize numerical attributes: AGE
  -- create normalization table
  DBMS_DATA_MINING_TRANSFORM.CREATE_NORM_LIN (
    norm_table_name => 'svmc_sh_sample_norm');        

  -- populate normalization table based on winsorized data
  DBMS_DATA_MINING_TRANSFORM.INSERT_NORM_LIN_MINMAX (
    norm_table_name => 'svmc_sh_sample_norm',
    data_table_name => 'svmc_sh_sample_winsor',
    exclude_list    => DBMS_DATA_MINING_TRANSFORM.COLUMN_LIST (
                       'affinity_card',
                       'bookkeeping_application',
                       'bulk_pack_diskettes',
                       'cust_id',
                       'flat_panel_monitor',
                       'home_theater_package',
                       'os_doc_set_kanji',
                       'printer_supplies',
                       'y_box_games'));

  -- normalize the original data
  DBMS_DATA_MINING_TRANSFORM.XFORM_NORM_LIN (
    norm_table_name => 'svmc_sh_sample_norm',
    data_table_name => 'mining_data_build',
    xform_view_name => 'svmc_sh_sample_build_prepared');
END;
/

-- BUILD DATA PREPARATION OBJECTS:
-- ------------------------------
-- 1. Numerical Missing Value Table:   svmc_sh_sample_miss_num
-- 2. Categorical Missing Value Table: svmc_sh_sample_miss_cat
-- 3. Winsorize Clip Table:            svmc_sh_sample_clip
-- 4. MinMax Normalization Table:      svmc_sh_sample_norm
-- 5. Input (view) to CREATE_MODEL:    svmc_sh_sample_build_prepared

------------------
-- SPECIFY SETTINGS
--
-- Cleanup old settings table for repeat runs
BEGIN EXECUTE IMMEDIATE 'DROP TABLE svmc_sh_sample_settings';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP TABLE svmc_sh_sample_class_wt';  
EXCEPTION WHEN OTHERS THEN NULL; END;
/

-- CREATE AND POPULATE A CLASS WEIGHTS TABLE
--
-- A class weights table is used to influence the weighting of target classes
-- during model creation. For example, weights of (0.9, 0.1) for a binary
-- problem specify that an error in the first class has significantly
-- higher penalty that an error in the second class. Weights of (0.5, 0.5)
-- do not introduce a differential weight and would produce the same
-- model as when no weights are provided.
--
CREATE TABLE svmc_sh_sample_class_wt (
  target_value NUMBER,
  class_weight NUMBER);
INSERT INTO svmc_sh_sample_class_wt VALUES (0,0.35);
INSERT INTO svmc_sh_sample_class_wt VALUES (1,0.65);
COMMIT;

-- CREATE AND POPULATE A SETTINGS TABLE
--
set echo off
CREATE TABLE svmc_sh_sample_settings (
  setting_name  VARCHAR2(30),
  setting_value VARCHAR2(30));
set echo on

-- The default classification algorithm is Naive Bayes. So override
-- this choice to SVM using a settings table.
-- SVM chooses a kernel type automatically. This choice can be overriden
-- by the user. Linear kernel is preferred high dimensional data, and 
-- Gaussian kernel for low dimensional data. Here we use linear kernel
-- to demonstrate the get_model_details_svm() API, which applies only for
-- models.
--    
BEGIN 
-- Populate settings table
  INSERT INTO svmc_sh_sample_settings (setting_name, setting_value) VALUES
  (dbms_data_mining.algo_name, dbms_data_mining.algo_support_vector_machines);
  INSERT INTO svmc_sh_sample_settings (setting_name, setting_value) VALUES
  (dbms_data_mining.svms_kernel_function, dbms_data_mining.svms_linear);
  INSERT INTO svmc_sh_sample_settings (setting_name, setting_value) VALUES
  (dbms_data_mining.clas_weights_table_name, 'svmc_sh_sample_class_wt');
  -- Examples of other possible overrides are:
  --(dbms_data_mining.svms_kernel_function, dbms_data_mining.svms_gaussian);
  --(dbms_data_mining.svms_kernel_function, dbms_data_mining.svms_complexity_factor); 
  --(dbms_data_mining.svms_kernel_cache_size,100000000);
END;
/

---------------------
-- CREATE A NEW MODEL
--
-- Build a new SVM Model
BEGIN
  DBMS_DATA_MINING.CREATE_MODEL(
    model_name          => 'SVMC_SH_Clas_sample',
    mining_function     => dbms_data_mining.classification,
    data_table_name     => 'svmc_sh_sample_build_prepared',
    case_id_column_name => 'cust_id',
    target_column_name  => 'affinity_card',
    settings_table_name => 'svmc_sh_sample_settings');
END;
/

-------------------------
-- DISPLAY MODEL SETTINGS
--
column setting_name format a30
column setting_value format a30
SELECT setting_name, setting_value
  FROM user_mining_model_settings
 WHERE model_name = 'SVMC_SH_CLAS_SAMPLE'
ORDER BY setting_name;

--------------------------
-- DISPLAY MODEL SIGNATURE
--
column attribute_name format a40
column attribute_type format a20
SELECT attribute_name, attribute_type
  FROM user_mining_model_attributes
 WHERE model_name = 'SVMC_SH_CLAS_SAMPLE'
ORDER BY attribute_name;

------------------------
-- DISPLAY MODEL DETAILS
--
-- The coefficient indicates the relative influence of a given
-- (attribute, value) pair on the target value. A negative
-- coefficient value indicates a negative influence.
--
-- NOTE: The FIRST row in the SVM model details output shows the
--       value for SVM bias under the COEFFICIENT column.
--
SET line 120
column class format a10
column aname format a25
column aval  format a25
column coeff format 9.99

WITH
mod_dtls AS (
SELECT *
  FROM TABLE(DBMS_DATA_MINING.GET_MODEL_DETAILS_SVM('SVMC_SH_Clas_sample'))
),
model_details AS (
SELECT D.class, A.attribute_name, A.attribute_value, A.coefficient
  FROM mod_dtls D,
       TABLE(D.attribute_set) A
ORDER BY D.class, ABS(A.coefficient) DESC
)
SELECT class, attribute_name aname, attribute_value aval, coefficient coeff
  FROM model_details
 WHERE ROWNUM < 11;	        

-----------------------------------------------------------------------
--                               TEST THE MODEL
-----------------------------------------------------------------------

BEGIN EXECUTE IMMEDIATE 'DROP VIEW mining_data_test_miss';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP VIEW mining_data_test';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP VIEW svmc_sh_sample_test_prepared';
EXCEPTION WHEN OTHERS THEN NULL; END;
/

--------------------
-- PREPARE TEST DATA
--
-- If the data for model creation has been prepared, then the data used
-- for testing the model must be prepared in the same manner in order to
-- obtain meaningful results.
--
-- 1. Missing Value treatment for all Predictors and
-- 2. Normalization
-- No outlier treatment will be performed during test and apply. The 
-- normalization step is sufficient, since the normalization parameters 
-- already capture the effects of outlier treatment done with build data.
--
BEGIN
  -- Xform Test data to replace missing values
  DBMS_DATA_MINING_TRANSFORM.XFORM_MISS_NUM(
    miss_table_name => 'svmc_sh_sample_miss_num',
    data_table_name => 'mining_data_test_v',
    xform_view_name => 'mining_data_test_miss');
  DBMS_DATA_MINING_TRANSFORM.XFORM_MISS_CAT(
    miss_table_name => 'svmc_sh_sample_miss_cat',
    data_table_name => 'mining_data_test_miss',
    xform_view_name => 'mining_data_test');

  -- Normalize Test data
  DBMS_DATA_MINING_TRANSFORM.XFORM_NORM_LIN (
    norm_table_name => 'svmc_sh_sample_norm',
    data_table_name => 'mining_data_test',
    xform_view_name => 'svmc_sh_sample_test_prepared');
END;
/

-- Cost matrix - not used here
-- It is possible to use a cost matrix on the apply results (see Naive
-- Bayes demo). However, for SVM we recommend that any known costs
-- be provided to the algorithm during build via the prior mechanism.
-- See discussion above on class weights table usage with SVM.
--

------------------------------------
-- COMPUTE METRICS TO TEST THE MODEL
--
-- The queries shown below demonstrate the use of new SQL data mining functions
-- along with analytic functions to compute various test metrics. In these
-- queries:
--
-- Modelname:             svmc_sh_clas_sample
-- # of Lift Quantiles:   10
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
       PREDICTION(svmc_sh_clas_sample USING *) AS predicted_target_value,
       COUNT(*) AS value
  FROM svmc_sh_sample_test_prepared
 GROUP BY affinity_card, PREDICTION(svmc_sh_clas_sample USING *)
 ORDER BY 1, 2;

-- Compute ACCURACY
--
column accuracy format 9.99

SELECT SUM(correct)/COUNT(*) AS accuracy
  FROM (SELECT DECODE(affinity_card,
                 PREDICTION(svmc_sh_clas_sample USING *), 1, 0) AS correct
          FROM svmc_sh_sample_test_prepared);

-- Compute CUMULATIVE LIFT, GAIN Charts.
--
-- The cumulative gain chart is a popular version of the lift chart, and
-- it maps cumulative gain (Y axis) against the cumulative records (X axis).
--
-- The cumulative lift chart is another popular representation of lift, and
-- it maps cumulative lift (Y axis) against the cumulative records (X axis).
--
-- The query also returns the probability associated with each quantile, so
-- that when the cut-off point for Lift is selected, you can correlate it
-- with a probability value (say P_cutoff). You can then use this value of
-- P_cutoff in a prediction query as follows:
--
-- SELECT *
--   FROM records_to_be_scored
--  WHERE PREDICTION_PROBABILITY(svmc_sh_clas_sample, 1 USING *) > P_cutoff;
--
-- In the query below
--
-- q_num     - Quantile Number
-- pos_cnt   - # of records that predict the positive target
-- pos_prob  - the probability associated with predicting a positive target
--             value for a given new record
-- cume_recs - % Cumulative Records upto quantile
-- cume_gain - % Cumulative Gain
-- cume_lift - Cumulative Lift
--
WITH
pos_prob_and_counts AS (
SELECT PREDICTION_PROBABILITY(svmc_sh_clas_sample, 1 USING *) pos_prob,
       -- hit count for positive target value
       DECODE(affinity_card, 1, 1, 0) pos_cnt
  FROM svmc_sh_sample_test_prepared
),
qtile_and_smear AS (
SELECT NTILE(10) OVER (ORDER BY pos_prob DESC) q_num,
       pos_prob,
       -- smear the counts across records with the same probability to
       -- eliminate potential biased distribution across qtl boundaries
       AVG(pos_cnt) OVER (PARTITION BY pos_prob) pos_cnt
  FROM pos_prob_and_counts
),
cume_and_total_counts AS (
SELECT q_num,
       -- inner sum for counts within q_num groups,
       -- outer sum for cume counts
       MIN(pos_prob) pos_prob,
       SUM(COUNT(*)) OVER (ORDER BY q_num) cume_recs,
       SUM(SUM(pos_cnt)) OVER (ORDER BY q_num) cume_pos_cnt,
       SUM(COUNT(*)) OVER () total_recs,
       SUM(SUM(pos_cnt)) OVER () total_pos_cnt
  FROM qtile_and_smear
 GROUP BY q_num
)
SELECT pos_prob,
       100*(cume_recs/total_recs) cume_recs,
       100*(cume_pos_cnt/total_pos_cnt) cume_gain,
       (cume_pos_cnt/total_pos_cnt)/(cume_recs/total_recs) cume_lift
  FROM cume_and_total_counts
 ORDER BY pos_prob DESC;

-- Compute ROC CURVE
--
-- This can be used to find the operating point for classification.
--
-- The ROC curve plots true positive fraction - TPF (Y axis) against
-- false positive fraction - FPF (X axis). Note that the query picks
-- only the corner points (top tpf switch points for a given fpf) and
-- the last point. It should be noted that the query does not generate
-- the first point, i.e (tpf, fpf) = (0, 0). All of the remaining points
-- are computed, but are then filtered based on the criterion above. For
-- example, the query picks points a,b,c,d and not points o,e,f,g,h,i,j. 
--
-- The Area Under the Curve (next query) is computed using the trapezoid
-- rule applied to all tpf change points (i.e. summing up the areas of
-- the trapezoids formed by the points for each segment along the X axis;
-- (recall that trapezoid Area = 0.5h (A+B); h=> hieght, A, B are sides).
-- In the example, this means the curve covering the area would trace
-- points o,e,a,g,b,c,d.
--
-- |
-- |        .c .j .d
-- |  .b .h .i
-- |  .g
-- .a .f
-- .e
-- .__.__.__.__.__.__
-- o
--
column prob format 9.9999
column fpf  format 9.9999
column tpf  format 9.9999

WITH
pos_prob_and_counts AS (
SELECT PREDICTION_PROBABILITY(svmc_sh_clas_sample, 1 USING *) pos_prob,
       -- hit count for positive target value
       DECODE(affinity_card, 1, 1, 0) pos_cnt
  FROM svmc_sh_sample_test_prepared
),
cume_and_total_counts AS (
SELECT pos_prob,
       pos_cnt,
       SUM(pos_cnt) OVER (ORDER BY pos_prob DESC) cume_pos_cnt, 
       SUM(pos_cnt) OVER () tot_pos_cnt,
       SUM(1 - pos_cnt) OVER (ORDER BY pos_prob DESC) cume_neg_cnt,
       SUM(1 - pos_cnt) OVER () tot_neg_cnt
  FROM pos_prob_and_counts
),
roc_corners AS (
SELECT MIN(pos_prob) pos_prob,
       MAX(cume_pos_cnt) cume_pos_cnt, cume_neg_cnt,
       MAX(tot_pos_cnt) tot_pos_cnt, MAX(tot_neg_cnt) tot_neg_cnt
  FROM cume_and_total_counts
 WHERE pos_cnt = 1                      -- tpf switch points
    OR (cume_pos_cnt = tot_pos_cnt AND  -- top-right point 
        cume_neg_cnt = tot_neg_cnt)
 GROUP BY cume_neg_cnt
)
SELECT pos_prob prob,
       cume_pos_cnt/tot_pos_cnt tpf,
       cume_neg_cnt/tot_neg_cnt fpf,
       cume_pos_cnt tp,
       tot_pos_cnt - cume_pos_cnt fn,
       cume_neg_cnt fp,
       tot_neg_cnt - cume_neg_cnt tn
  FROM roc_corners
 ORDER BY fpf;

-- Compute AUC (Area Under the roc Curve)
--
-- See notes on ROC Curve and AUC computation above
--
column auc format 9.99

WITH
pos_prob_and_counts AS (
SELECT PREDICTION_PROBABILITY(svmc_sh_clas_sample, 1 USING *) pos_prob,
       DECODE(affinity_card, 1, 1, 0) pos_cnt
  FROM svmc_sh_sample_test_prepared
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
--                               APPLY THE MODEL
-----------------------------------------------------------------------

-- Cleanup old scoring data preparation objects for repeat runs
BEGIN EXECUTE IMMEDIATE 'DROP VIEW mining_data_apply_miss';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP VIEW mining_data_apply';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP VIEW svmc_sh_sample_apply_prepared';
EXCEPTION WHEN OTHERS THEN NULL; END;
/

-----------------------
-- PREPARE SCORING DATA
--
-- If the data for model creation has been prepared, then the data
-- to be scored using the model must be prepared in the same manner
-- in order to obtain meaningful results.
--
-- 1. Missing Value treatment for all Predictors and
-- 2. Normalization
-- No outlier treatment will be performed during test and apply. The 
-- normalization step is sufficient, since the normalization parameters 
-- already capture the effects of outlier treatment done with build data.
--
BEGIN
  -- Xform Test data to replace missing values
  DBMS_DATA_MINING_TRANSFORM.XFORM_MISS_NUM(
    miss_table_name => 'svmc_sh_sample_miss_num',
    data_table_name => 'mining_data_apply_v',
    xform_view_name => 'mining_data_apply_miss');
  DBMS_DATA_MINING_TRANSFORM.XFORM_MISS_CAT(
    miss_table_name => 'svmc_sh_sample_miss_cat',
    data_table_name => 'mining_data_apply_miss',
    xform_view_name => 'mining_data_apply');

  -- Normalize the data to be scored
  DBMS_DATA_MINING_TRANSFORM.XFORM_NORM_LIN (
    norm_table_name => 'svmc_sh_sample_norm',
    data_table_name => 'mining_data_apply',
    xform_view_name => 'svmc_sh_sample_apply_prepared');
END;
/

-------------------------------------------------
-- SCORE NEW DATA USING SQL DATA MINING FUNCTIONS
--
-- Note that APPLY results do not need to be reverse transformed, as done
-- in the case of model details. This is because class values of a
-- classification target were not (required to be) binned or normalized.
--
------------------
-- BUSINESS CASE 1
-- Find the 10 customers who live in Italy that are most likely 
-- to use an affinity card.
--
SELECT cust_id
  FROM (SELECT cust_id
          FROM svmc_sh_sample_apply_prepared
         WHERE country_name = 'Italy'
        ORDER BY PREDICTION_PROBABILITY(svmc_sh_clas_sample, 1 USING *) DESC, 1)
WHERE ROWNUM < 11;

------------------
-- BUSINESS CASE 2
-- Find the average age of customers who are likely to use an
-- affinity card. Break out the results by gender.
--
SELECT cust_gender,
       COUNT(*) AS cnt,
       ROUND(AVG(age)) AS avg_age
  FROM svmc_sh_sample_apply_prepared
 WHERE PREDICTION(svmc_sh_clas_sample USING *) = 1
GROUP BY cust_gender
ORDER BY cust_gender;

------------------
-- BUSINESS CASE 3
-- List ten customers (ordered by their id) along with their likelihood to
-- use or reject the affinity card (Note: while this example has a
-- binary target, such a query is useful in multi-class classification -
-- Low, Med, High for example).
--
SELECT T.cust_id, S.prediction, S.probability
  FROM (SELECT cust_id,
               PREDICTION_SET(svmc_sh_clas_sample USING *) pset
          FROM svmc_sh_sample_apply_prepared
         WHERE cust_id < 100011) T,
       TABLE(T.pset) S
ORDER BY cust_id, S.prediction;

------------------
-- BUSINESS CASE 4
-- Find customers whose profession is Tech Support
-- with > 75% likelihood of using the affinity card
--
SELECT cust_id
  FROM svmc_sh_sample_apply_prepared
 WHERE PREDICTION_PROBABILITY(svmc_sh_clas_sample, 1 USING *) > 0.75
       AND occupation = 'TechSup'
ORDER BY cust_id;

