Rem
Rem $Header: dmnbdemo.sql 25-oct-2007.11:36:01 ramkrish Exp $
Rem
Rem dmnbdemo.sql
Rem
Rem Copyright (c) 2003, 2007, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      dmnbdemo.sql - Sample program for the DBMS_DATA_MINING package.
Rem
Rem    DESCRIPTION
Rem      This script creates a classification model
Rem      using the Naive Bayes algorithm
Rem      and data in the SH (Sales History) schema in the RDBMS.
Rem
Rem    NOTES
Rem     
Rem
Rem    MODIFIED   (MM/DD/YY) 
Rem    ramkrish    06/14/07 - remove commit after settings
Rem    ramkrish    10/25/07 - replace deprecated get_model calls with catalog
Rem                           queries
Rem    ktaylor     07/11/05 - Minor edits to comments
Rem    ramkrish    01/28/05 - prediction column format fixes 
Rem    jcjeon      01/18/05 - add column format 
Rem    jyarmus     11/05/04 - review and update cost code
Rem    ramkrish    09/21/04 - add data analysis and comments/cleanup
Rem    ramkrish    10/02/03 - Creation
  
SET serveroutput ON
SET trimspool ON  
SET pages 10000
SET echo ON

-----------------------------------------------------------------------
--                            SAMPLE PROBLEM
-----------------------------------------------------------------------
-- Given demographic data about a set of customers, predict the
-- customer response to an affinity card program using a classifier
-- based on the NB algorithm.

-----------------------------------------------------------------------
--                            SET UP AND ANALYZE THE DATA
-----------------------------------------------------------------------

-- See the corresponding section in dmabdemo.sql - Classification
-- using ABN. The analysis and preparation steps are very similar.

-----------------------------------------------------------------------
--                            BUILD THE MODEL
-----------------------------------------------------------------------

-- Cleanup old build data preparation objects for repeat runs
BEGIN EXECUTE IMMEDIATE 'DROP TABLE nb_sh_sample_num';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP TABLE nb_sh_sample_cat';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP VIEW nb_sh_sample_build_prepared';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP VIEW nb_sh_sample_build_cat';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
-- Cleanup old model with the same name for repeat runs
BEGIN DBMS_DATA_MINING.DROP_MODEL('NB_SH_Clas_sample');
EXCEPTION WHEN OTHERS THEN NULL; END;
/

--------------------------------
-- PREPARE BUILD (TRAINING) DATA
--
-- 1. Missing Value treatment for all Predictors
--    Skipped - see dmsvcdem.sql
--
-- 2. Outlier Treatment
--    Skipped - due to choice of quantile binning
--
-- 3. Binning
--
BEGIN
  -- Bin categorical attributes: OCCUPATION
  DBMS_DATA_MINING_TRANSFORM.CREATE_BIN_CAT (
    bin_table_name => 'nb_sh_sample_cat');
  DBMS_DATA_MINING_TRANSFORM.INSERT_BIN_CAT_FREQ (
    bin_table_name  => 'nb_sh_sample_cat',
    data_table_name => 'mining_data_build_v',
    bin_num         => 7,
    exclude_list    => DBMS_DATA_MINING_TRANSFORM.COLUMN_LIST (
                       'cust_gender',
                       'cust_marital_status',
                       'cust_income_level',
                       'education',
                       'household_size')
  );

  -- Bin numerical attributes: AGE
  DBMS_DATA_MINING_TRANSFORM.CREATE_BIN_NUM (
    bin_table_name => 'nb_sh_sample_num');
  DBMS_DATA_MINING_TRANSFORM.INSERT_BIN_NUM_QTILE (
    bin_table_name  => 'nb_sh_sample_num',
    data_table_name => 'mining_data_build_v',
    bin_num         => 7,
    exclude_list    => DBMS_DATA_MINING_TRANSFORM.COLUMN_LIST (
                       'affinity_card',
                       'bookkeeping_application',
                       'bulk_pack_diskettes',
                       'cust_id',
                       'flat_panel_monitor',
                       'home_theater_package',
                       'os_doc_set_kanji',
                       'printer_supplies',
                       'y_box_games')
  );

  -- Create the transformed view
  -- Execute the first transformation (categorical binning)
  DBMS_DATA_MINING_TRANSFORM.XFORM_BIN_CAT (
    bin_table_name  => 'nb_sh_sample_cat',
    data_table_name => 'mining_data_build_v',
    xform_view_name => 'nb_sh_sample_build_cat');    

  -- Provide the result (nb_sh_sample_build_cat)
  -- to the next transformation (numerical binning)
  DBMS_DATA_MINING_TRANSFORM.XFORM_BIN_NUM (
    bin_table_name  => 'nb_sh_sample_num',
    data_table_name => 'nb_sh_sample_build_cat',
    xform_view_name => 'nb_sh_sample_build_prepared');
END;
/

-- BUILD DATA PREPARATION OBJECTS:
-- ------------------------------
-- 1. Categorical Bin Table:        nb_sh_sample_cat
-- 2. Numerical Bin Table:          nb_sh_sample_num
-- 3. Input (view) to CREATE_MODEL: nb_sh_sample_build_prepared

-------------------
-- SPECIFY SETTINGS
--
-- Cleanup old settings table objects for repeat runs
BEGIN EXECUTE IMMEDIATE 'DROP TABLE nb_sh_sample_settings';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP TABLE nb_sh_sample_priors';  
EXCEPTION WHEN OTHERS THEN NULL; END;
/

-- Algorithm setting:
-- NB is the default classifier, thus there is no need to specify 
-- the algorithm in settings table when the mining_function parameter
-- of the CREATE_MODEL operation specifies classification.

-- CREATE AND POPULATE A PRIORS TABLE
-- The priors represent the overall distribution of the target in
-- the population. By default, the priors are computed from the sample
-- (in this case, the build data). If the sample is known to be a
-- distortion of the population target distribution (because, say,
-- stratified sampling has been employed, or due to some other reason),
-- then the user can override the default by providing a priors table
-- as a setting for model creation. See Oracle Data Mining Concepts Guide
-- for more details.
-- 
CREATE TABLE nb_sh_sample_priors (
  target_value      NUMBER,
  prior_probability NUMBER);
INSERT INTO nb_sh_sample_priors VALUES (0,0.65);
INSERT INTO nb_sh_sample_priors VALUES (1,0.35);

-- CREATE AND POPULATE A SETTINGS TABLE
--
-- Thresholding probabilities
-- To increase the accuracy of its estimates, ODM can optionally eliminate
-- probabilities whose estimates are based on relatively small amounts of data.
--
-- The Singleton fraction refers to the fraction of training data in which a 
-- predictor, e.g., X, takes on a specific value, e.g., x1. 
--
-- Pairwise fraction refers to the fraction of training data in which a 
-- predictor, X, takes on a specific value, x1, **when** the target, T,
-- takes on a specific value t1.
--
-- If the singleton fraction associated with a predictor value or the 
-- pairwise fraction associated with a (predictor value, target value)
-- pair are below certain respective thresholds, they are ignored 
-- in the probability calculations.
-- 
-- Examples settings are:
--(dbms_data_mining.nabs_pairwise_threshold,'0.0001')
--(dbms_data_mining.nabs_singleton_threshold,'0.01')
--
set echo off
CREATE TABLE nb_sh_sample_settings (
  setting_name  VARCHAR2(30),
  setting_value VARCHAR2(30));
set echo on
BEGIN       
  INSERT INTO nb_sh_sample_settings VALUES
    (dbms_data_mining.clas_priors_table_name, 'nb_sh_sample_priors');
END;
/

---------------------
-- CREATE A NEW MODEL
--
-- Build a new NB model
BEGIN
  DBMS_DATA_MINING.CREATE_MODEL(
    model_name          => 'NB_SH_Clas_sample',
    mining_function     => dbms_data_mining.classification,
    data_table_name     => 'nb_sh_sample_build_prepared',
    case_id_column_name => 'cust_id',
    target_column_name  => 'affinity_card',
    settings_table_name => 'nb_sh_sample_settings');
END;
/

-------------------------
-- DISPLAY MODEL SETTINGS
--
column setting_name format a30
column setting_value format a30
SELECT setting_name, setting_value
  FROM user_mining_model_settings
 WHERE model_name = 'NB_SH_CLAS_SAMPLE'
ORDER BY setting_name;

--------------------------
-- DISPLAY MODEL SIGNATURE
--
column attribute_name format a40
column attribute_type format a20
SELECT attribute_name, attribute_type
  FROM user_mining_model_attributes
 WHERE model_name = 'NB_SH_CLAS_SAMPLE'
ORDER BY attribute_name;

------------------------
-- DISPLAY MODEL DETAILS
--
-- Binned data for meaningful model statistics:
-- The number of distinct values for each predictor should be limited
-- so that the probabilities estimated by the model have sufficient support
-- (i.e. number of rows) to be statistically meaningful. If the original
-- data has predictors with large numbers of values, then the data should
-- be binned into groups. This can be done either by using third-party tools
-- or the DBMS_DATA_MINING_TRANSFORM package.
--
-- If the build data is prepared (as in this example), then the training
-- data has been encoded. For numeric data, this means that ranges of
-- values have been grouped into bins and the original value encoded by
-- the bin number. For categorical data, the k most frequent values are
-- retained and the rest are grouped into an OTHER category.
--
-- Thus to display the model statistics in a meaningful way to the user, 
-- the data must be decoded. The model statistics consist of:
-- . (unconditional) distribution of the target - the overall proportion
--   of each target value in the population (termed the prior), and
-- . the conditional probabilities of the various predictor values given
--   each specific target value.
--
-- Using the Bayes formula and the conditional independence assumptions
-- made by Naive Bayes, the probability of each target value can be 
-- computed from the prior and conditional probabilities.
--
--
-- The SQL query presented below does the following.
-- 1. Use the bin boundary tables to create labels
-- 2. Replace attribute values with labels
-- 3. For numeric bins, the labels are "[/(lower_boundary,upper_boundary]/)"
-- 4. For categorical bins, label matches the value it represents
--    Note that this method of categorical label representation
--    will only work for cases where one value corresponds to one bin
-- Target was not binned, hence we don't unbin the target.
--
-- You can replace the model name in the query with your own,
-- and also adapt the query to accomodate other transforms.
--
--
column tname format a14
column tval format a4
column pname format a20
column pval format a13
column priorp format 9.9999
column condp format 9.9999
WITH
bin_label_view AS (
SELECT col, bin, (DECODE(bin,'1','[','(') || lv || ',' || val || ']') label
  FROM (SELECT col,
               bin,
               LAST_VALUE(val) OVER (
                PARTITION BY col ORDER BY val
                ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) lv,
               val
          FROM nb_sh_sample_num)
UNION ALL
SELECT col, bin, val label
  FROM nb_sh_sample_cat
),
model_details AS (
SELECT T.target_attribute_name                                        tname,
       TO_CHAR(
       NVL(T.target_attribute_num_value,T.target_attribute_str_value)) tval,
       C.attribute_name                                               pname,
       NVL(L.label, NVL(C.attribute_str_value, C.attribute_num_value)) pval,
       T.prior_probability                                           priorp,
       C.conditional_probability                                      condp
  FROM TABLE(DBMS_DATA_MINING.GET_MODEL_DETAILS_NB('NB_SH_Clas_sample')) T,
       TABLE(T.conditionals) C,
       bin_label_view L
 WHERE C.attribute_name = L.col (+) AND
       (NVL(C.attribute_str_value,C.attribute_num_value) = L.bin(+))
ORDER BY 1,2,3,4,5,6
)
SELECT tname, tval, pname, pval, priorp, condp
  FROM model_details
 WHERE ROWNUM < 11;

-----------------------------------------------------------------------
--                               TEST THE MODEL
-----------------------------------------------------------------------

-- Cleanup old test data preparation objects for repeat runs
BEGIN EXECUTE IMMEDIATE 'DROP VIEW nb_sh_sample_test_targets';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP VIEW nb_sh_sample_test_prepared';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP VIEW nb_sh_sample_test_cat';
EXCEPTION WHEN OTHERS THEN NULL; END;
/

--------------------
-- PREPARE TEST DATA
--
-- If the data for model creation has been prepared, then the data used
-- for testing the model must be prepared in the same manner in order to
-- obtain meaningful results.
--
-- 1. Missing value treatment - see dmsvcdem.sql for example on what
--    needs to be done with Test and Apply data input.
--
-- 2. Outlier treatment and
-- 3. Binning -- Quantile binning handles both the outlier treatment and
--    binning in this case.
--
-- Transform the test data mining_test_v, using the transformation tables
-- generated during the Build data preparation, to generate a view representing
-- prepared test data.
--
-- If this model is tested in a different schema or instance, the 
-- data preparation objects generated in the CREATE step must also
-- be made available in the target schema/instance. So you must export
-- those objects (i.e. the num and cat bin tables and views) along with 
-- the model to the target user schema.
--
BEGIN
  -- Execute the first transform effected on training data
  DBMS_DATA_MINING_TRANSFORM.XFORM_BIN_CAT (
    bin_table_name  => 'nb_sh_sample_cat',
    data_table_name => 'mining_data_test_v',
    xform_view_name => 'nb_sh_sample_test_cat');

  -- Provide the result to the next transform effected on training data
  DBMS_DATA_MINING_TRANSFORM.XFORM_BIN_NUM (
    bin_table_name  => 'nb_sh_sample_num',
    data_table_name => 'nb_sh_sample_test_cat',
    xform_view_name => 'nb_sh_sample_test_prepared');
END;
/

-- Cleanup old test result objects for repeat runs
BEGIN EXECUTE IMMEDIATE 'DROP TABLE nb_sh_sample_test_apply';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP TABLE nb_sh_sample_confusion_matrix';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP TABLE nb_sh_sample_cm_no_cost';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP TABLE nb_sh_sample_lift';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP TABLE nb_sh_sample_roc';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP TABLE nb_alter_cost';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP TABLE nb_sh_alter_confusion_matrix';
EXCEPTION WHEN OTHERS THEN NULL; END;
/

------------------------------------
-- COMPUTE METRICS TO TEST THE MODEL
--
-- The COMPUTE interfaces that provide the test results require two
-- data inputs:
-- 1. A table or view of targets - i.e. one that provides only the
--    case identifier and target columns of your test data.
-- 2. The table with the results of an APPLY operation on test data.
--

-- CREATE TEST TARGETS VIEW
--
CREATE VIEW nb_sh_sample_test_targets AS
SELECT cust_id, affinity_card
  FROM nb_sh_sample_test_prepared;

-- APPLY MODEL ON TEST DATA
--
BEGIN
  DBMS_DATA_MINING.APPLY(
    model_name          => 'NB_SH_Clas_sample',
    data_table_name     => 'nb_sh_sample_test_prepared',
    case_id_column_name => 'cust_id',
    result_table_name   => 'nb_sh_sample_test_apply');
END;
/

----------------------------------
-- COMPUTE TEST METRICS, WITH COST
--
----------------------
-- Specify cost matrix
--
-- Consider an example where it costs $10 to mail a promotion to a
-- prospective customer and if the prospect becomes a customer, the
-- typical sale including the promotion, is worth $100. Then the cost
-- of missing a customer (i.e. missing a $100 sale) is 10x that of
-- incorrectly indicating that a person is good prospect (spending
-- $10 for the promo). In this case, all prediction errors made by
-- the model are NOT equal. To act on what the model determines to
-- be the most likely (probable) outcome may be a poor choice.
--
-- Suppose that the probability of a BUY reponse is 10% for a given
-- prospect. Then the expected revenue from the prospect is:
--   .10 * $100 - .90 * $10 = $1.
-- The optimal action, given the cost matrix, is to simply mail the
-- promotion to the customer, because the action is profitable ($1).
--
-- In contrast, without the cost matrix, all that can be said is
-- that the most likely response is NO BUY, so don't send the
-- promotion.
--
-- This shows that cost matrices can be very important. 
--
-- The caveat in all this is that the model predicted probabilities
-- may NOT be accurate. For binary targets, a systematic approach to
-- this issue exists. It is ROC, illustrated below. 
--
-- With ROC computed on a test set, the user can see how various model 
-- predicted probability thresholds affect the action of mailing a promotion.
-- Suppose I promote when the probability to BUY exceeds 5, 10, 15%, etc. 
-- What return can I expect? Note that the answer to this question does
-- not rely on the predicted probabilities being accurate, only that
-- they are in approximately the correct rank order. 
--
-- Assuming that the predicted probabilities are accurate, provide the
-- cost matrix table name as input to the RANK_APPLY procedure to get
-- appropriate costed scoring results to determine the most appropriate
-- action.

-- Cleanup old cost matrix table for repeat runs
BEGIN EXECUTE IMMEDIATE 'DROP TABLE nb_sh_cost';
EXCEPTION WHEN OTHERS THEN NULL; END;
/

-- CREATE A COST MATRIX TABLE
--
CREATE TABLE nb_sh_cost (
  actual_target_value    NUMBER,
  predicted_target_value NUMBER,
  cost                   NUMBER);

-- POPULATE THE COST MATRIX
--
INSERT INTO nb_sh_cost VALUES (0,0,0);
INSERT INTO nb_sh_cost VALUES (0,1,.35);
INSERT INTO nb_sh_cost VALUES (1,0,.65);
INSERT INTO nb_sh_cost VALUES (1,1,0);

-- Compute Test Metrics
DECLARE
  v_accuracy         NUMBER;
  v_area_under_curve NUMBER;
BEGIN
   DBMS_DATA_MINING.COMPUTE_CONFUSION_MATRIX (
     accuracy                    => v_accuracy,
     apply_result_table_name     => 'nb_sh_sample_test_apply',
     target_table_name           => 'nb_sh_sample_test_targets',
     case_id_column_name         => 'cust_id',
     target_column_name          => 'affinity_card',
     confusion_matrix_table_name => 'nb_sh_sample_confusion_matrix',
     score_column_name           => 'PREDICTION',   -- default
     score_criterion_column_name => 'PROBABILITY',  -- default
     cost_matrix_table_name      => 'nb_sh_cost');
   DBMS_OUTPUT.PUT_LINE('**** MODEL ACCURACY ****: ' || ROUND(v_accuracy,4));
 
   DBMS_DATA_MINING.COMPUTE_LIFT (
     apply_result_table_name => 'nb_sh_sample_test_apply',
     target_table_name       => 'nb_sh_sample_test_targets',
     case_id_column_name     => 'cust_id',
     target_column_name      => 'affinity_card',
     lift_table_name         => 'nb_sh_sample_lift',
     positive_target_value   => '1',
     num_quantiles           => '10',
     cost_matrix_table_name  => 'nb_sh_cost');

   DBMS_DATA_MINING.COMPUTE_ROC (
     roc_area_under_curve        => v_area_under_curve,
     apply_result_table_name     => 'nb_sh_sample_test_apply',
     target_table_name           => 'nb_sh_sample_test_targets',
     case_id_column_name         => 'cust_id',
     target_column_name          => 'affinity_card',
     roc_table_name              => 'nb_sh_sample_roc',
     positive_target_value       => '1',
     score_column_name           => 'PREDICTION',
     score_criterion_column_name => 'PROBABILITY');
   DBMS_OUTPUT.PUT_LINE('**** AREA UNDER ROC CURVE ****: ' ||
     ROUND(v_area_under_curve,4));
END;
/

-- TEST RESULT OBJECTS:
-- -------------------
-- 1. Confusion matrix Table: nb_sh_sample_confusion_matrix
-- 2. Lift Table:             nb_sh_sample_lift
-- 3. ROC Table:              nb_sh_sample_roc
--

-- DISPLAY CONFUSION MATRIX
--
-- NOTES ON COST (contd):
-- This section illustrates the effect of the cost matrix on the per-class
-- errors in the confusion matrix. First, compute the Confusion Matrix with
-- costs. Our cost matrix assumes that ratio of the cost of an error in 
-- class 1 to class 0 is 65:35 (say, where 1 => BUY and 0 => NO BUY).

column predicted format 9;
SELECT actual_target_value as actual, 
       predicted_target_value as predicted, 
       value as count
  FROM nb_sh_sample_confusion_matrix
ORDER BY actual_target_value, predicted_target_value;

-- Compute the confusion matrix without costs for later analysis
DECLARE
  v_accuracy         NUMBER;
  v_area_under_curve NUMBER;
BEGIN
   DBMS_DATA_MINING.COMPUTE_CONFUSION_MATRIX (
     accuracy                    => v_accuracy,
     apply_result_table_name     => 'nb_sh_sample_test_apply',
     target_table_name           => 'nb_sh_sample_test_targets',
     case_id_column_name         => 'cust_id',
     target_column_name          => 'affinity_card',
     confusion_matrix_table_name => 'nb_sh_sample_cm_no_cost',
     score_column_name           => 'PREDICTION',
     score_criterion_column_name => 'PROBABILITY');
   DBMS_OUTPUT.PUT_LINE('** ACCURACY W/ NO COST **: ' || ROUND(v_accuracy,4));
END;
/

-- The Cost matrix is:
--    0     0.35
--    0.65  0
--
-- Confusion matrix with Cost:
--    868  286
--     56  290
-- 868 correct predictions, and 290 misclassifications
-- Overall Cost (286 x 0.35 + 56 x 0.65 = 136.5)
-- Per class accuracy  0: 0.752166378
--                     1: 0.838150289 
-- Accuracy: 0.772
-- Overall Cost can be computed using this query:
--
SELECT (SELECT (C.cost * M.value)
          FROM nb_sh_sample_confusion_matrix M,
               nb_sh_cost C
         WHERE C.actual_target_value = '0' AND C.predicted_target_value = '1'
           AND M.actual_target_value = '0' AND M.predicted_target_value = '1')
       +
       (SELECT (C.cost * M.value) c2
          FROM nb_sh_sample_confusion_matrix M,
               nb_sh_cost C
         WHERE C.actual_target_value = '1' AND C.predicted_target_value = '0'
           AND M.actual_target_value = '1' AND M.predicted_target_value = '0')
       cost
FROM DUAL;

-- Confusion matrix without Cost:
--
column predicted format 9;
SELECT actual_target_value as actual, 
       predicted_target_value as predicted, 
       value as count
  FROM nb_sh_sample_cm_no_cost
ORDER BY actual_target_value, predicted_target_value;

-- Confusion matrix with Cost:
--    901  253
--     65  281
-- 901 correct predictions, and 281 misclassifications
-- Overall Cost (253 x 0.35 + 65 x 0.65) = 130.8
-- Per class accuracy  0: 0.780762565
--                     1: 0.812138728
-- Accuracy: 0.78
--
-- Several points are illustrated here:
-- 1. The cost matrix causes an increase in the class 1 accuracy
--    (0.83 vs 0.81) at the expense of the class 0 accuracy (0.75 vs 0.78).
-- 2. The overall accuracy is down (0.772 vs 0.78)
-- 3. The predicted probabilities are NOT accurate enough to minimize cost.
--    Note that the cost is lower for no cost matrix model (136.5 vs 130)!
--
-- Hence, the decision model is likely to benefit from an ROC analysis. 

-- DISPLAY ROC - TOP 10 PROBABILITIES
--
column prob format .9999
column tp format 9999
column fn format 9999
column fp format 9999
column tn format 9999
column tpf format 9.9999
column fpf format 9.9999
column nb_cost format 9999.99
SELECT *
  FROM (SELECT * 
          FROM (SELECT ROUND(probability,4) prob,
                       true_positives  tp,
                       false_negatives fn,
                       false_positives fp,
                       true_negatives  tn,
                       ROUND(true_positive_fraction,4) tpf,
                       ROUND(false_positive_fraction,4) fpf,
                       .35 * false_positives + .65 * false_negatives nb_cost
                  FROM nb_sh_sample_roc)
        ORDER BY nb_cost)
 WHERE ROWNUM < 11;

-- Here we see 10 different probability thresholds resulting in
-- confusion matrices with a lower overall cost (< 130) than a
-- straight-forward apply with or without a cost matrix. 
--
-- Now, let us create a cost matrix from the optimal threshold, i.e.,
-- one whose action is to most closely mimic the user cost matrix.
-- Let Poptimal = Probability corresponding to the minimum cost
--                computed from the ROC table above
--
-- Find the ratio of costs that causes breakeven expected cost at
-- at the optimal probability threshold:
--
--    Cost(misclassify 1) = (1 - Poptimal)/Poptimal
--    Cost(misclassify 0) = 1.0
--
-- The following query constructs the alternative cost matrix
-- based on the above rationale.
--
CREATE TABLE nb_alter_cost AS
WITH
cost_q AS (
SELECT probability,
       (.35 * false_positives + .65 * false_negatives) nb_cost
  FROM nb_sh_sample_roc
),
min_cost AS (
SELECT MIN(nb_cost) mincost
  FROM cost_q
),
prob_q AS (
SELECT min(probability) prob
  FROM cost_q, min_cost
 WHERE nb_cost = mincost
)
SELECT 1 actual_target_value,
       0 predicted_target_value, 
       (1.0 - prob)/prob cost
  FROM prob_q
UNION ALL 
SELECT 0 actual_target_value,
       1 predicted_target_value,
       1 cost
  FROM dual
UNION ALL 
SELECT 0 actual_target_value,
       0 predicted_target_value,
       0 cost
  FROM dual
UNION ALL
SELECT 1 actual_target_value,
       1 predicted_target_value,
       0 cost
  FROM dual;

SELECT *
  FROM nb_alter_cost;

-- Now, use this new cost matrix to compute the confusion matrix
--
DECLARE
  v_accuracy         NUMBER;
  v_area_under_curve NUMBER;
BEGIN
   DBMS_DATA_MINING.COMPUTE_CONFUSION_MATRIX (
     accuracy                    => v_accuracy,
     apply_result_table_name     => 'nb_sh_sample_test_apply',
     target_table_name           => 'nb_sh_sample_test_targets',
     case_id_column_name         => 'cust_id',
     target_column_name          => 'affinity_card',
     confusion_matrix_table_name => 'nb_sh_alter_confusion_matrix',
     score_column_name           => 'PREDICTION',   -- default
     score_criterion_column_name => 'PROBABILITY',  -- default
     cost_matrix_table_name      => 'nb_alter_cost');
   DBMS_OUTPUT.PUT_LINE('**** MODEL ACCURACY ****: ' || ROUND(v_accuracy,4)); 
END;
/

SELECT actual_target_value as actual, 
       predicted_target_value as predicted, 
       value as count
  FROM nb_sh_alter_confusion_matrix
  ORDER BY actual_target_value, predicted_target_value;

-- DISPLAY LIFT RESULTS
--
-- See dmabdemo.sql for example on generating a .CSV file
-- for charting using Excel
--
SELECT quantile_number               qtl,
       lift_cumulative               lcume,
       percentage_records_cumulative prcume,
       targets_cumulative            tcume,
       target_density_cumulative     tdcume
-- Other info in Lift results
-- quantile_total_count,
-- non_targets_cumulative,
-- lift_quantile,
-- target_density
  FROM nb_sh_sample_lift
ORDER BY quantile_number;

-----------------------------------------------------------------------
--                               APPLY THE MODEL
-----------------------------------------------------------------------

-- Cleanup old scoring data preparation objects for repeat runs
BEGIN EXECUTE IMMEDIATE 'DROP VIEW nb_sh_sample_apply_prepared';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP VIEW nb_sh_sample_apply_cat';
EXCEPTION WHEN OTHERS THEN NULL; END;
/

-----------------------
-- PREPARE SCORING DATA
--
-- If the data for model creation has been prepared, then the data
-- to be scored using the model must be prepared to the same scale
-- in order to obtain meaningful results.
--
-- 1. Missing value treatment - see dmsvcdem.sql for example of what
--    needs to be done with Test and Apply data input.
-- 2. Outlier treatment and
-- 3. Binning
-- Quantile binning handles both the outlier treatment and binning in
-- this case. Transform the test data mining_test_v, using the transformation
-- tables generated during the Build data preparation, to generate a view
-- representing prepared test data.
--
-- If this model is applied in a different schema or instance, the 
-- data preparation objects generated in the CREATE step must also
-- be made available in the target schema/instance. So you must export
-- those objects (i.e. the num and cat bin tables and views) along with 
-- the model to the target user schema.
--
BEGIN
  -- Execute the first transform effected on training data
  DBMS_DATA_MINING_TRANSFORM.XFORM_BIN_CAT (
    bin_table_name  => 'nb_sh_sample_cat',
    data_table_name => 'mining_data_apply_v',
    xform_view_name => 'nb_sh_sample_apply_cat');   

  -- Provide the result to the next transform effected on training data
  DBMS_DATA_MINING_TRANSFORM.XFORM_BIN_NUM (
    bin_table_name  => 'nb_sh_sample_num',
    data_table_name => 'nb_sh_sample_apply_cat',
    xform_view_name => 'nb_sh_sample_apply_prepared');
END;
/

-- Cleanup old scoring result objects for repeat runs
BEGIN EXECUTE IMMEDIATE 'DROP TABLE nb_sh_sample_apply_result';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP TABLE nb_sh_sample_apply_ranked';
EXCEPTION WHEN OTHERS THEN NULL; END;
/

------------------
-- APPLY THE MODEL
--
BEGIN
  DBMS_DATA_MINING.APPLY(
    model_name          => 'NB_SH_Clas_sample',
    data_table_name     => 'nb_sh_sample_apply_prepared',
    case_id_column_name => 'cust_id',
    result_table_name   => 'nb_sh_sample_apply_result');
END;
/

-- APPLY RESULT OBJECTS: nb_sh_sample_apply_result

------------------------
-- DISPLAY APPLY RESULTS
--
-- 1. The results table contains a prediction set - i.e. ALL the predictions
--    for a given case id, with their corresponding probability values.
-- 2. In this example, note that APPLY results do not need to be reverse
--    transformed, as done in the case of model details. This is because
--    class values of a classification target were not (required to be)
--    binned or normalized.
-- 3. Only the first 10 rows of the table are displayed here.
--
column probability format 9.99999
column prediction format 9
SELECT cust_id, prediction, probability
  FROM (SELECT cust_id, prediction, ROUND(probability,4) probability
          FROM nb_sh_sample_apply_result
        ORDER BY cust_id, prediction, probability)
 WHERE ROWNUM < 11
ORDER BY cust_id;
   
-----------------------------------------------------------
-- GENERATE RANKED APPLY RESULTS (OPTIONALLY BASED ON COST)
--
-- ALTER APPLY RESULTS TABLE (just for demo purposes)
--
-- The RANK_APPLY and COMPUTE() procedures do not necessarily have
-- to work on the result table generated from DBMS_DATA_MINING.APPLY
-- alone. They can work on any table with similar schema and content
-- that matches the APPLY result table. An example will be a table
-- generated from some other tool, scoring engine or a generated result.
--
-- To demonstrate this, we will make a simply change the column names in
-- the APPLY results schema table, and supply the new table as input to
-- RANK_APPLY. The only requirement is that the new column names have to be
-- reflected in the RANK_APPLY procedure. The table containing the ranked
-- results will reflect these new column names.
-- 
ALTER TABLE nb_sh_sample_apply_result RENAME COLUMN cust_id TO customer_id;
ALTER TABLE nb_sh_sample_apply_result RENAME COLUMN prediction TO score;
ALTER TABLE nb_sh_sample_apply_result RENAME COLUMN probability TO chance;

-- RANK APPLY RESULTS (WITH COST MATRIX INPUT)
--
BEGIN
  DBMS_DATA_MINING.RANK_APPLY (
    apply_result_table_name     => 'nb_sh_sample_apply_result',
    case_id_column_name         => 'customer_id',
    score_column_name           => 'score',
    score_criterion_column_name => 'chance',
    ranked_apply_table_name     => 'nb_sh_sample_apply_ranked',
    top_n                       => 2,
    cost_matrix_table_name      => 'nb_alter_cost');
END;
/

-- RANK_APPLY RESULT OBJECTS: smvc_sh_sample_apply_ranked

-------------------------------
-- DISPLAY RANKED APPLY RESULTS
-- using altered cost matrix
column chance format 9.99
column cost format 9.99 
SELECT customer_id, score, chance, cost, rank
  FROM (SELECT customer_id, score, ROUND(chance,4) chance,
               ROUND(cost,4) cost, rank
          FROM nb_sh_sample_apply_ranked
        ORDER BY customer_id, rank)
 WHERE ROWNUM < 11
ORDER BY customer_id;
