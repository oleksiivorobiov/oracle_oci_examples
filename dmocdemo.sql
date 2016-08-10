Rem $Header: dmocdemo.sql 25-oct-2007.11:34:44 ramkrish Exp $
Rem
Rem dmocdemo.sql
Rem
Rem Copyright (c) 2004, 2007, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      dmocdemo.sql - Sample program for the DBMS_DATA_MINING package
Rem
Rem    DESCRIPTION
Rem      This script creates a clustering model
Rem      using the O-Cluster algorithm
Rem      and data in the SH (Sales History) schema in the RDBMS.
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
Rem    mmcampos    11/10/04 - edit comments
Rem    ramkrish    10/26/04 - add data analysis and comments/cleanup 
Rem    gtang       08/25/04 - Fix bug #3808314 
Rem    gtang       08/18/04 - gtang_bug-3785950
Rem    gtang       08/09/04 - gtang_bug-3785785
Rem    gtang       07/23/04 - gtang_bug-3753633
Rem    gtang       07/15/04 - Renamed and moved to rdbms/demo
Rem    gtang       06/15/04 - add apply
Rem    gtang       05/20/04 - add get_model_details_oc
Rem    gtang       05/06/04 - gtang_txn110497
Rem    gtang       04/27/04 - Creation
Rem

SET serveroutput ON
SET trimspool ON  
SET pages 10000
SET linesize 120
SET echo ON

-----------------------------------------------------------------------
--                            SAMPLE PROBLEM
-----------------------------------------------------------------------
-- Segment the demographic data into 10 clusters and study the individual
-- clusters. Rank the clusters on probability.

-----------------------------------------------------------------------
--                            SET UP AND ANALYZE THE DATA
-----------------------------------------------------------------------

-- The data for this sample is composed from base tables in the SH Schema
-- (See Sample Schema Documentation) and presented through these views:
-- mining_data_build_str_v (build data)
-- (See dmsh.sql for view definitions).
--

-----------
-- ANALYSIS
-----------
-- For clustering using Orthogonal Clustering, perform the following on
-- mining data.
--
-- 1. Outlier Treatment for Predictors for Build data
--
--    Skipped in this example. See dmsvcdem.sql for discussion on outlier
--    treatment.
--
--    For O-cluster, the recommended outlier treatement is to trim the tails -
--    i.e. eliminate the outliers at the edges - rather than winsorize.
--
-- 2. Binning high cardinality data. O-Cluster is not a distance based
--    algorithm like K-Means, and instead works on histograms. So binning
--    is the preferred transformation for high cardinality data. O-Cluster
--    uses a special binning procedure that automatically determines the
--    number of bins based on data statistics. See notes in the source file
--    for DBMS_DATA_MINING_TRANSFORM (ORACLE_HOME/rdbms/admin/dbmsdmxf.sql)
--    for details on the binning procedure.
--

-----------------------------------------------------------------------
--                            BUILD THE MODEL
-----------------------------------------------------------------------

-- Cleanup build data preparation objects for repeat runs
BEGIN EXECUTE IMMEDIATE 'DROP TABLE oc_sh_sample_num';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP VIEW oc_sh_sample_build_prepared';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
-- Cleanup old model with the same name for repeat runs
BEGIN DBMS_DATA_MINING.DROP_MODEL('OC_SH_Clus_sample');
EXCEPTION WHEN OTHERS THEN NULL; END;
/

--------------------------------
-- PREPARE BUILD (TRAINING) DATA
--
-- 1. Outlier Treatment
--    Skipped - see dmsvcdem.sql for example
--
-- 2. Binning
--    Bin numerical data only; Convert binary data (0 and 1 only) to
--    char or varchar2 type.
--
BEGIN
  -- Bin numerical attributes:
  DBMS_DATA_MINING_TRANSFORM.CREATE_BIN_NUM(
    bin_table_name => 'oc_sh_sample_num');

  DBMS_DATA_MINING_TRANSFORM.INSERT_AUTOBIN_NUM_EQWIDTH(
    bin_table_name  => 'oc_sh_sample_num',
    data_table_name => 'mining_data_build_str_v',
    -- categoricals are automatically excluded by this procedure
    exclude_list    => DBMS_DATA_MINING_TRANSFORM.COLUMN_LIST (
                       'cust_id'),
    sample_size     => 30000 -- same as OCLT_MAX_BUFFER setting below
  );

  -- Create the transformed view
  DBMS_DATA_MINING_TRANSFORM.XFORM_BIN_NUM(
    bin_table_name  => 'oc_sh_sample_num',
    data_table_name => 'mining_data_build_str_v',
    xform_view_name => 'oc_sh_sample_build_prepared',
    -- see dbmsdmxf.sql for explanation of literal flag
    literal_flag    => TRUE);
END;
/

-- DATA PREPARATION OBJECTS:
-- ------------------------
-- 1. Bin boundary Table:           oc_sh_sample_num
-- 2. Input (view) to CREATE_MODEL: oc_sh_sample_build_prepared

-------------------
-- SPECIFY SETTINGS
--
-- Cleanup old settings table for repeat runs
--
BEGIN EXECUTE IMMEDIATE 'DROP TABLE oc_sh_sample_settings';
EXCEPTION WHEN OTHERS THEN NULL; END;
/

-- K-Means is the default clustering algorithm. Override the
-- default to set the algorithm to O-Cluster using a settings table.
-- 
-- CREATE AND POPULATE A SETTINGS TABLE
--
set echo off
CREATE TABLE oc_sh_sample_settings (
  setting_name  VARCHAR2(30), 
  setting_value VARCHAR2(30));
set echo on
BEGIN
  INSERT INTO oc_sh_sample_settings VALUES
    (dbms_data_mining.algo_name, dbms_data_mining.algo_ocluster);
  INSERT INTO oc_sh_sample_settings VALUES
    (dbms_data_mining.clus_num_clusters, 10);
  INSERT INTO oc_sh_sample_settings VALUES
    (dbms_data_mining.oclt_max_buffer, 30000);

  -- Other possible settings are:
  -- (dbms_data_mining.oclt_sensitivity, 0.5);
END;
/

---------------------
-- CREATE A NEW MODEL
--
-- Build a new OC model
BEGIN
  DBMS_DATA_MINING.CREATE_MODEL(
    model_name          => 'OC_SH_Clus_sample',
    mining_function     => dbms_data_mining.clustering,
    data_table_name     => 'oc_sh_sample_build_prepared',
    case_id_column_name => 'cust_id',
    settings_table_name => 'oc_sh_sample_settings');
END;
/

-------------------------
-- DISPLAY MODEL SETTINGS
--
column setting_name format a30
column setting_value format a30
SELECT setting_name, setting_value
  FROM user_mining_model_settings
 WHERE model_name = 'OC_SH_CLUS_SAMPLE'
ORDER BY setting_name;

--------------------------
-- DISPLAY MODEL SIGNATURE
--
column attribute_name format a40
column attribute_type format a20
SELECT attribute_name, attribute_type
  FROM user_mining_model_attributes
 WHERE model_name = 'OC_SH_CLUS_SAMPLE'
ORDER BY attribute_name;

------------------------
-- DISPLAY MODEL DETAILS
--
-- Cluster details are best seen in pieces - based on the kind of
-- associations and groupings that are needed to be observed.
--

-- CLUSTERS
-- For each cluster_id, provides the number of records in the cluster,
-- the parent cluster id, and the level in the hierarchy.
-- NOTE: Unlike K-means, O-Cluster does not return a value for the
--       dispersion associated with a cluster.
--
SELECT T.id           clu_id,
       T.record_count rec_cnt,
       T.parent       parent,
       T.tree_level   tree_level
  FROM (SELECT *
          FROM TABLE(DBMS_DATA_MINING.GET_MODEL_DETAILS_OC(
                 'OC_SH_Clus_sample'))
        ORDER BY id) T
 WHERE ROWNUM < 11;

-- TAXONOMY
-- 
SELECT clu_id, child_id
  FROM (SELECT T.id clu_id, C.id child_id
          FROM TABLE(DBMS_DATA_MINING.GET_MODEL_DETAILS_OC(
                 'OC_SH_Clus_sample')) T,
               TABLE(T.child) C
        ORDER BY T.id,C.id)
 WHERE ROWNUM < 11;

-- SPLIT PREDICATES
-- For each cluster, the split predicate indicates the attribute
-- and the condition used to assign records to the cluster's children
-- during model build. It provides an important piece of information
-- on how the population within a cluster can be divided up into
-- two smaller clusters.
--
column attribute_name format a20
column op format a2
column s_value format a50
SELECT clu_id, attribute_name, op, s_value
  FROM (SELECT a.id clu_id, sp.attribute_name, sp.conditional_operator op,
               sp.attribute_str_value s_value
          FROM TABLE(DBMS_DATA_MINING.GET_MODEL_DETAILS_OC(
                 'OC_SH_Clus_sample')) a,
               TABLE(a.split_predicate) sp
        ORDER BY a.id, op, s_value)
  WHERE ROWNUM < 11;

-- CENTROIDS FOR LEAF CLUSTERS
-- For each cluster_id, this output lists all the attributes that
-- constitute the centroid, with the mean (for numericals) or
-- mode (for categoricals). Unlike K-Means, O-Cluster does not return 
-- the variance for numeric attributes.
--
column mode_val format a60
SELECT clu_id, attribute_name, mean, mode_val
  FROM (SELECT a.id clu_id, c.attribute_name, c.mean, c.mode_value mode_val
          FROM TABLE(DBMS_DATA_MINING.GET_MODEL_DETAILS_OC(
                 'OC_SH_Clus_sample')) a,
               TABLE(a.centroid) c
        ORDER BY a.id, c.attribute_name)
  WHERE ROWNUM < 11;

-- HISTOGRAM FOR LEAF CLUSTERS
-- Histogram count is represented in frequency, rather than actual count.
column count format 9999.99
column bin_id format 9999999
column clu_id format 99999999
column label format a15;
SELECT clu_id, bin_id, attribute_name, label, cnt
  FROM (SELECT a.id clu_id, h.bin_id, h.attribute_name, h.label, h.count cnt
          FROM TABLE(DBMS_DATA_MINING.GET_MODEL_DETAILS_OC(
                 'OC_SH_Clus_sample')) a,
               TABLE(a.histogram) h
        ORDER BY a.id, h.attribute_name, h.bin_id)
 WHERE ROWNUM < 11;

-- RULES FOR LEAF CLUSTERS
-- See dmkmdemo.sql for explanation on output columns.
column confidence format 999999.99
SELECT *
  FROM (SELECT T.id rule_id,
               T.rule.rule_support support,
               T.rule.rule_confidence confidence
          FROM TABLE(DBMS_DATA_MINING.GET_MODEL_DETAILS_OC(
                      'OC_SH_Clus_sample')) T
        ORDER BY confidence DESC, support DESC)
 WHERE confidence > 0.5;

-- RULE DETAILS FOR LEAF CLUSTERS
--
-- The build data is prepared in this example, so reverse transform
-- the model contents to corresponding bin boundary labels. Translation
-- back to the exact value is not possible.
--
-- See dmkmdemo.sql for explanation on output columns.
column val format a60; 
WITH
mod_dtls AS (
SELECT *
  FROM TABLE(DBMS_DATA_MINING.GET_MODEL_DETAILS_OC('OC_SH_Clus_sample'))
),
bin_labels AS (
SELECT col, bin, (DECODE(bin,'1','[','(') || lv || ',' || val || ']') label
  FROM (SELECT col,
               bin,
               LAST_VALUE(val) OVER (
                 PARTITION BY col ORDER BY val
                 ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) lv,
               val
          FROM oc_sh_sample_num)
)
SELECT R.id rule_id,
       A.attribute_name attribute_name,
       A.conditional_operator op,
       NVL(L.label, NVL(A.attribute_str_value,A.attribute_num_value)) val,
       A.attribute_support support,
       A.attribute_confidence confidence
  FROM mod_dtls R,
       TABLE(R.rule.antecedent) A,
       bin_labels L
 WHERE A.attribute_name = L.col (+) AND
       (NVL(A.attribute_str_value,A.attribute_num_value) = L.bin(+)) AND
       A.attribute_confidence > 0.5
ORDER BY confidence DESC, support DESC, rule_id, attribute_name, val;

-- 0.5 is used as the threshold just to show some numerical attributes
-- that have been decoded back to their original ranges.
-- The ORDER BYs are there just to enable repeatable output; they are
-- not a mandatory part of the query.

-----------------------------------------------------------------------
--                               TEST THE MODEL
-----------------------------------------------------------------------

-- There is no specific set of testing parameters for Clustering.
-- Examination and analysis of clusters is the main method to prove
-- the efficacy of a clustering model.
--

-----------------------------------------------------------------------
--                               APPLY THE MODEL
-----------------------------------------------------------------------
-- For a descriptive mining function like Clustering, "Scoring" involves
-- assigning the probability with which a given case belongs to a given
-- cluster.

-- Cleanup scoring data preparation objects for repeat runs
BEGIN EXECUTE IMMEDIATE 'DROP VIEW oc_sh_sample_apply_prepared';
EXCEPTION WHEN OTHERS THEN NULL; END;
/

-----------------------
-- PREPARE SCORING DATA
--
-- 1. Outlier Treatment
--    Skipped. See dmsvcdem.sql for an example.
--
-- 2. Binning
--    Use the Bin Boundary Table obtained earlier to bin the apply data.
--
BEGIN
  DBMS_DATA_MINING_TRANSFORM.XFORM_BIN_NUM(
    bin_table_name  => 'oc_sh_sample_num',
    data_table_name => 'mining_data_apply_str_v',
    xform_view_name => 'oc_sh_sample_apply_prepared',
    literal_flag    => TRUE);
END;
/

-- Cleanup scoring result objects for repeat runs
BEGIN EXECUTE IMMEDIATE 'DROP TABLE oc_sh_sample_apply_result';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP TABLE oc_sh_sample_apply_ranked';
EXCEPTION WHEN OTHERS THEN NULL; END;
/

-------------------------------------------------
-- SCORE NEW DATA USING SQL DATA MINING FUNCTIONS
--
------------------
-- BUSINESS CASE 1
-- List the clusters into which the customers in this
-- given dataset have been grouped.
--
SELECT CLUSTER_ID(oc_sh_clus_sample USING *) AS clus, COUNT(*) AS cnt 
  FROM oc_sh_sample_apply_prepared
GROUP BY CLUSTER_ID(oc_sh_clus_sample USING *)
ORDER BY cnt DESC;

-- See dmkmdemo.sql for more examples
