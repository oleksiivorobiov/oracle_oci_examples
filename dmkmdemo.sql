Rem
Rem $Header: dmkmdemo.sql 25-oct-2007.11:35:30 ramkrish Exp $
Rem
Rem dmkmdemo.sql
Rem
Rem Copyright (c) 2003, 2007, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      dmkmdemo.sql - Sample program for the DBMS_DATA_MINING package.
Rem
Rem    DESCRIPTION
Rem      This script creates a clustering model
Rem      using the K-Means algorithm
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
Rem    ktaylor     07/11/05 - Minor edits to comments
Rem    jcjeon      01/18/05 - add column format 
Rem    mmcampos    11/09/04 - edit comments
Rem    ramkrish    10/04/04 - add data analysis and comments/cleanup
Rem    jiawang     07/22/04 - Add order by to fix sorting dif 
Rem    pstengar    10/17/03 - added denormalization of model details
Rem    ramkrish    10/02/03 - Creation
  
SET serveroutput ON
SET trimspool ON  
SET pages 10000
SET linesize 140
SET echo ON

-----------------------------------------------------------------------
--                            SAMPLE PROBLEM
-----------------------------------------------------------------------
-- Segment the demographic data into 10 clusters and study the individual
-- clusters. Rank the clusters on probability.

-----------------------------------------------------------------------
--                            SET UP AND ANALYZE THE DATA
-----------------------------------------------------------------------

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
-- For clustering using KM, perform the following on mining data.
--
-- 1. Use Data Auto Preparation
--

-----------------------------------------------------------------------
--                            BUILD THE MODEL
-----------------------------------------------------------------------

-- Cleanup old model with same name for repeat runs
BEGIN DBMS_DATA_MINING.DROP_MODEL('KM_SH_Clus_sample');
EXCEPTION WHEN OTHERS THEN NULL; END;
/

-------------------
-- SPECIFY SETTINGS
--
-- Cleanup old settings table for repeat runs
BEGIN EXECUTE IMMEDIATE 'DROP TABLE km_sh_sample_settings';
EXCEPTION WHEN OTHERS THEN NULL; END;
/

-- K-Means is the default Clustering algorithm. For this sample,
-- we skip specification of any overrides to defaults
--
-- Uncomment the appropriate sections of the code below for
-- changing settings values.
-- 
set echo on
CREATE TABLE km_sh_sample_settings (
   setting_name  VARCHAR2(30),
   setting_value VARCHAR2(30));
 
BEGIN       
   INSERT INTO km_sh_sample_settings (setting_name, setting_value) VALUES
   (dbms_data_mining.kmns_distance,dbms_data_mining.kmns_euclidean);

   INSERT INTO km_sh_sample_settings (setting_name, setting_value) VALUES 
   (dbms_data_mining.prep_auto,dbms_data_mining.prep_auto_on);
   -- Other examples of overrides are:
   -- (dbms_data_mining.kmns_iterations,3);
   -- (dbms_data_mining.kmns_block_growth,2);
   -- (dbms_data_mining.kmns_conv_tolerance,0.01);
   -- (dbms_data_mining.kmns_split_criterion,dbms_data_mining.kmns_variance);
   -- (dbms_data_mining.kmns_min_pct_attr_support,0.1);
   -- (dbms_data_mining.kmns_num_bins,10);
END;
/

---------------------
-- CREATE A NEW MODEL
--
BEGIN
  DBMS_DATA_MINING.CREATE_MODEL(
    model_name          => 'KM_SH_Clus_sample',
    mining_function     => dbms_data_mining.clustering,
    data_table_name     => 'mining_data_build_v',
    case_id_column_name => 'cust_id',
    settings_table_name => 'km_sh_sample_settings');
END;
/

-------------------------
-- DISPLAY MODEL SETTINGS
--
column setting_name format a30
column setting_value format a30
SELECT setting_name, setting_value
  FROM user_mining_model_settings
 WHERE model_name = 'KM_SH_CLUS_SAMPLE'
ORDER BY setting_name;

--------------------------
-- DISPLAY MODEL SIGNATURE
--
column attribute_name format a40
column attribute_type format a20
SELECT attribute_name, attribute_type
  FROM user_mining_model_attributes
 WHERE model_name = 'KM_SH_CLUS_SAMPLE'
ORDER BY attribute_name;

-------------------------
-- DISPLAY MODEL METADATA
--
column mining_function format a20
column algorithm format a20
SELECT mining_function, algorithm, model_size
  FROM user_mining_models
 WHERE model_name = 'KM_SH_CLUS_SAMPLE';

------------------------
-- DISPLAY MODEL DETAILS
--
-- Cluster details are best seen in pieces - based on the kind of
-- associations and groupings that are needed to be observed.
--
-- CLUSTERS
-- For each cluster_id, provides the number of records in the cluster,
-- the parent cluster id, the level in the hierarchy, and dispersion -
-- which is a measure of the quality of the cluster, and computationally,
-- the sum of square errors.
--
SELECT T.id           clu_id,
       T.record_count rec_cnt,
       T.parent       parent,
       T.tree_level   tree_level,
       T.dispersion   dispersion
  FROM (SELECT *
          FROM TABLE(DBMS_DATA_MINING.GET_MODEL_DETAILS_KM(
                     'KM_SH_Clus_sample'))
        ORDER BY id) T
 WHERE ROWNUM < 11;

-- TAXONOMY
--
SELECT clu_id, child_id
  FROM (SELECT T.id clu_id, C.id child_id
          FROM TABLE(DBMS_DATA_MINING.GET_MODEL_DETAILS_KM(
                     'KM_SH_Clus_sample')) T,
               TABLE(T.child) C
        ORDER BY T.id,C.id)
 WHERE ROWNUM < 11;

-- CENTROIDS FOR LEAF CLUSTERS
-- For each cluster_id, this output lists all the attributes that
-- constitute the centroid, with the mean (for numericals) or
-- mode (for categoricals), along with the variance from mean
--
column aname format a30
column mode_val format a60
WITH
centroid_tab AS (
SELECT T.id clu_id,
       C.attribute_name aname,
       C.mean mean_val,
       C.mode_value mode_val,
       C.variance variance
  FROM TABLE(DBMS_DATA_MINING.GET_MODEL_DETAILS_KM('KM_SH_Clus_sample')) T,
       TABLE(T.centroid) C
ORDER BY T.id, C.attribute_name
)
SELECT clu_id, aname, mean_val, mode_val, variance
  FROM centroid_tab
 WHERE clu_id > 17 AND ROWNUM < 11;

-- HISTOGRAM FOR TWO LEAF CLUSTERS
--
WITH
hist_tab AS (
SELECT T.id clu_id,
       H.attribute_name aname,
       H.lower_bound lower_b,
       H.upper_bound upper_b,
       H.count rec_cnt
  FROM TABLE(DBMS_DATA_MINING.GET_MODEL_DETAILS_KM('KM_SH_Clus_sample')) T,
       TABLE(T.histogram) H
ORDER BY T.id, H.attribute_name, H.label
)
SELECT clu_id, aname, lower_b, upper_b, rec_cnt
  FROM hist_tab
 WHERE clu_id > 17 AND ROWNUM < 11;

-- RULES FOR LEAF CLUSTERS
-- A rule_id corresponds to the associated cluster_id. The support
-- indicates the number of records (say M) that satisfies this rule.
-- This is an upper bound on the  number of records that fall within
-- the bounding box defined by the rule. Each predicate in the rule
-- antecedent defines a range for an attribute, and it can be
-- interpreted as the side of a bounding box which envelops most of
-- the data in the cluster.
-- Confidence = M/N, where N is the number of records in the cluster
-- and M is the rule support defined as above.
--
SELECT T.id                   rule_id,
       T.rule.rule_support    support,
       T.rule.rule_confidence confidence
  FROM TABLE(DBMS_DATA_MINING.GET_MODEL_DETAILS_KM('KM_SH_Clus_sample')) T
ORDER BY T.id;

-- RULE DETAILS FOR LEAF CLUSTERS
-- Attribute level details of each rule/cluster id.
-- For an attribute, support (say M) indicates the number of records that 
-- fall in the attribute range specified in the rule antecedent where the
-- given attribute is not null. Confidence is a number between 0 and 1
-- that indicates how relevant this attribute is in distinguishing the 
-- the records in the cluster from all the records in the whole data. The
-- larger the number, more relevant the attribute.
-- 
-- The query shown below reverse-transforms the data to its original
-- values, since build data was normalized.
--
column aname format a25
column op format a3
column val format a60
column support format 9999
column confidence format 9.9999
WITH
rule_tab AS (
SELECT T.id rule_id,
       A.attribute_name aname,
       A.conditional_operator op,
       NVL(A.attribute_str_value,
         ROUND(A.attribute_num_value,4)) val,
       A.attribute_support support,
       A.attribute_confidence confidence
  FROM TABLE(DBMS_DATA_MINING.GET_MODEL_DETAILS_KM('KM_SH_Clus_sample')) T,
       TABLE(T.rule.antecedent) A
ORDER BY T.id, A.attribute_name, support, confidence desc, val
)
SELECT rule_id, aname, op, val, support, confidence
  FROM rule_tab
 WHERE rule_id < 3;

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
-- providing the probability values for each cluster.

-- Cleanup scoring data preparation objects for repeat runs

BEGIN EXECUTE IMMEDIATE 'DROP TYPE Cattrs';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP TYPE Cattr';
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
SELECT CLUSTER_ID(km_sh_clus_sample USING *) AS clus, COUNT(*) AS cnt 
  FROM mining_data_apply_v
GROUP BY CLUSTER_ID(km_sh_clus_sample USING *)
ORDER BY cnt DESC;

------------------
-- BUSINESS CASE 2
-- List ten most representative (based on likelihood) customers of cluster 2
--
SELECT *
  FROM (SELECT cust_id, CLUSTER_PROBABILITY(km_sh_clus_sample, 2 USING *) prob
          FROM mining_data_apply_v
        ORDER BY prob DESC)
 WHERE ROWNUM < 11;

column prob format 9.9999
column attr format a15
column val  format a15
column conf format 9.9999
------------------
-- BUSINESS CASE 3
-- List the most relevant attributes (with confidence > 55%)
-- of each cluster in which a given customer (id 101362) belongs
-- to with > 20% likelihood.
--
set echo off
CREATE TYPE Cattr AS OBJECT (
  attr VARCHAR2(30),
  op   VARCHAR2(3),
  val  VARCHAR2(30),
  supp NUMBER,
  conf NUMBER)
/
set echo on
CREATE TYPE Cattrs AS TABLE OF Cattr
/

column attr format a30
column op format a2
WITH
clus_tab AS (
SELECT id,
       A.attribute_name aname,
       A.conditional_operator op,
       NVL(A.attribute_str_value,
         ROUND(A.attribute_num_value,4)) val,
       A.attribute_support support,
       A.attribute_confidence confidence
  FROM TABLE(DBMS_DATA_MINING.GET_MODEL_DETAILS_KM('km_sh_clus_sample')) T,
       TABLE(T.rule.antecedent) A
 WHERE A.attribute_confidence > 0.55
),
clust AS (
SELECT id,
       CAST(COLLECT(Cattr(aname, op, TO_CHAR(val), support, confidence))
         AS Cattrs) cl_attrs
  FROM clus_tab
GROUP BY id
),
custclus AS (
SELECT T.cust_id, S.cluster_id, S.probability
  FROM (SELECT cust_id, CLUSTER_SET(km_sh_clus_sample, NULL, 0.2 USING *) pset
          FROM mining_data_apply_v
         WHERE cust_id = 101362) T,
       TABLE(T.pset) S
)
SELECT A.probability prob, A.cluster_id cl_id,
       B.attr, B.op, B.val, B.supp, B.conf
  FROM custclus A,
       (SELECT T.id, C.*
          FROM clust T,
               TABLE(T.cl_attrs) C) B
 WHERE A.cluster_id = B.id
ORDER BY prob DESC, cl_id ASC, conf DESC, attr ASC, val ASC;
