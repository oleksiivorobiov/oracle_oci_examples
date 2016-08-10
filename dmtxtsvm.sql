Rem
Rem $Header: dmtxtsvm.sql 25-oct-2007.11:34:46 ramkrish Exp $
Rem
Rem
Rem Copyright (c) 2003, 2007, Oracle. All rights reserved.  
Rem
Rem      dmtxtsvm.sql - Sample program for the DBMS_DATA_MINING package.
Rem
Rem    DESCRIPTION
Rem      This script creates a text mining model using
Rem      SVM classification.
Rem
Rem    NOTES
Rem     
Rem
Rem    MODIFIED   (MM/DD/YY) 
Rem    ramkrish    10/25/07 - replace deprecated get_model calls with catalog
Rem                           queries
Rem    ktaylor     07/12/05 - minor edits to comments
Rem    ramkrish    01/28/05 - 4148186 - provide correct model names
Rem    ramkrish    10/28/04 - cleanup/comments
Rem    xbarr       06/25/04 - xbarr_dm_rdbms_migration
Rem    bmilenov    05/18/04 - Change zscore to minmax normalization
Rem    cbhagwat    02/25/04 - Format changes
Rem    cbhagwat    10/13/03 - cbhagwat_txn109175
Rem    cbhagwat    10/10/03 - fix
Rem    cbhagwat    10/08/03 - Creation
  
SET serveroutput ON
SET trimspool ON  
SET pages 10000
SET echo ON

-----------------------------------------------------------------------
--                            SAMPLE PROBLEM
-----------------------------------------------------------------------
-- Mine the text features extracted using dmtxtfe.sql using SVM
-- algorithm. 

-----------------------------------------------------------------------
--                            SET UP AND ANALYZE THE DATA
-----------------------------------------------------------------------
-- See dmtxtfe.sql. Note that the text features are input here
-- through a nested table column called 'COMMENTS'.

-----------------------------------------------------------------------
--                            BUILD THE MODEL
-----------------------------------------------------------------------

-- Cleanup old model and objects for repeat runs
BEGIN DBMS_DATA_MINING.DROP_MODEL('T_SVM_Clas_sample');
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP TABLE t_svmc_sample_settings';
EXCEPTION WHEN OTHERS THEN NULL; END;
/

-- Create settings table to choose linear kernel
CREATE TABLE t_svmc_sample_settings (
  setting_name  VARCHAR2(30),
  setting_value VARCHAR2 (30));
 
BEGIN
  -- Populate settings table
  INSERT INTO t_svmc_sample_settings VALUES
    (dbms_data_mining.algo_name,
     dbms_data_mining.algo_support_vector_machines);
  --(dbms_data_mining.svms_conv_tolerance,0.01);
  --(dbms_data_mining.svms_kernel_cache_size,50000000);
  INSERT INTO t_svmc_sample_settings VALUES
    (dbms_data_mining.svms_kernel_function,dbms_data_mining.svms_linear);
  COMMIT;
END;
/

---------------
-- CREATE MODEL

-- Cleanup old objects for repeat runs
BEGIN EXECUTE IMMEDIATE 'DROP TABLE t_svmc_sample_norm';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP VIEW t_svmc_sample_build_prepared';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
-- Prepare mining_data_build data as appropriate  
BEGIN
  -- Make a numerical bin boundary table   
  DBMS_DATA_MINING_TRANSFORM.CREATE_NORM_LIN (
    norm_table_name => 't_svmc_sample_norm');
                         
  -- Normalize data   
  DBMS_DATA_MINING_TRANSFORM.INSERT_NORM_LIN_MINMAX (
    norm_table_name => 't_svmc_sample_norm',
    data_table_name => 'mining_build_nested_text',
    exclude_list    => DBMS_DATA_MINING_TRANSFORM.COLUMN_LIST (
                       'CUST_ID',
                       'AFFINITY_CARD',
                       'BULK_PACK_DISKETTES',
                       'FLAT_PANEL_MONITOR',
                       'HOME_THEATER_PACKAGE',
                       'BOOKKEEPING_APPLICATION',
                       'PRINTER_SUPPLIES',
                       'Y_BOX_GAMES',
                       'OS_DOC_SET_KANJI',
                       'COMMENTS'),
    round_num       => 0
  );        

  -- Create the transformed view
  DBMS_DATA_MINING_TRANSFORM.XFORM_NORM_LIN (
    norm_table_name => 't_svmc_sample_norm',
    data_table_name => 'mining_build_nested_text',       
    xform_view_name => 't_svmc_sample_build_prepared');    
END;
/

-- Create SVM model
BEGIN
  DBMS_DATA_MINING.CREATE_MODEL(
    model_name => 'T_SVM_Clas_sample',
    mining_function => dbms_data_mining.classification,
    data_table_name => 't_svmc_sample_build_prepared',
    case_id_column_name => 'cust_id',
    target_column_name => 'affinity_card',
    settings_table_name => 't_svmc_sample_settings');
END;
/ 
 
-- Display the model settings
column setting_name format a30;
column setting_value format a30;
SELECT setting_name, setting_value
  FROM user_mining_model_settings
 WHERE model_name = 'T_SVM_CLAS_SAMPLE'
ORDER BY setting_name;

-- Display the model signature
column attribute_name format a40
column attribute_type format a20
SELECT attribute_name, attribute_type
  FROM user_mining_model_attributes
 WHERE model_name = 'T_SVM_CLAS_SAMPLE'
ORDER BY attribute_name;

-- Display model details
-- Note how several text terms extracted from the COMMENTs documents
-- show up as predictors.
--
SET line 120
column class format a10
column attribute_name format a25
column attribute_value format a25
column coefficient format 9.99
WITH
mod_dtls AS (
SELECT *
  FROM TABLE(DBMS_DATA_MINING.GET_MODEL_DETAILS_SVM('T_SVM_Clas_sample'))
),
model_details AS (
SELECT d.class, a.attribute_name, a.attribute_value, a.coefficient
  FROM mod_dtls d,
       TABLE(d.attribute_set) a
ORDER BY class, ABS(coefficient) DESC
)
--
SELECT TO_CHAR(class) class,
       attribute_name,
       attribute_value,
       coefficient
  FROM model_details
 WHERE ROWNUM < 6;	   

-----------------------------------------------------------------------
--                               TEST THE MODEL
-----------------------------------------------------------------------
-- See dmsvcdem.sql for examples. The key difference here
-- is the nested table input, and you can adapt that sample
-- code to accept the table with a nested table column as
-- input.

-----------------------------------------------------------------------
--                SCORE NEW DATA USING SQL DATA MINING FUNCTIONS
-----------------------------------------------------------------------

BEGIN EXECUTE IMMEDIATE 'DROP VIEW t_svmc_sample_apply_prepared';
EXCEPTION WHEN OTHERS THEN NULL; END;
/

-- Prepare scoring data
BEGIN
  -- Create the transformed view
  DBMS_DATA_MINING_TRANSFORM.XFORM_NORM_LIN (
    norm_table_name => 't_svmc_sample_norm',
    data_table_name => 'mining_apply_nested_text',
    xform_view_name => 't_svmc_sample_apply_prepared');
END;
/

-- The key here is to demonstrate the use of these functions on
-- a scoring table with nested table input for attributes.

------------------
-- BUSINESS CASE 1
--
-- Find the 10 customers that are most likely to use an affinity card.
-- Note that the SQL data mining functions seamless work against
-- tables that contain nested table columns of type DM_Nested_Numerical
-- or DM_Nested_Categorical. The nested column COMMENT is also part
-- of this input.
--
SELECT cust_id
  FROM (SELECT cust_id
          FROM t_svmc_sample_apply_prepared
        ORDER BY PREDICTION_PROBABILITY(T_SVM_Clas_sample, 1 USING *) DESC, 1)
WHERE ROWNUM < 11;

------------------
-- BUSINESS CASE 2
-- Find the average age of customers who are likely to use an
-- affinity card. Break out the results by gender.
--
SELECT cust_gender,
       COUNT(*) AS cnt,
       ROUND(AVG(age)) AS avg_age
  FROM t_svmc_sample_apply_prepared
 WHERE PREDICTION(T_SVM_Clas_sample USING *) = 1
GROUP BY cust_gender
ORDER BY cust_gender;
