Rem
Rem $Header: dmtxtnmf.sql 25-oct-2007.11:34:46 ramkrish Exp $
Rem
Rem nmfdemo.sql
Rem
Rem Copyright (c) 2003, 2007, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      dmtxtnmf.sql - Sample program for the DBMS_DATA_MINING package.
Rem
Rem    DESCRIPTION
Rem      This script creates a text mining model
Rem      using non-negative matrix factorization. 
Rem
Rem    NOTES
Rem     
Rem
Rem    MODIFIED   (MM/DD/YY) 
Rem    ramkrish    10/25/07 - replace deprecated get_model calls with catalog
Rem                           queries
Rem    ktaylor     07/12/05 - minor edits to comments
Rem    ramkrish    10/28/04 - cleanup/comments
Rem    amozes      07/30/04 - format coefficient 
Rem    xbarr       06/25/04 - xbarr_dm_rdbms_migration
Rem    cbhagwat    10/17/03 - feature_extraction
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
-- Mine the text features extracted using dmtxtfe.sql using NMF
-- algorithm. 

-----------------------------------------------------------------------
--                            SET UP AND ANALYZE THE DATA
-----------------------------------------------------------------------
-- See dmtxtfe.sql. Note that the text features are input here
-- through a nested table.

-----------------------------------------------------------------------
--                            BUILD THE MODEL
-----------------------------------------------------------------------

-- Cleanup old model and objects for repeat runs
BEGIN DBMS_DATA_MINING.DROP_MODEL('T_NMF_Sample');
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP TABLE t_nmf_sample_norm';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP VIEW t_nmf_sample_build_prepared';
EXCEPTION WHEN OTHERS THEN NULL; END;
/

--------------------------------
-- PREPARE BUILD (TRAINING) DATA
--
BEGIN
  -- Create a normalization table
  DBMS_DATA_MINING_TRANSFORM.CREATE_NORM_LIN (
    norm_table_name => 't_nmf_sample_norm');
                          
  -- Normalize appropriate columns   
  DBMS_DATA_MINING_TRANSFORM.INSERT_NORM_LIN_MINMAX (
    norm_table_name => 't_nmf_sample_norm',
    data_table_name => 'mining_build_nested_text',
    exclude_list    => dbms_data_mining_transform.column_list (
                       'AFFINITY_CARD',
                       'BULK_PACK_DISKETTES',
                       'FLAT_PANEL_MONITOR',
                       'HOME_THEATER_PACKAGE',
                       'BOOKKEEPING_APPLICATION',
                       'PRINTER_SUPPLIES',
                       'Y_BOX_GAMES',
                       'OS_DOC_SET_KANJI',
                       'cust_id',
                       'comments'),
    round_num       => 0
  );
  
  -- Create the transformed view
  DBMS_DATA_MINING_TRANSFORM.XFORM_NORM_LIN (
    norm_table_name => 't_nmf_sample_norm',
    data_table_name => 'mining_build_nested_text',
    xform_view_name => 't_nmf_sample_build_prepared');
END;
/       
-- NMFS settings (For info only)
-- insert into nmfs_settings values
--(dbms_data_mining.nmfs_conv_tolerance,0.05);
--(dbms_data_mining.nmfs_num_iterations,50);
--(dbms_data_mining.nmfs_random_seed,-1);
--(dbms_data_mining.nmfs_stop_criteria,dbms_data_mining.nmfs_sc_iter_or_conv);

---------------------
-- CREATE A NEW MODEL
--
BEGIN
  DBMS_DATA_MINING.CREATE_MODEL(
    model_name => 'T_NMF_Sample',
    mining_function => dbms_data_mining.feature_extraction,
    data_table_name => 't_nmf_sample_build_prepared',
    case_id_column_name => 'cust_id');
END;
/
    
-------------------------
-- DISPLAY MODEL SETTINGS
--
column setting_name format a30;
column setting_value format a30;
SELECT setting_name, setting_value
  FROM user_mining_model_settings
 WHERE model_name = 'T_NMF_SAMPLE'
ORDER BY setting_name;

--------------------------
-- DISPLAY MODEL SIGNATURE
--
column attribute_type format a20
SELECT attribute_name, attribute_type
  FROM user_mining_model_attributes
 WHERE model_name = 'T_NMF_SAMPLE'
ORDER BY attribute_name;

------------------------
-- DISPLAY MODEL DETAILS
--
column attribute_name format a30;
column attribute_value format a20;
column coefficient format 9.99999;
set pages 15;
SET line 120;
break ON feature_id;

SELECT t.feature_id,
       a.attribute_name,
       a.attribute_value,
       a.coefficient
  FROM TABLE(dbms_data_mining.get_model_details_nmf('T_NMF_Sample')) t,
       TABLE(t.attribute_set) a
WHERE feature_id < 3
ORDER BY 1,2,3,4;

-----------------------------------------------------------------------
--                               APPLY THE MODEL
-----------------------------------------------------------------------
-- See dmnmdemo.sql for examples. The key difference here
-- is the nested table input, and you can adapt that sample
-- code to accept the table with a nested table column as
-- input.
