Rem
Rem $Header: dmtxtfe.sql 12-jul-2005.07:43:16 ktaylor Exp $
Rem
Rem textfe.sql
Rem
Rem Copyright (c) 2003, 2005, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      textfe.sql - Oracle TEXT Term (or Feature) Extractor
Rem
Rem    DESCRIPTION
Rem      This script demonstrates a means to extract terms for
Rem      text mining from Oracle Text documents (i.e. CLOB or
Rem      VARCHAR2 columns containing text documents) using a Text
Rem      Index built on those documents. 
Rem
Rem      For more information on term extraction for text mining,
Rem      see Oracle Data Mining Application Developer's Guide.
Rem
Rem      This program uses the following schema object:
Rem      
Rem      . BUILD_TEXT_IDX - the Oracle Text Index built 
Rem        on MINING_BUILD_TEXT. Please see rdbms/demo/dmsh.sql
Rem        to see how MINING_BUILD_TEXT is constructed from
Rem        data from the Sample Schema.
Rem
Rem      Given this Text Index, the program demonstrates how to
Rem      extract terms from the documents using the Text Index.
Rem      The terms are generated in an output table in the multi-record
Rem      case format - i.e. in the form
Rem      (sequence_id, attribute_name, attribute_value)
Rem
Rem      This output table can then be used to generate a nested table
Rem      column (DM_Nested_Numerical) in the final table that represents
Rem      the training data that is to be used for model creation, or the
Rem      scoring table that is to be used for model scoring.
Rem
Rem      Simply put, terms extracted from text documents will become
Rem      generic attributes in training or scoring data. This data can
Rem      then be classified, clustered or feature-extracted using the
Rem      DBMS_DATA_MINING package.
Rem
Rem      Since the number of attributes (i.e. the terms) is typically
Rem      greater than 1000, these attributes are composed into a nested
Rem      column in the input table.
Rem
Rem      The goal of this program is to enable users to build their
Rem      own term extractors, given a document table with a text index
Rem      built on the documents.
Rem
Rem    NOTES
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    ktaylor     07/12/05 - minor edits to comments
Rem    ramkrish    10/28/04 - cleanup/comments
Rem    ramkrish    09/16/04 - update for 10gR2 
Rem    xbarr       06/25/04 - xbarr_dm_rdbms_migration
Rem    ramkrish    02/06/04 - fix nested table
Rem    ramkrish    12/09/03 - explain the steps with comments
Rem    cbhagwat    11/11/03 - NMF_CLUSTERING to SVM_CLASSIFICATION
Rem    cbhagwat    10/06/03 - Creation
  
SET serveroutput ON
SET trimspool ON
SET pages 10000
SET echo ON

-- Cleanup for repeat runs
BEGIN EXECUTE IMMEDIATE 'DROP TABLE txt_sample_pref_terms';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP TABLE txt_sample_tf_out';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP TABLE txt_sample_term_out';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP TABLE txt_sample_terms';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP TABLE txt_sample_text_cat';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP TABLE txt_sample_data_input';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN EXECUTE IMMEDIATE 'DROP TABLE nontxt_attrib';
EXCEPTION WHEN OTHERS THEN NULL; END;
/
BEGIN CTX_DDL.DROP_PREFERENCE(preference_name => 'txt_sample_pref');
EXCEPTION WHEN OTHERS THEN NULL; END;
/

-- Step 1. The sample Text document table is MINING_BUILD_TEXT
--         This table is constructed when you run rdbms/demo/dmsh.sql.

-- Step 2. The sample Text Index is BUILD_TEXT_INDEX
--         This index is also constructed when you run rdbms/demo/dmsh.sql

-- Given the presence of these two schema objects, you can now
-- proceed to extract terms from text documents in MINING_BUILD_TEXT
-- using the following steps:

-- Step 3. Create preferences for Text term extraction
--         (This routine must be used exclusively with ODM;
--          it is a public, but undocumented, routine in Oracle10g)
--
BEGIN CTX_DDL.CREATE_PREFERENCE('txt_sample_pref', 'SVM_CLASSIFIER');
EXCEPTION WHEN OTHERS THEN NULL; END;
/
-- Step 4. Define an ancillary table for categories
--         (required to execute preference choices)
--
CREATE TABLE txt_sample_text_cat (id NUMBER, cat NUMBER)
/

-- Step 5. Extract terms set up by preference into intermediate terms tables
--         1. txt_sample_term_out   - terms set by preference
--         2. txt_sample_pref_terms - term definitions
--
--         The parameters are:
--         'BUILD_TEXT_IDX',        -- index_name - Text index name
--         'CUST_ID',               -- doc_id     - the case-id
--         'txt_sample_text_cat',   -- cattab     - category table name
--         'id',                    -- catdocid   - docid for category tab
--         'cat',                   -- catid      - category id
--         'txt_sample_pref_terms', -- restab     - term definitions
--         'txt_sample_pref'        -- preference - Text preference name
--
CREATE TABLE txt_sample_term_out AS
SELECT *
  FROM TABLE(ctxsys.drvodm.feature_prep (
               'BUILD_TEXT_IDX',
               'CUST_ID',
               'txt_sample_text_cat',
               'id',
               'cat',
               'txt_sample_pref_terms',
               'txt_sample_pref'));

-- At the end of this step:
-- . txt_sample_term_out has the schema
--   (sequence_id, value)
--   sequence_id contains the values of CUST_ID, the case identifier
--   column in MINING_BUILD_TEXT.
--

-- Step 6. Explain the terms in the intermediate tables to generate
--         the final terms table. This table will contain the text
--         elements that can be used as attributes for mining.
--         (This drvodm routine must be used exclusively with ODM;
--          it is a public, but undocumented, routine in Oracle10g)
--
CREATE TABLE txt_sample_terms AS
SELECT A.sequence_id, B.text, A.value
  FROM txt_sample_term_out A,
       TABLE(ctxsys.drvodm.feature_explain(
               'txt_sample_pref_terms')) B
 WHERE A.attribute_id = B.id;

-- Step 7. (Optional) Display the extracted terms
--
column text format a45
SELECT *
  FROM (SELECT sequence_id,text,value
          FROM txt_sample_terms
        ORDER BY sequence_id, text)
 WHERE ROWNUM < 10;

-- Step 8. Now, assume that you have a table containing training data
--         with non-text attributes, with matching sequence ids
--         (i.e. matching values of CUST_ID).
--
--         See MINING_BUILD_TEXT in rdbms/demo/dmsh.sql as an example
--          
--         Add the text mining attributes to this table as a nested table,
--         as shown below.
--

-- Just for the sake of this demo, create a dummy table with
-- NULL non-text attributes, using txt_sample_terms itself as
-- the source (to prepare a table with matching case-id's)
-- In an actual scenario, the non-text columns may be
-- projected from a different table.
--
CREATE TABLE nontxt_attrib AS
SELECT A.sequence_id case_id, 1 age, 1 salary, 1 class
  FROM txt_sample_terms A;

-- You can now create the mining data input table (or a view)
-- with text and non-text attributes as follows:
--
CREATE TABLE txt_sample_data_input
  NESTED TABLE txt_terms STORE AS txt_terms AS
SELECT N.*,
       CAST(MULTISET(
       SELECT DM_Nested_Numerical (T.text, T.value)
         FROM txt_sample_terms T
        WHERE N.case_id = T.sequence_id)
       AS DM_Nested_Numericals) txt_terms
  FROM nontxt_attrib N;

-- Step 9. Read rdbms/demo/dmsh.sql to revise the above steps
--         in the context of an end-to-end demo of Text Mining.
--         dmsh.sql performs all of the steps discussed above,
--         against tables in the Sample Schema. The text data
--         prepared in this manner can be classified using
--         dmtxtsvm.sql, or feature extracted using dmtxtnmf.sql.
