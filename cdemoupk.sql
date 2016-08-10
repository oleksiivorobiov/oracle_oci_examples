Rem
Rem $Header: cdemoupk.sql 10-apr-2007.18:07:13 azhao Exp $
Rem
Rem cdemoupk.sql
Rem
Rem Copyright (c) 2004, 2007, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      cdemoupk.sql - create user for cdemooupk 
Rem
Rem    DESCRIPTION
Rem      <short description of component this file declares/defines>
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    azhao       04/10/07 - case sensitive password
Rem    jchai       03/23/04 - jchai_add_cdemoucb_cdempupk_shiptest 
Rem    jchai       03/18/04 - Created
Rem

SET ECHO ON
SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100
connect system/manager;
drop user cdemoupk cascade;
grant connect, resource to cdemoupk identified by CDEMOUPK;


