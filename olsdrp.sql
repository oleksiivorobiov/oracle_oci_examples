Rem
Rem $Header: olsdrp.sql 29-may-2007.01:56:44 srramara Exp $
Rem
Rem olsdrp.sql
Rem
Rem Copyright (c) 2007, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      olsdrp.sql - Drop Oracle Label Security demo
Rem
Rem    DESCRIPTION
Rem      Drops Oracle Label Security demonstration policies, tables and users
Rem
Rem    NOTES
Rem      Run this script before olsdemo to clean up previous demo installs
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    srramara    05/29/07 - passwd case sensitivity changes
Rem    gmurphy     02/02/01 - Merged gmurphy_ols_2rdbms
Rem    vpesati     01/18/01 - oracle label security demo drop script
Rem    vpesati     01/18/01 - Created
Rem

rem ====================================================================
rem  Dropping HUMAN_RESOURCES and DEFENSE policies
rem ====================================================================

CONNECT SA_DEMO/SA_DEMO
EXECUTE SA_SYSDBA.DROP_POLICY('human_resources');
EXECUTE SA_SYSDBA.DROP_POLICY('defense');

rem ====================================================================
rem  Dropping SA_DEMO, HR_ADMIN, DEFENSE_ADMIN and other users
rem ====================================================================

CONNECT SYSTEM/manager as sysdba; 
DROP USER SA_DEMO    CASCADE;
DROP USER HR_ADMIN   CASCADE;
DROP USER DEFENSE_ADMIN CASCADE;

DROP USER PRES CASCADE;
DROP USER MD10 CASCADE;
DROP USER ED10 CASCADE;
DROP USER ED20 CASCADE;

DROP USER UN_SP CASCADE;
DROP USER SE_SP CASCADE;
DROP USER TS_SP CASCADE;
DROP USER SE_US CASCADE;
DROP USER SE_CA CASCADE;
DROP USER SE_UK CASCADE;
DROP USER SE_AU CASCADE;
DROP USER SE_CO CASCADE;
DROP USER TS_US CASCADE;
DROP USER SE_USO CASCADE;
DROP USER TS_USO CASCADE;


rem ====================================================================
rem  Drop Demo ..... Complete
rem ====================================================================
