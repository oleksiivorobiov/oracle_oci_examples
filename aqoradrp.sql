Rem
Rem $Header: aqoradrp.sql 15-nov-2000.10:01:53 ociqa Exp $
Rem
Rem aqoradrp.sql
Rem
Rem  Copyright (c) Oracle Corporation 2000. All Rights Reserved.
Rem
Rem    NAME
Rem      aqoradrp.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem      <short description of component this file declares/defines>
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    ociqa       11/15/00 - 
Rem    rbhyrava    07/10/00 - demodrop script
Rem    rbhyrava    07/10/00 - Created
Rem
REM --------------------
REM Drop the user
REM --------------------

CONNECT system/manager ;
DROP USER aqjava CASCADE ;
EXIT;
