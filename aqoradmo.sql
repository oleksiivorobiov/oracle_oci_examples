Rem
Rem $Header: aqoradmo.sql 16-nov-2004.15:56:51 rbhyrava Exp $
Rem
Rem aqoradmo.sql
Rem
Rem Copyright (c) 2000, 2004, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      aqoradmo.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem      <short description of component this file declares/defines>
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    rbhyrava    11/16/04 - user creation 
Rem    rbhyrava    07/10/00 - AQ API demo - setup
Rem    rbhyrava    07/10/00 - Created
Rem

REM ===========================================================
REM SETUP for AQ Java API demos:create user and payload types 
REM ===========================================================
SET echo on;
CONNECT system/manager;
DROP USER aqjava CASCADE;
CREATE USER aqjava IDENTIFIED BY aqjava;
GRANT CONNECT, RESOURCE, AQ_ADMINISTRATOR_ROLE TO aqjava;
GRANT EXECUTE ON DBMS_AQADM TO aqjava;
GRANT EXECUTE ON DBMS_AQ TO aqjava;
CONNECT aqjava/aqjava;

CREATE TYPE address AS OBJECT (
    street VARCHAR (30), 
    city VARCHAR(30)
);
/
CREATE TYPE person AS OBJECT (
    name VARCHAR (30), 
    home ADDRESS
);
/
EXIT;
REM ==============================================
REM SETUP complete
REM ==============================================
