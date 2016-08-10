Rem
Rem $Header: aqjmsdmo.sql 05-jun-2007.15:02:55 aatam Exp $
Rem
Rem aqjmsdmo.sql
Rem
Rem Copyright (c) 2000, 2007, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      aqjmsdmo.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem      <short description of component this file declares/defines>
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    aatam       06/05/07 - password need to be consistent
Rem    rbhyrava    11/16/04 - user creation 
Rem    rbhyrava    07/10/00 - AQ JMS demo -setup
Rem    rbhyrava    07/10/00 - Created
Rem
REM =====================================================
REM SETUP for AQ JMS Demos:create user and payload types 
REM =====================================================
SET echo on;
CONNECT system/manager;
DROP USER jmsuser CASCADE ;
CREATE USER jmsuser IDENTIFIED BY JMSUSER;
GRANT CONNECT, RESOURCE, AQ_ADMINISTRATOR_ROLE TO jmsuser;

CONNECT jmsuser/JMSUSER;

CREATE TYPE message AS OBJECT (
    id NUMBER,
    city VARCHAR2(30),
    priority NUMBER
);
/

CREATE OR REPLACE TYPE cars AS OBJECT (
    carno NUMBER,
    make  VARCHAR2(20) ,
    year  NUMBER,
    price NUMBER(10,2) ,
    color VARCHAR2(10)
);
/
CREATE OR REPLACE TYPE emp AS OBJECT (
    id    NUMBER ,
    name  VARCHAR2(20),
    carown  cars,
    rank  NUMBER,
    zip   VARCHAR2(5)
);
/
EXIT;

REM ==============================================
REM SETUP complete 
REM ==============================================
