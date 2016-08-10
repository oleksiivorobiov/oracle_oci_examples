Rem
Rem $Header: dattime2.sql 10-apr-2001.15:02:35 jxfan Exp $
Rem
Rem dattime2.sql
Rem
Rem  Copyright (c) Oracle Corporation 2001. All Rights Reserved.
Rem
Rem    NAME
Rem      dattime2.sql - Daylight Saving Time (DST)
Rem
Rem    DESCRIPTION
Rem      <short description of component this file declares/defines>
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    jxfan       04/10/01 - Merged jxfan_demo
Rem    jxfan       04/10/01 - Created
Rem

SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100
SET ECHO OFF

CONNECT OE/OE

PROMPT Calculate Daylight Saving Time (DST) when the session TIME_ZONE is a region
PROMPT
SET ECHO ON
UPDATE orders
SET order_date = TIMESTAMP '2000-4-1 23:24:54 AMERICA/LOS_ANGELES' 
WHERE order_id = 2457;
ALTER SESSION SET TIME_ZONE='AMERICA/LOS_ANGELES';
SELECT order_date + INTERVAL '8' HOUR FROM orders
WHERE order_id = 2457;

SET ECHO OFF
PROMPT
PAUSE Press enter to continue ...
PROMPT
PROMPT The time period from 02:00:00 AM to 02:59:59 AM does not exist 
PROMPT during APRIL boundary
PROMPT

SET ECHO ON
UPDATE orders
SET order_date = TIMESTAMP'2000-04-02 02:30:30 AMERICA/LOS_ANGELES'
WHERE order_id = 2457;

SET ECHO OFF
PROMPT
PROMPT
PAUSE Press enter to continue ...
PROMPT
PROMPT The time period from 01:00:01 AM to 02:00:00 AM is repeated during
PROMPT OCTOBER boundary
PROMPT

SET ECHO ON
ALTER SESSION SET ERROR_ON_OVERLAP_TIME = TRUE;

UPDATE orders
SET order_date = TIMESTAMP '2000-10-29 01:00:01 AMERICA/LOS_ANGELES'
WHERE order_id = 2457;


