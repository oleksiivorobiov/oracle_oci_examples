Rem
Rem $Header: rdbms/demo/dattime1.sql /main/3 2009/07/04 16:43:57 wezhu Exp $
Rem
Rem dattime1.sql
Rem
Rem Copyright (c) 2007, 2009, Oracle and/or its affiliates. 
Rem All rights reserved. 
Rem
Rem    NAME
Rem      dattime1.sql - create table with 5 different data type
Rem                     TIMESTAMP
Rem                     TIMESTAMP WITH TIME ZONE
Rem                     TIMESTAMP WITH LOCAL TIME ZONE
Rem                     INTERVAL YEAR TO MONTH
Rem                     INTERVAL DAY TO SECOND
Rem
Rem    DESCRIPTION
Rem      <short description of component this file declares/defines>
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    wezhu       07/01/09 - add default session time zone
Rem    chli        04/09/07 - fix password issue
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

COL c1 FORMAT A30
COL c2 FORMAT A48
COL c3 FORMAT A30
COL c4 FORMAT A48
COL c5 FORMAT A30

COL c1 HEADING TIMESTAMP
COL c2 HEADING "TIMESTAMP WITH TIME ZONE"
COL c3 HEADING "TIMESTAMP WITH LOCAL TIME ZONE"
COL c4 HEADING "INTERVAL YEAR TO MONTH"
COL c5 HEADING "INTERVAL DAY TO SECOND"

CONNECT oe/oe

ALTER SESSION SET time_zone = 'US/Pacific';

CREATE TABLE dattime (
 c1 TIMESTAMP,
 c2 TIMESTAMP WITH TIME ZONE,
 c3 TIMESTAMP WITH LOCAL TIME ZONE,
 c4 INTERVAL YEAR TO MONTH,
 c5 INTERVAL DAY TO SECOND);

INSERT INTO dattime
VALUES ( TIMESTAMP'1980-1-12 15:13:23.33', 
         TIMESTAMP'2000-10-28 11:26:38 AMERICA/LOS_ANGELES',
         TIMESTAMP'1985-3-1 1:11:11.11',
	 INTERVAl '1-2' YEAR TO MONTH,
	 INTERVAL '90 00:00:00' DAY TO SECOND);

INSERT INTO dattime
VALUES (TO_TIMESTAMP('08-11-83 3:43:55.49','DD-MM-RR HH24:MI:SSXFF'),
        TO_TIMESTAMP_TZ('2-4-58 14:2:56.18+8:53','DD-MM-RR HH24:MI:SSXFFTZH:TZM'),
        TO_TIMESTAMP('12-8-38 1:2:56.1','DD-MM-RR HH24:MI:SSXFF'),
	TO_YMINTERVAL('02-03'),
	TO_DSINTERVAL('65 10:00:00'));


SELECT * FROM dattime;

DROP TABLE dattime;

