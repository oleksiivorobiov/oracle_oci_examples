rem
rem $Header: cdemorid.sql 14-jul-99.13:52:59 mjaeger Exp $
rem
rem cdemorid.sql
rem
rem Copyright (c) 1998, 1999, Oracle Corporation.  All rights reserved.
rem
rem    NAME
rem      cdemorid.sql - SQL to set up cdemorid demo
rem
rem    DESCRIPTION
rem      set up for testing ROWID with UPDATE
rem
rem    NOTES
rem
rem
rem    MODIFIED   (MM/DD/YY)
rem    mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem    dchatter    10/16/98 - SQL for cdemorid
rem    dchatter    10/16/98 - Created
rem

connect system/manager
drop user cdemorid cascade;
grant connect, resource to cdemorid identified by cdemorid;


CONNECT cdemorid/cdemorid

CREATE TABLE FOO (C1 INTEGER, C2 VARCHAR2(20));

INSERT INTO FOO VALUES (1, 'Row 1');
INSERT INTO FOO VALUES (2, 'Row 2');
INSERT INTO FOO VALUES (3, 'Row 3');
INSERT INTO FOO VALUES (4, 'Row 4');
INSERT INTO FOO VALUES (5, 'Row 5');
INSERT INTO FOO VALUES (6, 'Row 6');
INSERT INTO FOO VALUES (7, 'Row 7');
INSERT INTO FOO VALUES (8, 'Row 8');
INSERT INTO FOO VALUES (9, 'Row 9');
INSERT INTO FOO VALUES (10, 'Row 10');
INSERT INTO FOO VALUES (11, 'Row 11');
INSERT INTO FOO VALUES (12, 'Row 12');
INSERT INTO FOO VALUES (13, 'Row 13');
INSERT INTO FOO VALUES (14, 'Row 14');
INSERT INTO FOO VALUES (15, 'Row 15');
INSERT INTO FOO VALUES (17, 'Row 16');
INSERT INTO FOO VALUES (18, 'Row 17');
INSERT INTO FOO VALUES (19, 'Row 18');
INSERT INTO FOO VALUES (20, 'Row 19');
INSERT INTO FOO VALUES (21, 'Row 20');

COMMIT


EXIT

