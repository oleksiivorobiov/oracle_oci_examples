Rem
Rem $Header: rdbms/demo/cdemoqc.sql /main/3 2008/10/23 23:36:35 dgopal Exp $
Rem
Rem cdemoqc.sql
Rem
Rem Copyright (c) 2007, 2008, Oracle and/or its affiliates.
Rem All rights reserved. 
Rem
Rem    NAME
Rem      cdemoqc.sql
Rem
Rem    DESCRIPTION
Rem      Creates schema for query cache OCI demo cdemoqc.c
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    dgopal      10/10/08 - Added a new table from cdemoqc2.c
Rem    dgopal      12/19/07 - Modified schema
Rem    dgopal      05/10/07 - Created
Rem

SET ECHO ON
SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100

CONNECT system/manager
GRANT CONNECT, RESOURCE TO ocitest IDENTIFIED BY ocitest;

CONNECT ocitest/ocitest

DROP TABLE qctable;
DROP TABLE ssntable;

CREATE TABLE qctable (empno NUMBER, ename VARCHAR2(20), sal NUMBER);
CREATE TABLE ssntable (ssn NUMBER, empno NUMBER);

CREATE OR REPLACE PROCEDURE insert_val
AS BEGIN
for i in 1..1000 loop
  INSERT INTO qctable VALUES (i, 'EMP_'||i, i*100);
  INSERT INTO ssntable VALUES (56345000+i, i);
end loop;
end;
/
EXECUTE insert_val;

COMMIT;
