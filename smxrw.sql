Rem
Rem $Header: smxrw.sql 03-apr-2008.20:53:59 mmoy Exp $
Rem
Rem smxrw.sql
Rem
Rem Copyright (c) 2001, 2008, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      smxrw.sql 
Rem
Rem    DESCRIPTION
Rem      This is a demo script for DBMS_MVIEW.EXPLAIN_REWRITE().
Rem      We create a materialized view in SALES schema and
Rem      use EXPLAIN_REWRITE() to find out why query rewrite
Rem      failed for some queries. The output of EXPLAIN_REWRITE()
Rem      can go into a VARRAY or REWRITE_TABLE.
Rem
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    mmoy        04/03/08 - Fix sys password.
Rem    gvincent    11/02/06 - downcase connects for secure verifiers
Rem    mthiyaga    03/28/05 - Add examples using XRW utility 
Rem    mthiyaga    04/09/01 - Merged mthiyaga_xrw_demo
Rem    mthiyaga    04/06/01 - Created
Rem
Rem ===============================================================
Rem

connect sh/sh;

SET ECHO ON;

DROP MATERIALIZED VIEW month_sales_mv;

CREATE MATERIALIZED VIEW month_sales_mv
  ENABLE QUERY REWRITE
  AS
  SELECT t.calendar_month_number,
         SUM(s.amount_sold) AS sum_dollars
  FROM sales s, times t
  WHERE s.time_id = t.time_id
  GROUP BY t.calendar_month_number;
  
REM Create REWRITE_TABLE for the output
  
DROP TABLE REWRITE_TABLE;

REM Create REWRITE_TABLE. Note the script to create the REWRITE_TABLE, utlxrw.sql, can 
REM be found in the /rdbms/admin directory.
  
@utlxrw.sql;
  

REM Enable query rewrite
ALTER SESSION SET QUERY_REWRITE_ENABLED = TRUE;

REM Stale tolerated mode
ALTER SESSION SET QUERY_REWRITE_INTEGRITY = STALE_TOLERATED;

DELETE FROM REWRITE_TABLE;

REM -----------------------------------------------------------------------------
REM Example 1: Output goes into REWRITE_TABLE
REM -----------------------------------------------------------------------------
  
SET SERVEROUTPUT ON;
DECLARE
   querytxt VARCHAR2(1000) := 'SELECT t.calendar_month_number, SUM(s.amount_sold) AS sum_dollars
   FROM sales s, times t
   WHERE s.time_id = t.time_id AND
         t.calendar_month_number = 5
   GROUP BY t.calendar_month_number';
BEGIN
 dbms_mview.Explain_Rewrite(querytxt, 'MONTH_SALES_MV');
END;
/

REM  Check the results in REWRITE_TABLE

SELECT * FROM REWRITE_TABLE;

REM -----------------------------------------------------------------------------
REM Example 2: Query doesn't have the join in MONTH_SALES_MV. 
REM -----------------------------------------------------------------------------


DELETE FROM REWRITE_TABLE;

EXECUTE dbms_mview.Explain_Rewrite('SELECT t.calendar_month_number, SUM(s.amount_sold) AS sum_dollars FROM sales s, times t  GROUP BY t.calendar_month_number', 'MONTH_SALES_MV');

SELECT * FROM REWRITE_TABLE;


REM -----------------------------------------------------------------------------
REM Example 3: In this example, we run explain_rewrite on two different queries. 
REM            Their outputs are distinguished by using two different statement 
REM            ids (query_1 and query_2).
REM -----------------------------------------------------------------------------

DELETE FROM REWRITE_TABLE;

SET SERVEROUTPUT ON;
DECLARE
   querytxt1 VARCHAR2(1000) := 'SELECT t.calendar_month_number, SUM(s.amount_sold) AS sum_dollars
   FROM sales s, times t
   WHERE s.time_id = t.time_id AND
         t.calendar_month_number = 5
     GROUP BY t.calendar_month_number';
   querytxt2 VARCHAR2(1000) := 'SELECT t.calendar_month_number, SUM(s.amount_sold) AS sum_dollars
   FROM sales s, times t
   WHERE s.time_id = t.time_id AND
         t.calendar_month_number = 10
     GROUP BY t.calendar_month_number';
  
BEGIN
 dbms_mview.Explain_Rewrite(querytxt1, 'MONTH_SALES_MV', 'query_1');
 dbms_mview.Explain_Rewrite(querytxt2, 'MONTH_SALES_MV', 'query_2');
END;
/

SELECT statement_id, message FROM REWRITE_TABLE;


REM -----------------------------------------------------------------------------
REM Test 4:  Query txt is the only argument passed to EXPLAIN_REWRITE. Output
REM          goes to REWRITE_TABLE. Note that since there is no materialized view
REM          specified in the argument list, EXPLAIN_REWRITE will output
REM          messages related to all the MV's considered by query rewrite.
REM -----------------------------------------------------------------------------


DELETE FROM REWRITE_TABLE;

SET SERVEROUTPUT ON;
DECLARE
   querytxt VARCHAR2(1000) := 'SELECT t.calendar_month_number, SUM(s.amount_sold) AS sum_dollars
   FROM sales s, times t
   WHERE t.calendar_month_number = 5
   GROUP BY t.calendar_month_number';
BEGIN
 dbms_mview.Explain_Rewrite(querytxt);
END;
/

-- Check the results in REWRITE_TABLE

SELECT * FROM REWRITE_TABLE;



REM -----------------------------------------------------------------------------
REM Test 5: Output goes into a VARRAY. Note that we need to turn on SERVEROUTPUT
REM         in order to see the output below.
REM -----------------------------------------------------------------------------

SET SERVEROUTPUT ON;
DECLARE
   Rewrite_Array SYS.RewriteArrayType := SYS.RewriteArrayType();
   querytxt VARCHAR2(1000) := 'SELECT t.calendar_month_number, SUM(s.amount_sold) AS sum_dollars
   FROM sales s, times t
   WHERE s.time_id = t.time_id AND
         t.calendar_month_number = 5
   GROUP BY t.calendar_month_number';
   no_of_msgs NUMBER;
   i NUMBER;
BEGIN
 dbms_mview.Explain_Rewrite(querytxt, 'MONTH_SALES_MV',  Rewrite_Array);
   no_of_msgs := rewrite_array.count;
   FOR i IN 1..no_of_msgs
   LOOP
      DBMS_OUTPUT.PUT_LINE('>> MV_NAME  : ' || Rewrite_Array(i).mv_name);
      DBMS_OUTPUT.PUT_LINE('>> BEFORE VM: ' || Rewrite_Array(i).pass);
      DBMS_OUTPUT.PUT_LINE('>> QUERY    : ' || Rewrite_Array(i).query_text);
      DBMS_OUTPUT.PUT_LINE('>> MESSAGE  : ' || Rewrite_Array(i).message);
   END LOOP;
END;
/



REM -----------------------------------------------------------------------------
REM Test 6: Query rewrite is disabled
REM -----------------------------------------------------------------------------

-- Disable query rewrite and try the query
ALTER SESSION SET QUERY_REWRITE_ENABLED = FALSE;

SET SERVEROUTPUT ON;
DECLARE
   Rewrite_Array SYS.RewriteArrayType := SYS.RewriteArrayType();
   querytxt VARCHAR2(1000) := 'SELECT t.calendar_month_number, SUM(s.amount_sold) AS sum_dollars
   FROM sales s, times t
   WHERE s.time_id = t.time_id AND
         t.calendar_month_number = 5
   GROUP BY t.calendar_month_number';
   no_of_msgs NUMBER;
   i NUMBER;
BEGIN
 dbms_mview.Explain_Rewrite(querytxt, 'MONTH_SALES_MV',  Rewrite_Array);
   no_of_msgs := rewrite_array.count;
   FOR i IN 1..no_of_msgs
   LOOP
      DBMS_OUTPUT.PUT_LINE('>> MV_NAME  : ' || Rewrite_Array(i).mv_name);
      DBMS_OUTPUT.PUT_LINE('>> BEFORE VM: ' || Rewrite_Array(i).pass);
      DBMS_OUTPUT.PUT_LINE('>> QUERY    : ' || Rewrite_Array(i).query_text);
      DBMS_OUTPUT.PUT_LINE('>> MESSAGE  : ' || Rewrite_Array(i).message);
   END LOOP;
END;
/

REM -----------------------------------------------------------------------------
REM Test 7: Query rewrite is enabled
REM -----------------------------------------------------------------------------

REM  Enable query rewrite and try the query again
ALTER SESSION SET QUERY_REWRITE_ENABLED = TRUE;

SET SERVEROUTPUT ON;
DECLARE
   Rewrite_Array SYS.RewriteArrayType := SYS.RewriteArrayType();
   querytxt VARCHAR2(1000) := 'SELECT t.calendar_month_number, SUM(s.amount_sold) AS sum_dollars
   FROM sales s, times t
   WHERE s.time_id = t.time_id AND
         t.calendar_month_number = 5
   GROUP BY t.calendar_month_number';
   no_of_msgs NUMBER;
   i NUMBER;
BEGIN
 dbms_mview.Explain_Rewrite(querytxt, 'MONTH_SALES_MV',  Rewrite_Array);
   no_of_msgs := rewrite_array.count;
   FOR i IN 1..no_of_msgs
   LOOP
      DBMS_OUTPUT.PUT_LINE('>> MV_NAME  : ' || Rewrite_Array(i).mv_name);
      DBMS_OUTPUT.PUT_LINE('>> BEFORE VM: ' || Rewrite_Array(i).pass);
      DBMS_OUTPUT.PUT_LINE('>> QUERY    : ' || Rewrite_Array(i).query_text);
      DBMS_OUTPUT.PUT_LINE('>> MESSAGE  : ' || Rewrite_Array(i).message);
   END LOOP;
END;
/

REM -----------------------------------------------------------------------------
REM Clean up
REM -----------------------------------------------------------------------------
  
DROP MATERIALIZED VIEW MONTH_SALES_MV;

---------------------------------------------------------------------------------
--- Following EXPLAIN_REWRITE() examples use a utility called SYS.XRW. This
--- utility helps the user to easily select the desired output fields from
--- EXPLAIN_REWRITE(). Since the output of EXPLAIN_REWRITE() consists of
--- many fields, this utility can be very helpful to the user.
---------------------------------------------------------------------------------

CONNECT sys/knl_test7 as sysdba;

SET ECHO OFF;
--- Load the XRW utility
---
@xrwutl;
                                                                                
DROP user xrwdemo cascade;
CREATE user xrwdemo identified by xrwdemo;
GRANT connect, resource, dba to xrwdemo;
GRANT CREATE MATERIALIZED VIEW TO xrwdemo;
GRANT global query rewrite TO xrwdemo;

CONNECT xrwdemo/xrwdemo;
                                                                                
DROP TABLE FT;
CREATE TABLE FT (a1 INT, b1 INT, sales INT);

DROP MATERIALIZED VIEW MV1;

CREATE MATERIALIZED VIEW MV1
ENABLE QUERY REWRITE AS
SELECT a1, b1, sum(sales) sum_sales
FROM FT
GROUP BY a1, b1;


-- Following query rewrites. Note the rewritten query text and query block number
--
set serveroutput on  size 999999
DECLARE
 querytxt VARCHAR2(1500) := 'SELECT a1, b1, SUM(sales) FROM FT GROUP BY a1, b1';
BEGIN
  SYS.XRW('', 'COSTS, QUERY_TXT, PASS, REWRITTEN_TXT, QUERY_BLOCK_NO', querytxt);
END;
/

DROP MATERIALIZED VIEW MV1;

DROP TABLE FT;
CREATE TABLE FT (a1 INT, b1 INT, sales INT);


CREATE MATERIALIZED VIEW MV1 AS
SELECT a1, b1, sum(sales) sum_sales
FROM FT
GROUP BY a1, b1;


--- Rewrite is disabled for MV1. This query doesn't rewrite
set serveroutput on  size 999999
DECLARE
 querytxt VARCHAR2(1500) := 'SELECT a1, b1, SUM(sales) FROM FT GROUP BY a1, b1';
BEGIN
  SYS.XRW('', 'QUERY_TXT, REWRITTEN_TXT', querytxt);
END;
/

--- Enable rewrite for MV1.  It should rewrite now.

DROP MATERIALIZED VIEW MV1;

CREATE MATERIALIZED VIEW MV1 
ENABLE QUERY REWRITE AS
SELECT a1, b1, sum(sales) sum_sales
FROM FT
GROUP BY a1, b1;

DECLARE
 querytxt VARCHAR2(1500) := 'SELECT a1, b1, SUM(sales) FROM FT GROUP BY a1, b1';
BEGIN
  SYS.XRW('', 'QUERY_TXT, REWRITTEN_TXT', querytxt);
END;
/


DROP TABLE FT;
CREATE TABLE FT (a1 INT, b1 INT, sales INT);

DROP MATERIALIZED VIEW MV1;

CREATE MATERIALIZED VIEW MV1
ENABLE QUERY REWRITE AS
SELECT a1, b1, sum(sales) sum_sales
FROM FT
GROUP BY a1, b1;

INSERT INTO FT VALUES (5, 5, 500);


-- Following query doesn't rewrite in ENFORCED mode
--
set serveroutput on  size 999999
DECLARE
 querytxt VARCHAR2(1500) := 'SELECT a1, b1, SUM(sales) FROM FT GROUP BY a1, b1';
BEGIN
  SYS.XRW('', 'QUERY_TXT, REWRITTEN_TXT', querytxt);
END;
/

drop table a;
drop table b;
drop table c;
drop table d;

create table A(a1 int, a2 int, a3 int);
create table B(b1 int, b2 int, b3 int);
create table C(c1 int, c2 int, c3 int);
create table D(d1 int, d2 int, d3 int);

ALTER TABLE b ADD CONSTRAINT pk_b PRIMARY KEY (b1);
ALTER TABLE C ADD CONSTRAINT PK_C PRIMARY KEY (c1);

-- Make join between A and B lossless
ALTER TABLE A ADD CONSTRAINT fk_a FOREIGN KEY(a1) REFERENCES b(b1);
ALTER TABLE A MODIFY a1 NOT NULL;

-- Make join between B and C lossless
ALTER TABLE B ADD CONSTRAINT fk_b FOREIGN KEY(b2) REFERENCES c(c1);
ALTER TABLE B MODIFY b2 NOT NULL;

drop materialized view mv1;

-- MV1: A--->B--->C
--      |
--      D

create materialized view mv1
enable query rewrite as
select a.a1, b.b1, sum(a.a3) as sum_a
from a, b, c, d
where a.a1 = b.b1 
  and a.a2 = d.d2
  and b.b2 = c.c1
group by a.a1, b.b1;


-- Query doesn't rewrite due to anchor table in the MV
--
set serveroutput on;
DECLARE
        querytxt VARCHAR2(1500) := 'select a.a1, b.b1, sum(a.a3)
from a, b, c
where a.a1 = b.b1
  and b.b2 = c.c1
group by a.a1, b.b1';
BEGIN
  SYS.XRW('', '', querytxt);
END;
/

drop table a;
drop table b;
drop table c;

create table A(a1 int, a2 int, a3 int);
create table B(b1 int, b2 int, b3 int);
create table C(c1 int, c2 int, c3 int);

ALTER TABLE b ADD CONSTRAINT pk_b PRIMARY KEY (b1);

-- Make join between A and B lossless
ALTER TABLE A ADD CONSTRAINT fk_a FOREIGN KEY(a1) REFERENCES b(b1);
ALTER TABLE A MODIFY a1 NOT NULL;

drop materialized view mv1;

-- MV1: A--->B
--      |
--      C

create materialized view mv1
enable query rewrite as
select a.a1, b.b1, c.c1, sum(a.a3) as sum_a
from a, b, c
where a.a1 = c.c1 and a.a1 = b.b1
group by a1, b1, c1;

-- should NOT rewrite as the MV delta has a lossy join

set serveroutput on
DECLARE
        querytxt VARCHAR2(1500) := 'select a.a1, b.b1, sum(a.a3)
from a, b
where a.a1 = b.b1
group by a.a1, b.b1';
BEGIN
  SYS.XRW('', '', querytxt);
END;
/

drop table a;
drop table b;
drop table c;
drop table d;

create table A(a1 int, a2 int, a3 int);
create table B(b1 int, b2 int, b3 int);
create table C(c1 int, c2 int, c3 int);
create table D(d1 int, d2 int, d3 int);

ALTER TABLE b ADD CONSTRAINT pk_b PRIMARY KEY (b1);
ALTER TABLE C ADD CONSTRAINT PK_C PRIMARY KEY (c1);

-- Make join between A and B lossless
ALTER TABLE A ADD CONSTRAINT fk_a FOREIGN KEY(a1) REFERENCES b(b1);
ALTER TABLE A MODIFY a1 NOT NULL;

-- Make join between B and C lossless
ALTER TABLE B ADD CONSTRAINT fk_b FOREIGN KEY(b2) REFERENCES c(c1);
ALTER TABLE B MODIFY b2 NOT NULL;

drop materialized view mv1;

-- MV1: A--->B--->C
--      |
--      D

drop materialized view mv1;

create materialized view mv1
enable query rewrite as
select a.a1, b.b1, sum(a.a3) as sum_a
from a, b, c, d
where a.a1 = b.b1 
  and a.a2 = d.d2
  and b.b2 = c.c1
group by a.a1, b.b1;


-- Doesn't rewrite due to join back failures
--
set serveroutput on
DECLARE
        querytxt VARCHAR2(1500) := 'select a.a1, b.b1, sum(a.a3)
from a, b, c, d
where a.a1 = b.b1 
  and a.a2 = d.d2
  and b.b2 = c.c1
  and b.b3 = c.c2
group by a.a1, b.b1';
BEGIN
  SYS.XRW('', '', querytxt);
END;
/

drop table a;
drop table b;
drop table c;
drop table d;

create table A(a1 int, a2 int, a3 int);
create table B(b1 int, b2 int, b3 int);
create table C(c1 int, c2 int, c3 int);
create table D(d1 int, d2 int, d3 int);

ALTER TABLE b ADD CONSTRAINT pk_b PRIMARY KEY (b1);
ALTER TABLE C ADD CONSTRAINT PK_C PRIMARY KEY (c1);

-- Make join between A and B lossless
ALTER TABLE A ADD CONSTRAINT fk_a FOREIGN KEY(a1) REFERENCES b(b1);
ALTER TABLE A MODIFY a1 NOT NULL;

-- Make join between B and C lossless
ALTER TABLE B ADD CONSTRAINT fk_b FOREIGN KEY(b2) REFERENCES c(c1);
ALTER TABLE B MODIFY b2 NOT NULL;

drop materialized view mv1;

-- MV1: A--->B--->C
--      |
--      D

drop materialized view mv1;

create materialized view mv1
enable query rewrite as
select a.a1, b.b1, sum(a.a3) as sum_a
from a, b, c, d
where a.a1 = b.b1 
  and a.a2 = d.d2
  and b.b2 = c.c1
  and b.b2 = c.c2
group by a.a1, b.b1;


-- Does not rewrite due to a lossy join in the MV
--
set serveroutput on
DECLARE
        querytxt VARCHAR2(1500) := 'select a.a1, b.b1, sum(a.a3)
from a, b, c, d
where a.a1 = b.b1 
  and a.a2 = d.d2
  and b.b2 = c.c1
  and b.b3 = c.c2
group by a.a1, b.b1';
BEGIN
  SYS.XRW('', '', querytxt);
END;
/




DROP TABLE FT1;
DROP MATERIALIZED VIEW NESTED_MV1;
DROP MATERIALIZED VIEW NESTED_MV2;
DROP MATERIALIZED VIEW NESTED_MV3;
DROP MATERIALIZED VIEW NESTED_MV4;
DROP MATERIALIZED VIEW NESTED_MV5;

REM create a fact table, FT1, for nested MAV tests 

CREATE TABLE FT1 (akey INT, bkey INT, ckey INT, dkey INT, ekey INT, sales INT);

REM CREATE NESTED_MV1 on fact

CREATE MATERIALIZED VIEW NESTED_MV1
ENABLE QUERY REWRITE
AS
   SELECT akey, bkey, ckey, dkey, ekey, sum(sales) sum_sales
   FROM FT1
     GROUP BY akey, bkey, ckey, dkey, ekey;
   
REM create NESTED_MV2 on NESTED_MV1

CREATE MATERIALIZED VIEW NESTED_MV2
AS
   SELECT akey, bkey, ckey, dkey, sum(sum_sales) sum_sales2
   FROM NESTED_MV1
     GROUP BY akey, bkey, ckey, dkey;
   
REM create NESTED_MV3 on NESTED_MV2

CREATE MATERIALIZED VIEW NESTED_MV3
ENABLE QUERY REWRITE
AS
   SELECT akey, bkey, ckey, sum(sum_sales2) sum_sales3
   FROM NESTED_MV2
   GROUP BY akey, bkey, ckey;
   
REM create NESTED_MV4 on NESTED_MV3

CREATE MATERIALIZED VIEW NESTED_MV4
ENABLE QUERY REWRITE
AS
   SELECT akey, bkey, sum(sum_sales3) sum_sales4
   FROM NESTED_MV3
   GROUP BY akey, bkey;
   
REM create NESTED_MV5 on NESTED_MV4

CREATE MATERIALIZED VIEW NESTED_MV5
ENABLE QUERY REWRITE
AS
   SELECT akey, sum(sum_sales4) sum_sales5
   FROM NESTED_MV4
     GROUP BY akey;
  

-- Doesn't Rewrite with NESTED_MV5 as NESTED_MV2 is not enabled yet

set serveroutput on;

DECLARE
        querytxt VARCHAR2(100) := 'SELECT akey, sum(sales) FROM FT1 GROUP BY akey';
BEGIN
  SYS.XRW('', 'REWRITTEN_TXT', querytxt);
END;
/

alter materialized view nested_mv2 enable query rewrite;


--Rewrites with NESTED_MV5 now

set serveroutput on;

DECLARE
        querytxt VARCHAR2(100) := 'SELECT akey, sum(sales) FROM FT1 GROUP BY akey';
BEGIN
  SYS.XRW('', 'REWRITTEN_TXT', querytxt);
END;
/


DROP TABLE FT;
CREATE TABLE FT (f1 INT, f2 INT, s INT);

DROP TABLE A;
CREATE TABLE A(a1 int, a2 int);

DROP TABLE B;
CREATE TABLE B(b1 int);

ALTER TABLE A ADD CONSTRAINT pk_a PRIMARY KEY (a1);
ALTER TABLE B ADD CONSTRAINT pk_b PRIMARY KEY (b1);

ALTER TABLE FT ADD CONSTRAINT fk_ft_a FOREIGN KEY (f1) REFERENCES A(a1);
ALTER TABLE FT MODIFY (f1 NOT NULL) ;
ALTER TABLE FT ADD CONSTRAINT fk_ft_b FOREIGN KEY (f2) REFERENCES B(b1);
ALTER TABLE FT MODIFY (f2 NOT NULL); 

DROP MATERIALIZED VIEW MV1;

CREATE MATERIALIZED VIEW MV1
ENABLE QUERY REWRITE AS
SELECT f1, a1, sum(s) sum_s
FROM FT, A
WHERE FT.f1 = A.a1
GROUP BY f1, a1;

DROP MATERIALIZED VIEW MV2;

CREATE MATERIALIZED VIEW MV2 
ENABLE QUERY REWRITE AS
SELECT f1, a1, b1, sum(s) sum_s
FROM FT, A, B
WHERE FT.f1 = A.a1 AND FT.f2 = B.b1
GROUP BY f1, a1, b1;

set serveroutput on;
DECLARE
        querytxt VARCHAR2(100) := 'SELECT a1, SUM(s)
FROM FT, A
WHERE FT.f1 = A.a1
GROUP BY a1';
BEGIN
  SYS.XRW('', 'QUERY_TXT, REWRITTEN_TXT, COSTS', querytxt);
END;
/

                                                                              
drop table a;
create table a(a1 int, s int);
                                                                                
drop materialized view mv1;
create materialized view mv1
enable query rewrite as
select a1, sum(s) sum_s
from a
WHERE a1 < 50
group by a1;
                                                                                
drop materialized view mv2;
create materialized view mv2
enable query rewrite as
select a1, sum(s) sum_s
from a
WHERE a1 >= 50
group by a1;


-- query doesn't rewrite as cost is more expensive than the
-- unrewritten query.
--
set serveroutput on
DECLARE
        querytxt VARCHAR2(1500) := 'select a1, sum(s) sum_s from a group by a1';
BEGIN
  SYS.XRW('', 'QUERY_TXT, REWRITTEN_TXT, COSTS, QUERY_BLOCK_NO', querytxt);
END;
/


--- Create MV3 such that it doesn't have SUM(s) in it.
--
drop materialized view mv3;
create materialized view mv3
enable query rewrite as
select a1, count(s) count_s
from a
WHERE a1 >= 50
group by a1;

-- rewrites with mv1 and mv2.
--
set serveroutput on
DECLARE
        querytxt VARCHAR2(1500) := 'select a1, sum(s) sum_s from a group by a1';
BEGIN
  SYS.XRW('MV3', 'QUERY_TXT, REWRITTEN_TXT, COSTS, QUERY_BLOCK_NO', querytxt);
END;
/



DROP TABLE A cascade constraints;
create table a(a1 int, a2 int, a3 int, s int);

drop materialized view mv1;
create materialized view mv1
enable query rewrite as
select a1, sum(s) 
from a
group by a1;


-- query has an inline view which can be rewritten with MV1. Outer query
-- cannot be rewritten. Note the rewritten query and the query block numbers
-- 

set serveroutput on
DECLARE
    querytxt VARCHAR2(1500) := 'select a_1, sum_s from (select a1 a_1, sum(s) sum_s from a group by a1) iv1';
BEGIN
  SYS.XRW('', 'COSTS, QUERY_TXT, PASS, REWRITTEN_TXT, QUERY_BLOCK_NO',
            querytxt);
END;
/



DROP TABLE A cascade constraints;
create table a(a1 int, a2 int, a3 int, s int);
                                                                                
DROP TABLE B cascade constraints;
create table b(b1 int, b2 int, b3 int, t int);
                                                                                
drop materialized view mv1;
create materialized view mv1
enable query rewrite as
select a1, b1, sum(s)
from a, b
group by a1, b1;


-- Note the query block numbers for the rewritten inline views. Inline view
-- IV1 gets rewritten using exact text match
--

set serveroutput on
DECLARE
        querytxt VARCHAR2(1500) := 'select a_1, x_1, sum_s from (select a1 a_1, b1 b_1, sum(s) sum_s from a, b group by a1, b1) iv1, (select sum(s) sum_ss, a1 x_1 from a group by a1) iv2';
BEGIN
  SYS.XRW('', 'REWRITTEN_TXT, QUERY_BLOCK_NO',
            querytxt);
END;
/


-- Note the query block numbers for the rewritten inline views. Inline view
-- IV1 gets rewritten using exact text match and inline view IV3 gets 
-- rewritten using general rewrite
--

set serveroutput on
DECLARE
        querytxt VARCHAR2(1500) := 'select a_1, x_1, sum_s from (select a1 a_1, b1 b_1, sum(s) sum_s from a, b group by a1, b1) iv1, (select sum(s) sum_ss, a1 x_1 from a group by a1) iv2, (select a1 a_11, b1 b_11, sum(s) sum_sss from b, a group by b1, a1) iv3';
BEGIN
  SYS.XRW('', 'REWRITTEN_TXT, QUERY_BLOCK_NO',
            querytxt);
END;
/


DROP TABLE A CASCADE CONSTRAINTS;
DROP TABLE "A_small" CASCADE CONSTRAINTS;
DROP TABLE "A_SMALL" CASCADE CONSTRAINTS;
DROP TABLE B CASCADE CONSTRAINTS;
DROP TABLE C CASCADE CONSTRAINTS;
DROP TABLE D CASCADE CONSTRAINTS;

CREATE TABLE A (a1 INTEGER, a2 INTEGER constraint nn_a2 not null, 
                a3 INTEGER, a4 INTEGER, a5 INTEGER);
CREATE TABLE "A_small" (a1 INTEGER, a2 INTEGER constraint small1_a2 not null, 
                a3 INTEGER, a4 INTEGER, a5 INTEGER, a6 varchar(30));
CREATE TABLE "A_SMALL" (a1 INTEGER, a2 INTEGER constraint small2_a2 not null, 
                a3 INTEGER, a4 INTEGER, a5 INTEGER, a6 varchar(30));
CREATE TABLE B (b1 INTEGER, b2 INTEGER, y INTEGER constraint nn_y not null, 
                g INTEGER, 
                h INTEGER);
CREATE TABLE C (c1 INTEGER, c2 INTEGER, c3 INTEGER,
                c4 INTEGER);
CREATE TABLE D (d1 INTEGER, d2 INTEGER, d3 INTEGER,
                d4 INTEGER);

DROP MATERIALIZED VIEW inline_mv1;

CREATE MATERIALIZED VIEW inline_mv1
ENABLE QUERY REWRITE AS
SELECT A.a1, A.a2, SUM(V1.a3) as sum_s
FROM A, (SELECT * FROM A) V1
WHERE A.a1 = V1.a1
GROUP BY A.a1, A.a2;


DROP MATERIALIZED VIEW inline_mv3;

CREATE MATERIALIZED VIEW inline_mv3
ENABLE QUERY REWRITE AS
SELECT A.a1, A.a2, B.b1, SUM(V1.a3) as sum_s1, SUM(V2.b2) as sum_s2
FROM A, B, (SELECT * FROM A) V1, (SELECT * FROM B) V2
WHERE A.a1 = V1.a1 AND
      A.a2 = B.b2
GROUP BY A.a1, A.a2, B.b1;


DROP MATERIALIZED VIEW inline_mv4;

--- MV has multiple inline views
CREATE MATERIALIZED VIEW inline_mv4
ENABLE QUERY REWRITE AS
SELECT A.a1, A.a2, B.b1, SUM(V1.a3) as sum_s1, SUM(V2.b2) as sum_s2
FROM A, B, (SELECT * FROM A) V1, (SELECT * FROM B) V2
GROUP BY A.a1, A.a2, B.b1;


DROP MATERIALIZED VIEW inline_mv5;

--- MV has multiple inline views
CREATE MATERIALIZED VIEW inline_mv5
ENABLE QUERY REWRITE AS
SELECT A.a1, A.a2, B.b1, SUM(V1.a3) as sum_s1, SUM(V2.b2) as sum_s2
FROM A, B, (SELECT * FROM A) V1, (SELECT * FROM B) V2
WHERE (A.a1 = V1.a1) AND (B.b1 = V2.b2)
GROUP BY A.a1, A.a2, B.b1;


DROP MATERIALIZED VIEW INLINE_MV6;

--- MV has multiple inline views, with joins
CREATE MATERIALIZED VIEW inline_mv6
ENABLE QUERY REWRITE AS
SELECT A.a1, sum(V2.sum_v1) sum_v2 FROM
A, (SELECT A.a1, A.a2, SUM(V1.a3) as sum_v1
    FROM A, (SELECT * FROM A) V1
    GROUP BY A.a1, A.a2) V2
WHERE (A.a2 = V2.a2)
GROUP BY A.a1;


DROP MATERIALIZED VIEW INLINE_MV7;

--- MV has multiple inline views, with joins between views
CREATE MATERIALIZED VIEW inline_mv7
ENABLE QUERY REWRITE AS
SELECT A.a1, sum(V2.sum_v1) sum_v2 FROM
A, (SELECT A.a1, A.a2, SUM(V1.a3) as sum_v1
    FROM A, (SELECT * FROM A) V1
    GROUP BY A.a1, A.a2) V2,
   (SELECT B.b1, B.b2 FROM B) V3
WHERE (V2.a1 = V3.b1)
GROUP BY A.a1;


DROP MATERIALIZED VIEW INLINE_MV8;

--- MV has multiple inline views, with joins between views
CREATE MATERIALIZED VIEW inline_mv8
ENABLE QUERY REWRITE AS
SELECT A.a1, sum(V2.sum_v1) sum_v2 FROM
A, (SELECT A.a1, A.a2, SUM(V1.a3) as sum_v1
    FROM A, (SELECT * FROM A) V1
    GROUP BY A.a1, A.a2) V2,
   (SELECT B.b1, B.b2 FROM B) V3
WHERE (V2.a1 = V3.b1)
GROUP BY A.a1;


DROP MATERIALIZED VIEW INLINE_MV9;

--- MV has multiple inline views, but no joins
CREATE MATERIALIZED VIEW inline_mv9
ENABLE QUERY REWRITE AS
SELECT A.a1, B.b1, v1.a2, SUM(V1.a3) as sum_s1
FROM A, B, (SELECT * FROM A) V1, (SELECT * FROM B) V2
WHERE A.a1 = V1.a1 AND
      A.a2 = B.b2
GROUP BY A.a1, B.b1, V1.a2;


DROP MATERIALIZED VIEW INLINE_MV10;

CREATE MATERIALIZED VIEW inline_mv10
ENABLE QUERY REWRITE AS
SELECT A.a1, B.b1, v3.a2, SUM(V3.a3) as sum_s1
FROM A, B, (SELECT * FROM (SELECT * FROM A) V1) V3, (SELECT * FROM B) V2
WHERE A.a1 = V3.a1 AND
      A.a2 = B.b2
GROUP BY A.a1, B.b1, V3.a2;


DROP MATERIALIZED VIEW INLINE_MV11;

CREATE MATERIALIZED VIEW inline_mv11
ENABLE QUERY REWRITE
AS
SELECT A.a1, B.b1, SUM(A.a1) as sum_a1, COUNT(A.a1) as cnt_a1
FROM A, B, (SELECT a1, a2, SUM(a1) FROM A GROUP BY a1, a2) V1
WHERE A.a1 = V1.a1
GROUP BY A.a1, B.b1;

DROP MATERIALIZED VIEW INLINE_MV_CASE1;
CREATE MATERIALIZED VIEW inline_mv_case1
ENABLE QUERY REWRITE
AS
SELECT A.a1, A.a2, SUM(V1.a3) as sum_s
FROM A, (SeLeCT /*doen't matter ''""' comments */  
         * FroM "A_small") V1
WHERE A.a1 = V1.a1
GROUP BY A.a1, A.a2;

DROP MATERIALIZED VIEW INLINE_MV_CASE2;
CREATE MATERIALIZED VIEW inline_mv_case2
ENABLE QUERY REWRITE
AS
SELECT A.a1, A.a2, SUM(V1.a3) as sum_s
FROM A, (SeLeCT * fROm "A_SMALL") V1
WHERE A.a1 = V1.a1
GROUP BY A.a1, A.a2;

DROP MATERIALIZED VIEW INLINE_MV_CASE3;
CREATE MATERIALIZED VIEW inline_mv_case3
ENABLE QUERY REWRITE
AS
SELECT A.a1, A.a2, SUM(V1.a3) as sum_s
FROM A, (SeLeCT * FroM "A_small"
  where a6='small') V1
WHERE A.a1 = V1.a1
GROUP BY A.a1, A.a2;


--
-- Rewrites with inline_mv_case2
--
set serveroutput on;
DECLARE
        querytxt VARCHAR2(200) := 'SELECT A.a2, A.a1, SUM(V1.a3) FROM A, (SELECT * FROM A_small) V1
WHERE V1.a1 = A.a1
GROUP BY A.a2, A.a1';
BEGIN
  SYS.XRW('', 'QUERY_TXT, REWRITTEN_TXT, QUERY_BLOCK_NO, COSTS', querytxt);
END;
/


--
-- Rewrites with inline_mv_case3
--
set serveroutput on;
DECLARE
        querytxt VARCHAR2(200) := 'SELECT A.a2, A.a1, SUM(V1.a3) as sum_s
FROM A, (SeLeCT * FroM "A_small"
  where a6=''small'') V1
WHERE A.a1 = V1.a1
GROUP BY A.a1, A.a2';
BEGIN
  SYS.XRW('', 'QUERY_TXT, REWRITTEN_TXT, QUERY_BLOCK_NO, COSTS', querytxt);
END;
/


--
-- Rewrites using exact text match
--
set serveroutput on;
DECLARE
        querytxt VARCHAR2(200) := 'SELECT A.a1, A.a2, SUM(V1.a3) FROM A, (SELECT * FROM A) V1
WHERE A.a1 = V1.a1
GROUP BY A.a1, A.a2';
BEGIN
  SYS.XRW('', 'QUERY_TXT, REWRITTEN_TXT, QUERY_BLOCK_NO, COSTS', querytxt);
END;
/

--
-- No full or partial text match possible. Rewrites with general rewrite
--
set serveroutput on;
DECLARE
        querytxt VARCHAR2(200) := 'SELECT A.a1, B.b1, SUM(V1.a3) as sum_s1, SUM(V2.b2) as sum_s2
FROM A, B, (SELECT * FROM A) V1, (SELECT * FROM B) V2
WHERE V1.a1 = A.a1 AND
      B.b2 = A.a2
GROUP BY A.a1, B.b1';
BEGIN
  SYS.XRW('', 'QUERY_TXT, REWRITTEN_TXT, QUERY_BLOCK_NO, COSTS', querytxt);
END;
/


-- Query has multiple inline views. 
-- 
set serveroutput on;
DECLARE
  querytxt VARCHAR2(200) := 'SELECT A.a1, A.a2, B.b1, SUM(V1.a3) as sum_s1, SUM(V2.b2) as sum_s2
FROM A, B, (SELECT * FROM A) V1, (SELECT * FROM B) V2
WHERE (A.a1 = V1.a1) AND (B.b1 = V2.b2)
GROUP BY A.a1, A.a2, B.b1';
BEGIN
  SYS.XRW('', 'QUERY_TXT, REWRITTEN_TXT, QUERY_BLOCK_NO, COSTS', querytxt);
END;
/

set serveroutput on;
DECLARE
  querytxt VARCHAR2(200) := 'SELECT A.a1, B.b1, A.a2, SUM(V1.a3) as sum_s1, SUM(V2.b2) as sum_s2
FROM A, B, (SELECT * FROM A) V1, (SELECT * FROM B) V2
group BY A.a1, B.b1, A.a2';
BEGIN
  SYS.XRW('', 'QUERY_TXT, REWRITTEN_TXT, QUERY_BLOCK_NO, COSTS', querytxt);
END;
/

--
-- rewrites with multiple nested inline views
--
set serveroutput on;
DECLARE
  querytxt VARCHAR2(200) := 'SELECT A.a1, sum(V2.sum_v1) sum_v2 FROM
A, (SELECT A.a1, A.a2, SUM(V1.a3) as sum_v1
    FROM A, (SELECT * FROM A) V1
    GROUP BY A.a1, A.a2) V2
WHERE (V2.a2 = A.a2)
GROUP BY A.a1';
BEGIN
  SYS.XRW('', 'QUERY_TXT, REWRITTEN_TXT, QUERY_BLOCK_NO, COSTS', querytxt);
END;
/


--
--- Different aliases used in query for inline views. Should rewrite with
--- inline_mv10
--
set serveroutput on;
DECLARE
  querytxt VARCHAR2(200) := 'SELECT A.a1, VX.a2, SUM(VX.a3) as sum_s1
FROM A, B, (SELECT * FROM (SELECT * FROM A) V1) VX, (SELECT * FROM B) VY
WHERE A.a1 = VX.a1 AND
      B.b2 = A.a2
GROUP BY A.a1, VX.a2';
BEGIN
  SYS.XRW('', 'QUERY_TXT, REWRITTEN_TXT, QUERY_BLOCK_NO, COSTS', querytxt);
END;
/


--- rewrites with inline_mv11
--
set serveroutput on;
DECLARE
  querytxt VARCHAR2(200) := 'SELECT A.a1, AVG(A.a1) as avg_a1
FROM A, B, (SELECT a1, a2, SUM(a1) FROM A GROUP BY a1, a2) V1
WHERE V1.a1 = A.a1
GROUP BY A.a1';
BEGIN
  SYS.XRW('', 'QUERY_TXT, REWRITTEN_TXT, QUERY_BLOCK_NO, COSTS', querytxt);
END;
/

connect sys/knl_test7 as sysdba;

drop public synonym XRW;

drop user xrwdemo cascade;


DISCONNECT;


