Rem
Rem $Header: smxmv1.sql 03-aug-2004.12:12:07 mmoy Exp $
Rem
Rem smxmv1.sql
Rem
Rem Copyright (c) 2001, 2004, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      smxmv1.sql 
Rem
Rem    DESCRIPTION
Rem      This is a demo script for explain_mview().  It creates
Rem      a materialized view in SALES schema and uses explain_mview() 
Rem      to describe its capabilities and how to enhance it.  
Rem      The output of explain_mview() is stored in MV_CAPABILITIES_TABLE   
Rem
Rem    NOTE      
Rem      Please run utlxmv.sql in the admin directory to create 
Rem      MV_CAPABILITIES_TABLE in your schema before running this 
Rem      demo script
Rem  
Rem    MODIFIED   (MM/DD/YY)
Rem    mmoy        08/03/04 - Fix order by. 
Rem    twtong      04/02/01 - review comment
Rem    twtong      03/26/01 - Created
Rem
Rem ================================================================

CONNECT sh/sh;
SET ECHO ON;  
REM -----------------------------------------------------------
REM use explain_mview to analyze a potential materialized view
REM -----------------------------------------------------------
  
BEGIN
  DBMS_MVIEW.EXPLAIN_MVIEW 
    ('SELECT t.calendar_month_desc, 
             SUM(s.amount_sold) AS dollars 
      FROM sales s, times t 
      WHERE s.time_id = t.time_id 
      GROUP BY t.calendar_month_desc','ID1');
END;
/

SELECT SUBSTR(capability_name,1,30) capability_name,
       possible,
       SUBSTR(related_text,1,15) related_text,
       related_num,
       msgno,
       SUBSTR(msgtxt,1,110) msgtxt
FROM mv_capabilities_table
ORDER BY capability_name, seq;

TRUNCATE TABLE mv_capabilities_table;

REM -------------------------------------------------------------------
REM make corrections and enhancement to increase the capabilities of
REM the potential materialized view according to messages from
REM explain_mview()
REM -------------------------------------------------------------------

REM -------------------------------------------------------------------
REM create materialized view log on sales, times, and products
REM -------------------------------------------------------------------

CREATE MATERIALIZED VIEW LOG ON sales WITH ROWID;
CREATE MATERIALIZED VIEW LOG ON times;

BEGIN
  DBMS_MVIEW.EXPLAIN_MVIEW 
    ('SELECT t.calendar_month_desc, 
             SUM(s.amount_sold) AS dollars 
      FROM sales s, times t 
      WHERE s.time_id = t.time_id 
      GROUP BY t.calendar_month_desc','ID2');
END;
/

SELECT SUBSTR(capability_name,1,30) capability_name,
       possible,
       SUBSTR(related_text,1,15) related_text,
       related_num,
       msgno,
       SUBSTR(msgtxt,1,110) msgtxt
FROM mv_capabilities_table
ORDER BY seq;

TRUNCATE TABLE mv_capabilities_table;

REM ----------------------------------------------------------------
REM make more corrections on the materialized view log suggested
REM by explain_mview() 
REM ----------------------------------------------------------------

DROP MATERIALIZED VIEW LOG ON sales;
DROP MATERIALIZED VIEW LOG ON times;
CREATE MATERIALIZED VIEW LOG ON sales WITH ROWID (time_id, amount_sold)
   INCLUDING NEW VALUES;
CREATE MATERIALIZED VIEW LOG ON times WITH ROWID (time_id, calendar_month_desc)
   INCLUDING NEW VALUES;

REM -------------------------------------------------------------------
REM modify the select statement in the potential materialized view
REM to enhance the capabilities
REM 1) use PMARKER in the select and group by list to enable
REM    PCT on sales
REM 2) add COUNT(*) and COUNT(amount) in the select list to
REM    enhance fast refresh capabilities
REM -------------------------------------------------------------------
   
BEGIN
  DBMS_MVIEW.EXPLAIN_MVIEW
    ( 'SELECT DBMS_MVIEW.PMARKER(s.rowid),
              t.calendar_month_desc,
              COUNT(*) AS cnt_str,
              COUNT(s.amount_sold) AS cnt_amt,
              SUM(s.amount_sold) AS dollars
       FROM sales s, times t
       WHERE s.time_id = t.time_id
       GROUP BY DBMS_MVIEW.PMARKER(s.rowid),
    t.calendar_month_desc','ID3');
END;
/

SELECT SUBSTR(capability_name,1,30) capability_name,
       possible,
       SUBSTR(related_text,1,15) related_text,
       related_num,
       msgno,
       SUBSTR(msgtxt,1,110) msgtxt
FROM mv_capabilities_table
ORDER BY seq;

TRUNCATE TABLE mv_capabilities_table;

REM -------------------------------------------------------------------
REM re-create the materialized log to include sequence#
REM -------------------------------------------------------------------

DROP MATERIALIZED VIEW LOG ON sales;
DROP MATERIALIZED VIEW LOG ON times;
CREATE MATERIALIZED VIEW LOG ON sales WITH ROWID, SEQUENCE (time_id, amount_sold)
   INCLUDING NEW VALUES;
CREATE MATERIALIZED VIEW LOG ON times WITH ROWID, SEQUENCE (time_id, calendar_month_desc)
   INCLUDING NEW VALUES;

BEGIN
  DBMS_MVIEW.EXPLAIN_MVIEW
    ( 'SELECT DBMS_MVIEW.PMARKER(s.rowid),
              t.calendar_month_desc,
              COUNT(*) AS cnt_str,
              COUNT(s.amount_sold) AS cnt_amt,
              SUM(s.amount_sold) AS dollars
       FROM sales s, times t
       WHERE s.time_id = t.time_id
       GROUP BY DBMS_MVIEW.PMARKER(s.rowid),
    t.calendar_month_desc','ID4');
END;
/

SELECT SUBSTR(capability_name,1,30) capability_name,
       possible,
       SUBSTR(related_text,1,15) related_text,
       related_num,
       msgno,
       SUBSTR(msgtxt,1,110) msgtxt
FROM mv_capabilities_table
ORDER BY seq;

TRUNCATE TABLE mv_capabilities_table;

REM -------------------------------------------------------------------
REM create the materialized view
REM -------------------------------------------------------------------

CREATE MATERIALIZED VIEW month_sales_mv
  BUILD IMMEDIATE
  REFRESH FORCE
  DISABLE QUERY REWRITE
  AS
  SELECT DBMS_MVIEW.PMARKER(s.rowid),
         t.calendar_month_desc,
         COUNT(*) AS cnt_str,
         COUNT(s.amount_sold) AS cnt_amt,
         SUM(s.amount_sold) AS dollars
  FROM sales s, times t
  WHERE s.time_id = t.time_id
  GROUP BY DBMS_MVIEW.PMARKER(s.rowid),
           t.calendar_month_DESC;

REM ----------------------------------------------------------------
REM use explain_mview() on the materialized view that has just been
REM created to check if it has all the capabilities
REM -----------------------------------------------------------------
  
EXECUTE DBMS_MVIEW.EXPLAIN_MVIEW('month_sales_mv','ID5');
 
SELECT SUBSTR(capability_name,1,30) capability_name,
       possible,
       SUBSTR(related_text,1,15) related_text,
       related_num,
       msgno,
       SUBSTR(msgtxt,1,110) msgtxt
FROM mv_capabilities_table
ORDER BY seq;

TRUNCATE TABLE mv_capabilities_table;

REM ----------------------------------------------------------------
REM enable query rewrite on the materialized view
REM -----------------------------------------------------------------
  
ALTER materialized VIEW month_sales_mv enable query rewrite;
  
EXECUTE DBMS_MVIEW.EXPLAIN_MVIEW('month_sales_mv','ID6');
 
SELECT SUBSTR(capability_name,1,30) capability_name,
       possible,
       SUBSTR(related_text,1,15) related_text,
       related_num,
       msgno,
       SUBSTR(msgtxt,1,110) msgtxt
FROM mv_capabilities_table
ORDER BY seq;

TRUNCATE TABLE mv_capabilities_table;

REM --------------------------------
REM Clean up
REM --------------------------------

DROP MATERIALIZED VIEW month_sales_mv;
DROP MATERIALIZED VIEW LOG ON sales;
DROP MATERIALIZED VIEW LOG ON times;
DISCONNECT



