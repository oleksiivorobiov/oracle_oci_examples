Rem
Rem $Header: smxmv2.sql 02-apr-2001.15:00:24 twtong Exp $
Rem
Rem smxmv2.sql
Rem
Rem  Copyright (c) Oracle Corporation 2001. All Rights Reserved.
Rem
Rem    NAME
Rem      smxmv2.sql
Rem
Rem    DESCRIPTION
Rem      This is a demo script for explain_mview().  It uses 
Rem      explain_mview() on a potential materialized view and
Rem      display its capabilities and how to enhance it.  The
Rem      materialized view is not created by explain_mview().
Rem      Only the select statement for the potential materialized
Rem      view is used by explain_mview to analyze its capabilities
Rem      The output is stored in a VARRAY 
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    twtong      04/02/01 - review comment
Rem    twtong      03/26/01 - Created
Rem
REM ================================================================

CONNECT sh/sh;
SET ECHO ON
SET SERVEROUTPUT ON

DECLARE
  XMV_Array SYS.ExplainMVArrayType := SYS.ExplainMVArrayType();
  no_of_msgs NUMBER;
  i NUMBER;
BEGIN
   DBMS_OUTPUT.enable(32512);
   dbms_mview.explain_mview ('SELECT t.week_ending_day, p.prod_subcategory, s.channel_id,
                                     s.promo_id, sum(s.amount_sold) AS dollars
                              FROM sales s, times t, products p
                              WHERE s.time_id = t.time_id AND
                                    s.prod_id = p.prod_id
                              GROUP BY t.week_ending_day, p.prod_subcategory,
                                       s.channel_id, s.promo_id',
                             XMV_Array);
   no_of_msgs := XMV_Array.count;
   FOR i IN 1..no_of_msgs
   LOOP
      DBMS_OUTPUT.PUT_LINE(' CAPABILITY : ' || XMV_Array(i).capability_name);
      DBMS_OUTPUT.PUT_LINE(' POSSIBLE   : ' || XMV_Array(i).possible);
      DBMS_OUTPUT.PUT_LINE(' REL_TEXT   : ' || XMV_Array(i).related_text);
      DBMS_OUTPUT.PUT_LINE(' REL_NUM    : ' || XMV_Array(i).related_num);
      DBMS_OUTPUT.PUT_LINE(' MSGNO      : ' || XMV_Array(i).msgno);
      DBMS_OUTPUT.PUT_LINE(' MSGTXT     : ' || XMV_Array(i).msgtxt);
      DBMS_OUTPUT.PUT_LINE(' ');
   END LOOP;
END;
/

REM -------------------------------------------------------------------
REM make corrections and enhancement to increase the capabilities of 
REM the potential materialized view according to messages from 
REM explain_mview()
REM -------------------------------------------------------------------

REM -------------------------------------------------------------------
REM create materialized view log on sales, times, and products 
REM -------------------------------------------------------------------

CREATE MATERIALIZED VIEW LOG ON sales WITH ROWID;
CREATE MATERIALIZED VIEW LOG ON times WITH ROWID;
CREATE MATERIALIZED VIEW LOG ON products WITH ROWID;

REM -------------------------------------------------------------------
REM modify the select statement to enhance the capabilities
REM 1) use PMARKER in the select and group by list to enable
REM    PCT on sales
REM 2) add COUNT(*) and COUNT(amount) in the select list to
REM    enhance fast refresh capabilities
REM -------------------------------------------------------------------

DECLARE
  XMV_Array SYS.ExplainMVArrayType := SYS.ExplainMVArrayType();
  no_of_msgs NUMBER;
  i NUMBER;
BEGIN
   DBMS_OUTPUT.enable(32512);
   dbms_mview.explain_mview ('SELECT DBMS_MVIEW.PMARKER(s.rowid), t.week_ending_day, 
                                     p.prod_subcategory, s.channel_id, s.promo_id, 
                                     COUNT(*) AS cnt_str, COUNT(s.amount_sold) AS cnt_amt,
                                     SUM(s.amount_sold) AS dollars
                              FROM sales s, times t, products p
                              WHERE s.time_id = t.time_id AND
                                    s.prod_id = p.prod_id
                              GROUP BY DBMS_MVIEW.PMARKER(s.rowid),
                                       t.week_ending_day, p.prod_subcategory,
                                       s.channel_id, s.promo_id',
                             XMV_Array);
   no_of_msgs := XMV_Array.count;
   FOR i IN 1..no_of_msgs
   LOOP
      DBMS_OUTPUT.PUT_LINE(' CAPABILITY : ' || XMV_Array(i).capability_name);
      DBMS_OUTPUT.PUT_LINE(' POSSIBLE   : ' || XMV_Array(i).possible);
      DBMS_OUTPUT.PUT_LINE(' REL_TEXT   : ' || XMV_Array(i).related_text);
      DBMS_OUTPUT.PUT_LINE(' REL_NUM    : ' || XMV_Array(i).related_num);
      DBMS_OUTPUT.PUT_LINE(' MSGNO      : ' || XMV_Array(i).msgno);
      DBMS_OUTPUT.PUT_LINE(' MSGTXT     : ' || XMV_Array(i).msgtxt);
      DBMS_OUTPUT.PUT_LINE(' ');
   END LOOP;
END;
/

REM ----------------------------------------------------------------
REM make more corrections on the materialized view log suggested
REM by explain_mview() to enable all fast refresh capabilities
REM ----------------------------------------------------------------

DROP MATERIALIZED VIEW LOG ON sales;
CREATE MATERIALIZED VIEW LOG ON sales WITH ROWID, SEQUENCE
   (time_id, prod_id, channel_id, promo_id, amount_sold) INCLUDING NEW VALUES;

DROP MATERIALIZED VIEW LOG ON times; 
CREATE MATERIALIZED VIEW LOG ON times WITH ROWID, SEQUENCE
   (time_id, week_ending_day) INCLUDING NEW VALUES;

DROP MATERIALIZED VIEW LOG ON products;
CREATE MATERIALIZED VIEW LOG ON products WITH ROWID, SEQUENCE
   (prod_id, prod_subcategory) INCLUDING NEW VALUES;

REM ----------------------------------------------------------------
REM retry explain_mview() to see if the potential materialized view
REM has all the capabilities
REM ----------------------------------------------------------------

DECLARE
  XMV_Array SYS.ExplainMVArrayType := SYS.ExplainMVArrayType();
  no_of_msgs NUMBER;
  i NUMBER;
BEGIN
   DBMS_OUTPUT.enable(32512);
   dbms_mview.explain_mview ('SELECT DBMS_MVIEW.PMARKER(s.rowid), t.week_ending_day, 
                                     p.prod_subcategory, s.channel_id, s.promo_id, 
                                     COUNT(*) AS cnt_str, COUNT(s.amount_sold) AS cnt_amt,
                                     SUM(s.amount_sold) AS dollars
                              FROM sales s, times t, products p
                              WHERE s.time_id = t.time_id AND
                                    s.prod_id = p.prod_id
                              GROUP BY DBMS_MVIEW.PMARKER(s.rowid),
                                       t.week_ending_day, p.prod_subcategory,
                                       s.channel_id, s.promo_id',
                             XMV_Array);
   no_of_msgs := XMV_Array.count;
   FOR i IN 1..no_of_msgs
   LOOP
      DBMS_OUTPUT.PUT_LINE(' CAPABILITY : ' || XMV_Array(i).capability_name);
      DBMS_OUTPUT.PUT_LINE(' POSSIBLE   : ' || XMV_Array(i).possible);
      DBMS_OUTPUT.PUT_LINE(' REL_TEXT   : ' || XMV_Array(i).related_text);
      DBMS_OUTPUT.PUT_LINE(' REL_NUM    : ' || XMV_Array(i).related_num);
      DBMS_OUTPUT.PUT_LINE(' MSGNO      : ' || XMV_Array(i).msgno);
      DBMS_OUTPUT.PUT_LINE(' MSGTXT     : ' || XMV_Array(i).msgtxt);
      DBMS_OUTPUT.PUT_LINE(' ');
   END LOOP;
END;                                                                                               
/

REM ----------------------------------------------------------------
REM create the materialized view
REM ----------------------------------------------------------------

CREATE MATERIALIZED VIEW week_sales_mv
BUILD IMMEDIATE
REFRESH FORCE
ENABLE QUERY REWRITE
AS
SELECT DBMS_MVIEW.PMARKER(s.rowid), t.week_ending_day,
       p.prod_subcategory, s.channel_id, s.promo_id,
       COUNT(*) AS cnt_str, COUNT(s.amount_sold) AS cnt_amt,
       SUM(s.amount_sold) AS dollars
FROM sales s, times t, products p
WHERE s.time_id = t.time_id AND
      s.prod_id = p.prod_id
GROUP BY DBMS_MVIEW.PMARKER(s.rowid),
         t.week_ending_day, p.prod_subcategory,
         s.channel_id, s.promo_id;

REM ----------------------------------------------------------------
REM use explain_mview() on the materialized view that has just
REM been created 
REM ----------------------------------------------------------------

DECLARE
  XMV_Array SYS.ExplainMVArrayType := SYS.ExplainMVArrayType();
  no_of_msgs NUMBER;
  i NUMBER;
BEGIN
   DBMS_OUTPUT.enable(32512);
   dbms_mview.explain_mview ('week_sales_mv', XMV_Array);
   no_of_msgs := XMV_Array.count;
   FOR i IN 1..no_of_msgs
   LOOP
      DBMS_OUTPUT.PUT_LINE(' CAPABILITY : ' || XMV_Array(i).capability_name);
      DBMS_OUTPUT.PUT_LINE(' POSSIBLE   : ' || XMV_Array(i).possible);
      DBMS_OUTPUT.PUT_LINE(' REL_TEXT   : ' || XMV_Array(i).related_text);
      DBMS_OUTPUT.PUT_LINE(' REL_NUM    : ' || XMV_Array(i).related_num);
      DBMS_OUTPUT.PUT_LINE(' MSGNO      : ' || XMV_Array(i).msgno);
      DBMS_OUTPUT.PUT_LINE(' MSGTXT     : ' || XMV_Array(i).msgtxt);
      DBMS_OUTPUT.PUT_LINE(' ');
   END LOOP;
END;
/

REM -----------------------------------------------------------
REM clean up
REM -----------------------------------------------------------

DROP MATERIALIZED VIEW LOG ON sales;
DROP MATERIALIZED VIEW LOG ON products;
DROP MATERIALIZED VIEW LOG ON times;
DISCONNECT;


