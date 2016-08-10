rem
rem $Header: o8demo.sql 30-jan-2003.13:53:32 hyeh Exp $
rem
rem o8demo.sql
rem
rem Copyright (c) 1996, 2003, Oracle Corporation.  All rights reserved.  
rem
rem    NAME
rem      o8demo.sql - Oracle8 demo
rem
rem    DESCRIPTION
rem    This demo features Oracle8's object extensions, such as, abstract
rem    data types (ADTs) and methods.
rem
rem    NOTES
rem    Schema -
rem    A standalone instance of the typed table Orders contains REFs to
rem    instances of  Companies and SalesReps., and an embedded instance of an
rem    order Status.  An Order also contains two methods, "totalCost" and
rem    "statusSummary".
rem
rem    MODIFIED   (MM/DD/YY)
rem    hyeh        01/30/03 - add ORDER BY to all select
rem    hyeh        08/10/99 - use sqlplus syntax
rem    mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem    yaggarwa    08/06/98 - Add select statements
rem    mchien      01/22/98 - name resolution
rem    cchau       08/18/97 - enable dictionary protection
rem    mchien      05/29/97 - fix type syntax
rem    mchien      07/16/96 - Created (adopted from kschultz & dherkime)
rem

REMARK >>>> Set System Variables For Current SQLPlus Session <<<<
SET FEEDBACK 1
SET NUMWIDTH 10
SET PAGESIZE 24
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET DEFINE '^'

set echo on
connect system/manager

grant connect,resource, unlimited tablespace to demo8 identified by demo8;
connect demo8/demo8

create or replace type product_t AS OBJECT (
  productId number,
  productName varchar2(64),
  cost number(9,2));
/

create table products of product_t;

create or replace type salesrep_t AS OBJECT (
  repId number,
  repName varchar2(64));
/

create table salesreps of salesrep_t;

create or replace type company_t AS OBJECT (
  companyId number,
  companyName varchar2(64));
/

create table companies of company_t;

CREATE TABLE tempTot (
        total number, orderID number);

create table tempResult (result number);

create table tempStat (status varchar2(64));

create or replace type status_t AS OBJECT (
  status varchar2(16),
  statusDate date,
  comments varchar2(32));
/

create OR REPLACE type order_t AS OBJECT (
  orderId    number,
  orderDate  date,
  salesRep   ref salesrep_t,
  company    ref company_t,
  status     status_t,
  MEMBER FUNCTION totalCost RETURN NUMBER,
  MEMBER FUNCTION statusSummary RETURN VARCHAR2
  );
/

create table orders of order_t;

CREATE OR REPLACE TYPE BODY Order_t AS
    MEMBER FUNCTION totalCost RETURN NUMBER IS
        tot NUMBER;
    BEGIN
        SELECT total INTO tot
        FROM tempTot
        WHERE tempTot.orderID = SELF.orderID;
        RETURN tot;
    END;
    MEMBER FUNCTION statusSummary RETURN VARCHAR2 IS
        st VARCHAR2(64);
    BEGIN
      SELECT status INTO st
        FROM tempStat;
        RETURN st;
    END;
END;
/
show errors
/


create  or replace type item_t AS OBJECT (
  itemId number,
  quantity number,
  product REF product_t,
  orderRef ref order_t);
/

create table items of item_t;

insert into products values (101,'ISDN Phone',50.00);
insert into products values (201,'ISDN Switch',5000.00);
insert into products values (301,'100 Yd. Cable',25.00);
insert into products values (401,'Installation Guide',29.95);

select * from products order by productId;

insert into salesReps values (16473,'Karl J. Schultz');
insert into salesReps values (10000,'Don J. Herkimer');
insert into salesReps values (20000,'Robin A. Wada');

select * from salesReps order by repId;

insert into companies values (1111, 'ABC Graphics, Inc');
insert into companies values (2222, 'First National Bank');
insert into companies values (3333, 'United Carbonation, Ltd');

select * from companies order by companyId;

insert into orders values(11,sysdate,NULL,NULL,
  status_t('Shipped',TO_DATE('01-JUN-96'),'UPS Ground'));
insert into orders values(12,sysdate,NULL,NULL,
  status_t('Pending',TO_DATE('02-JUN-96'),'BO ISDN Switch'));
insert into orders values(13,sysdate,NULL,NULL,
  status_t('Shipped',TO_DATE('03-JUN-96'),'UPS Ground'));
insert into orders values(21,sysdate,NULL,NULL,
  status_t('On Hold',TO_DATE('04-JUN-96'),'Waiting for PO'));
insert into orders values(22,sysdate,NULL,NULL,
  status_t('Pending',TO_DATE('05-JUN-96'),'BO Cable'));
insert into orders values(23,sysdate,NULL,NULL,
  status_t('Shipped',TO_DATE('06-JUN-96'),'UPS Ground'));
insert into orders values(31,sysdate,NULL,NULL,
  status_t('Shipped',TO_DATE('07-JUN-96'),'UPS Ground'));
insert into orders values(32,sysdate,NULL,NULL,
  status_t('Shipped',TO_DATE('08-JUN-96'),'UPS Ground'));
insert into orders values(33,sysdate,NULL,NULL,
  status_t('On Hold',TO_DATE('09-JUN-96'),'Waiting for PO'));

select o.orderId, o.orderDate,
  o.status.status, o.status.statusDate, o.status.comments
  from orders o order by orderId;

update orders
  set salesrep = (select ref(s) from salesreps s where repId = 16473)
  where orderId between 10 and 20;

update orders
  set salesrep = (select ref(s) from salesreps s where repId = 20000)
  where orderId between 20 and 30;

update orders
  set salesrep = (select ref(s) from salesreps s where repId = 10000)
  where orderId between 30 and 40;

update orders
  set company = (select ref(c) from companies c where companyId = 1111)
  where orderId in (11,21,31);

update orders
  set company = (select ref(c) from companies c where companyId = 2222)
  where orderId in (12,22,32);

update orders
  set company = (select ref(c) from companies c where companyId = 3333)
  where orderId in (13,23,33);

select o.orderId, o.orderDate,
       o.status.status, o.status.statusDate, o.status.comments
  from orders o order by orderId;

select o.orderId, o.orderDate, o.status.status,
       o.status.statusDate, o.status.comments,
       o.salesRep.repId, o.salesRep.repName,
       o.company.companyId, o.company.companyName
  from orders o order by orderId;


insert into items values(1,100,NULL,NULL);
insert into items values(2,1,NULL,NULL);
insert into items values(3,2,NULL,NULL);
insert into items values(4,3,NULL,NULL);

insert into items values(5,200,NULL,NULL);
insert into iteMs values(6,2,NULL,NULL);
insert into items values(7,4,NULL,NULL);

update items
  set product = (select ref(p) from products p where productId = 101)
  where itemId = 1;

update items
  set product = (select ref(p) from products p where productId = 201)
  where itemId = 2;

update items
  set product = (select ref(p) from products p where productId = 301)
  where itemId = 3;

update items
  set product = (select ref(p) from products p where productId = 401)
  where itemId = 4;

update items
  set product = (select ref(p) from products p where productId = 101)
  where itemId = 5;

update items
  set product = (select ref(p) from products p where productId = 201)
  where itemId = 6;

update items
  set product = (select ref(p) from products p where productId = 301)
  where itemId = 7;

update items
  set orderRef = (select ref(o) from orders o where orderId = 11)
  where itemId = 1;

update items
  set orderRef = (select ref(o) from orders o where orderId = 11)
  where itemId = 2;

update items
  set orderRef = (select ref(o) from orders o where orderId = 11)
  where itemId = 3;

update items
  set orderRef = (select ref(o) from orders o where orderId = 11)
  where itemId = 4;

update items
  set orderRef = (select ref(o) from orders o where orderId = 12)
  where itemId = 5;

update items
  set orderRef = (select ref(o) from orders o where orderId = 12)
  where itemId = 6;

update items
  set orderRef = (select ref(o) from orders o where orderId = 12)
  where itemId = 7;

select i.itemId, i.quantity,
       i.product.productId, i.product.productName, i.product.cost,
       i.orderRef.orderId, i.orderRef.orderDate,
       i.orderRef.status.status,  i.orderRef.status.comments,
       i.orderRef.salesRep.repId, i.orderRef.salesRep.repName,
       i.orderRef.company.companyId, i.orderRef.company.companyName
  from items i order by itemId;

CREATE OR REPLACE PACKAGE utils AS
FUNCTION computeTotalForOrder(ordID NUMBER) RETURN NUMBER;
FUNCTION getStatusForOrder(ordID NUMBER) RETURN VARCHAR2;
END;
/

CREATE OR REPLACE PACKAGE BODY utils AS
FUNCTION computeTotalForOrder(ordID NUMBER) RETURN NUMBER IS
    anOrder order_t;
BEGIN
    anOrder.OrderID := ordID;
    RETURN anOrder.totalCost;
End;
FUNCTION getStatusForOrder(ordID NUMBER) RETURN VARCHAR2 IS
    anOrder order_t;
BEGIN
    anOrder.OrderID := ordID;
    RETURN anOrder.statusSummary;
End;
END;
/
show errors

commit;

-- Three-way join required in Oracle7 to do a simple select in Oracle8
--.
-- SELECT OrderID, SalesReps.repName, Company.companyName,
-- FROM   Orders o, SalesReps reps, Companies c, OrderStatuses st
-- WHERE  o.ID = 11 AND
--        o.ID = st.OrderID AND
--        o.RepID = reps.ID AND
--        o.CompanyID = c.ID;

SELECT orderId, o.salesRep.repName, o.company.companyName, o.status.status
  FROM   Orders o
  WHERE  o.orderID = 11 order by orderId;

connect sys/knl_test7 as sysdba;
drop user demo8 cascade;
set echo off
