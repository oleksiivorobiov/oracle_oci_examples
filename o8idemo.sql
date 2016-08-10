rem
rem $Header: o8idemo.sql 31-oct-2006.13:40:56 ytsai Exp $
rem
rem o8idemo.sql
rem
rem Copyright (c) 1998, 2006, Oracle. All rights reserved.  
rem
rem    NAME
rem      o8idemo.sql - Purchase Order example and DDL sample on object view
rem
rem    DESCRIPTION
rem      This demonstrate the Purchase Order example described in manual
rem      Application Developer's Guide
rem      and some extra examples on object view DDL
rem
rem    NOTES
rem      In order to run this sample, the database must be started up with
rem      option compatible=8.1.0.0.0 (or a higher compatible value)
rem
rem    MODIFIED   (MM/DD/YY)
rem    ytsai       10/31/06 - fix connect
rem    hyeh        01/30/03 - add ORDER BY to all SELECT
rem    mchien      11/04/99 - 8.1.6 additions from KOSINSKI
rem    hyeh        08/11/99 - set session format
rem    mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem    hyeh        01/06/99 - use compressed nested iot
rem    hyeh        12/22/98 - do not use compressed index - bug 785326, 785403
rem    mkrishna    10/22/98 - add FORCE option for MAKE_REF
rem    mchien      10/21/98 - do not use compressed index for now - bug 740405
rem    yaggarwa    10/13/98 -
rem    yaggarwa    10/13/98 - Created
rem

REMARK >>>> Set System Variables For Current SQLPlus Session <<<<
SET FEEDBACK 1
SET NUMWIDTH 10
SET PAGESIZE 24
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET DEFINE '^'

SET ECHO ON
CONNECT system/manager;
DROP USER po CASCADE
/
GRANT RESOURCE,DBA TO po IDENTIFIED BY po
/
CONNECT po/po

CREATE TABLE Customer_reltab (
       Custno     NUMBER,         -- Customer NUMBER
       Custname   VARCHAR2(200),  -- Customer name
       Street     VARCHAR2(200),  -- Customer Street
       City       VARCHAR2(200),  -- Customer City
       State      CHAR(2),
       Zip        VARCHAR2(20),   -- Customer Zip
       Phone1     VARCHAR2(20),
       Phone2     VARCHAR2(20),
       Phone3     VARCHAR2(20),
       PRIMARY KEY (Custno)
   )
/
CREATE TABLE PurchaseOrder_reltab (
        PONo         NUMBER,         -- purchase order no
        Custno       NUMBER references Customer_reltab ,
                                     -- foreign KEY referencing customer
        OrderDate      DATE,           -- Date OF order
        ShipDate      DATE,           -- Date to be shipped
        ToStreet      VARCHAR2(200),  -- shipto Address
        ToCity        VARCHAR2(200),
        ToState       CHAR(2),
        ToZip         VARCHAR2(20),
        PRIMARY KEY(PONo)
   )
/
CREATE TABLE Stock_reltab (
        StockNo      NUMBER PRIMARY KEY,
        Price        NUMBER,
        TaxRate      NUMBER
   )
/
CREATE TABLE LineItems_reltab (
      LineItemNo   NUMBER,
        PONo         NUMBER references PurchaseOrder_reltab,
        StockNo      NUMBER references Stock_reltab,
        Quantity     NUMBER,
        Discount     NUMBER,
        PRIMARY KEY (PONo,LineItemNo)
   );


-- 2. Values Inserted

INSERT INTO Customer_reltab
      VALUES(1,'Jean Nance','2, Avocet Drive','Redwood Shores','CA','95054',
      '801-010-0090',NULL,NULL);
INSERT INTO Customer_reltab
      VALUES(2,'John Nike','323, College Drive','Edison','NJ','08820',
      '901-090-0000','908-201-1002',NULL);
INSERT INTO PurchaseOrder_reltab
      VALUES(1001,1,SYSDATE,'10-MAY-1997',NULL,NULL,NULL,NULL);
INSERT INTO PurchaseOrder_reltab
      VALUES(2001,2,SYSDATE,'20-MAY-1997','55, Madison Ave','Madison','WI',
             '53715');

INSERT INTO Stock_reltab VALUES(1004,6750.00,2);
INSERT INTO Stock_reltab VALUES(1011,4500.23,2);
INSERT INTO Stock_reltab VALUES(1534,2234.00,2);
INSERT INTO Stock_reltab VALUES(1535,3456.23,2);

INSERT INTO LineItems_reltab VALUES(01,1001,1534,12,0)
/
INSERT INTO LineItems_reltab VALUES(02,1001,1535,10,10)
/
INSERT INTO LineItems_reltab VALUES(10,2001,1004,1,0)
/
INSERT INTO LineItems_reltab VALUES(11,2001,1011,2,1)
/
-- 3. Selection :-

-- List Customer and Lineitems FOR a given Purchase Order

SELECT c.Custno, c.Custname, c.Street, c.City, c.State, c.Zip, c.Phone1,
         c.Phone2, c.Phone3,
         p.PONo, p.OrderDate,
         l.StockNo, l.LineItemNo, l.Quantity, l.Discount
FROM   Customer_reltab c, PurchaseOrder_reltab p, LineItems_reltab l
WHERE  c.Custno = p.Custno AND p.PONo = l.PONo
AND p.PONo = 1001 ORDER BY c.Custno, l.LineItemNo;

-- List the Total value OF Purchase Orders

SELECT p.PONo, SUM(s.Price * l.Quantity)
FROM   PurchaseOrder_reltab p, LineItems_reltab l, Stock_reltab s
WHERE  p.PONo = l.PONo AND l.StockNo = s.StockNo
GROUP BY p.PONo ORDER BY p.PONo;

-- List all the Purchase Order information and  the Line Item information
-- FOR those LineItems_tab that use the stock WITH  StockNo 1004

SELECT p.PONo, p.Custno,
       l.StockNo, l.LineItemNo, l.Quantity, l.Discount
FROM  PurchaseOrder_reltab p, LineItems_reltab l
WHERE p.PONo = l.PONo (+) AND
      l.StockNo = 1004 
ORDER BY p.PONo, l.LineItemNo;


-- ========================================================================
-- THE OBJECT-RELATIONAL SYSTEM (Oracle 8.0.3)


CREATE TYPE StockItem_objtyp
/

CREATE TYPE LineItem_objtyp
/

CREATE TYPE PurchaseOrder_objtyp
/

CREATE TYPE PhoneList_vartyp AS VARRAY(10) OF VARCHAR2(20)
/

CREATE TYPE Address_objtyp AS OBJECT (
  Street           VARCHAR2(200),
  City             VARCHAR2(200),
  State            CHAR(2),
  Zip              VARCHAR2(20),

  MEMBER PROCEDURE
    display
  ) 
/


CREATE OR REPLACE TYPE Customer_objtyp AS OBJECT (
  Custno           NUMBER,
  Custname         VARCHAR2(200),
  Address_obj      Address_objtyp,
  PhoneList_var    PhoneList_vartyp,

  ORDER MEMBER FUNCTION
    compareCustOrders(x IN Customer_objtyp) RETURN INTEGER,

  MEMBER PROCEDURE
    display
  ) 
/

CREATE TYPE LineItem_objtyp AS OBJECT (
  LineItemNo       NUMBER,
  Stock_ref        REF StockItem_objtyp,
  Quantity         NUMBER,
  Discount         NUMBER,

  MEMBER PROCEDURE
    display
  ) 
/
CREATE TYPE LineItemList_ntabtyp AS TABLE OF LineItem_objtyp
/
CREATE TYPE PurchaseOrder_objtyp AS OBJECT (
  PONo              NUMBER,
  Cust_ref          REF Customer_objtyp,
  OrderDate         DATE,
  ShipDate          DATE,
  LineItemList_ntab LineItemList_ntabtyp,
  ShipToAddr_obj    Address_objtyp,

  MAP MEMBER FUNCTION
    getPONo RETURN NUMBER,

  MEMBER FUNCTION
    sumLineItems RETURN NUMBER,

  MEMBER PROCEDURE 
    purchase_item (item NUMBER, 
                   qty NUMBER, 
                   discount NUMBER),

  MEMBER PROCEDURE 
    update_item (lineno NUMBER, 
                 item NUMBER, 
                 qty NUMBER, 
                 discount NUMBER),

  MEMBER PROCEDURE
    display
  ) 
/

CREATE TYPE StockItem_objtyp AS OBJECT (
  StockNo          NUMBER,
  Price            NUMBER,
  TaxRate          NUMBER
  ) 
/

-- If no relational data exist, and tables need to be created, then create the

-- following object tables

CREATE TABLE Customer_objtab OF Customer_objtyp (Custno PRIMARY KEY)
  OBJECT ID PRIMARY KEY
/

CREATE TABLE Stock_objtab OF StockItem_objtyp (StockNo PRIMARY KEY)
  OBJECT ID PRIMARY KEY
/

CREATE TABLE PurchaseOrder_objtab OF PurchaseOrder_objtyp
          (PRIMARY KEY (PONo),
           FOREIGN KEY (Cust_ref) REFERENCES Customer_objtab)
           OBJECT ID PRIMARY KEY
           NESTED TABLE LineItemList_ntab STORE AS PoLine_ntab(
                 (PRIMARY KEY(NESTED_TABLE_ID, LineItemNo))
                  ORGANIZATION INDEX COMPRESS
           )  RETURN AS LOCATOR
/

ALTER TABLE PoLine_ntab ADD (SCOPE FOR (Stock_ref) IS stock_objtab);

-- Following statement creates an INDEX on the internal column
-- "NESTED_TABLE_ID" OF a storage TABLE.
CREATE INDEX Po_nidx on PoLine_ntab (NESTED_TABLE_ID)
/
-- Insertion OF VALUES --

INSERT INTO Stock_objtab VALUES(StockItem_objtyp(1004,6750.00,2));
INSERT INTO Stock_objtab VALUES(StockItem_objtyp(1011,4500.23,2));
INSERT INTO Stock_objtab VALUES(StockItem_objtyp(1534,2234.00,2));
INSERT INTO Stock_objtab VALUES(1535,3456.23,2)
/
INSERT INTO Customer_objtab
    VALUES(1,'Jean Nance',
           Address_objtyp('2 Avocet Drive','Redwood Shores','CA','95054'),
           PhoneList_vartyp('415-904-0940','408-506-2020'));
INSERT INTO Customer_objtab
    VALUES(2,'John Nike',
           Address_objtyp('323 College Drive','Edison','NJ','08820'),
           PhoneList_vartyp('908-904-0940'));

INSERT INTO PurchaseOrder_objtab
      SELECT 1001,REF(C),SYSDATE,'10-MAY-1997',LineItemList_ntabtyp(),NULL
      FROM Customer_objtab C
      WHERE C.Custno = 1;

INSERT INTO PurchaseOrder_objtab
     SELECT 2001,REF(C),SYSDATE,'20-MAY-1997',
            CAST(MULTISET(SELECT 01,
                                 REF(S),
                                 12,
                                 0
                          FROM Stock_objtab S
                          WHERE S.StockNo = 1534) AS LineItemList_ntabtyp),
            Address_objtyp('55, Madison Ave','Madison','WI','53715')
     FROM Customer_objtab C
     WHERE C.Custno = 2;

INSERT INTO TABLE(SELECT P.LineItemList_ntab
                  FROM PurchaseOrder_objtab P WHERE P.PONo = 1001)
      SELECT 01,REF(S),10,10 FROM Stock_objtab S WHERE S.StockNo = 1535;

INSERT INTO TABLE(SELECT P.LineItemList_ntab
                  FROM PurchaseOrder_objtab P WHERE P.PONo = 2001)
      SELECT 10,REF(S), 1,0  FROM Stock_objtab S WHERE S.StockNo = 1004;

INSERT INTO TABLE(SELECT P.LineItemList_ntab
                  FROM PurchaseOrder_objtab P WHERE P.PONo = 2001 )
        VALUES(LineItem_objtyp( 11,NULL,2,1))
/
UPDATE TABLE(SELECT P.LineItemList_ntab
              FROM PurchaseOrder_objtab P WHERE P.PONo = 2001)  plist
      SET  plist.Stock_ref = (SELECT REF(S)
                              FROM Stock_objtab S WHERE S.StockNo = 1011)
      WHERE plist.LineItemNo = 11
/

 -- Functional definitions
CREATE OR REPLACE TYPE BODY PurchaseOrder_objtyp AS
  MEMBER FUNCTION sumLineItems RETURN NUMBER IS
    i          INTEGER;
    StockVal   StockItem_objtyp;
    LineItem   LineItem_objtyp;
    Total      NUMBER := 0;

  BEGIN
    IF (UTL_COLL.IS_LOCATOR(LineItemList_ntab))
    THEN
      SELECT SUM(L.Quantity * L.Stock_ref.Price) INTO Total
      FROM   TABLE(CAST(LineItemList_ntab AS LineItemList_ntabtyp)) L;
    ELSE
      FOR i IN 1..SELF.LineItemList_ntab.COUNT LOOP
	LineItem := SELF.LineItemList_ntab(i);
	   UTL_REF.SELECT_OBJECT(LineItem.Stock_ref, StockVal);
	   Total := Total + LineItem.Quantity * StockVal.Price * 
		    (1 - LineItem.Discount/100);
       END LOOP;
    END IF;
    RETURN Total;
  END;

  MAP MEMBER FUNCTION getPONo RETURN NUMBER is
  BEGIN
    RETURN PONo;
  END;

  MEMBER PROCEDURE purchase_item (item NUMBER, 
                                  qty NUMBER, 
                                  discount NUMBER) IS
    StockRef REF StockItem_objtyp;
  BEGIN
    SELECT REF(s) INTO StockRef
      FROM Stock_objtab s
     WHERE s.StockNo = item;

    LineItemList_ntab.EXTEND;
    LineItemList_ntab(LineItemList_ntab.LAST) := 
      lineitem_objtyp(LineItemList_ntab.LAST, StockRef, qty, discount);
  END;

  MEMBER PROCEDURE update_item (lineno NUMBER, 
                                item NUMBER, 
                                qty NUMBER, 
                                discount NUMBER) IS
    StockRef REF StockItem_objtyp;
  BEGIN
    SELECT REF(s) INTO StockRef
      FROM Stock_objtab s
     WHERE s.StockNo = item;

    LineItemList_ntab(lineno).Stock_ref := StockRef;
    LineItemList_ntab(lineno).Quantity := qty;
    LineItemList_ntab(lineno).Discount := discount;
  END;

  MEMBER PROCEDURE display IS
    cust Customer_objtyp;
  BEGIN
    dbms_output.put_line('Purchase Order ' || pono);
    dbms_output.put_line(' ');
    SELECT DEREF(Cust_ref) INTO cust FROM DUAL;
    cust.display;
    IF (ShipToAddr_obj IS NOT NULL) THEN
      dbms_output.put_line('Shipping address:');
      ShipToAddr_obj.display;
    END IF;
    dbms_output.put_line(' ');
    dbms_output.put_line('Order date: ' || OrderDate);
    dbms_output.put_line('Ship date:  ' || ShipDate);
    dbms_output.put_line(' ');
    dbms_output.put_line('Line   Item  Quantity        Price   ' || 
                         'Discount     Extension');
    FOR i IN 1 .. LineItemList_ntab.COUNT LOOP
      LineItemList_ntab(i).display;
    END LOOP;
    dbms_output.put_line(' ');
    dbms_output.put_line('Total cost:                                ' ||
      to_char(sumLineItems, '$999,999,990.99'));
  END;
END;
/

CREATE OR REPLACE TYPE BODY Customer_objtyp AS
  ORDER MEMBER FUNCTION compareCustOrders (x IN Customer_objtyp)
    RETURN INTEGER IS
  BEGIN
    RETURN Custno - x.Custno;
  END;

  MEMBER PROCEDURE display IS
  BEGIN
    dbms_output.put_line(CustName);
    Address_obj.display;
    FOR i IN 1 .. PhoneList_var.COUNT LOOP
      dbms_output.put_line(PhoneList_var(i));
    END LOOP;
  END;
END;
/

CREATE OR REPLACE TYPE BODY Address_objtyp AS
  MEMBER PROCEDURE display IS
  BEGIN
    dbms_output.put_line(Street);
    dbms_output.put_line(City || ', ' || State || ' ' || Zip);
  END;
END;
/

CREATE OR REPLACE TYPE BODY LineItem_objtyp AS
  MEMBER PROCEDURE display IS
    item StockItem_objtyp;
    ext NUMBER;
    textline VARCHAR2(255);
  BEGIN
    SELECT DEREF(Stock_Ref) INTO item FROM DUAL;
    ext := Quantity * item.Price * (1 - Discount/100);

    textline := to_char(LineItemNo, '999') || '  ' ||
                to_char(item.StockNo, '0999') || '  ' ||
                to_char(Quantity, '9999999') || '  ' ||
                to_char(item.Price, '$99,999.99') || '    ' ||
                to_char(Discount, '90.99') || '%  ' ||
                to_char(ext, '$999,999.99');
    dbms_output.put_line(textline);
  END;                         
END;
/

-- Select the line item list and the Custname FOR each purchase order
SELECT DEREF(p.Cust_ref) CUSTOMER_INFO,
       p.ShipToAddr_obj DESTINATION,
       p.PONo PURCHASE_ORDER_NUMBER,
       p.OrderDate ORDER_DATE,
       CURSOR(SELECT LineItemNo,
                     Quantity, Discount
               FROM TABLE(p.LineItemList_ntab) li
               ORDER BY LineItemNo)
FROM PurchaseOrder_objtab p
WHERE p.PONo = 1001
/

-- Select the line item list and the Custname FOR each purchase order.
-- Please note this is the same query as above but unnesting feature
-- of Oracle8i has been used using TABLE() operator.
SELECT DEREF(p.Cust_ref) CUSTOMER_INFO,
       p.ShipToAddr_obj DESTINATION,
       p.PONo PURCHASE_ORDER_NUMBER,
       p.OrderDate ORDER_DATE, li.LineItemNo, li.Quantity, li.Discount
FROM PurchaseOrder_objtab p,   TABLE(p.LineItemList_ntab) li
WHERE p.PONo = 1001
ORDER BY p.PONo, li.LineItemNo
/

-- Select the Total value FOR each purchse_order
SELECT p.PONo PURCHASE_ORDER_NUMBER,
       p.sumLineItems() SUM_LINE_ITEMS
FROM PurchaseOrder_objtab p
ORDER BY p.PONo
/

-- Select the line items and the purchase numbers FOR all those poUs having
-- a StockNo 1535

SELECT po.PONo PURCHASE_ORDER_NUMBER,
       po.Cust_ref.Custno CUSTOMER_NUMBER,
       li.LineItemNo LINE_ITEM_NO,
       li.Stock_ref.StockNo STOCK_NO,
       li.Quantity QUANTITY,
       li.Discount DISCOUNT
FROM PurchaseOrder_objtab po, TABLE(po.LineItemList_ntab) li
WHERE li.Stock_ref.StockNo = 1535
ORDER BY po.PONo, li.LineItemNo
/

-- Example of how Varray column can be unnested/flattened in
-- Oracle8i using TABLE() operator.
SELECT p1.custno, p2.*
FROM Customer_objtab p1, TABLE( p1.PhoneList_var)p2
ORDER BY 1, 2
/

-- USING PL/SQL TO CALL TYPE METHODS --

-- Required for DBMS_OUTPUT
SET SERVEROUTPUT ON FORMAT WRAPPED

-- Display all customer info

DECLARE
  cref REF Customer_objtyp;
  cust Customer_objtyp;
  CURSOR cr IS SELECT REF(c) FROM Customer_objtab c ORDER BY Custname;  
BEGIN
  OPEN cr;
  LOOP
    FETCH cr INTO cref;
    EXIT WHEN cr%NOTFOUND;
    UTL_REF.SELECT_OBJECT(cref, cust);
    cust.display;
    dbms_output.put_line(' ');
  END LOOP;
  CLOSE cr;
END;
/

-- Display purchase order

CREATE OR REPLACE PROCEDURE display_order (ponum NUMBER) IS
  poref REF PurchaseOrder_objtyp;
  po PurchaseOrder_objtyp;
BEGIN
  SELECT REF(p) INTO poref FROM PurchaseOrder_objtab p WHERE p.pono = ponum;
  UTL_REF.SELECT_OBJECT(poref, po);
  po.display;
END;
/

BEGIN
  display_order(1001);
END;
/

-- Add to purchase order
-- This procedure uses the LOCK_OBJECT and UPDATE_OBJECT procedures in 
-- package UTL_REF to modify the nested table of line items

CREATE OR REPLACE PROCEDURE add_item (ponum NUMBER, stockno NUMBER,
                                      qty NUMBER, discount NUMBER) IS
  poref REF PurchaseOrder_objtyp;
  po PurchaseOrder_objtyp;
BEGIN
  SELECT REF(p) INTO poref 
    FROM PurchaseOrder_objtab p 
    WHERE p.pono = ponum;

  UTL_REF.LOCK_OBJECT(poref, po);
  po.purchase_item(stockno, qty, discount);
  UTL_REF.UPDATE_OBJECT(poref, po);
END;
/

BEGIN
  add_item(1001, 1534, 6, 20);
  display_order(1001);
END;
/

-- Change a line item
-- This procedure updates the line items list using SQL instead of UTL_REF.

CREATE OR REPLACE PROCEDURE change_item (ponum NUMBER, lineno NUMBER,
                                         stockno NUMBER, qty NUMBER, 
                                         discount NUMBER) IS
  poref REF PurchaseOrder_objtyp;
  po PurchaseOrder_objtyp;
BEGIN
  SELECT REF(p) INTO poref 
    FROM PurchaseOrder_objtab p 
    WHERE p.pono = ponum
    FOR UPDATE OF p.LineItemList_ntab;

  UTL_REF.SELECT_OBJECT(poref, po);
  po.update_item(lineno, stockno, qty, discount);

  UPDATE PurchaseOrder_objtab p 
    SET p.LineItemList_ntab = po.LineItemList_ntab 
    WHERE p.pono = ponum;
END;
/

BEGIN
  change_item(1001, 1, 1011, 7, 35);
  display_order(1001);
END;
/

-- Object Views
--      If relational schema exists WITH relational tables containing the
--  purchase order, lineitem and customer data, then objects may be
-- materialized from the relational data WITH the help of object views.

CREATE OR REPLACE VIEW Customer_objview OF Customer_objtyp
   WITH OBJECT OID(Custno)
   AS SELECT C.Custno,
             C.Custname,
             Address_objtyp(C.Street,C.City,C.State,C.Zip),
             PhoneList_vartyp(Phone1,Phone2,Phone3)
      FROM Customer_reltab C
/
CREATE OR REPLACE VIEW Stock_objview OF StockItem_objtyp
   WITH OBJECT OID(StockNo)
   AS SELECT *
      FROM Stock_reltab
/
CREATE OR REPLACE VIEW PurchaseOrder_objview OF PurchaseOrder_objtyp
   WITH OBJECT OID (PONo)
   AS SELECT P.PONo,
             MAKE_REF(Customer_objview,P.Custno),
             P.OrderDate,
             P.ShipDate,
             CAST(
               MULTISET(SELECT LineItem_objtyp(L.LineItemNo,
                                           MAKE_REF(Stock_objview,L.StockNo),
                                           L.Quantity,
                                           L.Discount)
                         FROM  LineItems_reltab L
                         WHERE L.PONo= P.PONo)
                AS LineItemList_ntabtyp),
             Address_objtyp(P.ToStreet,P.ToCity,P.ToState,P.ToZip)
      FROM PurchaseOrder_reltab P
/
-------
-- Since OBJECT views queries are complex making them inherently non-updatable,
-- Instead-OF triggers may be specified FOR them to make them updatable.
-- For example, the following creates an INSTEAD-OF TRIGGER on the
-- PurchaseOrder_tab VIEW
-- which encapsulates the semantics of inserting into that view.
CREATE OR REPLACE TRIGGER POView_instdinserttr
        INSTEAD OF INSERT on PurchaseOrder_objview
DECLARE
  LineItems_ntab   LineItemList_ntabtyp;
  i                INTEGER;
  CustVar_obj      Customer_objtyp;
  StockVar_obj     StockItem_objtyp;
  StockVarTemp_ref REF StockItem_objtyp;

BEGIN
 LineItems_ntab := :new.LineItemList_ntab;
 UTL_REF.SELECT_OBJECT(:new.Cust_ref, CustVar_obj);
 INSERT INTO PurchaseOrder_reltab
     VALUES(:new.PONo,CustVar_obj.Custno,:new.OrderDate,:new.ShipDate,
            :new.ShipToAddr_obj.Street,:new.ShipToAddr_obj.City,
            :new.ShipToAddr_obj.State,:new.ShipToAddr_obj.Zip) ;

 FOR i in 1..LineItems_ntab.count LOOP
   UTL_REF.SELECT_OBJECT(LineItems_ntab(i).Stock_ref, StockVar_obj);
   INSERT INTO LineItems_reltab
       VALUES(LineItems_ntab(i).LineItemNo,:new.PONo,StockVar_obj.StockNo,
              LineItems_ntab(i).Quantity,LineItems_ntab(i).Discount);
 END LOOP;
END;
/

CREATE OR REPLACE TRIGGER POLineItems_instdinsertr
   INSTEAD OF INSERT ON NESTED TABLE LineItemList_ntab OF PurchaseOrder_objview
DECLARE
   StockVar StockItem_objtyp;
BEGIN
   UTL_REF.SELECT_OBJECT(:NEW.Stock_ref, StockVar);
   INSERT INTO LineItems_reltab
    VALUES (:NEW.LineItemNo, :PARENT.PONo, StockVar.StockNo, :NEW.Quantity,
            :NEW.Discount);
END;
/
CREATE OR REPLACE TRIGGER POLineItems_instddeltr
   INSTEAD OF DELETE ON NESTED TABLE LineItemList_ntab OF PurchaseOrder_objview
BEGIN
   DELETE FROM LineItems_reltab
     WHERE LineItemNo = :OLD.LineItemNo AND PONo = :PARENT.PONo;
END;
/


CREATE OR REPLACE TRIGGER CustView_instdinserttr
       INSTEAD OF INSERT on Customer_objview
DECLARE
   Phones_var  PhoneList_vartyp;
   TPhone1 Customer_reltab.Phone1%TYPE := NULL;
   TPhone2 Customer_reltab.Phone2%TYPE := NULL;
   TPhone3 Customer_reltab.Phone3%TYPE := NULL;
BEGIN
    Phones_var := :new.PhoneList_var;
    IF Phones_var.COUNT > 2 then
     TPhone3 := Phones_var(3);
    END IF;
    IF Phones_var.COUNT > 1 then
     TPhone2 := Phones_var(2);
    END IF;
    IF Phones_var.COUNT > 0 then
     TPhone1 := Phones_var(1);
    END IF;
    INSERT INTO Customer_reltab
       VALUES(:new.Custno,:new.Custname,:new.Address_obj.Street,
              :new.Address_obj.City,:new.Address_obj.State,
              :new.Address_obj.Zip,
              TPhone1,TPhone2,TPhone3);
END;
/

CREATE OR REPLACE TRIGGER StockView_instdinsertr
        INSTEAD OF INSERT on Stock_objview
BEGIN
    INSERT INTO Stock_reltab
        VALUES(:new.StockNo,:new.Price,:new.TaxRate);
END;
/

INSERT INTO Customer_objview
   VALUES (13,'Ellan White',
           Address_objtyp('25, I Street','memphis','TN','05456'),
                 PhoneList_vartyp('901-000-0000'));
COMMIT
/

INSERT INTO PurchaseOrder_objview
  SELECT 3001, REF(c),SYSDATE,SYSDATE,
          CAST(MULTISET(SELECT LineItem_objtyp(41, REF(S),20,1)
                FROM Stock_objview S WHERE S.StockNo = 1535)
            AS LineItemList_ntabtyp),
          Address_objtyp('22 Nothingame Ave','Cockstown','AZ','44045')
    FROM Customer_objview c
    WHERE c.Custno = 1
/

DELETE FROM TABLE(SELECT p.LineItemList_ntab
             FROM   PurchaseOrder_objview p
             WHERE  p.PONo = 3001) L
WHERE L.LineItemNo = 41
/
INSERT INTO TABLE(SELECT p.LineItemList_ntab
             FROM   PurchaseOrder_objview p
             WHERE  p.PONo = 3001)
   SELECT  LineItem_objtyp(41, REF(S),20,1)
   FROM Stock_objview S WHERE S.StockNo = 1535
/

SELECT DEREF(p.Cust_ref) CUSTOMER_INFO,
       p.ShipToAddr_obj DESTINATION,
       p.PONo PURCHASE_ORDER_NUMBER,
       p.OrderDate ORDER_DATE,
       CURSOR(SELECT LineItemNo,
                     Quantity, Discount
               FROM TABLE(p.LineItemList_ntab) li
               ORDER BY LineItemNo)
FROM PurchaseOrder_objview p
WHERE p.PONo = 1001
/

-- Please note this is the same query as above but unnesting feature
-- of Oracle8i has been used using TABLE() operator.
SELECT DEREF(p.Cust_ref) CUSTOMER_INFO,
       p.ShipToAddr_obj DESTINATION,
       p.PONo PURCHASE_ORDER_NUMBER,
       p.OrderDate ORDER_DATE,  li.LineItemNo, li.Quantity, li.Discount
FROM PurchaseOrder_objview p, TABLE(p.LineItemList_ntab) li
WHERE p.PONo = 1001
ORDER BY li.LineItemNo
/

-- Select the TotalSELF value FOR each purchse_order
SELECT p.PONo,p.sumLineItems () FROM PurchaseOrder_objview p
ORDER BY p.PONo
/

-- SELECT the lineitems and the purchase order numbers
-- returning the linitems as a nested cursor
SELECT po.PONo  PURCHASE_ORDER_NUMBER,
       po.Cust_ref.Custno CUSTOMER_NUMBER,
       CURSOR(SELECT *
               FROM TABLE(po.LineItemList_ntab) li
               WHERE li.Stock_ref.StockNo = 1535
               ORDER BY LineItemNo)
FROM PurchaseOrder_objview po
ORDER BY PONo
/

-- Please note this is the same query as above but unnesting feature
-- of Oracle8i has been used using TABLE() operator.
SELECT po.PONo  PURCHASE_ORDER_NUMBER,
       po.Cust_ref.Custno CUSTOMER_NUMBER, li.*
FROM PurchaseOrder_objview po,  TABLE(po.LineItemList_ntab) li
WHERE li.Stock_ref.StockNo = 1535
ORDER BY po.PONo, li.LineItemNo;

-- Select the line items and the purchase numbers FOR all those poUs having
-- a StockNo 1535

SELECT po.PONo  PURCHASE_ORDER_NUMBER,
       po.Cust_ref.Custno CUSTOMER_NUMBER,
       li.LineItemNo LINEITEM_NUMBER,
       li.Quantity QUANTITY
FROM   PurchaseOrder_objview po, TABLE(po.LineItemList_ntab) li
WHERE  li.Stock_ref.StockNo = 1535
ORDER BY po.PONo, li.LineItemNo
/

-- Select the Total value FOR each purchse_order
SELECT p.PONo PURCHASE_ORDER_NUMBER,
       p.sumLineItems() SUM_LINE_ITEMS
FROM PurchaseOrder_objtab p
ORDER BY PONo
/

COMMIT
/

CONNECT system/manager
DROP USER po CASCADE
/

/* demonstrate creating object views using MAKE_REF and also self-referencing
   view */

CONNECT system/manager
DROP USER fnd CASCADE;

GRANT CONNECT, RESOURCE TO fnd IDENTIFIED BY fnd;

CONNECT fnd/fnd

CREATE TABLE fnd_application (
 application_id                  NUMBER PRIMARY KEY,
 application_short_name          VARCHAR2(50) NOT NULL,
 application_name                VARCHAR2(240)
);

CREATE TABLE fnd_form_functions(
 function_id                     NUMBER PRIMARY KEY,
 function_name                   VARCHAR2(30) NOT NULL,
 application_id                  NUMBER,
 form_id                         NUMBER,
 parameters                      VARCHAR2(2000)
);

CREATE TABLE fnd_menus (
 menu_id                         NUMBER PRIMARY KEY,
 menu_name                       VARCHAR2(30) NOT NULL
);

CREATE TABLE fnd_form_vl (
 application_id                  NUMBER PRIMARY KEY,
 form_id                         NUMBER NOT NULL,
 form_name                       VARCHAR2(30) NOT NULL,
 AUDIT_ENABLED_FLAG              VARCHAR2(1) NOT NULL,
 user_form_name                  VARCHAR2(80) NOT NULL,
 description                     VARCHAR2(240)
);

CREATE TABLE fnd_form_functions_vl (
 function_id                     NUMBER  PRIMARY KEY,
 function_name                   VARCHAR2(30) NOT NULL,
 application_id                  NUMBER,
 form_id                         NUMBER,
 parameters                      VARCHAR2(2000),
 type                            VARCHAR2(30),
 user_function_name              VARCHAR2(80) NOT NULL,
 description                     VARCHAR2(240)
);


CREATE TABLE fnd_menu_entries_vl (
 menu_id                         NUMBER PRIMARY KEY,
 entry_sequence                  NUMBER NOT NULL,
 sub_menu_id                     NUMBER,
 function_id                     NUMBER,
 prompt                          VARCHAR2(30),
 description                     VARCHAR2(240)
);


CREATE TABLE fnd_menus_vl (
 menu_id                         NUMBER PRIMARY KEY,
 menu_name                       VARCHAR2(30) NOT NULL,
 user_menu_name                  VARCHAR2(80) NOT NULL,
 description                     VARCHAR2(240)
);


CREATE TYPE fnd_application_t AS OBJECT
(
  application_short_name varchar2(50),
  application_name varchar2(240)
)
/

CREATE OR REPLACE VIEW
  fnd_application_object_view OF fnd_application_t
  WITH OBJECT OID(application_short_name) AS
  SELECT application_short_name, application_name
  FROM fnd_application;

CREATE TYPE fnd_form_t AS OBJECT
(
  form_name VARCHAR2(30),
  application REF fnd_application_t,
  user_form_name varchar2(80),
  description varchar2(240)
)
/

CREATE OR REPLACE VIEW
  fnd_form_view OF fnd_form_t
  WITH OBJECT OID(form_name) AS
  SELECT F.form_name,
    MAKE_REF(fnd_application_object_view, A.application_short_name),
    F.user_form_name, F.description
  FROM fnd_form_vl F, fnd_application A
  WHERE F.application_id = A.application_id;

CREATE TYPE fnd_function_t AS OBJECT
(
  function_name varchar2(30),
  form REF fnd_form_t,
  type varchar2(30),
  parameters varchar2(2000),
  user_function_name varchar2(80),
  description varchar2(240)
)
/

CREATE OR REPLACE VIEW
  fnd_function_object_view OF fnd_function_t
  WITH OBJECT OID(function_name) AS
  SELECT N.function_name,
    MAKE_REF(fnd_form_view, F.form_name),
    N.type, N.parameters, N.user_function_name, N.description
  FROM fnd_form_functions_vl N, fnd_form_vl F
  WHERE N.form_id = F.form_id;

CREATE TYPE fnd_menu_t
/

CREATE TYPE fnd_menu_entry_t AS OBJECT
(
  entry_sequence number,
  prompt varchar2(30),
  description varchar2(240),
  submenu REF fnd_menu_t,
  function REF fnd_function_t
)
/

CREATE TYPE fnd_menu_entry_list_t AS TABLE OF fnd_menu_entry_t
/

CREATE TYPE fnd_menu_t AS OBJECT
(
  menu_name varchar2(30),
  user_menu_name varchar2(80),
  description varchar2(240),
  entries fnd_menu_entry_list_t
)
/

/* create self-reference view */
CREATE OR REPLACE FORCE VIEW
  fnd_menu_object_view OF fnd_menu_t
  WITH OBJECT OID(menu_name) AS
  SELECT M.menu_name, M.user_menu_name, M.description,
        CAST(
          MULTISET(
            SELECT fnd_menu_entry_t(
                Q.entry_sequence, Q.prompt, Q.description,
                MAKE_REF(fnd_menu_object_view, S.menu_name), NULL)
            FROM fnd_menu_entries_vl Q, fnd_menus S
            WHERE Q.menu_id = M.menu_id AND Q.sub_menu_id = S.menu_id
            )
          AS fnd_menu_entry_list_t)
  FROM fnd_menus_vl M;

CONNECT system/manager;
DROP USER fnd CASCADE
/
SET ECHO OFF
