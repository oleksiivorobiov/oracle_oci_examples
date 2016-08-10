Rem
Rem $Header: xmltype1.sql 04-may-2001.14:29:48 rbooredd Exp $
Rem
Rem xmltype1.sql
Rem
Rem  Copyright (c) Oracle Corporation 2001. All Rights Reserved.
Rem
Rem    NAME
Rem      xmltype1.sql - XMLType demo 1
Rem
Rem    DESCRIPTION
Rem      This demo shows how to use XMLType to store and access
Rem      xml documents inside database
Rem
Rem    NOTES
Rem      
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    rbooredd    04/29/01 - Merged rbooredd_xml_demo1
Rem    rbooredd    04/29/01 - 
Rem    rbooredd    04/27/01 - Created
Rem

SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100
SET ECHO ON

connect system/manager
grant connect,resource,query rewrite  to xmltype1 identified by xmltype1;
connect xmltype1/xmltype1;

-- create xml table to store Purchase Order documents
CREATE TABLE po_tab(
  po   sys.XMLType
) XMLType COLUMN po
  STORE AS CLOB (
    TABLESPACE system
    STORAGE(INITIAL 4K NEXT 8K)
    CHUNK 4096 NOCACHE LOGGING
  );

------------------------------------
-- DML on XMLType column
------------------------------------

-- insert PurchaseOrder into po_tab table
-- with createXML constructor
insert into po_tab
values(sys.XMLType.createXML(
'<?xml version="1.0"?>
<PO pono="1">
   <PNAME>Po_1</PNAME>
   <CUSTNAME>John</CUSTNAME>
   <SHIPADDR>
      <STREET>1033, Main Street</STREET>
      <CITY>Sunnyvalue</CITY>
      <STATE>CA</STATE>
   </SHIPADDR>
</PO>')
);

insert into po_tab
values(sys.XMLType.createXML(
'<?xml version="1.0"?>
<PO pono="2">
   <PNAME>Po_2</PNAME>
   <CUSTNAME>Nance</CUSTNAME>
   <SHIPADDR>
      <STREET>1033, Main Street</STREET>
      <CITY>Sunnyvalue</CITY>
      <STATE>CA</STATE>
   </SHIPADDR>
</PO>')
);


-- Updating Purchase Order 2 with new shipping address
update po_tab e
set  e.po=sys.XMLType.createXML(
'<?xml version="1.0"?>
<PO pono="2">
   <PNAME>Po_2</PNAME>
   <CUSTNAME>Nance</CUSTNAME>
   <SHIPADDR>
      <STREET>2 Avocet Drive</STREET>
      <CITY>Redwood Shores</CITY>
      <STATE>CA</STATE>
   </SHIPADDR>
</PO>')
where e.po.extract('/PO/@pono').getNumberVal()=2;


-- deleting Purchase Order 2
delete from po_tab e
where e.po.extract('/PO/PNAME/text()').getStringVal()='Po_2';

commit;

------------------------------------
-- Queries on XMLType column
------------------------------------

-- selecting XMLType document as CLOB
set long 2000
select e.po.getClobval()
from po_tab e;

-- extract Purchase Order NAME where pono attribue exists and equal to 1
-- and customer name is like '%John%'

select e.po.extract('/PO/PNAME/text()').getStringVal() PNAME
from po_tab e
where e.po.existsNode('/PO/@pono') = 1 and
      e.po.extract('/PO/@pono').getNumberVal() = 1 and
      e.po.extract('/PO/CUSTNAME/text()').getStringVal() like '%John%';

-- check if /PO/SHIPADDR/STATE node is a xml fragment
select e.po.extract('/PO/SHIPADDR/STATE').isFragment()
from po_tab e;

-- use Extract(), ExitsNode() XML Operators to query XML document
select extract(e.po,'/PO/PNAME/text()').getStringVal() PNAME
from po_tab e
where existsNode(e.po,'/PO/SHIPADDR') = 1 and
      extract(e.po,'/PO/@pono').getNumberVal() = 1 and
      extract(e.po,'/PO/CUSTNAME/text()').getStringVal() like '%John%';

---------------------------------------
-- Functional Indexes on XMLType column
---------------------------------------

-- create a unique functional index on pono to enforce
-- unique constraint on pono attribute
create unique index pono_fidx on po_tab(sys.xmltype.getNumberVal(
           sys.xmltype.extract(po,'/PO/@pono')));

-- insert fails due to unique constraint violation!
insert into po_tab
values(sys.XMLType.createXML(
'<?xml version="1.0"?>
<PO pono="1">
   <PNAME>Po_1</PNAME>
   <CUSTNAME>John</CUSTNAME>
   <SHIPADDR>
      <STREET>1033, Main Street</STREET>
      <CITY>Sunnyvalue</CITY>
      <STATE>CA</STATE>
   </SHIPADDR>
</PO>')
);


-- create a functional index on customer name
create index cname_fidx on po_tab(substr(sys.xmltype.getStringVal(
           sys.xmltype.extract(po,'/PO/CUSTNAME/text()')),1,200));

-- create a functional index for existsNode() of SHIPADDR
create index sadd_fidx on po_tab(po.existsNode('/PO/SHIPADDR'));


-- use functional indexes to improve query performance
alter session set query_rewrite_enabled=true;
alter session set query_rewrite_integrity=trusted;

select /*+ index(e pono_fidx) */ e.po.getClobVal()
from po_tab e
where e.po.extract('/PO/@pono').getNumberVal() = 1;

-- cleanup
connect system/manager
drop user xmltype1 cascade;
