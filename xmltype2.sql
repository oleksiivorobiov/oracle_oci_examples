Rem
Rem $Header: xmltype2.sql 04-may-2001.12:22:18 rbooredd Exp $
Rem
Rem xmltype2.sql
Rem
Rem  Copyright (c) Oracle Corporation 2001. All Rights Reserved.
Rem
Rem    NAME
Rem      xmltype2.sql - XMLType demo 2
Rem
Rem    DESCRIPTION
Rem      This demo shows how to use XMLType inside PL/SQL
Rem
Rem    NOTES
Rem      
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    rbooredd    04/29/01 - Merged rbooredd_xml_demo1
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
grant connect,resource  to xmltype2 identified by xmltype2;
connect xmltype2/xmltype2;


--------------------------------------------------------------------------------
-- Exploding the Purchase Order xml documnet to Store in a Relational Table
-- using XMLType methods and Table Function (pipelined)
--------------------------------------------------------------------------------
create type poRow_type as object
(
  poname varchar2(20),
  postreet varchar2(20),
  pocity varchar2(20),
  postate char(2),
  pozip char(10)
);
/

create type poRow_list as TABLE of poRow_type;
/

create or replace function poExplode_func (poList IN sys.XMLType) return
poRow_list
pipelined is
  out_rec poRow_type := poRow_type(null,null,null,null,null);
  poxml sys.XMLType;
  i binary_integer := 1;
begin

  loop

   -- extract the i'th purchase order!
   poxml := poList.extract('//PO['||i||']');

   exit when poxml is null;

   -- extract the required attributes..!!!
   out_rec.poname := 
            poxml.extract('/PO/PONAME/text()').getStringVal();
   out_rec.postreet :=
            poxml.extract('/PO/POADDR/STREET/text()').getStringVal();
   out_rec.pocity :=
            poxml.extract('/PO/POADDR/CITY/text()').getStringVal();
   out_rec.postate :=
            poxml.extract('/PO/POADDR/STATE/text()').getStringVal();
   out_rec.pozip :=
            poxml.extract('/PO/POADDR/ZIP/text()').getStringVal();

   -- output the row
   PIPE ROW(out_rec);
   i := i+1;
  end loop;
  return;

end;
/

-- generate poRow_type rowset from xml documnet
select *
   from TABLE( CAST(
      poExplode_func(
       sys.XMLType.createXML(
         '<?xml version="1.0"?>
          <POLIST>
           <PO>
            <PONAME>Po_1</PONAME>
            <POADDR>
              <STREET>100 Main Street</STREET>
              <CITY>Sunnyvale</CITY>
              <STATE>CA</STATE>
              <ZIP>94086</ZIP>
            </POADDR>
           </PO>
           <PO>
            <PONAME>Po_2</PONAME>
            <POADDR>
              <STREET>200 First Street</STREET>
              <CITY>Oaksdale</CITY>
              <STATE>CA</STATE>
              <ZIP>95043</ZIP>
            </POADDR>
           </PO>
          </POLIST>')
   ) AS poRow_list));

-- cleanup
connect system/manager
drop user xmltype2 cascade;
