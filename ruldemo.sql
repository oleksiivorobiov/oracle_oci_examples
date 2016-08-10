Rem
Rem $Header: ruldemo.sql 14-dec-2006.09:36:47 ayalaman Exp $
Rem
Rem ruldemo.sql
Rem
Rem Copyright (c) 2004, 2006, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      ruldemo.sql - Rules Manager demo script
Rem
Rem    DESCRIPTION
Rem      This script demonstrates the use of Rules Manager in 
Rem      a few configurations. See the Application Developer's
Rem      Guide - Rules Manager and Expression Filter for 
Rem      additional information. 
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    ayalaman    12/14/06 - add demo for collection events
Rem    ayalaman    11/15/04 - XPath expression example and rule descriptions 
Rem    ayalaman    11/02/04 - replace homeland security with law enforcement
Rem    ayalaman    10/07/04 - ayalaman_bug-3922526
Rem    ayalaman    09/29/04 - Created
Rem

SET ECHO ON
SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100

connect / as sysdba;
set serveroutput on;
drop user ruldemo cascade;
create user ruldemo identified by ruldemo;
grant connect, resource to ruldemo;
grant execute on dbms_lock to ruldemo;
grant create view to ruldemo;

connect ruldemo/ruldemo;
set echo on;
set pagesize 150;
column mesg format a78;
column attime format a30;
column rlm$rulecond format a95;
column rlm$ruleid format a5;

/*****************************************************************
  The tests in this file are organized as follows: 
   I. Application demonstrating the various steps in a Rules 
      Manager application. 
  II. Application demonstrating the use of Table aliases and 
      DML (Insert) Events for rule management. 
 III. Application demonstrating the use of ADD_EVENT API and 
      Rule class results view
  IV. Application demonstrating the use of collection events 
******************************************************************/
set linesize 110;
---
--- I. Application demostrating the various steps in a Rules 
---    Manager application. 
---
--- The following script uses Law Enforcement application to demonstrate
--- the use of Rules Manager. In this application, rules are defined to 
--- raise security alerts, place a person on the watch list, etc based 
--- on certain criteria. For this purpose, some real-world events such as
--- Bank Transactions, Transportation and Field Reports are used 
--- to describe the criteria. 
---
create table messagequeue (attime timestamp, mesg varchar2(4000));

---
--- 1. Create the basic types that represent the primitive event
---    structures
---
create or replace type BankTransaction as object 
  (subjectId NUMBER,         --- refer to entity such as personnel
                             --- could be SSN
   tranType  VARCHAR2(30),   --- DEPOSIT / TRANSFER / WITHDRAW
   amount    NUMBER,         ---
   fundFrom  VARCHAR2(30));  --- location from which it is transfered
/

create or replace type Transportation as object 
  (subjectId NUMBER, 
   vesselType VARCHAR2(30),  --- TRUCK / CAR / PLANE / TRAIN 
   locFrom   VARCHAR2(30),   --- starting location 
   locTo     VARCHAR2(30),   --- ending location 
   startDate DATE,           --- start date 
   endDate   DATE);          --- end date of
/

create or replace type FieldReport as object 
  (subjectId NUMBER, 
   rptType   VARCHAR2(30),   --- Tel call / Meeting / Bg Check
   whoWith   NUMBER,         --- identifier of the person the subject 
                             --- is in touch with 
   rptOrg    VARCHAR2(30),   --- Organization reporting it 
   rptReg    VARCHAR2(30),   --- Region 
   rptBody   sys.XMLType);   --- the actual report
/

---
--- 2. Create a composite event type that consists of these basic types. 
---
create or replace type LawEnforcement as object 
  (bank      BankTransaction,
   transport Transportation,
   fldrpt    FieldReport);
/

---
--- 3. Create a repository for the rules defined on the composite event 
---    structure
---
BEGIN
   DBMS_RLMGR.CREATE_RULE_CLASS(
     rule_class      => 'LawEnforcementRC', 
     event_struct    => 'LawEnforcement', 
     action_cbk      => 'LawEnforcementCBK',
     actprf_spec     => 'actionType VARCHAR2(40), actionParam VARCHAR2(100)',
     rslt_viewnm     => 'MatchedCriteria',
     rlcls_prop      => '<composite
          equal="bank.subjectId, transport.subjectId, fldrpt.subjectId"
          ordering="rlm$rule.rlm$ruleid, bank.subjectId, transport.subjectId"/>');
END;
/

--- The above steps create a relational table, LawEnforcementRC, which acts 
--- as the repository for the rules in this application. This table has 
--- a set of pre-defined columns to store the Rule identifiers, Rule
--- conditions and the descriptions. In addition to these columns, this
--- rule class table is defined with two columns, actionType and 
--- actionParam, as specified through the actpref_spec argument. These 
--- columns capture the type of action that should be carried for each 
--- rule. 
desc LawEnforcementRC;

--- The above steps also creates the skeleton for an action callback
--- procedure with the specified name. 

select text from user_source where name = 'LAWENFORCEMENTCBK' order by line;

---
--- 4. Implement the callback procedure to perform appropriate action
---    for each matching rule, based on the event instances that 
---    matched the rule and the action preferences associated with 
---    the rule. 
---
CREATE OR REPLACE PROCEDURE LAWENFORCEMENTCBK (
   bank                 banktransaction, 
   transport    	transportation,
   fldrpt 		fieldreport,
   rlm$rule		LawEnforcementRC%ROWTYPE) IS
   mesg                 VARCHAR2(4000);
   msgl                 VARCHAR2(100);
begin
  msgl := 'Rule '||rlm$rule.rlm$ruleid||' matched following primitive events';
  dbms_output.put_line(msgl);
  mesg := msgl||chr(10);
  if (bank is not null) then 
   msgl := '->Bank Transaction by subject ('||bank.subjectId||') of type ['||bank.tranType||']';
   dbms_output.put_line(msgl);
   mesg := mesg||msgl||chr(10);
  end if;
  if (transport is not null) then 
   msgl := 
   '->Transportation by subject('||transport.subjectId||') use vessel ['||transport.vesselType||']';
   dbms_output.put_line(msgl);
   mesg := mesg||msgl||chr(10);
  end if;
  if (fldrpt is not null) then 
   msgl := 
   '->Field report refer to('||fldrpt.subjectId||' and '||fldrpt.whowith||')';
   dbms_output.put_line(msgl);
   mesg := mesg||msgl||chr(10);
  end if;

  msgl := '=>Recommended Action : Action Type ['||rlm$rule.actionType||
                       '] Action Parameter ['||rlm$rule.actionParam||']';

  dbms_output.put_line(msgl||chr(10));
  mesg := mesg||msgl||chr(10);
  insert into messagequeue values (systimestamp, mesg);
end;
/

show errors;

---
--- 5. The rules defined in the rule class can make use of user-defined
---    functions in the database schema. The following commands create 
---    some dummy functions that are later used in the rule conditions. 
---

--
-- For the value of the region passed in, query the restricted 
-- areas table and return 1 if the current region is a restricted
-- area. 
CREATE OR REPLACE FUNCTION IsRestrictedArea(region VARCHAR2)
                                            RETURN NUMBER IS
BEGIN
   -- User can expand this function and implement a logic that 
   -- relies on other relational tables. 
   RETURN 1;
END;
/

--
-- Check if the subject chosen is on the watch list
--
CREATE OR REPLACE FUNCTION OnWatchList(subject NUMBER)
                                       RETURN NUMBER IS
BEGIN
   -- User can expand this function and implement a logic that 
   -- relies on other relational tables. 
   RETURN 1;
END;
/

--
-- Verify if two parties are associates. Return 1 is the two 
-- subjects passed in are associates according to the registry. 
--
CREATE OR REPLACE FUNCTION AreAssociates(subjectA NUMBER,
                                         subjectB NUMBER) 
                                         RETURN NUMBER IS
BEGIN
   -- User can expand this function and implement a logic that 
   -- relies on other relational tables. 
   RETURN 1;
END;
/

-- Add all three user-defined functions to composite event
-- "LawEnforcement", so that these functions can be used 
-- in the corresponding rule conditions. 
EXEC DBMS_RLMGR.ADD_FUNCTIONS('LawEnforcement', 'OnWatchList');
EXEC DBMS_RLMGR.ADD_FUNCTIONS('LawEnforcement', 'IsRestrictedArea');
EXEC DBMS_RLMGR.ADD_FUNCTIONS('LawEnforcement', 'AreAssociates');

---
--- 6. Define the rules that suggest some actions 
---

--- Rule : Add a person to the NYPD watch list if he recieves a money 
--- transfer for more than 10000 dollars and if he rents a truck,
--- one-way to one of the restricted areas. Note that the join 
--- predicate is specified at the rule class elevel. ---
INSERT INTO LawEnforcementRC (rlm$ruleid, actionType, actionParam, rlm$rulecond, 
                              rlm$ruledesc)
VALUES ('1', 'ADD2WATCHLIST','NYPD',
        '<condition>
          <and>
           <object name="bank">
             tranType = ''TRANSFER'' AND amount > 10000 AND fundFrom != ''usa''
           </object>
           <object name="transport">
             vesselType = ''TRUCK'' AND locFrom != locTo AND IsRestrictedArea(locTo)=1
           </object>
         </and>
        </condition>',
'Add a person to the NYPD watch list if he recieves a money transfer
for more than 10000 dollars and if he rents a truck one-way to one
of the restricted areas');

--- 
--- Rule : Add a person to the NYPD watch list if 2 out of the following 3 
--- conditions are met : 
---    * The person get a money transfer for over 10000 dollars from outside USA
---    * He rented a truck, one-way, into one of the restricted areas and
---    * He had a phone conversation with a person alreadt on the watch list. 
---
--- The following rule demonstrates the use of ANY where a rule condition is 
--- considered true is m out of n events are detected. ---
INSERT INTO LawEnforcementRC (rlm$ruleid, actionType, actionParam, rlm$rulecond,
                              rlm$ruledesc)
VALUES ('2', 'ADD2WATCHLIST','NYPD',
        '<condition>
           <any count="2">
             <object name="bank">
                tranType = ''TRANSFER'' AND amount > 10000 AND fundFrom != ''usa''
             </object>
             <object name="transport">
                vesselType = ''TRUCK'' AND locFrom != locTo AND IsRestrictedArea(locTo)=1
             </object>
             <object name="fldrpt">
               rptType = ''TELCALL'' AND OnWatchList(whoWith) = 1
             </object>
           </any>
        </condition>',
'Add a person to the NYPD watch list if 2 out of the 3 specified
conditions are met');

---
--- Rule : Start a background check on a person if he receives a large 
--- sum of money from outside USA, he rents a truck one-way into one of 
--- the restricted areas and there is no field report with his background
--- information. 
---
--- The following rule demonstrates the use of negation where a rule condition
--- is considered true if some of the specified events are detected and the 
--- other events are not detected ---
INSERT INTO LawEnforcementRC (rlm$ruleid, actionType, actionParam, rlm$rulecond,
                              rlm$ruledesc)
VALUES ('3','STARTBACKGROUNDCHECK','RENTAL_DESTINATION',
        '<condition>
           <and>
            <object name="bank">
              tranType = ''TRANSFER'' AND amount > 10000 AND fundFrom != ''USA''
            </object>
            <object name="transport">
              vesselType=''TRUCK'' AND locFrom != locTo AND IsRestrictedArea(locTo)=1
            </object>
            <not>
              <object name="fldrpt"> 
                rptType = ''BG Check''
              </object>
            </not>
         </and>
       </condition>',
'Start a background check on a person if he receives a large
sum of money from outside USA, he rents a truck one-way into
one of restricted areas and there is no field report with his
background information');

--- Rule : If a subject received over 10000 dollars from outside US,  
--- he rented a truck for one direction into a restricted area and a
--- field report saying that the subject was never arrested before was
--- not submitted within "certain" (0.001 fraction of a day; could be
--- days. But seconds are used to demonstrate the use of deadline) 
--- period, add the destination of the truck to high-risk areas ---
--- 
--- This rule demonstrates the use of Negation with a deadline. 
---
INSERT INTO LawEnforcementRC (rlm$ruleid, actionType, actionParam, rlm$rulecond,
                              rlm$ruledesc)
VALUES ('4','ADD2HIGH_RISK_AREA','RENTAL_DESTINATION',
        '<condition>
           <and>
            <object name="bank">
              tranType = ''TRANSFER'' AND amount > 10000 AND fundFrom != ''usa''
            </object>
            <object name="transport">
                vesselType = ''TRUCK'' AND locFrom != locTo AND IsRestrictedArea(locTo)=1
            </object>
            <not by="systimestamp+0.001">
             <object name="fldrpt">
               rptType = ''BACKGROUNDCHECK'' and 
                 extract(rptBody, ''/history/arrests[@number=0]'') is not null
             </object>
            </not>
          </and>
        </condition>',
'If a subject received over 10000 dollars from outside US,  
he rented a truck for one direction into a restricted area and a
field report saying that the subject was never arrested before was
not submitted within "certain" period, add the destination of the
truck to high-risk areas');

commit;

--- Browse the rules ---
select rlm$ruleid, rlm$rulecond from LawEnforcementRC order by 1;

---
--- 7. Process the rules for the primitive events 
---
set serveroutput on size 10000;

BEGIN 
  dbms_rlmgr.process_rules (
     rule_class  => 'LawEnforcementRC',
     event_inst  =>
       sys.anydata.convertobject(
         fieldreport(123302122, 'TELCALL',123302123, 'NSA', 'NE', null)));
END;
/

--- The following event partially matches some of the rules defined above ---
BEGIN
   dbms_rlmgr.process_rules (
     rule_class  => 'LawEnforcementRC',
     event_inst  =>
         sys.anydata.convertobject(
           banktransaction(123302122, 'TRANSFER', 100000, 'USSR')));
END;
/

--- This event in combination with the other event evaluates some of the 
--- rules to true and thus calls the action call-back procedure. 
BEGIN
   dbms_rlmgr.process_rules (
     rule_class  => 'LawEnforcementRC',
     event_inst  =>
       sys.anydata.convertobject(
         transportation(123302122, 'TRUCK', 'WIS', 'MD', 
                        sysdate, sysdate + 7)));
END;
/

select mesg from messagequeue order by attime;

truncate table messagequeue;

--- Let us sleep past the deadline for rule 5. The scheduler process picks up
--- this rule and executes its action. The result is a new message in the 
--- message queue. 
exec dbms_lock.sleep(180); 

--- The action is executed for the rule 5 after the deadline time is elapsed. 
select mesg from messagequeue;

exec dbms_rlmgr.drop_rule_class('LawEnforcementRC');
exec dbms_rlmgr.drop_event_struct('LawEnforcement');

drop type LawEnforcement;

exec dbms_rlmgr.drop_event_struct('BankTransaction');
drop type BankTransaction;

exec dbms_rlmgr.drop_event_struct('Transportation');
drop type Transportation;

exec dbms_rlmgr.drop_event_struct('FieldReport');
drop type fieldreport;

/*************************************************************************/
---
---  II. Application demonstrating the use of Table aliases and 
---      DML (Insert) Events for rule management. 
---
---  Order Management application to demonstrate the use of Table
---  Aliases and DMLEVENTS with rules. 
--
column attime format a30;
column mesg format a50;
set echo on;
set pagesize 150;
set linesize 150; 

---
--- The following script uses Order Management application to demonstrate 
--- the use of Rules Manager for the (event) data that is stored in 
--- Relational tables. 
---
---
--- Let us consider 3 relational tables that store the information 
--- about the Purchase Orders, Shipping information, and Payment 
--- information. 
--- 
create table PurchaseOrders 
  (orderId       NUMBER, 
   custId        NUMBER, 
   itemId        NUMBER,
   itemType      VARCHAR2(30), 
   quantity      NUMBER,
   shipBy        DATE); 

create table ShipmentInfo
  (orderId       NUMBER, 
   destState     VARCHAR2(2), 
   address       VARCHAR2(50),
   shipTime      DATE,
   shipType      VARCHAR2(10));

create table PaymentInfo
  (orderId       NUMBER,
   payType       VARCHAR2(10),  -- Credit Card / Check --
   amountPaid    NUMBER,
   pymtTime      DATE,
   billState     VARCHAR2(2));
   
---
--- 1. Create the event structure. 
---    The event structures that refer to existing table using 
---    table alias constructs cannot be created from object types. 
---    Instead, such event structures should be modeled as 
---    Expression Filter attribute sets.  
---
begin
  DBMS_RLMGR.CREATE_EVENT_STRUCT (event_struct => 'OrderMgmt');
 
  DBMS_RLMGR.ADD_ELEMENTARY_ATTRIBUTE(
            event_struct => 'OrderMgmt', 
            attr_name    => 'po',
            tab_alias    => RLM$TABLE_ALIAS('PurchaseOrders'));

  DBMS_RLMGR.ADD_ELEMENTARY_ATTRIBUTE(
            event_struct => 'OrderMgmt', 
            attr_name    => 'si',
            tab_alias    => RLM$TABLE_ALIAS('ShipmentInfo'));

  DBMS_RLMGR.ADD_ELEMENTARY_ATTRIBUTE(
            event_struct => 'OrderMgmt', 
            attr_name    => 'py',
            tab_alias    => RLM$TABLE_ALIAS('PaymentInfo'));
end;
/

---
--- 2. Create the rule class (repository for rules) for the 
---    OrderMgmt composite event. Also specify the DMLEVENTS property 
---    to process the rules for each Inserted row into the (event) data
---    tabled.  
--- 
BEGIN
  DBMS_RLMGR.CREATE_RULE_CLASS (
     rule_class      => 'OrderMgmtRC', 
     event_struct    => 'OrderMgmt', 
     action_cbk      => 'OrderMgmtCBK',
     actprf_spec     => 'actionType VARCHAR2(40), actionParam VARCHAR2(100)',
     rslt_viewnm     => 'MatchingOrders',
     rlcls_prop      => '<composite
          equal="po.orderId, si.orderId, py.orderId" 
          ordering="rlm$rule.rlm$ruleid, po.orderId"
          dmlevents="I"/>');
END;
/

desc OrderMgmtCBK;

---
--- 3. Implement the action callback procedure to 
---
CREATE OR REPLACE PROCEDURE OrderMgmtCBK (
   po        ROWID, -- rowid from the PurchaseOrders table
   si        ROWID, -- rowid from the ShipmentInfo table
   py        ROWID, -- rowid from the PaymentInfo table
   rlm$rule  OrderMgmtRC%ROWTYPE) IS 
  ordId      NUMBER;
  msg        VARCHAR2(2000);
begin
  -- the rowid arguments represent the primitive events that are 
  -- rows inserted into the corresponding tables. Use the rowids
  -- to fetch necessary values. 
  if (po is not null) then 
    select orderId into ordId from PurchaseOrders where rowid = po;
  elsif (si is not null) then 
    select orderId into ordId from ShipmentInfo where rowid = si;
  elsif (py is not null) then 
    select orderId into ordId from PaymentInfo where rowid = py;
  end if;
 
  msg := 'Order number: '||ordId||' Matched rule: '
          ||rlm$rule.rlm$ruleid||chr(10)||
          '-> Recommended Action : '||chr(10)||
          '      Action Type ['||rlm$rule.actionType||
          ']'||chr(10)||'      Action Parameter ['||
          rlm$rule.actionParam||']';

  dbms_output.put_line (msg||chr(10));
end;
/

---
--- 4. Add User-Defined functions that may be useful in rule
---    conditions. 
--- 
create or replace function getCustType(custId number) 
  return VARCHAR2 is 
begin
  -- the actual function implementation can rely on other 
  -- relational tables to derive the customer type information
  return 'GOLD'; 
end;
/

exec DBMS_RLMGR.ADD_FUNCTIONS('OrderMgmt','getCustType');

---
--- 4. Add some rules 
---
--- Rule : If the order is for more than 100 routers and the 
---        payment is received as a check, contact the customer 
---        to update on the status of the order. 
--- Note that the join predicate across event types is specified
--- at the rule class level 
---
INSERT INTO OrderMgmtRC (rlm$ruleid, actionType, actionParam, rlm$rulecond)
VALUES (1, 'CALL_CUSTOMER','UPDATE_ORDER_STATUS',
     '<condition> 
        <and>
          <object name="po">
             itemType = ''ROUTER'' and quantity > 100
          </object>
          <object name="py">
             payType = ''CHECK''
          </object>
        </and>
      </condition>');

--- Rule : If the order is placed by a GOLD customer, 
---        and the items are shipped before receiving a payment, 
---        adjust the customer's credit. 
INSERT INTO OrderMgmtRC (rlm$ruleid, actionType, actionParam, rlm$rulecond)
VALUES (2, 'UPDATE_CUST_PROFILE', 'DECR_AVAILABLE_CREDIT',
  '<condition>
     <and>
       <object name="po"> getCustType(custid) = ''GOLD'' </object>
       <object name="si"/>
       <not>
         <object name="py"/>
       </not>
     </and>
   </condition>');
       

--- Rule :  If the order is place by a Gold customer and the item 
---         item is shipped within 1 day prior to the shipby date, 
---         increment the Quality of service statistics.
---
INSERT INTO OrderMgmtRC (rlm$ruleid, actionType, actionParam, rlm$rulecond)
VALUES (3, 'UPDATE_STATISTICS', 'INCREMENT QOS',
  '<condition>
     <and join="po.shipby > si.shiptime-1">
       <object name="po"> getCustType(custid) = ''GOLD'' </object>
       <object name="si"/>
     </and>
    </condition>');
       
commit;

set serveroutput on;

insert into PurchaseOrders (orderId, custId, itemId, itemType,
                            quantity, shipBy) values
(1, 123, 234, 'ROUTER', 120, '01-OCT-2004');

insert into ShipmentInfo (orderId, deststate, address, shipTime,
                            shipType) values
(1, 'CA','1 Main street, San Jose','29-SEP-2004','1 Day Air');

insert into PaymentInfo (orderId, paytype, amountpaid, pymttime,
                         billstate) values
(1, 'CHECK', 100000, '30-SEP-2004', 'CA');

commit;

---
--- III. Application demonstrating the use of ADD_EVENT API and 
---      Rule class results view
---
--- Now let us consider a similar application without the use of 
--- DMLEVENTS. This implies that the user explicitly invokes the
--- rules manager APIs to process the rules for some data stored
--- in relational tables. 
---
--- We can share the same event structure for another rule class. 
---
BEGIN
  DBMS_RLMGR.CREATE_RULE_CLASS (
     rule_class      => 'OrderMgmtRC2', 
     event_struct    => 'OrderMgmt', 
     action_cbk      => 'OrderMgmtCBK2',
     actprf_spec     => 'actionType VARCHAR2(40), actionParam VARCHAR2(100)',
     rslt_viewnm     => 'MatchingOrders2',
     rlcls_prop      => '<composite
          equal="po.orderId, si.orderId, py.orderId"/>');
END;
/

--- Implement the action callback procedure --
CREATE OR REPLACE PROCEDURE OrderMgmtCBK2 (
   po        ROWID, -- rowid from the PurchaseOrders table
   si        ROWID, -- rowid from the ShipmentInfo table
   py        ROWID, -- rowid from the PaymentInfo table
   rlm$rule  OrderMgmtRC2%ROWTYPE) IS 
  ordId      NUMBER;
  msg        VARCHAR2(2000);
begin
  -- the rowid argument represent the primitive events that are 
  -- rows inseted into the corresponding tables. Use the rowids
  -- to fetch necessary values. 
  if (po is not null) then 
    select orderId into ordId from PurchaseOrders where rowid = po;
  elsif (si is not null) then 
    select orderId into ordId from ShipmentInfo where rowid = si;
  elsif (py is not null) then 
    select orderId into ordId from PaymentInfo where rowid = py;
  end if;
 
  msg := 'Order number: '||ordId||' Matched rule: '
          ||rlm$rule.rlm$ruleid||chr(10)||
          '-> Recommended Action : '||chr(10)||
          '      Action Type ['||rlm$rule.actionType||
          ']'||chr(10)||'      Action Parameter ['||
          rlm$rule.actionParam||']';

  dbms_output.put_line (msg||chr(10));
end;
/

--- insert the same set of rules into the new rule class. 
insert into OrderMgmtRC2 (select * from OrderMgmtRC);

delete from OrderMgmtRC; 

commit;

--- Since DML events are not configured for this rule class, 
--- the application has to explicitly process rules for the 
--- rows in the data table. The rowids of the rows are used 
--- as references to the events. 

var datarid varchar2(40); 


insert into PurchaseOrders (orderId, custId, itemId, itemType,
                            quantity, shipBy) values
(2, 123, 234, 'ROUTER', 120, '01-OCT-2004')
returning rowid into :datarid;

BEGIN
  dbms_rlmgr.process_rules (rule_class => 'OrderMgmtRC2',
                            event_type => 'PurchaseOrders',
                            event_inst => :datarid);
END;
/

insert into ShipmentInfo (orderId, deststate, address, shipTime,
                            shipType) values
(2, 'CA','1 Main street, San Jose','29-SEP-2004','1 Day Air') 
returning rowid into :datarid;

BEGIN
  dbms_rlmgr.process_rules (rule_class => 'OrderMgmtRC2',
                            event_type => 'ShipmentInfo',
                            event_inst => :datarid);
END;
/

insert into PaymentInfo (orderId, paytype, amountpaid, pymttime,
                         billstate) values
(2, 'CHECK', 100000, '30-SEP-2004', 'CA')
returning rowid into :datarid;

BEGIN
  dbms_rlmgr.process_rules (rule_class => 'OrderMgmtRC2',
                            event_type => 'PaymentInfo',
                            event_inst => :datarid);
END;
/

--
-- Now try the session oriented evaluation of rules where the 
-- results from matching rules are available in the results view 
-- to be queried. 
--
set linesize 80;
desc MatchingOrders2;

select count(*) from MatchingOrders2;

insert into PurchaseOrders (orderId, custId, itemId, itemType,
                            quantity, shipBy) values
(3, 123, 234, 'ROUTER', 120, '01-OCT-2004')
returning rowid into :datarid;

--- Use ADD_EVENT API in the place of PROCESS_RULES ---
BEGIN
  dbms_rlmgr.add_event (rule_class => 'OrderMgmtRC2',
                        event_type => 'PurchaseOrders',
                        event_inst => :datarid);
END;
/

insert into ShipmentInfo (orderId, deststate, address, shipTime,
                            shipType) values
(3, 'CA','1 Main street, San Jose','29-SEP-2004','1 Day Air') 
returning rowid into :datarid;

BEGIN
  dbms_rlmgr.add_event (rule_class => 'OrderMgmtRC2',
                        event_type => 'ShipmentInfo',
                        event_inst => :datarid);
END;
/

insert into PaymentInfo (orderId, paytype, amountpaid, pymttime,
                         billstate) values
(3, 'CHECK', 100000, '30-SEP-2004', 'CA')
returning rowid into :datarid;

BEGIN
  dbms_rlmgr.add_event (rule_class => 'OrderMgmtRC2',
                        event_type => 'PaymentInfo',
                        event_inst => :datarid);
END;
/

--- Since the event structure is configired with table aliases, 
--- the events are represented using the rowids from the 
--- corresponding tables. 
column rlm$ruleid format a7;
column actiontype format a25;
column actionparam format a25;
select po, si, py, rlm$ruleid, actionType, actionParam from MatchingOrders2 order by 4; 

select 
  (select orderId from purchaseOrders where rowid = po) as OrderId,
   rlm$ruleid, actionType, actionParam from MatchingOrders2 order by 2;

exec dbms_rlmgr.drop_rule_class('OrderMgmtRC2'); 
exec dbms_rlmgr.drop_rule_class('OrderMgmtRC'); 
exec dbms_rlmgr.drop_event_struct('OrderMgmt'); 

drop table PurchaseOrders; 
drop table ShipmentInfo;
drop table PaymentInfo; 
drop table messagequeue; 

--
--- IV. Use of Collection events in an Order Management Application
--

--
-- 1. Create the object types for the event structure. 
--
create or replace type PurchaseOrder as object
  ( orderid       number,
    customerid    number,
    itemid        number,
    itemcount     number,
    amount        number,
    exptddate     date);
/

create or replace type ShipItem as object
 (  itemid        number,
    itemtype      varchar2(30),
    orderid       number,
    truckid       number);
/

create or replace type TruckAtDock as object
 (  truckid       number,
    loadid        date,
    status        varchar2(30),
    capacity      number);
/

create or replace type OrderMgmt as object
 (
    porder  PurchaseOrder,
    sitem   ShipItem,
    truck   TruckAtDock
 );
/

--
-- 2. Create a rule class for the OrderMgmt composite event and enable 
--    collection for PurchaseOrder and ShipItem events. 
--
BEGIN
   DBMS_RLMGR.CREATE_RULE_CLASS(
     rule_class      => 'OrderMgmtRC',
     event_struct    => 'OrderMgmt',
     action_cbk      => 'OrderMgmtCBK',
     actprf_spec     => 'actionType VARCHAR2(40), actionParam VARCHAR2(100),
                         poAggrRet VARCHAR2(20) default null',
     rslt_viewnm     => 'MatchedScenarios',
     rlcls_prop      => '<composite
                           equal="(porder.orderid, sitem.orderid) |
                                       (sitem.truckid, truck.truckid)"
                           ordering="rlm$rule.rlm$ruleid, porder.orderid,
                                         porder.itemid, truck.loadid">
                           <collection type="PurchaseOrder" groupby="orderid, customerid,
                                                               itemid"/>
                           <collection type="ShipItem" groupby="itemid, truckid"/>
                         </composite>');
END;
/

desc OrderMgmtCBK; 

--
-- 3. Implement the action callback procedure
--

create or replace procedure "ORDERMGMTCBK" (
  PORDER     PURCHASEORDER,
  PO_EVTID   ROWID,
  SITEM      SHIPITEM,
  SI_EVTID   ROWID,
  TRUCK      TRUCKATDOCK,
  rlm$rule   ORDERMGMTRC%ROWTYPE) is
  mesg       VARCHAR2(100); 
  aggrval    VARCHAR2(100); 
begin
  mesg := ' Rule "'||rlm$rule.rlm$ruleid||
                            '" matched '||
     case when porder.orderid is not null then 'Purchase Order '||porder.orderid
          when porder.customerid is not null then 'Customer '||porder.customerid
          when sitem.truckid is not null then '||Truck '||sitem.truckid
     end; 

  if (porder is not null and rlm$rule.poAggrRet is not null) then 
    aggrval := dbms_rlmgr.get_aggregate_value ('OrderMgmtRC', po_evtid,
                                               rlm$rule.poAggrRet);
    aggrval := ' with '||rlm$rule.poAggrRet||' equal to '||aggrval;
  end if; 
  dbms_output.put_line (mesg||aggrval); 
end;
/

--
-- 4. User defined functions for the rules
--
create or replace function CustomerType (custId int) return VARCHAR2 is
begin
  return 'GOLD';
end;
/

exec dbms_rlmgr.add_functions('OrderMgmt','CustomerType');

--
-- 5. Add rules
--
insert into OrderMgmtRC (rlm$ruleid, actionType, actionParam, rlm$ruledesc,
                         rlm$rulecond) values
('High priority Order redirect', 'REDIRECT','HIGH_PRIORITY_QUEUE',
 'Route the order to a high priority queue if it is large order from a Gold Customer',
'<condition>
   <object name="porder"> CustomerType(customerid) = ''GOLD'' and amount > 10000 </object>
</condition>');

insert into OrderMgmtRC (rlm$ruleid, actionType, actionParam, rlm$ruledesc,
                         rlm$rulecond) values
('Delayed Order redirect', 'REDIRECT','HIGH_PRIORITY_QUEUE',
 'Route the order to a high priority queue if there is a potential for it to get delayed',
'<condition>
   <and equal="porder.orderid, sitem.orderid">
     <object name="porder"> CustomerType(customerid) = ''GOLD'' </object>   
     <not by="porder.exptddate-2">
       <object name="sitem"/>
     </not>
   </and>
</condition>');

insert into OrderMgmtRC (rlm$ruleid, actionType, actionParam, rlm$ruledesc,
                         rlm$rulecond) values
('Large number of orders promo', 'PROMOTION','ELITE_STATUS',
 'Offer an elite status to a customer if he submited a large number of orders, each with a minimum of 10000 dollars',
 '<condition>
    <collection name="porder" groupby="customerid" having="count(*) > 10">
      amount > 10000
    </collection>
</condition>');


insert into OrderMgmtRC (rlm$ruleid, actionType, actionParam, rlm$ruledesc,
                         rlm$rulecond) values
('Expanding customer', 'PROMOTION', 'LARGE_ORDER',
'Offer a promotion for ordering in bulk if the average size of the last 10 orders is over 20000 dollars',
'<condition>
   <collection name="porder" groupby="customerid" windowsize="10" having="avg(amount) > 20000"/>
</condition>');

insert into OrderMgmtRC (rlm$ruleid, actionType, actionParam, rlm$ruledesc,
                         rlm$rulecond) values
('Promo on Total size of orders in 10 days', 'PROMOTION','ELITE_STATUS',
 'Offer an elite status to a customer if he submited a large number of orders, each with a minimum of 1000 dollars, in a 30 day period',
'<condition>
    <collection name="porder" groupby="customerid" windowlen="30" having="sum(amount) > 50000">
       amount > 1000
    </collection>
</condition>');

insert into OrderMgmtRC (rlm$ruleid, actionType, actionParam, rlm$ruledesc,
                         rlm$rulecond) values
('Completed order', 'UPDATE_ORDER_STATUS','COMPLETE',
'Compare the number of items ordered and the items shipped to mark the order complete',
'<condition>
   <and equal="porder.orderid, sitem.orderid" having="count(sitem.*) = porder.itemcount">
      <object name="porder"/> 
      <collection name="sitem" groupby="orderid" compute="count(*)">
         itemtype != ''Reusable Container''
      </collection>
   </and>
</condition>');

insert into OrderMgmtRC (rlm$ruleid, actionType, actionParam, rlm$ruledesc,
                         rlm$rulecond) values
('Ready to ship', 'READY_TO_SHIP', 'LOADED_TRUCK',
'Signal readiness to ship when the truck is at least 90% full',
'<condition>
  <and equal="sitem.truckid, truck.truckid" having="count(sitem.*) >= truck.capacity*0.9" >
    <object name="truck"> status = ''Loading'' </object>
    <collection name="sitem" groupby="truckid" compute="count(*)">
      itemtype = ''Reusable Container''
    </collection>
  </and>
</condition>');

commit;

--
-- 6. Add events
--
set serveroutput on; 
exec dbms_rlmgr.process_rules('OrderMgmtRC',sys.anydata.convertobject(purchaseorder(100, 300, 51, 8, 11000, '25-OCT-2006')));
exec dbms_rlmgr.process_rules('OrderMgmtRC',sys.anydata.convertobject(purchaseorder(101, 300, 52, 8, 111000, '25-OCT-2006')));
exec dbms_rlmgr.process_rules('OrderMgmtRC',sys.anydata.convertobject(purchaseorder(102, 300, 52, 8, 11000, '25-OCT-2006')));
exec dbms_rlmgr.process_rules('OrderMgmtRC',sys.anydata.convertobject(purchaseorder(103, 300, 54, 8, 111000, '25-OCT-2006')));
exec dbms_rlmgr.process_rules('OrderMgmtRC',sys.anydata.convertobject(purchaseorder(104, 300, 55, 8, 11000, '25-OCT-2006')));
exec dbms_rlmgr.process_rules('OrderMgmtRC',sys.anydata.convertobject(purchaseorder(105, 300, 56, 8, 111000, '25-OCT-2006')));
exec dbms_rlmgr.process_rules('OrderMgmtRC',sys.anydata.convertobject(purchaseorder(106, 300, 56, 8, 11000, '25-OCT-2006')));
exec dbms_rlmgr.process_rules('OrderMgmtRC',sys.anydata.convertobject(purchaseorder(107, 300, 57, 8, 111000, '25-OCT-2006')));
exec dbms_rlmgr.process_rules('OrderMgmtRC',sys.anydata.convertobject(purchaseorder(108, 300, 58, 8, 11000, '25-OCT-2006')));
exec dbms_rlmgr.process_rules('OrderMgmtRC',sys.anydata.convertobject(purchaseorder(109, 300, 59, 8, 111000, '25-OCT-2006')));
exec dbms_rlmgr.process_rules('OrderMgmtRC',sys.anydata.convertobject(purchaseorder(110, 300, 59, 8, 11000, '25-OCT-2006')));
exec dbms_rlmgr.process_rules('OrderMgmtRC',sys.anydata.convertobject(purchaseorder(111, 300, 59, 8, 111000, '25-OCT-2006')));
exec dbms_rlmgr.process_rules('OrderMgmtRC',sys.anydata.convertobject(purchaseorder(112, 300, 59, 8, 11000, '25-OCT-2006')));

exec dbms_rlmgr.process_rules('OrderMgmtRC',sys.anydata.convertobject(TruckAtDock(900, sysdate, 'Loading', 6)));

exec dbms_rlmgr.process_rules('OrderMgmtRC',sys.anydata.convertobject(shipitem(401, 'Reusable Container', 100, 900)));
exec dbms_rlmgr.process_rules('OrderMgmtRC',sys.anydata.convertobject(shipitem(402, 'Reusable Container', 101, 900)));
exec dbms_rlmgr.process_rules('OrderMgmtRC',sys.anydata.convertobject(shipitem(403, 'Reusable Container', 102, 900)));
exec dbms_rlmgr.process_rules('OrderMgmtRC',sys.anydata.convertobject(shipitem(404, 'Reusable Container', 103, 900)));
exec dbms_rlmgr.process_rules('OrderMgmtRC',sys.anydata.convertobject(shipitem(405, 'Reusable Container', 104, 900)));
exec dbms_rlmgr.process_rules('OrderMgmtRC',sys.anydata.convertobject(shipitem(406, 'Reusable Container', 105, 900)));
exec dbms_rlmgr.process_rules('OrderMgmtRC',sys.anydata.convertobject(shipitem(407, 'Reusable Container', 107, 900)));

connect / as sysdba;
drop user ruldemo cascade;

exit;
