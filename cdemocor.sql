rem
rem $Header: cdemocor.sql 18-jun-2004.14:32:05 stsun Exp $
rem
rem Copyright (c) 1997, 2004, Oracle. All rights reserved.  
rem
rem Owner : echen
rem
rem NAME
rem   cdemocor.sql
rem DESCRIPTION
rem   A sql script to setup schema before running the demo cdemocor
rem
rem NOTE
rem MODIFIED  (MM/DD/YY)
rem   stsun    06/18/04 - system/manager instead of sysdba 
rem   mjaeger  07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem   svedala  09/11/98 - a "/" required after create type - bug 717842
rem   cchau    08/18/97 -   enable dictionary protection
rem   echen    05/30/97  -  Creation
rem

set echo on;

connect system/manager;

drop user cdemocor cascade;

grant connect, resource to cdemocor identified by cdemocor;

connect cdemocor/cdemocor

create type person as object (name char(20), age number);
/

create type person_tab as table of REF person;
/

create type address_object as object (state char(2), zip char(10),
       owner REF person);
/

create type addr_tab is table of address_object;
/

create type CUSTOMER as object
(
 name           CHAR(20),
 age            NUMBER,
 addr           addr_tab
);
/

create type PURCHASE_ORDER as object
(
 po_number      NUMBER,
 cust           REF CUSTOMER,
 related_orders REF PURCHASE_ORDER,
 signatories    person_tab
);
/

create table person_table of person;
create table po_table of purchase_order
nested table signatories store as purchase_order_nt_person_tab;
create table cust_table of customer
nested table addr store as customer_nt_addr_tab;

INSERT INTO person_table VALUES ('JOHN1', 42);
INSERT INTO person_table VALUES ('JOHN2', 42);
INSERT INTO person_table VALUES ('JOHN3', 42);

INSERT INTO person_table VALUES ('MIKE1', 29);
INSERT INTO person_table VALUES ('MIKE2', 29);
INSERT INTO person_table VALUES ('MIKE3', 29);

INSERT INTO person_table VALUES ('GREG1', 22);
INSERT INTO person_table VALUES ('GREG2', 22);
INSERT INTO person_table VALUES ('GREG3', 22);

INSERT INTO cust_table VALUES ('JOHN', 42, addr_tab());
INSERT INTO cust_table VALUES ('MIKE', 29, addr_tab());

insert into the (select addr from cust_table where name='JOHN')
  select 'ca', '90416', ref(x) from person_table x where name='JOHN1';
insert into the (select addr from cust_table where name='JOHN')
  select 'ca', '90417', ref(x) from person_table x where name='JOHN2';
insert into the (select addr from cust_table where name='JOHN') values (NULL);
insert into the (select addr from cust_table where name='JOHN')
  select 'ca', '90418', ref(x) from person_table x where name='JOHN3';

insert into the (select addr from cust_table where name='MIKE')
  select NULL, '90419', ref(x) from person_table x where name='MIKE1';
insert into the (select addr from cust_table where name='MIKE')
  select 'ca', '90420', ref(x) from person_table x where name='MIKE2';
insert into the (select addr from cust_table where name='MIKE')
  select 'ca', '90421', ref(x) from person_table x where name='MIKE3';

INSERT INTO po_table (po_number, signatories) VALUES (1, person_tab());
INSERT INTO po_table (po_number, signatories) VALUES (2, person_tab());
INSERT INTO po_table (po_number, signatories) VALUES (3, person_tab());

UPDATE po_table
  set cust = (select ref(x) from cust_table x where name = 'JOHN')
  where po_number = 1;
UPDATE po_table
  set cust = (select ref(x) from cust_table x where name = 'MIKE')
  where po_number = 2;
UPDATE po_table
  set cust = (select ref(x) from cust_table x where name = 'MIKE')
  where po_number = 3;

UPDATE po_table
  set related_orders = (select ref(x) from po_table x where po_number = 2)
  where po_number = 1;
UPDATE po_table
  set related_orders = (select ref(x) from po_table x where po_number = 3)
  where po_number = 2;
UPDATE po_table
  set related_orders = (select ref(x) from po_table x where po_number = 1)
  where po_number = 3;

insert into the (select signatories from po_table where po_number = 1)
  select ref(x) from person_table x where name='JOHN1';
insert into the (select signatories from po_table where po_number = 1)
  select ref(x) from person_table x where name='MIKE1';
insert into the (select signatories from po_table where po_number = 1)
  select ref(x) from person_table x where name='GREG1';

insert into the (select signatories from po_table where po_number = 2)
  select ref(x) from person_table x where name='JOHN2';
insert into the (select signatories from po_table where po_number = 2)
  select ref(x) from person_table x where name='MIKE2';
insert into the (select signatories from po_table where po_number = 2)
  select ref(x) from person_table x where name='GREG2';

insert into the (select signatories from po_table where po_number = 3)
  select ref(x) from person_table x where name='JOHN3';
insert into the (select signatories from po_table where po_number = 3)
  select ref(x) from person_table x where name='MIKE3';
insert into the (select signatories from po_table where po_number = 3)
  select ref(x) from person_table x where name='GREG3';

commit;

set echo off;
