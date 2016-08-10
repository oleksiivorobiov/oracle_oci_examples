Rem
Rem $Header: cdemdp9i.sql 15-aug-2006.11:46:52 jkaloger Exp $
Rem
Rem cdemdp9i.sql
Rem
Rem+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Rem Copyright (c) 2006, Oracle. All rights reserved.  
Rem                    All Rights Reserved.                                  
Rem+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Rem
Rem
Rem NAME:
Rem
Rem   Cdemdp9i.sql 
Rem
Rem DESCRIPTION:
Rem
Rem   Provides Oracle9i Objects for Direct Path API demo.
Rem
Rem NOTES:
Rem
Rem   Objects and tables will be used by several DP API demo clients.
Rem
Rem   Access the database with the oe/oe(Order Entry) user/password.
Rem
Rem   The demo tables incorporate various "Sample Schema" types from the 
Rem   Order Entry schema scripts at $ORACLE_HOME/demo/schema.
Rem   Refer to "Oracle9i Sample Schemas" documentation for schema layout and
Rem   how to reinitialize the Sample Schema.
Rem
Rem   This demo adds three tables to the Sample Schema.
Rem
Rem   Sample Schemas can be built using the Database Configuration Assistant
Rem   (DBCA).  Refer to appropriate Oracle9i documentation for using DBCA.
Rem
Rem   Refer to OCI Programmer Guide, Chapter 12, for complete DP API 
Rem   information and Appendix B for DP API demo information.
Rem
Rem   Sample Schema types used for DP API demos:
Rem
Rem     cust_address_type
Rem     customer_typ
Rem     corporate_cust_typ (derived from customer_typ)
Rem     inventory_typ
Rem     warehouse_typ      (nested column object within inventory_typ)
Rem
Rem   DP API Demo tables created for DP API demos: 
Rem
Rem     dp_api_demo1
Rem     dp_api_demo2
Rem     dp_api_demo3
Rem
Rem
Rem   Used by clients:
Rem
Rem   Cdemdpco.c    -  Column object
Rem   Cdemdpno.c    -  Nested Column object
Rem   Cdemdpro.c    -  Reference column object
Rem   Cdemdpss.c    -  Sql string
Rem   Cdemdpin.c    -  Inheritance
Rem   Cdemdpit.c    -  Table object with inheritance
Rem
Rem
Rem MODIFIED   (MM/DD/YY)
Rem    jkaloger 08/15/06 - Lowercase passwords for secure verifiers project
Rem    eegolf   04/04/01 - Merged eegolf_demo_update
Rem
Rem   eegolf    04/27/01   - Creation
Rem
Rem------------------------------------------------------------------------

connect oe/oe

set echo on

drop table dp_api_demo1 force;
drop table dp_api_demo2 force;
drop table dp_api_demo3 force;


Rem --------------------------------------------
Rem Create Column objects
Rem --------------------------------------------

Rem Place any DP API objects here


Rem --------------------------------------------
Rem Create tables
Rem --------------------------------------------

create table dp_api_demo2 of warehouse_typ;
create table dp_api_demo3 of customer_typ
nested table cust_orders store as cust_orders2
(nested table order_item_list store as invnt_oi_list);

create table dp_api_demo1 
(cust_first_name        varchar2(10),
 cust_last_name         varchar2(10),
 cust_address           cust_address_typ,
 cust_more_info         customer_typ,
 current_inventory      inventory_typ,
 inventory_string       varchar2(20),
 inventory_string2      varchar2(20),
 inventory_warehouse    ref warehouse_typ,
 inventory_stock_date   varchar2(20))
nested table cust_more_info.cust_orders store as cust_more_orders
(nested table order_item_list store as inventory_oi_list);

exit;


