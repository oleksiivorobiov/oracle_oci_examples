/* Copyright (c) 2003, 2005, Oracle. All rights reserved.  */
/*

   NAME
     occiuni1.sql - Create Objects for OCCI Globalization demo

   DESCRIPTION

     This sql script creates objects for occi unicode demo
     This program assumes sample HR schema is setup.


   MODIFIED   (MM/DD/YY)
   sudsrini    08/02/04 - Copyright Info
   shiyer      03/05/03 - Creation

*/

connect hr/hr;

drop table countries_tab;

create table countries_tab
(
  CID   Number(10),
  CENGLISHNAME Varchar2(100),
  CNATIONALNAME NVarchar2(100)
)
/

