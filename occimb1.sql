/* Copyright (c) 2003, 2004, Oracle Corporation.  All rights reserved.  */
/*

   NAME
     occimb1.sql - Create Objects for OCCI Globalization demo

   DESCRIPTION

     This sql script creates objects for occi multibyte demo
     This program assumes sample HR schema is setup. 

   NOTES
     The database characterset must be UTF8.

   MODIFIED   (MM/DD/YY)
   sudsrini    08/02/04 - Copyright Info
   shiyer      03/05/03 - Creation

*/

connect hr/hr;

drop table globalinfo_tab;

create table globalinfo_tab
(
  g_lang varchar2(10),
  g_infodesc varchar2(50),
  g_info varchar2(100)
);

