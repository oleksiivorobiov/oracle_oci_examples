/* Copyright (c) 2003, 2005, Oracle. All rights reserved.  */
/*

   NAME
     occiuni2.sql - Create Objects for OCCI Globalization demo

   DESCRIPTION

     This sql script creates objects for OCCI Windows Specific unicode demo
     This program assumes sample HR schema is setup.

   NOTES
     The demo to be run only on windows

   MODIFIED   (MM/DD/YY)
   sudsrini    08/02/04 - Copyright Info
   shiyer      03/05/03 - Creation

*/

connect hr/hr

drop table DocumentsTab;

drop type DocObjType;

create type DocObjType as object
(
 DocName     varchar2(100),
 --nclob to store Unicode data
 DocText     nclob
)
/

create table DocumentsTab of DocObjType;

