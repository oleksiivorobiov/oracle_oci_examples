Rem
Rem $Header: template.sql 06-feb-96.13:23:14 kosinski Exp $
Rem
Rem sadvuwk.sql
Rem
Rem  Copyright (c) Oracle Corporation 2000. All Rights Reserved.
Rem
Rem    NAME
Rem      sadvuwk.SQL - Sample script to create a user-workload table
Rem
Rem    DESCRIPTION
Rem      This is a simple example of creating a user-defined workload table from
Rem      which Summary Advisor can extract a workload.  
Rem
Rem    NOTES
Rem      The indexes are optional; however, if you plan on applying workload filters
Rem      to the load workload operation, then you should create indexes on the
Rem      most significant filter columns fo the user workload.
Rem
Rem      The QUERY and OWNER columns are the only required columns for a user-supplied
Rem      workload.
Rem
Rem      If the QUERY column will contain SQL statements longer than 2000 characters,
Rem      then it is acceptable to create the column as a LONG datatype.
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    gssmith     11/06/00 - Created
Rem

CREATE TABLE advisor_user_workload
  (
   query                varchar2(2000),         /* full sql text */
   owner                varchar(30),            /* user submitting the query */
   application          varchar(30),            /* application name */
   frequency            number,                 /* query frequency */
   lastuse              date,                   /* lastuse date of the query */
   priority             number,                 /* priority of the query */
   responsetime         number,                 /* query response time */
   resultsize           number,                 /* result size in bytes */
   sql_hash             number,                 /* server generated hash */
   sql_addr             raw(4)                  /* lib-cache address */
  );

CREATE INDEX adv_uwk_idx_01
   ON advisor_user_workload (application,lastuse);

CREATE INDEX adv_uwk_idx_02
   ON advisor_user_workload (owner,lastuse);

