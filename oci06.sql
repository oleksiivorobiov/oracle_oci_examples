rem
rem $Header: oci06.sql 11-oct-2006.15:55:32 azhao Exp $
rem
rem Copyright (c) 1995, 2006, Oracle. All rights reserved.  
rem
rem    NAME
rem      oci06.sql
rem    DESCRIPTION
rem      Script for A22400 OCI Techniques White Paper
rem      Demo script for oci06.c
rem    MODIFIED   (MM/DD/YY)
rem     azhao      10/11/06  - case-senstive password change
rem     mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
rem     cchau      08/18/97 -  enable dictionary protection
rem     echen      01/10/97 -  change internal to sys/change_on_install
rem     vraghuna   03/01/95 -  Creation

set echo on;
connect sys/knl_test7 as sysdba;
rem
rem create a view to get the name, length and colid of all the columns in
rem a table
create or replace view ocicol
   (tobjid, townerid,  tname, cname, clength, colid) AS
   SELECT o$.obj#, o$.owner#,  o$.name, c$.name, c$.length,  c$.col#
   FROM   sys.col$ c$, sys.obj$ o$
   WHERE  o$.obj# = c$.obj#
/

CREATE OR REPLACE view ocicolu AS                /* user version for ocitest */
       SELECT * from ocicol WHERE townerid = uid
/
grant select on ocicolu to public;
drop public synonym ocicolu;
create public synonym ocicolu for sys.ocicolu;

rem
rem Create a new user - call it ocitest
rem
drop user ocitest cascade;
create user ocitest identified by OCITEST;
grant connect, resource to ocitest;

rem
rem Created needed tables
rem
connect ocitest/OCITEST;

create table test1 (this_col_name_is_30_chars_long number,
                    two long, three date, four char(10));

