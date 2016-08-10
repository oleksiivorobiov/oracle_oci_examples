rem
rem $Header: oci11.sql 11-oct-2006.15:55:33 azhao Exp $
rem
rem Copyright (c) 1995, 2006, Oracle. All rights reserved.  
rem
rem    NAME
rem      oci11.sql
rem    DESCRIPTION
rem      Script for A22400 OCI Techniques White Paper
rem      Demo script for oci11.c
rem    MODIFIED   (MM/DD/YY)
rem     azhao      10/11/06  - case-senstive password change
rem     aliu       01/05/06  - add order by in the select from ocicolu to fix 
rem                            intermittent diffs 
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
   select o$.obj#, o$.owner#,  o$.name, c$.name, c$.length,  c$.col#
    from sys.col$ c$, sys.obj$ o$
    where o$.obj# = c$.obj#
/

CREATE OR REPLACE view ocicolu AS                 /* current user's columns */
       SELECT * from ocicol WHERE townerid = uid
/
grant select on ocicolu to public;
drop public synonym ocicolu;
create public synonym ocicolu for sys.ocicolu;

rem Create a new user - call it ocitest
drop user ocitest cascade;
create user ocitest identified by OCITEST;
grant connect,resource to ocitest;
connect ocitest/OCITEST;

create table test1 (col1 number);
create table test2 (col1 number, col2 number);
create table test3 (col1 number, col2 number, col3 number);
create table test4 (one number, two long, three date, four char(10));
create table test5(
        col1 number, col2 char, col3 number, col4 char,
        col5 number, col6 char, col7 number, col8 char,
        col9 number, col10 char);
create table test6(
        col1 number, col2 char, col3 number, col4 char,
        col5 number, col6 char, col7 number, col8 char,
        col9 number, col10 char, col11 number, col12 char,
        col13 number, col14 char, col15 number, col16 char,
        col17 number, col18 char, col19 number);

create or replace package oci11pkg as

        type rectype is record (cname ocicolu.cname%type,
                                clength ocicolu.clength%type,
                                colid ocicolu.colid%type);
        type ctype is ref cursor return rectype;

        procedure oci11proc(
                curs      in out  ctype,
                tabname in varchar2);

end;
/

create or replace package body oci11pkg as

        procedure oci11proc(
                curs in out ctype,
                tabname in varchar2

        ) is

        begin

                open curs for
                select cname, clength, colid
                from ocicolu
                where tname = tabname order by colid;


        end;

end;
/
