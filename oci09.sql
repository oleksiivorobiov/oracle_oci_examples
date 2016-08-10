rem
rem $Header: oci09.sql 11-oct-2006.15:55:32 azhao Exp $
rem
rem Copyright (c) 1995, 2006, Oracle. All rights reserved.  
rem
rem    NAME
rem      oci09.sql
rem    DESCRIPTION
rem      Script for A22400 OCI Techniques White Paper
rem      Demo script for oci09.c
rem    MODIFIED   (MM/DD/YY)
rem     azhao      10/11/06  - case-senstive password change
rem     aliu       01/05/06  - add order by in the select statements from 
rem                            ocicolu to fix intermittent diffs 
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

rem
rem Create a new user - call it ocitest
rem
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

create table debug_cursor (col1 char);
create table debug_result (col1 varchar2(20));
create or replace package oci09pkg as

           procedure oci09proc(
                tabname    in  varchar2,
                colname    out varchar2,
                collen     out varchar2,
                colid      out varchar2);
end;
/

create or replace package body oci09pkg as

        cursor get_col(
                table_name in varchar2) is
        select cname, clength, colid from ocicolu
        where tname = table_name order by colid;

        procedure  oci09proc(
                tabname    in varchar2,
                colname    out varchar2,
                collen     out varchar2,
                colid      out varchar2) is

        begin

                if NOT get_col%ISOPEN then
                        open get_col(tabname);
                        insert into debug_cursor values('y');
                else
                        insert into debug_cursor values('n');
                end if;

                fetch get_col into colname, collen, colid;

                if get_col%NOTFOUND then
                        close get_col;
                end if;  -- close the cursor for the next table

                insert into debug_result values (colname);
                commit;
        end;

end;
/

