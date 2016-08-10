rem
rem $Header: oci10.sql 11-oct-2006.15:55:32 azhao Exp $
rem
rem Copyright (c) 1995, 2006, Oracle. All rights reserved.  
rem
rem    NAME
rem      oci10.sql
rem    DESCRIPTION
rem      Script for A22400 OCI Techniques White Paper
rem      Demo script for oci10.c
rem    MODIFIED   (MM/DD/YY)
rem     azhao      10/11/06  - case-senstive password change
rem     aliu       01/10/06  - add order by in the select from ocicolu to fix 
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

create or replace package oci10pkg as

        type char_array is table of varchar2(20) index by binary_integer;
        type num_array is table of integer index by binary_integer;

        procedure oci10proc(
                cname     out char_array,  -- array to put cname in
                collen    out num_array,   -- array to put column lengths in
                colid     out num_array,   -- array to put column ids in
                tabname   in  varchar2,    -- table name to get col definitions
                batchsize in  integer,     -- number of rows per trip
                numret    in out integer,  -- number of rows ACTUALLY RETURNED
                donefetch in out integer); -- are we done fetching for this
                                           -- table
end;
/

create or replace package body oci10pkg as

        cursor get_col(
                table_name in varchar2) is
        select cname, clength, colid from ocicolu
        where tname = table_name order by colid;


        procedure oci10proc(
                cname     out char_array,
                collen    out num_array,
                colid     out num_array,
                tabname   in varchar2,
                batchsize in integer,
                numret    in out integer,
                donefetch in out integer
        ) is

        begin

                donefetch := 0;
                numret := 0;

                if NOT get_col%ISOPEN then
                        open get_col(tabname);
                end if;

                for i in 1..batchsize loop
                        fetch get_col
                           into cname(i), collen(i), colid(i);

                        if get_col%NOTFOUND then
                                close get_col;
                                donefetch := 1;
                                exit;
                        else
                                numret := numret + 1;
                        end if;
                end loop;

        end;

end;
/
