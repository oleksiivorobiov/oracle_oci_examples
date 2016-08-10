Rem
Rem $Header: strmmv2s.sql 19-jan-2006.16:52:08 wesmith Exp $
Rem
Rem strmmv2s.sql
Rem
Rem Copyright (c) 2006, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      strmmv2s.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem      <short description of component this file declares/defines>
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    wesmith     01/19/06 - Created
Rem

create table customer (
        c_id  number primary key,
        zip   number,
        c_name varchar(30) default NULL     /* non-filter column */
);

create table orders (
        o_id  number primary key,
        c_id  number,
        ol_num number default 0           /* non-filter column */
);

create table order_line (
        ol_id number,
        o_id  number,
        i_id  number default NULL          /* non-filter column */
);

        
create index c_ind on orders(c_id);
create index oid_ind on order_line(o_id);

alter table order_line add SUPPLEMENTAL LOG DATA (ALL) columns;
alter table orders add SUPPLEMENTAL LOG DATA (ALL) columns;
alter table customer add SUPPLEMENTAL LOG DATA (ALL) columns;

create table sales (region varchar2(10), amt number);
alter table sales add SUPPLEMENTAL LOG DATA (ALL) columns;

-- This sleep is needed to as the min flashback window should be >= 3 seconds 
exec dbms_lock.sleep(10);

set echo off
Rem -------------------------------------------------------.
Rem Insert rows into the master tables(2-1 fanout) which are 
Rem  not part of the snapshot definitions. The pk columns of 
Rem these rows start with digits between 0-4
Rem -------------------------------------------------------.
insert into customer (c_id, zip) values (0, 1);
insert into customer (c_id, zip) values (1, 1);
insert into customer (c_id, zip) values (2, 1);
insert into customer (c_id, zip) values (3, 1);
insert into customer (c_id, zip) values (4, 1);

insert into orders (o_id, c_id) values (01, 0);
insert into orders (o_id, c_id) values (02, 0);
insert into orders (o_id, c_id) values (11, 1);
insert into orders (o_id, c_id) values (12, 1);
insert into orders (o_id, c_id) values (21, 2);
insert into orders (o_id, c_id) values (22, 2);
insert into orders (o_id, c_id) values (31, 3);
insert into orders (o_id, c_id) values (32, 3);
insert into orders (o_id, c_id) values (41, 4);
insert into orders (o_id, c_id) values (42, 4);
  
insert into order_line (ol_id, o_id) values (011, 01);
insert into order_line (ol_id, o_id) values (012, 01);
insert into order_line (ol_id, o_id) values (021, 02);
insert into order_line (ol_id, o_id) values (022, 02);
insert into order_line (ol_id, o_id) values (111, 11);
insert into order_line (ol_id, o_id) values (112, 11);
insert into order_line (ol_id, o_id) values (121, 12);
insert into order_line (ol_id, o_id) values (122, 12);
insert into order_line (ol_id, o_id) values (211, 21);
insert into order_line (ol_id, o_id) values (212, 21);
insert into order_line (ol_id, o_id) values (221, 22);
insert into order_line (ol_id, o_id) values (222, 22);
insert into order_line (ol_id, o_id) values (311, 31);
insert into order_line (ol_id, o_id) values (312, 31);
insert into order_line (ol_id, o_id) values (321, 32);
insert into order_line (ol_id, o_id) values (322, 32);
insert into order_line (ol_id, o_id) values (411, 41);
insert into order_line (ol_id, o_id) values (412, 41);
insert into order_line (ol_id, o_id) values (421, 42);
insert into order_line (ol_id, o_id) values (422, 42);

Rem -------------------------------------------------------.
Rem Insert rows into the master tables(2-1 fanout) which
Rem  are a part of the snapshot definition. The pk columns 
Rem of these rows start with digits between 5-9
Rem -------------------------------------------------------.
insert into customer (c_id, zip) values (5, 19555);
insert into customer (c_id, zip) values (6, 19555);
insert into customer (c_id, zip) values (7, 19555);
insert into customer (c_id, zip) values (8, 19555);
insert into customer (c_id, zip) values (9, 19555);

insert into orders (o_id, c_id) values (51, 5);
insert into orders (o_id, c_id) values (52, 5);
insert into orders (o_id, c_id) values (61, 6);
insert into orders (o_id, c_id) values (62, 6);
insert into orders (o_id, c_id) values (71, 7);
insert into orders (o_id, c_id) values (72, 7);
insert into orders (o_id, c_id) values (81, 8);
insert into orders (o_id, c_id) values (82, 8);
insert into orders (o_id, c_id) values (91, 9);
insert into orders (o_id, c_id) values (92, 9);

insert into order_line (ol_id, o_id) values (511, 51);
insert into order_line (ol_id, o_id) values (512, 51);
insert into order_line (ol_id, o_id) values (521, 52);
insert into order_line (ol_id, o_id) values (522, 52);
insert into order_line (ol_id, o_id) values (611, 61);
insert into order_line (ol_id, o_id) values (612, 61);
insert into order_line (ol_id, o_id) values (621, 62);
insert into order_line (ol_id, o_id) values (622, 62);
insert into order_line (ol_id, o_id) values (711, 71);
insert into order_line (ol_id, o_id) values (712, 71);
insert into order_line (ol_id, o_id) values (721, 72);
insert into order_line (ol_id, o_id) values (722, 72);
insert into order_line (ol_id, o_id) values (811, 81);
insert into order_line (ol_id, o_id) values (812, 81);
insert into order_line (ol_id, o_id) values (821, 82);
insert into order_line (ol_id, o_id) values (822, 82);
insert into order_line (ol_id, o_id) values (911, 91);
insert into order_line (ol_id, o_id) values (912, 91);
insert into order_line (ol_id, o_id) values (921, 92);
insert into order_line (ol_id, o_id) values (922, 92);
commit;

insert into sales values ('West', null);
commit;
