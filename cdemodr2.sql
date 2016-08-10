rem
rem $Header: cdemodr2.sql 14-jul-99.13:50:24 mjaeger Exp $
rem
rem cdemodr2.sql
rem
rem Copyright (c) 1997, 1999, Oracle Corporation.  All rights reserved.
rem
rem    NAME
rem      cdemodr2.sql - create and populate test table TAB2.
rem
rem    DESCRIPTION
rem      Demonstrate INSERT/UPDATE/DELETE statements RETURNING LOBS.
rem
rem    NOTES
rem      <other useful comments, qualifications, etc.>
rem
rem    MODIFIED   (MM/DD/YY)
rem    mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem    azhao       06/03/97 - Created
rem

connect scott/tiger
drop table tab2;

create table tab2 (c1 integer, c2 blob, c3 clob);

rem
rem insert some rows for UPDATE later
rem

insert into tab2 values(101, null, null);
insert into tab2 values(102, null, null);
insert into tab2 values(103, null, null);
insert into tab2 values(104, null, null);
insert into tab2 values(105, null, null);
insert into tab2 values(106, null, null);
insert into tab2 values(107, null, null);
insert into tab2 values(108, null, null);
insert into tab2 values(109, null, null);
insert into tab2 values(110, null, null);

rem
rem insert some rows for DELETE later
rem

insert into tab2 values(201, '111', 'AAA');
insert into tab2 values(202, '111', 'BBB');
insert into tab2 values(203, '333', 'CCC');
insert into tab2 values(204, '444', 'DDD');
insert into tab2 values(205, '555', 'EEE');
insert into tab2 values(206, '666', 'FFF');
insert into tab2 values(207, '777', 'GGG');
insert into tab2 values(208, '888', 'HHH');
insert into tab2 values(209, '999', 'III');
insert into tab2 values(210, '000', 'JJJ');


commit;

quit

