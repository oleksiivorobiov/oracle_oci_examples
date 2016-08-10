rem
rem $Header: cdemodr1.sql 14-jul-99.13:49:48 mjaeger Exp $
rem
rem cdemodr1.sql
rem
rem Copyright (c) 1997, 1999, Oracle Corporation.  All rights reserved.
rem
rem    NAME
rem      cdemodr1.sql - <one-line expansion of the name>
rem
rem    DESCRIPTION
rem      <short description of component this file declares/defines>
rem
rem    NOTES
rem      <other useful comments, qualifications, etc.>
rem
rem    MODIFIED   (MM/DD/YY)
rem    mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem    azhao       05/30/97 - Created
rem

connect scott/tiger
drop table tab1;

create table tab1 (c1 integer, c2 char(40), c3 varchar2(40), c4 float,
                   c5 decimal, c6 decimal(8,3), c7 numeric, c8 numeric(7,2),
                   C9 date, C10 raw(40));

rem
rem Insert some data for UPDATE
rem

insert into tab1 values (101, 'To be updated, row 101', 'varchar2, row 101',
                   201.520, 301, 401.450, 501, 601.550, '01-JAN-85',
                   '123456789012345678901234567890');
insert into tab1 values (102, 'To be updated, row 102', 'varchar2, row 102',
                   202.520, 302, 402.450, 502, 602.550, '02-FEB-86',
                   '234567890123456789012345678901');
insert into tab1 values (103, 'To be updated, row 103', 'varchar2, row 103',
                   203.520, 303, 403.450, 503, 603.550, '03-MAR-87',
                   '345678901234567890123456789012');
insert into tab1 values (104, 'To be updated, row 104', 'varchar2, row 104',
                   204.520, 304, 404.450, 504, 604.550, '04-APR-88',
                   '456789012345678901234567890123');
insert into tab1 values (105, 'To be updated, row 105', 'varchar2, row 105',
                   205.520, 305, 405.450, 505, 605.550, '05-MAY-89',
                   '567890123456789012345678901234');
insert into tab1 values (106, 'To be updated, row 106', 'varchar2, row 106',
                   206.520, 306, 406.450, 506, 606.550, '06-JUN-90',
                   '678901234567890123456789012345');
insert into tab1 values (107, 'To be updated, row 107', 'varchar2, row 107',
                   207.520, 307, 407.450, 507, 607.550, '07-JUL-91',
                   '789012345678901234567890123456');
insert into tab1 values (108, 'To be updated, row 108', 'varchar2, row 108',
                   208.520, 308, 408.450, 508, 608.550, '08-AUG-92',
                   '890123456789012345678901234567');
insert into tab1 values (109, 'To be updated, row 109', 'varchar2, row 109',
                   209.520, 309, 409.450, 509, 609.550, '09-SEP-93',
                   '901234567890123456789012345678');
insert into tab1 values (110, 'To be updated, row 110', 'varchar2, row 110',
                   210.520, 310, 410.450, 510, 610.550, '10-OCT-94',
                   '012345678901234567890123456789');
insert into tab1 values (111, 'To be updated, row 111', 'varchar2, row 111',
                   211.520, 311, 411.450, 511, 611.550, '11-NOV-95',
                   '123456789012345678901234567890');
insert into tab1 values (112, 'To be updated, row 112', 'varchar2, row 112',
                   212.520, 312, 412.450, 512, 612.550, '12-DEC-96',
                   '234567890123456789012345678901');
insert into tab1 values (113, 'To be updated, row 113', 'varchar2, row 113',
                   213.520, 313, 413.450, 513, 613.550, '13-JAN-97',
                   '345678901234567890123456789012');
insert into tab1 values (114, 'To be updated, row 114', 'varchar2, row 114',
                   214.520, 314, 414.450, 514, 614.550, '14-FEB-98',
                   '456789012345678901234567890123');
insert into tab1 values (115, 'To be updated, row 115', 'varchar2, row 115',
                   215.520, 315, 415.450, 515, 615.550, '15-MAR-99',
                   '567890123456789012345678901234');

rem
rem Insert some data for DELETE
rem

insert into tab1 values (201, null, 'varchar2, row 201',
                   2001.520, 3001, 4001.4500, 5001, 6001.550, '01-JAN-95',
                   '123456789012345678901234567890');
insert into tab1 values (202, 'To be deleted, row 202', null,
                   2002.520, 3002, 4002.4500, 5002, 6002.550, '02-FEB-94',
                   '234567890123456789012345678901');
insert into tab1 values (203, 'To be deleted, row 203', 'varchar2, row 203',
                   null, 3003, 4003.4500, 5003, 6003.550, '03-MAR-93',
                   '345678901234567890123456789012');
insert into tab1 values (204, 'To be deleted, row 204', 'varchar2, row 204',
                   2004.520, 3004, 4004.4500, 5004, 6004.550, '04-APR-92',
                   '456789012345678901234567890123');
insert into tab1 values (205, 'To be deleted, row 205', 'varchar2, row 205',
                   2005.520, null, 4005.4500, 5005, 6005.550, '05-MAY-91',
                   '567890123456789012345678901234');
insert into tab1 values (206, 'To be deleted, row 206', 'varchar2, row 206',
                   2006.520, 3006, null, 5006, 6006.550, '06-JUN-90',
                   '678901234567890123456789012345');
insert into tab1 values (207, 'To be deleted, row 207', 'varchar2, row 207',
                   2007.520, 3007, 4007.4500, null, 6007.550, '07-JUL-89',
                   '789012345678901234567890123456');
insert into tab1 values (208, 'To be deleted, row 208', 'varchar2, row 208',
                   2008.520, 3008, 4008.4500, 5008, null, '08-AUG-88',
                   '890123456789012345678901234567');
insert into tab1 values (209, 'To be deleted, row 209', 'varchar2, row 209',
                   2009.520, 3009, 4009.4500, 5009, 6009.550, '09-SEP-87',
                   '901234567890123456789012345678');
insert into tab1 values (210, 'To be deleted, row 210', 'varchar2, row 210',
                   2010.520, 3010, 4010.4500, 5010, 6010.550, '10-OCT-86',
                   '012345678901234567890123456789');
insert into tab1 values (211, 'To be deleted, row 211', 'varchar2, row 211',
                   2011.520, 3011, 4011.4500, 5011, 6011.550, null,
                   '123456789012345678901234567890');
insert into tab1 values (212, 'To be deleted, row 212', 'varchar2, row 212',
                   2012.520, 3012, 4012.4500, 5012, 6012.550, '12-DEC-84',
                   null);
insert into tab1 values (213, 'To be deleted, row 213', 'varchar2, row 213',
                   2013.520, 3013, 4013.4500, 5013, 6013.550, '13-JAN-83',
                   '345678901234567890123456789012');
insert into tab1 values (214, 'To be deleted, row 214', 'varchar2, row 214',
                   2014.520, 3014, 4014.4500, 5014, 6014.550, '14-FEB-82',
                   '456789012345678901234567890123');
insert into tab1 values (215, 'To be deleted, row 215', 'varchar2, row 215',
                   2015.520, 3015, 4015.4500, 5015, 6015.550, '15-MAR-81',
                   '567890123456789012345678901234');

commit;

quit

