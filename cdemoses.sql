rem
rem $Header: cdemoses.sql 14-nov-00.18:39:03 hdnguyen Exp $
rem
rem cdemoses.sql
rem
rem Copyright (c) 1998, 1999,, 2000 Oracle Corporation.  All rights reserved.
rem
rem    NAME
rem      cdemoses.sql - <one-line expansion of the name>
rem
rem    DESCRIPTION
rem      <short description of component this file declares/defines>
rem
rem    NOTES
rem      <other useful comments, qualifications, etc.>
rem
rem    MODIFIED   (MM/DD/YY)
rem    hdnguyen    11/14/00 - fixed connect internal
rem    mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem    lchidamb    10/14/98 - Add Primary User
rem    lchidamb    10/13/98 - SQL script for Session management demo
rem    lchidamb    10/13/98 - Created
rem
connect / as sysdba;
drop user user0 cascade;
drop user user1 cascade;
drop user user2 cascade;
drop user user3 cascade;
drop user user4 cascade;
drop user user5 cascade;
drop user user6 cascade;
drop user user7 cascade;
drop user user8 cascade;
drop user user9 cascade;

drop user primary cascade;

grant connect, resource to user0 identified by user0;
grant connect, resource to user1 identified by user1;
grant connect, resource to user2 identified by user2;
grant connect, resource to user3 identified by user3;
grant connect, resource to user4 identified by user4;
grant connect, resource to user5 identified by user5;
grant connect, resource to user6 identified by user6;
grant connect, resource to user7 identified by user7;
grant connect, resource to user8 identified by user8;
grant connect, resource to user9 identified by user9;


grant connect, resource to primary identified by primary;

