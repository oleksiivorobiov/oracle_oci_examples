Rem
Rem $Header: epgdemo.sql 02-nov-2006.11:39:35 rpang Exp $
Rem
Rem epgdemo.sql
Rem
Rem Copyright (c) 2006, Oracle.  All rights reserved.  
Rem
Rem    NAME
Rem      epgdemo.sql - Demo program for the embedded PL/SQL gateway
Rem
Rem    DESCRIPTION
Rem      This is a sample program to demonstrate the usage of the embedded
Rem      PL/SQL gateway.
Rem
Rem    NOTES
Rem      This demo requires Oracle XML DB installed and the demo account SCOTT.
Rem
Rem      To execute this demo, you need to start the XML DB HTTP listener and
Rem      to unlock the database account "anonymous".
Rem
Rem      > sqlplus system/<password>
Rem
Rem      SQL> Rem Start XML DB HTTP listener:
Rem      SQL> exec dbms_xdb.setHttpPort(8080);
Rem
Rem      SQL> Rem Unlock the database account "anonymous":
Rem      SQL> alter user anonymous account unlock;
Rem
Rem      Then, execute this demo script to set up the database access
Rem      descriptor (DAD) and to create the demo program in the account SCOTT:
Rem
Rem      SQL> @epgdemo.sql
Rem
Rem      Then, open the URL http://<your-database-host>:8080/demo/plsql/hello
Rem
Rem    *** Security notes ***
Rem      This demo requires the database account "anonymous" to be unlocked
Rem      so that the demo program "hello" can be accessed from the browser
Rem      without authentication. You should ensure that all XML DB repository
Rem      resources and servlets which are not intended for unauthenticated
Rem      access are properly secured. Please refer to the XML DB documentation
Rem      on protecting its repository resources and servlets.
Rem    **********************
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    rpang       11/02/06 - Created
Rem

SET ECHO ON
SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100

Rem Set up the database access descriptor (DAD) as SYSTEM

begin
  dbms_epg.create_dad('DemoDAD', '/demo/plsql/*');
  dbms_epg.set_dad_attribute('DemoDAD', 'database-username', 'SCOTT');
end;
/

Rem Create the demo program as SCOTT

connect scott/tiger

begin
  /* Authorize DemoDAD to use my schema and my privileges */
  dbms_epg.authorize_dad('DemoDAD');
end;
/

create or replace procedure hello is
begin
  /* Generate "Hello, World!" in HTML */
  htp.print('<b>Hello, World!</b>');
end;
/
