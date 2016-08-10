rem
rem $Header: cdemosyev.sql 14-nov-00.18:39:09 hdnguyen Exp $
rem
rem cdemosyev.sql
rem
rem Copyright (c) 1998, 1999,, 2000 Oracle Corporation.  All rights reserved.
rem
rem    NAME
rem      cdemosyev.sql - C DEMO for SYstem EVents
rem
rem    DESCRIPTION
rem      This Demo script creates users and schema objects necessary
rem      for setting up system event triggers and subscriptions using
rem      Advanced Queues (AQ).
rem      The following are created:
rem      triggers which fire on the events
rem      queue table, used by persistent queues for storage
rem      queues to which triggers enqueue messages
rem      subscriptions to queues
rem
rem
rem    NOTES
rem      This file should be executed before running the OCI registration
rem      Script cdemosyev.c, followed by cdemosyex.sql

rem      This script will setup the users and schema objects required for
rem      creating subscriptions.  After executing this file, cdemosyev.c
rem      should be executed. On execution the user should see, on the
rem      standard output, messages indicating  successful registration for
rem      various subscriptions. During the registration process the user
rem      specifies a callback function which is invoked when the user needs
rem      to be notified.
rem      The script cdemosyex performs a set of actions for each of which the
rem      corresponding callback function is invoked at the user end.

rem    MODIFIED   (MM/DD/YY)
rem    hdnguyen    11/14/00 - fixed connect internal
rem    mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem    sasuri      11/23/98 - add shutdown notification
rem    sasuri      06/16/98 - system event demo files
rem    sasuri      06/16/98 - Created
rem

connect / as sysdba;
set serveroutput on
set echo on

rem ---------------------------------------------------------------------------
rem
rem  Set up a user with appropriate privileges
rem
rem ---------------------------------------------------------------------------

create user event identified by event;
grant dba to event;

create user pubsub identified by pubsub;
grant connect, resource, dba to pubsub;
grant AQ_ADMINISTRATOR_ROLE, AQ_USER_ROLE to pubsub;
grant select_catalog_role to pubsub;
grant execute on dbms_aq to pubsub;
execute dbms_aqadm.grant_type_access('pubsub');
execute dbms_aqadm.grant_system_privilege('ENQUEUE_ANY','pubsub',FALSE);
execute dbms_aqadm.grant_system_privilege('DEQUEUE_ANY','pubsub',FALSE);
connect pubsub/pubsub;

rem ---------------------------------------------------------------------------
rem
rem create queue tables for persistent multiple consumers
rem
rem ---------------------------------------------------------------------------

connect pubsub/pubsub;

rem  Create or replace a queue table
begin
DBMS_AQADM.CREATE_QUEUE_TABLE(
    QUEUE_TABLE=>'pubsub.raw_msg_table',
    MULTIPLE_CONSUMERS => TRUE,
    QUEUE_PAYLOAD_TYPE =>'RAW',
    COMPATIBLE => '8.1.3');
end;
/

rem ---------------------------------------------------------------------------
rem
rem  Create various persistent queues for publishing messages
rem
rem ---------------------------------------------------------------------------

rem  Create a queue for error events
begin
DBMS_AQADM.CREATE_QUEUE(QUEUE_NAME=>'pubsub.error',
            QUEUE_TABLE=>'pubsub.raw_msg_table',
            COMMENT=>'Q for error triggers');
end;
/

rem  Create a queue for startup
begin
DBMS_AQADM.CREATE_QUEUE(QUEUE_NAME=>'pubsub.startup',
            QUEUE_TABLE=>'pubsub.raw_msg_table',
            COMMENT=>'Q for startup triggers');
end;
/

rem  Create a queue for shutdown
begin
DBMS_AQADM.CREATE_QUEUE(QUEUE_NAME=>'pubsub.shutdown',
            QUEUE_TABLE=>'pubsub.raw_msg_table',
            COMMENT=>'Q for shutdown triggers');
end;
/

rem  Create a queue for logon events
begin
DBMS_AQADM.CREATE_QUEUE(QUEUE_NAME=>'pubsub.logon',
            QUEUE_TABLE=>'pubsub.raw_msg_table',
            COMMENT=>'Q for logon triggers');
end;
/

rem  Create a queue for logoff events
begin
DBMS_AQADM.CREATE_QUEUE(QUEUE_NAME=>'pubsub.logoff',
            QUEUE_TABLE=>'pubsub.raw_msg_table',
            COMMENT=>'Q for logoff triggers');
end;
/

rem ---------        Create non-persistent queues     -----------------------

rem  Create a queue for create on schema events
begin
DBMS_AQADM.CREATE_NP_QUEUE(QUEUE_NAME=>'pubsub.create_sch',
            MULTIPLE_CONSUMERS => TRUE);
end;
/

begin
DBMS_AQADM.CREATE_NP_QUEUE(QUEUE_NAME=>'pubsub.drop_sch',
                        MULTIPLE_CONSUMERS => TRUE);
end;
/

rem ---------------------------------------------------------------------------
rem
rem  Start all queues
rem
rem ---------------------------------------------------------------------------


begin
DBMS_AQADM.START_QUEUE('pubsub.error');
DBMS_AQADM.START_QUEUE('pubsub.startup');
DBMS_AQADM.START_QUEUE('pubsub.shutdown');
DBMS_AQADM.START_QUEUE('pubsub.logon');
DBMS_AQADM.START_QUEUE('pubsub.logoff');
DBMS_AQADM.START_QUEUE('pubsub.create_sch');
DBMS_AQADM.START_QUEUE ('pubsub.drop_sch');

end;
/

rem ---------------------------------------------------------------------------
rem
rem  define new_enqueue/new_np_enqueue for convenience
rem
rem ---------------------------------------------------------------------------


create or replace procedure new_enqueue(queue_name  in varchar2,
                                        payload  in raw ,
                                        correlation  in varchar2 := NULL,
                                        exception_queue  in varchar2 := NULL)
as

enq_ct     dbms_aq.enqueue_options_t;
msg_prop   dbms_aq.message_properties_t;
enq_msgid  raw(16);
userdata   raw(1000);

begin
    msg_prop.exception_queue := exception_queue;
    msg_prop.correlation := correlation;
    userdata := payload;

    DBMS_AQ.ENQUEUE(queue_name, enq_ct, msg_prop, userdata, enq_msgid);
end;
/
grant execute on new_enqueue to public;

create or replace procedure new_np_enqueue(queue  varchar2,
                                           id  integer,
                                           correlation  varchar2)
as

msgprop        dbms_aq.message_properties_t;
enqopt         dbms_aq.enqueue_options_t;
enq_msgid      raw(16);
payload        raw(10);

begin
    payload := hextoraw('123');
    enqopt.visibility:=dbms_aq.IMMEDIATE;
    msgprop.correlation:=correlation;
    DBMS_AQ.ENQUEUE( queue, enqopt, msgprop, payload, enq_msgid);
end;
/

grant execute on new_np_enqueue to public;

rem ---------------------------------------------------------------------------
rem
rem  create subscriptions to all event-publications for agent 'ADMIN'
rem
rem ---------------------------------------------------------------------------



DECLARE

subscriber sys.aq$_agent;

begin

subscriber := sys.aq$_agent('admin', null, null);

dbms_aqadm.add_subscriber(queue_name => 'pubsub.error',
                          subscriber => subscriber);

dbms_aqadm.add_subscriber(queue_name => 'pubsub.startup',
                          subscriber => subscriber);

dbms_aqadm.add_subscriber(queue_name => 'pubsub.shutdown',
                          subscriber => subscriber);

dbms_aqadm.add_subscriber(queue_name => 'pubsub.logoff',
                          subscriber => subscriber);

dbms_aqadm.add_subscriber(queue_name => 'pubsub.logon',
                          subscriber => subscriber);

dbms_aqadm.add_subscriber(queue_name => 'pubsub.create_sch',
                          subscriber => subscriber);

dbms_aqadm.add_subscriber(queue_name => 'pubsub.drop_sch',
                          subscriber => subscriber);
end;
/

rem ---------------------------------------------------------------------------
rem
rem  add subscriber with rule based on curent user name, using correlation_id
rem
rem ---------------------------------------------------------------------------

declare
subscriber sys.aq$_agent;
begin
subscriber := sys.aq$_agent('SNOOP', null, null);
dbms_aqadm.add_subscriber(queue_name => 'pubsub.logon',
                          subscriber         => subscriber,
                          rule => 'CORRID = ''EVENT'' ');
end;
/

rem ---------------------------------------------------------------------------
rem
rem  now create triggers on various events
rem
rem ---------------------------------------------------------------------------


rem  create trigger on after server errors
create or replace trigger systrig0
   AFTER SERVERERROR
   ON DATABASE
   begin
     new_enqueue('pubsub.error', hextoraw('9999'));
   end;
/

rem  create trigger on after startup
create or replace trigger systrig1
   AFTER STARTUP
   ON DATABASE
   begin
     new_enqueue('pubsub.startup', hextoraw('9999'));
   end;
/

rem  create trigger on after shutdown
create or replace trigger systrig2
   BEFORE SHUTDOWN
   ON DATABASE
   begin
     new_enqueue('pubsub.shutdown', hextoraw('9999'));
   end;
/

rem  create trigger on after logon
create or replace trigger systrig3
   AFTER LOGON
   ON DATABASE
   begin
      new_enqueue('pubsub.logon', hextoraw('9999'), dbms_standard.login_user);
   end;
/

rem  create trigger on before logoff
create or replace trigger systrig4
   BEFORE LOGOFF
   ON DATABASE
   begin
          new_enqueue('pubsub.logoff', hextoraw('9999'));
   end;
/

rem  create trigger on after create on sch
create or replace trigger systrig5
   AFTER CREATE
   ON SCHEMA
   begin
     new_np_enqueue('pubsub.create_sch', 1, 'non persistent');
   end;
/

rem  create trigger on after drop on sch

create or replace trigger systrig6
   AFTER DROP
   ON SCHEMA
   begin
       new_np_enqueue('pubsub.drop_sch', 1, 'non persistent');
   end;
/

disconnect
