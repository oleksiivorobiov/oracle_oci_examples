Rem
Rem $Header: aqdemo08.sql 16-nov-2004.16:35:23 rbhyrava Exp $
Rem
Rem aqdemo08.sql
Rem
Rem Copyright (c) 2001, 2004, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      aqdemo08.sql - AQ Notifications Demo 
Rem
Rem    DESCRIPTION
Rem       Make sure the database is started with following parameters
Rem        aq_tm_processes =2 
Rem        job_queue_processes=2
Rem        compatible=8.1.0 # or above 
Rem       Modify the email host , port and sendfrom .
Rem       Modify the email you@company.com to valid email address
Rem    NOTES
Rem      This demo does the following 
Rem       - setup mail server - change mailhost and sender email address
Rem       - setup users/queues/queue tables 
Rem       - Create callback functions used in registration 
Rem       - Register for event notification for the subscriber ADMIN 
Rem       - Registrations are added using default presentation  
Rem            and xml presentation 
Rem       - Do enqueues 
Rem       - Verify notifications 
Rem       - Cleanup
Rem 
Rem 
Rem    MODIFIED   (MM/DD/YY)
Rem    rbhyrava    11/16/04 - user
Rem    ksurlake    06/10/04 - 3229354: Wait before unregistering
Rem    rbhyrava    05/16/01 - fix typo /
Rem    rbhyrava    04/29/01 - Merged rbhyrava_aqxmltype_demos
Rem    rbhyrava    04/27/01 - Created
Rem

spool aqdemo08.log
SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100
SET ECHO ON

CONNECT sys/change_on_install as sysdba;
SET SERVEROUTPUT ON
SET ECHO ON

Rem set the mailserver etc.
call dbms_aqelm.set_mailhost('youmailhost.company.com');
call dbms_aqelm.set_mailport(25);
call dbms_aqelm.set_sendfrom('you@company.com');

Rem user pubsub1 is used for registering on a queue
DROP USER pubsub1 CASCADE;
CREATE USER pubsub1 IDENTIFIED BY pubsub1;

Rem grant all the roles to pubsub1
GRANT connect, resource, dba TO pubsub1;
GRANT aq_administrator_role, aq_user_role TO pubsub1;
GRANT EXECUTE ON dbms_aq TO pubsub1;
GRANT EXECUTE ON dbms_aqadm TO pubsub1;
EXECUTE dbms_aqadm.grant_type_access('pubsub1');
EXECUTE dbms_aqadm.grant_system_privilege('ENQUEUE_ANY','pubsub1',FALSE);
EXECUTE dbms_aqadm.grant_system_privilege('DEQUEUE_ANY','pubsub1',FALSE);

CONNECT pubsub1/pubsub1;

rem stop the adt queue
BEGIN
DBMS_AQADM.STOP_QUEUE('pubsub1.adtevents');
END;
/

rem drop the adt queue
BEGIN
DBMS_AQADM.DROP_QUEUE(QUEUE_NAME=>'pubsub1.adtevents');
END;
/

rem drop the adt queue table
BEGIN
DBMS_AQADM.DROP_QUEUE_TABLE(QUEUE_TABLE => 'pubsub1.adt_msg_table', force => TRUE);
END;
/

rem create the adt
CREATE OR REPLACE TYPE adtmsg AS OBJECT (id NUMBER, data VARCHAR2(4000)) ;
/

rem create the raw queue table
BEGIN
DBMS_AQADM.CREATE_QUEUE_TABLE(
    QUEUE_TABLE=>'pubsub1.raw_msg_table',
    MULTIPLE_CONSUMERS => TRUE,
    QUEUE_PAYLOAD_TYPE =>'RAW',
    COMPATIBLE => '8.1.3');
END;
/

rem creat the adt queue table
BEGIN
DBMS_AQADM.CREATE_QUEUE_TABLE(
    QUEUE_TABLE=>'pubsub1.adt_msg_table',
    MULTIPLE_CONSUMERS => TRUE,
    QUEUE_PAYLOAD_TYPE =>'ADTMSG',
    COMPATIBLE => '8.1.3');
END;
/
rem  Create a queue for raw events
BEGIN
DBMS_AQADM.CREATE_QUEUE(QUEUE_NAME=>'pubsub1.events',
            QUEUE_TABLE=>'pubsub1.raw_msg_table',
            COMMENT=>'Q for events triggers');
END;
/

rem  Create a queue for adt events
BEGIN
DBMS_AQADM.CREATE_QUEUE(QUEUE_NAME=>'pubsub1.adtevents',
            QUEUE_TABLE=>'pubsub1.adt_msg_table',
            COMMENT=>'Q for adt events triggers');
END;
/

rem start the queues
BEGIN
DBMS_AQADM.START_QUEUE('pubsub1.events');
END; 
/

BEGIN
DBMS_AQADM.START_QUEUE('pubsub1.adtevents');
END;
/
rem Create a non-persistent queue for events
BEGIN
 DBMS_AQADM.CREATE_NP_QUEUE(QUEUE_NAME=>'pubsub1.nonperevents',
            MULTIPLE_CONSUMERS => TRUE);
END;
/

rem start the np queue
BEGIN
DBMS_AQADM.START_QUEUE('pubsub1.nonperevents');
END;
/

rem procedure to enqueue raw into persistent queue
CREATE OR REPLACE PROCEDURE new_rawenqueue(queue_name  IN VARCHAR2,
                                     correlation  IN VARCHAR2 := NULL,
                                     exception_queue  IN VARCHAR2 := NULL)
AS

enq_ct     dbms_aq.enqueue_options_t;
msg_prop   dbms_aq.message_properties_t;
enq_msgid  RAW(16);
userdata   RAW(1000);

BEGIN
    msg_prop.exception_queue := exception_queue;
    msg_prop.correlation := correlation;
    userdata := hextoraw('666');

    DBMS_AQ.ENQUEUE(queue_name, enq_ct, msg_prop, userdata, enq_msgid);
END;
/
GRANT EXECUTE ON new_rawenqueue TO PUBLIC;

rem procedure to enqueue adt into persistent queue
CREATE OR REPLACE PROCEDURE new_adtenqueue(queue_name  IN VARCHAR2,
                                     correlation  IN VARCHAR2 := NULL,
                                     exception_queue  IN VARCHAR2 := NULL)
AS

enq_ct     dbms_aq.enqueue_options_t;
msg_prop   dbms_aq.message_properties_t;
enq_msgid  raw(16);
payload    adtmsg;

BEGIN
    msg_prop.exception_queue := exception_queue;
    msg_prop.correlation := correlation;
    payload := adtmsg(1, 'p queue Hello World!');

    DBMS_AQ.ENQUEUE(queue_name, enq_ct, msg_prop, payload, enq_msgid);
END;
/
GRANT EXECUTE ON new_adtenqueue TO PUBLIC;

rem create procedure to enqueue raw into np queue
CREATE OR REPLACE PROCEDURE new_np_rawenqueue(queue  VARCHAR2,
                                           id  INTEGER,
                                           correlation  VARCHAR2)
AS

msgprop        dbms_aq.message_properties_t;
enqopt         dbms_aq.enqueue_options_t;
enq_msgid      RAW(16);
payload        RAW(10);

BEGIN
    payload := hextoraw('999');
    enqopt.visibility:=dbms_aq.IMMEDIATE;
    msgprop.correlation:=correlation;
    DBMS_AQ.ENQUEUE( queue, enqopt, msgprop, payload, enq_msgid);
END;
/
GRANT EXECUTE ON new_np_rawenqueue TO PUBLIC;

rem create procedure to enqueue adt into np queue
CREATE OR REPLACE PROCEDURE new_np_adtenqueue(queue  VARCHAR2,
                                           id  INTEGER,
                                           correlation  VARCHAR2)
AS

msgprop        dbms_aq.message_properties_t;
enqopt         dbms_aq.enqueue_options_t;
enq_msgid      raw(16);
payload        adtmsg;

BEGIN
    payload := adtmsg(1, 'np queue Hello World!');
    enqopt.visibility:=dbms_aq.IMMEDIATE;
    msgprop.correlation:=correlation;
    DBMS_AQ.ENQUEUE( queue, enqopt, msgprop, payload, enq_msgid);
END;
/
GRANT EXECUTE ON new_np_adtenqueue TO PUBLIC;

DECLARE
   subscriber sys.aq$_agent;

BEGIN

   subscriber := sys.aq$_agent('admin', null, null);

   dbms_aqadm.add_subscriber(queue_name => 'pubsub1.events',
                          subscriber => subscriber);

   dbms_aqadm.add_subscriber(queue_name => 'pubsub1.adtevents',
                          subscriber => subscriber);

   dbms_aqadm.add_subscriber(queue_name => 'pubsub1.nonperevents',
                          subscriber => subscriber);

END;
/

SET ECHO ON;
CONNECT / as sysdba;
set serveroutput on;

DROP TABLE plsqlregtr;
CREATE TABLE plsqlregtr
(
  descr    sys.aq$_descriptor,
  reginfo  sys.aq$_reg_info,
  payload  RAW(2000),
  payloadl NUMBER
);

GRANT ALL ON plsqlregtr TO PUBLIC;

DROP TABLE plsqlregta;
CREATE TABLE plsqlregta
(
  descr    sys.aq$_descriptor,
  reginfo  sys.aq$_reg_info,
  payload  VARCHAR2(4000),
  payloadl NUMBER
);

GRANT ALL ON plsqlregta TO PUBLIC;

CONNECT pubsub1/pubsub1

CREATE OR REPLACE PROCEDURE plsqlregproc1(
   context RAW , reginfo sys.aq$_reg_info, descr sys.aq$_descriptor, 
   payload RAW,  payloadl NUMBER)
AS
BEGIN
  INSERT INTO sys.plsqlregtr (descr, reginfo, payload, payloadl) 
         VALUES (descr, reginfo, payload, payloadl);
END;
/

CREATE OR REPLACE PROCEDURE plsqlregproc2(
   context RAW , reginfo sys.aq$_reg_info, descr sys.aq$_descriptor, 
   payload VARCHAR2,  payloadl NUMBER)
AS
BEGIN
  INSERT INTO sys.plsqlregta (descr, reginfo, payload, payloadl) 
         VALUES (descr, reginfo, payload, payloadl);
END;
/
rem  Do all the registerations
SET ECHO ON;
CONNECT pubsub1/pubsub1;
SET SERVEROUTPUT ON;

DECLARE

  reginfo1            sys.aq$_reg_info;
  reginfo2            sys.aq$_reg_info;
  reginfo3            sys.aq$_reg_info;
  reginfo4            sys.aq$_reg_info;
  reginfo5            sys.aq$_reg_info;
  reginfo6            sys.aq$_reg_info;
  reginfolist         sys.aq$_reg_info_list;

BEGIN
-- register for p raw q default pres
  reginfo1 := sys.aq$_reg_info('PUBSUB1.EVENTS:ADMIN',1,'plsql://plsqlregproc1',HEXTORAW('FF'));

-- register for p raw q xml pres
  reginfo2 := sys.aq$_reg_info('PUBSUB1.EVENTS:ADMIN',1,'plsql://plsqlregproc1?PR=1',HEXTORAW('FF'));

-- register for p adt q default pres
  reginfo3 := sys.aq$_reg_info('PUBSUB1.ADTEVENTS:ADMIN',1,'plsql://plsqlregproc2',HEXTORAW('FF'));

-- register for p adt q xml pres
  reginfo4 := sys.aq$_reg_info('PUBSUB1.ADTEVENTS:ADMIN',1,'plsql://plsqlregproc2?PR=1',HEXTORAW('FF'));

-- for np q raw and adt can be enqueued into the same queue
-- register for np raw and adt q default pres
  reginfo5 := sys.aq$_reg_info('PUBSUB1.NONPEREVENTS:ADMIN',1,'plsql://plsqlregproc1',HEXTORAW('FF'));

-- register for np raw and adt q xml pres
  reginfo6 := sys.aq$_reg_info('PUBSUB1.NONPEREVENTS:ADMIN',1,'plsql://plsqlregproc2?PR=1',HEXTORAW('FF'));

  reginfolist := sys.aq$_reg_info_list(reginfo1);
  reginfolist.EXTEND;
  reginfolist(2) := reginfo2;
  reginfolist.EXTEND;
  reginfolist(3) := reginfo3;
  reginfolist.EXTEND;
  reginfolist(4) := reginfo4;
  reginfolist.EXTEND;
  reginfolist(5) := reginfo5;
  reginfolist.EXTEND;
  reginfolist(6) := reginfo6;

  sys.dbms_aq.register(reginfolist, 6);

  commit;

-- registerations are done

END;
/

Rem Do all the registerations
CONNECT pubsub1/pubsub1;
SET ECHO ON;
SET SERVEROUTPUT ON;

DECLARE

  reginfo1            sys.aq$_reg_info;
  reginfo2            sys.aq$_reg_info;
  reginfo3            sys.aq$_reg_info;
  reginfo4            sys.aq$_reg_info;
  reginfo5            sys.aq$_reg_info;
  reginfo6            sys.aq$_reg_info;
  reginfolist         sys.aq$_reg_info_list;

BEGIN
-- register for p raw q default pres
  reginfo1 := sys.aq$_reg_info('PUBSUB1.EVENTS:ADMIN',1,'mailto://you@company.com',HEXTORAW('FF'));

-- register for p raw q xml pres
  reginfo2 := sys.aq$_reg_info('PUBSUB1.EVENTS:ADMIN',1,'mailto://you@company.com?PR=1',HEXTORAW('FF'));

-- register for p adt q default pres
  reginfo3 := sys.aq$_reg_info('PUBSUB1.ADTEVENTS:ADMIN',1,'mailto://you@company.com',HEXTORAW('FF'));

-- register for p adt q xml pres
  reginfo4 := sys.aq$_reg_info('PUBSUB1.ADTEVENTS:ADMIN',1,'mailto://you@company.com?PR=1',HEXTORAW('FF'));

-- for np q raw and adt can be enqueued into the same queue
-- register for np raw and adt q default pres
  reginfo5 := sys.aq$_reg_info('PUBSUB1.NONPEREVENTS:ADMIN',1,'mailto://you@company.com',HEXTORAW('FF'));

-- register for np raw and adt q xml pres
  reginfo6 := sys.aq$_reg_info('PUBSUB1.NONPEREVENTS:ADMIN',1,'mailto://you@company.com?PR=1',HEXTORAW('FF'));

  reginfolist := sys.aq$_reg_info_list(reginfo1);
  reginfolist.EXTEND;
  reginfolist(2) := reginfo2;
  reginfolist.EXTEND;
  reginfolist(3) := reginfo3;
  reginfolist.EXTEND;
  reginfolist(4) := reginfo4;
  reginfolist.EXTEND;
  reginfolist(5) := reginfo5;
  reginfolist.EXTEND;
  reginfolist(6) := reginfo6;

  sys.dbms_aq.register(reginfolist, 6);

  COMMIT;

-- registrations are done

END;
/
CONNECT pubsub1/pubsub1;
SET ECHO ON;
SET SERVEROUTPUT ON;

DECLARE
BEGIN
-- wait for registerations to happen
--  dbms_lock.sleep(90);

-- now start enqueing

-- raw into p queue
  new_rawenqueue('PUBSUB1.EVENTS', 'PR CORRELATION STRING', 'PREQ');
  commit;

-- adt into p queue
  new_adtenqueue('PUBSUB1.ADTEVENTS', 'PA CORRELATION STRING', 'PAEQ');
  commit;

-- raw into np queue
  new_np_rawenqueue('PUBSUB1.NONPEREVENTS', 1, 'NPR CORRELATION STRING'); 
  commit;

-- adt into np queue
  new_np_adtenqueue('PUBSUB1.NONPEREVENTS', 1, 'NPA CORRELATION STRING');
  commit;

END;
/

DECLARE
BEGIN
-- wait for PL/SQL callbacks to be invoked
  dbms_lock.sleep(120);
END;
/

set echo on;
CONNECT pubsub1/pubsub1;
SET SERVEROUTPUT ON;

SELECT count(*) FROM sys.plsqlregtr t;

SELECT count(*) FROM sys.plsqlregta t;

REM Do all the unregisterations
CONNECT pubsub1/pubsub1;
SET ECHO ON;
SET SERVEROUTPUT ON;

DECLARE

  reginfo1            sys.aq$_reg_info;
  reginfo2            sys.aq$_reg_info;
  reginfo3            sys.aq$_reg_info;
  reginfo4            sys.aq$_reg_info;
  reginfo5            sys.aq$_reg_info;
  reginfo6            sys.aq$_reg_info;
  reginfolist         sys.aq$_reg_info_list;

BEGIN
-- register for p raw q default pres
  reginfo1 := sys.aq$_reg_info('PUBSUB1.EVENTS:ADMIN',1,'plsql://plsqlregproc1',HEXTORAW('FF'));

-- register for p raw q xml pres
  reginfo2 := sys.aq$_reg_info('PUBSUB1.EVENTS:ADMIN',1,'plsql://plsqlregproc1?PR=1',HEXTORAW('FF'));

-- register for p adt q default pres
  reginfo3 := sys.aq$_reg_info('PUBSUB1.ADTEVENTS:ADMIN',1,'plsql://plsqlregproc2',HEXTORAW('FF'));

-- register for p adt q xml pres
  reginfo4 := sys.aq$_reg_info('PUBSUB1.ADTEVENTS:ADMIN',1,'plsql://plsqlregproc2?PR=1',HEXTORAW('FF'));

-- for np q raw and adt can be enqueued into the same queue
-- register for np raw and adt q default pres
  reginfo5 := sys.aq$_reg_info('PUBSUB1.NONPEREVENTS:ADMIN',1,'plsql://plsqlregproc1',HEXTORAW('FF'));

-- register for np raw and adt q xml pres
  reginfo6 := sys.aq$_reg_info('PUBSUB1.NONPEREVENTS:ADMIN',1,'plsql://plsqlregproc2?PR=1',HEXTORAW('FF'));

  reginfolist := sys.aq$_reg_info_list(reginfo1);
  reginfolist.EXTEND;
  reginfolist(2) := reginfo2;
  reginfolist.EXTEND;
  reginfolist(3) := reginfo3;
  reginfolist.EXTEND;
  reginfolist(4) := reginfo4;
  reginfolist.EXTEND;
  reginfolist(5) := reginfo5;
  reginfolist.EXTEND;
  reginfolist(6) := reginfo6;

  sys.dbms_aq.unregister(reginfolist, 6);

  COMMIT;

-- unregisterations are done

END;
/
REM Do all the unregisterations
CONNECT pubsub1/pubsub1;
SET ECHO ON;
SET SERVEROUTPUT ON;

DECLARE

  reginfo1            sys.aq$_reg_info;
  reginfo2            sys.aq$_reg_info;
  reginfo3            sys.aq$_reg_info;
  reginfo4            sys.aq$_reg_info;
  reginfo5            sys.aq$_reg_info;
  reginfo6            sys.aq$_reg_info;
  reginfolist         sys.aq$_reg_info_list;

BEGIN
-- register for p raw q default pres
  reginfo1 := sys.aq$_reg_info('PUBSUB1.EVENTS:ADMIN',1,'mailto://you@company.com',HEXTORAW('FF'));

-- register for p raw q xml pres
  reginfo2 := sys.aq$_reg_info('PUBSUB1.EVENTS:ADMIN',1,'mailto://you@company.com?PR=1',HEXTORAW('FF'));

-- register for p adt q default pres
  reginfo3 := sys.aq$_reg_info('PUBSUB1.ADTEVENTS:ADMIN',1,'mailto://you@company.com',HEXTORAW('FF'));

-- register for p adt q xml pres
  reginfo4 := sys.aq$_reg_info('PUBSUB1.ADTEVENTS:ADMIN',1,'mailto://you@company.com?PR=1',HEXTORAW('FF'));

-- for np q raw and adt can be enqueued into the same queue
-- register for np raw and adt q default pres
  reginfo5 := sys.aq$_reg_info('PUBSUB1.NONPEREVENTS:ADMIN',1,'mailto://you@company.com',HEXTORAW('FF'));

-- register for np raw and adt q xml pres
  reginfo6 := sys.aq$_reg_info('PUBSUB1.NONPEREVENTS:ADMIN',1,'mailto://you@company.com?PR=1',HEXTORAW('FF'));

  reginfolist := sys.aq$_reg_info_list(reginfo1);
  reginfolist.EXTEND;
  reginfolist(2) := reginfo2;
  reginfolist.EXTEND;
  reginfolist(3) := reginfo3;
  reginfolist.EXTEND;
  reginfolist(4) := reginfo4;
  reginfolist.EXTEND;
  reginfolist(5) := reginfo5;
  reginfolist.EXTEND;
  reginfolist(6) := reginfo6;

  sys.dbms_aq.unregister(reginfolist, 6);

  COMMIT;

-- unregisterations are done

END;
/


CONNECT sys/change_on_install as  sysdba;

drop user pubsub1 cascade ;
drop table plsqlregtr ;
drop table plsqlregta ;

spool off
exit ;
