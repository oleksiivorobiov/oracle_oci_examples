Rem
Rem $Header: aqdemo07.sql 16-nov-2004.16:33:26 rbhyrava Exp $
Rem
Rem aqdemo07.sql
Rem
Rem Copyright (c) 2001, 2004, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      aqdemo07.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem      Demonstrate Enqueue/Dequeue to queues using XMLType
Rem      
Rem
Rem    NOTES
Rem     To create Queue table with ADT containing XmlType : 
Rem     the database compatility should be 9.0.0 or higher 
Rem
Rem     Restart the database after adding the following line to init.ora
Rem     compatible=9.0.0
Rem 
Rem    MODIFIED   (MM/DD/YY)
Rem    rbhyrava    11/16/04 - user
Rem    rbhyrava    10/15/03 - 
Rem    rbhyrava    10/15/03 - sys user 
Rem    rbhyrava    04/29/01 - Merged rbhyrava_aqxmltype_demos
Rem    rbhyrava    04/26/01 - Created
Rem

SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100
SET ECHO ON

set serveroutput on
spool aqdemo07.log

rem ====================================================================
rem create a queue user
rem ====================================================================
CONNECT system/manager;

CREATE USER aqadmn IDENTIFIED by aqadmn;
CREATE USER aquser IDENTIFIED by aquser;
GRANT CONNECT, RESOURCE, AQ_USER_ROLE TO aquser;
GRANT CONNECT, RESOURCE, AQ_ADMINISTRATOR_ROLE TO aqadmn;

GRANT EXECUTE ON dbms_aq TO aquser;
GRANT EXECUTE ON dbms_aqadm TO aqadmn ;

CONNECT aqadmn/aqadmn;
set serveroutput on;

rem ====================================================================
rem create a type
rem ====================================================================

/* create a message type  */
CREATE OR REPLACE TYPE message AS OBJECT (
            id               NUMBER,
            data             VARCHAR2(30),
            myxmldata        sys.XMLType) ;
/

GRANT EXECUTE ON message TO aquser ;

rem ====================================================================
rem create queue table and queue 
rem ====================================================================

/* create and start a multiple consumer queue */
BEGIN
   DBMS_AQADM.CREATE_QUEUE_TABLE (
    QUEUE_TABLE      => 'xmltypemsgqtab',
    MULTIPLE_CONSUMERS => TRUE,
    QUEUE_PAYLOAD_TYPE => 'message') ;

   DBMS_AQADM.CREATE_QUEUE(
    QUEUE_NAME=>'msg_queue',
    QUEUE_TABLE =>'xmltypemsgqtab');

   DBMS_AQADM.START_QUEUE ( QUEUE_NAME => 'msg_queue');
END;
/

 
rem ====================================================================
REM Add rule based subscribers using message selectors
rem ====================================================================

BEGIN 
 -- subscriber - get first three Ranks from each class from the XML message 
 -- rule based subscription 
   DBMS_AQADM.ADD_SUBSCRIBER('msg_queue',
          SYS.AQ$_AGENT('SUB1', null, null ),
   'XMLType.extract(tab.user_data.myxmldata,
           ''/STUDENT/RANK/text()'').getNumberVal() between 1 and 3') ;

   -- another subscriber 
   DBMS_AQADM.ADD_SUBSCRIBER('msg_queue', 
        SYS.AQ$_AGENT('SUB2', null, null )) ;

END;
/

REM GRANT ENQUEUE/DEQUEUE privileges to AQUSER on Queue
BEGIN 

   dbms_aqadm.grant_queue_privilege('ENQUEUE','msg_queue', 'aquser',FALSE);
   dbms_aqadm.grant_queue_privilege('DEQUEUE','msg_queue', 'aquser',FALSE);
END;
/

CONNECT aquser/aquser ;
set serveroutput on size 200000;

REM Do enqueues using XMLType 

CREATE OR REPLACE PROCEDURE MSGENQ(qnm VARCHAR2, 
                                   txnno   NUMBER,
                                   nmesgs  NUMBER) 
AS
enq_msgid      RAW(16);
eopt           dbms_aq.enqueue_options_t;
mprop          dbms_aq.message_properties_t;
enq_userdata   aqadmn.message;
xd    sys.XMLType ;
begin

FOR i in 1..nmesgs LOOP
  mprop.priority := 10*txnno + i;
  xd := sys.XMLType.createXML('<STUDENT><CLASS>' || txnno || '</CLASS><RANK>'
           || i || '</RANK></STUDENT>');

  enq_userdata := aqadmn.message(
                     txnno, 'Class: ' || txnno || ' Rank#: ' || i,xd );
  dbms_aq.enqueue(
        queue_name => qnm,
        enqueue_options => eopt,
        message_properties => mprop,
        payload => enq_userdata,
        msgid => enq_msgid);
END LOOP;
commit;
END;
/
SHOW ERRORS 

rem ====================================================================
REM Enqueue messages 
rem ====================================================================
Rem Now Enqueue some messages 
execute MSGENQ('AQADMN.msg_queue', 1, 5);
execute MSGENQ('AQADMN.msg_queue', 2, 6);
execute MSGENQ('AQADMN.msg_queue', 3, 7);
execute MSGENQ('AQADMN.msg_queue', 4, 8);
execute MSGENQ('AQADMN.msg_queue', 5, 9);


REM Now dequeue messages for the subscribers 

CREATE OR REPLACE PROCEDURE MSGDEQ(appname varchar2, 
                                   qname varchar2, 
                                   cond varchar2) AS
dequeue_options     dbms_aq.dequeue_options_t;
message_properties  dbms_aq.message_properties_t;
message_handle      RAW(16);
payload             aqadmn.message;

no_messages   exception;

pragma exception_init (no_messages, -25228);

BEGIN
dequeue_options.wait       := 30;
dequeue_options.navigation := DBMS_AQ.FIRST_MESSAGE;
dequeue_options.consumer_name := appname;

dbms_output.put_line('\nDequeue from : ' || qname || ' subscriber ' || appname);
if ( cond is not null) then 
  dbms_output.put_line ('  ->Using Dequeue Condition: ' || cond) ;
  dequeue_options.deq_condition := cond ;

dbms_output.put_line('') ;
end if ;

LOOP
   BEGIN
   dbms_aq.dequeue(queue_name              => qname,
                   dequeue_options         => dequeue_options,
                   message_properties      => message_properties,
                   payload                 => payload,
                   msgid                   => message_handle);

   dbms_output.put('Id:' ||  payload.id || ' Data:' || payload.data || ' ');
   dbms_output.put_line(' Myxmldata:'  || payload.myxmldata.getStringVal());
   dequeue_options.navigation := DBMS_AQ.NEXT_MESSAGE;
  END;
END LOOP;
EXCEPTION
  WHEN no_messages THEN
    dbms_output.put_line ('No more messages');
END;
/

SHOW ERRORS

rem ====================================================================
REM Dequeue messages 
rem ====================================================================

BEGIN 

      MSGDEQ('SUB1', 'AQADMN.MSG_QUEUE' , null) ;

     -- Dequeue for messages which has XMLType data matching the condition 
     -- XML should contain CLASS tag  AND  CLASS should be in 4 or 5
     -- Specify rule during dequeue 

      MSGDEQ('SUB2', 'AQADMN.MSG_QUEUE', 
 'XMLType.existsNode(tab.user_data.myxmldata,''/STUDENT/CLASS'')=1 AND ' ||  
 ' XMLType.extract(tab.user_data.myxmldata, ''/STUDENT/CLASS/text()'').getNumberVal() in ( 4, 5)') ;

END;
/

REM ===============================================================
REM CLEANUP QUEUES/USERS 
REM ===============================================================

connect aqadmn/aqadmn 
execute dbms_aqadm.stop_queue ('msg_queue') ;
execute dbms_aqadm.drop_queue ('msg_queue') ;
execute dbms_aqadm.drop_queue_table ('xmltypemsgqtab' , TRUE) ;

DROP TYPE message ;

CONNECT sys/change_on_install AS SYSDBA;

drop user aquser cascade ;
drop user aqadmn cascade ;

spool off
