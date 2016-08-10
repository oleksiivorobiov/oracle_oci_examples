Rem
Rem $Header: aqxmlhtp.sql 02-nov-2004.22:26:43 rbhyrava Exp $
Rem
Rem aqxmlhtp.sql
Rem
Rem Copyright (c) 2001, 2004, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      aqxmlhtp.sql - AQ xml HTTP Propagation Demo
Rem    
Rem    DESCRIPTION
Rem      args passed : PROTOCOL,HOST, PORT arguments with appropriate values 
Rem      PROTOCOL is http or https
Rem      HOST is webserver hostname
Rem      PORT is http/(s) port number 
Rem      for eg: http://my-pc:18081/aqserv/servlet/AQPropServlet
Rem      HOST is my-pc, PORT is 18081
Rem    NOTES
Rem      See aqxmlREADME.txt for setup instructions  
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    rbhyrava    11/02/04 - use oc4j 
Rem    rbhyrava    07/20/01 - args
Rem    rbhyrava    06/20/01 - comments- dblink creation
Rem    rbhyrava    05/09/01 - dequeue all messages
Rem    rbhyrava    04/12/01 - Merged rbhyrava_aqxmldemos
Rem    rbhyrava    03/30/01 - Created
Rem

SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100
SET ECHO ON

REM create database link using http protocol for aq http propagation demo
REM john is http user using propagation with password welcome

spool aqxmlhtp.log 

set serveroutput on
CONNECT / as sysdba ;
GRANT EXECUTE ON dbms_aqjms TO scott;
GRANT EXECUTE ON dbms_aqadm TO scott;
GRANT EXECUTE ON dbms_aqjms TO aqxmluser;
GRANT EXECUTE ON dbms_aqadm TO aqxmluser;
GRANT EXECUTE ON dbms_aq TO aqxmladmn ;

REM create database link using http protocol
REM ----------------------------------------

drop public database link dba ;

CREATE PUBLIC DATABASE LINK dba
   CONNECT TO '"aqdemo.com/john"'  IDENTIFIED BY "welcome" USING 
  '(DESCRIPTION=(ADDRESS=(PROTOCOL=&1)(HOST=&2)(PORT=&3)))' ;

REM Verify the dblink status


SELECT owner, db_link, username, host, created
  FROM dba_db_links
 WHERE username IS NOT NULL ;

connect aqxmladmn/aqxmladmn
set serveroutput on

REM setup remote subscribers 
BEGIN

  dbms_aqadm.remove_subscriber( queue_name=> 'CARS_Q1', 
           subscriber=> sys.aq$_agent(
              'HTTP_DEALS_CARS_Q1Q2','aqxmladmn.CARS_Q2@dba', NULL));

END;
/

BEGIN
  dbms_aqadm.add_subscriber( queue_name=> 'CARS_Q1', 
           subscriber=> sys.aq$_agent(
              'HTTP_DEALS_CARS_Q1Q2','aqxmladmn.CARS_Q2@dba', NULL));
END;
/

REM Schedule propagation 
BEGIN 
  dbms_aqadm.unschedule_propagation(queue_name => 'aqxmladmn.CARS_Q1', 
      destination => 'dba');
END;
/

BEGIN 
  dbms_aqadm.schedule_propagation(queue_name => 'aqxmladmn.CARS_Q1', 
      destination => 'dba');
END;
/

connect aqxmluser/aqxmluser 
set serveroutput on

REM Enqueue messages 

CREATE OR REPLACE PROCEDURE ENQUEUE_CARS( cars aqxmladmn.CARS_TYP, pri in number) as
enq_msgid      RAW(16);
eopt           dbms_aq.enqueue_options_t;
mprop          dbms_aq.message_properties_t;
rcpt           dbms_aq.aq$_recipient_list_t;

BEGIN

  mprop.priority := pri;
  
  dbms_aq.enqueue(
        queue_name => 'aqxmladmn.cars_q1',
        enqueue_options => eopt,
        message_properties => mprop,
        payload => cars,
        msgid => enq_msgid);
  dbms_output.put_line ('Enqueued ' || cars.car_details) ;
END ENQUEUE_CARS;
/
show errors 

DECLARE 
 carspayload aqxmladmn.cars_typ ;
BEGIN 
   carspayload := aqxmladmn.cars_typ(
             '3MEW123', 1999, 'BMW', 'RED','1999 Model BMW RED car', 45000) ;
   enqueue_cars(carspayload, 1) ;

   carspayload := aqxmladmn.cars_typ(
        '4MAT567', 2001, 'LEXUS', 'BLACK','2001 Model LEXUS BLACK car', 48000);
   enqueue_cars(carspayload, 2) ;

END;
/
commit ;

REM Dequeue Messages Now 


CREATE OR REPLACE PROCEDURE DEQUEUE_CARS(appname varchar2) AS
    deq_msgid           RAW(16);
    dopt                dbms_aq.dequeue_options_t;
    mprop               dbms_aq.message_properties_t;
    payload             aqxmladmn.cars_typ;
    no_messages         exception;
    pragma exception_init(no_messages, -25228);

BEGIN

    dopt.consumer_name := appname;
    dopt.wait := 30;
    dopt.navigation := DBMS_AQ.FIRST_MESSAGE;
    loop
    dbms_aq.dequeue(
        queue_name => 'aqxmladmn.cars_q2',
        dequeue_options => dopt,
        message_properties => mprop,
        payload => payload,
        msgid => deq_msgid);

        dbms_output.put_line ('-Carno:' ||  payload.carno  || 
                              ' Year :' ||  payload.year  || 
                              ' Model:' ||  payload.Model || 
                              ' Color:' ||  payload.color || 
                              ' Price:' ||  payload.Price ) ;

    commit;
    end loop;

EXCEPTION
    WHEN no_messages THEN
    dbms_output.put_line('No more messages in queue ');
    commit;

END DEQUEUE_CARS;
/
show errors 

execute dbms_lock.sleep(100) ;

BEGIN 
    DEQUEUE_CARS ('HTTP_DEALS_CARS_Q1Q2' ) ;
END;
/
commit ;
spool off
