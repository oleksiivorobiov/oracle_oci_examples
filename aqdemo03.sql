Rem
Rem $Header: template.sql 06-feb-96.13:23:14 kosinski Exp $
Rem
Rem aqdemo03.sql
Rem
Rem  Copyright (c) Oracle Corporation 2000. All Rights Reserved.
Rem
Rem    NAME
Rem      aqdemo03.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
rem      This file loads the dequeue packages demo_dequeue and
rem      demo_prop_dequeue
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    rbhyrava    07/10/00 - bug: 1319922
rem    mjaeger    07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem    kmeiyyap   10/07/98 - added wait time for dequeue
rem    kmeiyyap   09/15/98 - Created
rem

connect aquser/aquser
set echo on
set serveroutput on


CREATE OR REPLACE PROCEDURE DEMO_DEQUEUE(appname varchar2) AS
    deq_msgid           RAW(16);
    dopt                dbms_aq.dequeue_options_t;
    mprop               dbms_aq.message_properties_t;
    payload             message;
    no_messages         exception;
    pragma exception_init(no_messages, -25228);

BEGIN

    dopt.consumer_name := appname;
    dopt.wait := 30;
    dopt.navigation := DBMS_AQ.FIRST_MESSAGE;


    dbms_aq.dequeue(
        queue_name => 'input_queue',
        dequeue_options => dopt,
        message_properties => mprop,
        payload => payload,
        msgid => deq_msgid);

    IF appname = 'prog1' THEN
        insert into  prog1_processed_data
        values (payload.id, payload.city, payload.priority);
    ELSIF appname = 'prog2' THEN
        insert into  prog2_processed_data
        values (payload.id, payload.city, payload.priority);
    END IF;

    commit;

EXCEPTION
    WHEN no_messages THEN
    dbms_output.put_line('No more messages in queue ');
    commit;

END demo_dequeue;
/

CREATE OR REPLACE PROCEDURE DEMO_PROP_DEQUEUE (appname varchar2) AS
    deq_msgid           RAW(16);
    dopt                dbms_aq.dequeue_options_t;
    mprop               dbms_aq.message_properties_t;
    payload             message;
    no_messages         exception;
    pragma exception_init(no_messages, -25228);
    start_tx            NUMBER;
    finish_tx           NUMBER;

BEGIN

    dopt.consumer_name := appname;
    dopt.wait := DBMS_AQ.NO_WAIT;
    dopt.navigation := DBMS_AQ.FIRST_MESSAGE;

    start_tx := dbms_utility.get_time;
    LOOP

        BEGIN
            finish_tx := dbms_utility.get_time;
            IF finish_tx - start_tx > 12000 THEN
                exit;
            END IF;

            dbms_aq.dequeue(
                queue_name => 'prop_queue',
                dequeue_options => dopt,
                message_properties => mprop,
                payload => payload,
                msgid => deq_msgid);

            IF appname = 'prog3' THEN
                insert into  prog3_processed_data
                values (payload.id, payload.city, payload.priority);
            END IF;

            commit;

        EXCEPTION
            WHEN no_messages THEN
            null;
        END;

    END LOOP;
END demo_prop_dequeue;
/

