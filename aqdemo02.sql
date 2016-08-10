Rem
Rem $Header: template.sql 06-feb-96.13:23:14 kosinski Exp $
Rem
Rem aqdemo02.sql
Rem
Rem  Copyright (c) Oracle Corporation 2000. All Rights Reserved.
Rem
Rem    NAME
Rem      aqdemo02.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
rem      This file loads the enqueue package and enqueues 100 messages
rem      (10 messages are enqueued every 3 seconds to a maximum of 100
rem      messages).
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    rbhyrava    07/10/00 - Created - bug: 1319922
rem    mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem    kmeiyyap    01/27/99 - spool to aqdemo02.log
rem    kmeiyyap    09/15/98 - Created
rem
rem
rem

connect aquser/aquser
set serveroutput on
set echo on
spool aqdemo02.log

CREATE OR REPLACE PROCEDURE DEMO_ENQUEUE (userinfo message,
                                          priority number) AS

    enq_msgid   RAW(16);
    eopt                dbms_aq.enqueue_options_t;
    mprop               dbms_aq.message_properties_t;

BEGIN
    mprop.priority := priority;
    dbms_aq.enqueue(
        queue_name => 'input_queue',
        enqueue_options => eopt,
        message_properties => mprop,
        payload => userinfo,
        msgid => enq_msgid);

    commit;

END demo_enqueue;
/

DECLARE
    payload message;
    city1 varchar2(30) := 'BELMONT';
    city2 varchar2(30) := 'REDWOOD SHORES';
    city3 varchar2(30) := 'SUNNYVALE';
    city4 varchar2(30) := 'BURLINGAME';

BEGIN
    for i in 1..100 LOOP
            IF mod (i, 3) = 0 THEN
                payload := message(i, city1, mod(i, 3) + 1);
            ELSIF mod(i, 4) = 0 THEN
                payload := message(i, city2, mod(i, 3) + 1);
            ELSIF mod(i, 2) = 0 THEN
                payload := message(i, city3, mod(i, 3) + 1);
            ELSE
                payload := message(i, city4, mod(i, 3) + 1);
            END IF;

            demo_enqueue(payload, (mod(i, 3) + 1));

            IF mod (i, 10) = 0 THEN
                dbms_lock.sleep(3);
            END IF;

    END LOOP;
END;
/


spool off
