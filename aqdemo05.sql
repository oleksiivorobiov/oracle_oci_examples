Rem
Rem $Header: aqdemo05.sql 25-sep-2001.11:19:28 lzhao Exp $
Rem
Rem aqdemo05.sql
Rem
Rem Copyright (c) 2000, 2001, Oracle Corporation.  All rights reserved.  
Rem
Rem    NAME
Rem      aqdemo05.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
rem     Listening on  input_queue for messages for
rem     agents "prog1" and "prog2" for approximately 2 minutes;
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    lzhao       09/25/01  -  set feedback on
Rem    rbhyrava    07/10/00 -  Bug 1319922
rem    mjaeger    07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem    kmeiyyap   01/27/99 - type agent_list_t changed to aq$_agent_list_t
rem    kmeiyyap   10/07/98 - Added waiting time for listen
rem    kmeiyyap   09/15/98 - Created
rem
set feedback on
connect aquser/aquser
set serveroutput on
set echo on
spool aqdemo05.log

DECLARE
    qlist dbms_aq.aq$_agent_list_t;
    agent_w_msg sys.aq$_agent;
    start_tx number;
    finish_tx number;
    listen_timeout exception;
    pragma exception_init(listen_timeout, -25254);

BEGIN

    qlist(0) := sys.aq$_agent('prog1', 'input_queue', NULL);
    qlist(1) := sys.aq$_agent('prog2', 'input_queue', NULL);

    dbms_output.put_line ('Listening on input_queue.');

    start_tx := dbms_utility.get_time;
    LOOP
        BEGIN

            finish_tx := dbms_utility.get_time;
            IF finish_tx - start_tx > 12000 THEN
                exit;
            END IF;

            DBMS_AQ.LISTEN(
                agent_list => qlist,
                wait => 30,
                agent =>  agent_w_msg);

            IF agent_w_msg.name = 'PROG1' THEN
                demo_dequeue('prog1');
            ELSIF agent_w_msg.name = 'PROG2' THEN
                demo_dequeue('prog2');
            END IF;

        EXCEPTION
            when listen_timeout THEN
                null;
        END;

    END LOOP;
END;
/

rem show the number of messages in prog1_processed_data table
select count(*) from prog1_processed_data;

rem show the number of messages in prog2_processed_data table
select count(*) from prog2_processed_data;


spool off






