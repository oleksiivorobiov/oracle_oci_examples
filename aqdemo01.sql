Rem
Rem $Header: template.sql 06-feb-96.13:23:14 kosinski Exp $
Rem
Rem aqdemo01.sql
Rem
Rem  Copyright (c) Oracle Corporation 2000. All Rights Reserved.
Rem
Rem    NAME
Rem      aqdemo01.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
rem      This file creates queue tables, queues, subscribers needed for
rem      the demo. Propagation is scheduled between input_queue and other
rem      queues in the same database
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    rbhyrava    07/10/00 - Created - bug :1319922
rem    mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem    kmeiyyap    09/15/98 - Created
Rem

rem Create Queue table, Queues, Subscribers

connect aquser/aquser
set serveroutput on
set echo on

rem ==============================
rem Error Handling Routine
rem ==============================

CREATE or REPLACE PROCEDURE catch_error ( error_code in number,
                                          error_string in varchar2)
AS
BEGIN
    dbms_output.put_line('Oracle Server Error = '|| to_char (error_code));
    dbms_output.put_line('Oracle Server Message = '|| error_string);
END;
/



rem ==========================================
rem Stop Queue input_queue
rem ==========================================

DECLARE
BEGIN
    dbms_output.put_line ('Stopping  Queue input_queue...');
    dbms_aqadm.stop_queue
    (
        queue_name => 'input_queue',
        wait       => TRUE
    );

    dbms_output.put_line ('Stopped Queue input_queue.');

exception
    when others then
        catch_error(SQLCODE, 'Stop Queue ' || substr(SQLERRM, 1, 256));

END;
/

rem ==========================================
rem Stop Queue prop_queue
rem ==========================================

DECLARE
BEGIN
    dbms_output.put_line ('Stopping  Queue prop_queue...');
    dbms_aqadm.stop_queue
    (
        queue_name => 'prop_queue',
        wait       => TRUE
    );

    dbms_output.put_line ('Stopped Queue prop_queue.');

exception
    when others then
        catch_error(SQLCODE, 'Stop Queue ' || substr(SQLERRM, 1, 256));

END;
/

rem ==========================================
rem Drop Queue input_queue
rem ==========================================

DECLARE
BEGIN
    dbms_output.put_line ('Dropping  Queue input_queue...');
    dbms_aqadm.drop_queue
    (
        queue_name => 'input_queue'
    );

    dbms_output.put_line ('Dropped Queue input_queue.');

exception
    when others then
        catch_error(SQLCODE, 'Drop Queue ' || substr(SQLERRM, 1, 256));

END;
/


rem ==========================================
rem Drop Queue prop_queue
rem ==========================================

DECLARE
BEGIN
    dbms_output.put_line ('Dropping  Queue prop_queue...');
    dbms_aqadm.drop_queue
    (
        queue_name => 'prop_queue'
    );

    dbms_output.put_line ('Dropped Queue prop_queue.');

exception
    when others then
        catch_error(SQLCODE, 'Drop Queue ' || substr(SQLERRM, 1, 256));

END;
/

rem ==========================================
rem Drop Input Queue Table
rem ==========================================

DECLARE
BEGIN
    dbms_output.put_line ('Dropping Queue Table input_queue_table...');
    dbms_aqadm.drop_queue_table
    (
        queue_table => 'input_queue_table',
        force => TRUE
    );

    dbms_output.put_line ('Dropped Queue Table input_queue_table.');

exception
    when others then
        catch_error(SQLCODE, 'Drop Queue Table ' || substr(SQLERRM, 1, 256));

END;
/


rem ==========================================
rem Drop Prop Queue Table
rem ==========================================

DECLARE
BEGIN
    dbms_output.put_line ('Dropping Queue Table prop_queue_table...');
    dbms_aqadm.drop_queue_table
    (
        queue_table => 'prop_queue_table',
        force => TRUE
    );

    dbms_output.put_line ('Dropped Queue Table prop_queue_table.');

exception
    when others then
        catch_error(SQLCODE, 'Drop Queue Table ' || substr(SQLERRM, 1, 256));

END;
/
rem ==========================================
rem Create a queue table  input_queue_table
rem ==========================================

DECLARE
BEGIN
    dbms_output.put_line ('Creating Queue Table input_queue_table...');

    dbms_aqadm.CREATE_queue_table(
        queue_table => 'input_queue_table',
        multiple_consumers => TRUE,
        queue_payload_type => 'message',
        compatible => '8.1.3',
        comment => 'Creating input queue table');


    dbms_output.put_line ('Created Queue Table input_queue_table.');

exception
    when others then
        catch_error(SQLCODE, 'Create Queue Table ' || substr(SQLERRM, 1, 256));

END;
/

rem ==========================================
rem Create a queue table  prop_queue_table
rem ==========================================

DECLARE
BEGIN
    dbms_output.put_line ('Creating Queue Table prop_queue_table...');

    dbms_aqadm.CREATE_queue_table(
        queue_table => 'prop_queue_table',
        multiple_consumers => TRUE,
        queue_payload_type => 'message',
        compatible => '8.1.3',
        comment => 'Creating prop queue table');

    dbms_output.put_line ('Created Queue Table prop_queue_table.');

exception
    when others then
        catch_error(SQLCODE, 'Create Queue Table ' || substr(SQLERRM, 1, 256));

END;
/


rem ==========================================
rem Create a queue input_queue
rem ==========================================

DECLARE
BEGIN

    dbms_output.put_line ('Creating Queue input_queue...');

    dbms_aqadm.CREATE_queue(
        queue_name => 'input_queue',
        queue_table => 'input_queue_table',
        comment => 'Demo Queue');

    dbms_output.put_line ('Created Queue input_queue.');

exception
    when others then
        catch_error(SQLCODE, 'Create Queue ' || substr(SQLERRM, 1, 256));

END;
/


rem ==========================================
rem Create a queue prop_queue
rem ==========================================

DECLARE
BEGIN

    dbms_output.put_line ('Creating Queue prop_queue...');

    dbms_aqadm.CREATE_queue(
        queue_name => 'prop_queue',
        queue_table => 'prop_queue_table',
        comment => 'Propagation Queue');

    dbms_output.put_line ('Created Queue prop_queue.');

exception
    when others then
        catch_error(SQLCODE, 'Create Queue ' || substr(SQLERRM, 1, 256));

END;
/

rem ====================================
rem Start input queue input_queue
rem ====================================

DECLARE
BEGIN

    dbms_output.put_line('starting queue input_queue...');

    dbms_aqadm.start_queue(
        queue_name => 'input_queue');

    dbms_output.put_line ('Started Queue input_queue.');

exception
    when others then
        catch_error(SQLCODE, 'Start Queue ' || substr(SQLERRM, 1, 256));

END;
/


rem ====================================
rem Start input queue prop_queue
rem ====================================

DECLARE
BEGIN

    dbms_output.put_line('starting Prop queue prop_queue...');

    dbms_aqadm.start_queue(
        queue_name => 'prop_queue');

    dbms_output.put_line ('Started Queue prop_queue.');

exception
    when others then
        catch_error(SQLCODE, 'Start Queue ' || substr(SQLERRM, 1, 256));

END;
/


rem ========================================
rem Create queue subscribers
rem ========================================

DECLARE
    subscriber sys.aq$_agent;
BEGIN
    subscriber := sys.aq$_agent('prog1', NULL, NULL);
    dbms_aqadm.add_subscriber(
        queue_name => 'input_queue',
        subscriber => subscriber);
    dbms_output.put_line ('Added subscriber prog1 to input_queue.');
END;
/

DECLARE
    subscriber1 sys.aq$_agent;
BEGIN
    subscriber1 := sys.aq$_agent('prog2', NULL, NULL);
    dbms_aqadm.add_subscriber(
        queue_name => 'input_queue',
        subscriber => subscriber1,
        rule       => 'priority > 2');
    dbms_output.put_line ('Added subscriber prog2 to input_queue.');
END;
/

DECLARE
    subscriber sys.aq$_agent;
BEGIN
    subscriber := sys.aq$_agent('prog3', 'prop_queue', NULL);
    dbms_aqadm.add_subscriber(
        queue_name => 'input_queue',
        subscriber => subscriber,
        rule => 'priority = 2');
    dbms_output.put_line ('Added subscriber prog3@prop_queue to input_queue.');
END;
/

rem ========================================
rem Schedule propagation
rem ========================================

DECLARE
BEGIN

    dbms_aqadm.schedule_propagation(
        queue_name => 'input_queue',
        latency => '10');
    dbms_output.put_line (
      'Scheduled propagation from input_queue to other queues.');
END;
/

rem ========================================
rem Setup complete
rem ========================================
