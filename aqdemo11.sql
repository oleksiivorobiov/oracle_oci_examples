Rem
Rem $Header: aqdemo11.sql 17-oct-2003.16:57:17 aahluwal Exp $
Rem
Rem aqdemo11.sql
Rem
Rem Copyright (c) 2003, Oracle Corporation.  All rights reserved.  
Rem
Rem    NAME
Rem      aqdemo11.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem      Performs an array deq of a batch of messages from my_queue
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    aahluwal    10/17/03 - aahluwal_create_arrenqdeq_demos 
Rem    aahluwal    10/07/03 - Created
Rem

SET ECHO ON
SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100

Rem perform array dequeue from AQ queue my_queue 

connect aquser/aquser
set serveroutput on
set echo on

DECLARE
 deqopt dbms_aq.dequeue_options_t ;
 msgproparr dbms_aq.message_properties_array_t := 
               dbms_aq.message_properties_array_t();
 payloadarr  message_arr := message_arr() ;
 msgidarr dbms_aq.msgid_array_t ;
 retval pls_integer ;
BEGIN
  payloadarr.extend(10);
  msgproparr.extend(10);
  deqopt.consumer_name := 'SUB1';
  retval := dbms_aq.dequeue_array( queue_name => 'AQUSER.MY_QUEUE',
                 dequeue_options => deqopt ,
                 array_size => payloadarr.count,
                 message_properties_array => msgproparr,
                 payload_array => payloadarr,
                 msgid_array => msgidarr ) ;
  commit;
  dbms_output.put_line('Dequeued ' || retval || ' messages') ;
  for i in 1..retval loop
   dbms_output.put_line ('Message ' || i || ' payload: ' || payloadarr(i).data) ;
  end loop ;
                   
END;
/





