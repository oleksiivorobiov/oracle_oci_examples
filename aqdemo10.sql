Rem
Rem $Header: aqdemo10.sql 17-oct-2003.16:57:16 aahluwal Exp $
Rem
Rem aqdemo10.sql
Rem
Rem Copyright (c) 2003, Oracle Corporation.  All rights reserved.  
Rem
Rem    NAME
Rem      aqdemo10.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem      Performs an array enq of a batch of messages into my_queue
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

Rem perform array enqueue into AQ queue my_queue 

connect aquser/aquser
set serveroutput on
set echo on

DECLARE
 enqopt dbms_aq.enqueue_options_t;
 msgproparr dbms_aq.message_properties_array_t;
 msgprop dbms_aq.message_properties_t;
 payloadarr  message_tbl;
 msgidarr dbms_aq.msgid_array_t;
 retval pls_integer;

BEGIN
  payloadarr := message_tbl(message('00000'),
                            message('11111'),
                            message('22222'),
                            message('33333'),
                            message('44444'),
                            message('55555'),
                            message('66666'),
                            message('77777'),
                            message('88888'),
                            message('99999')) ;
  msgproparr := dbms_aq.message_properties_array_t(msgprop, 
                                                   msgprop,
                                                   msgprop,
                                                   msgprop,
                                                   msgprop,
                                                   msgprop,
                                                   msgprop,
                                                   msgprop,
                                                   msgprop,
                                                   msgprop);

  retval := dbms_aq.enqueue_array( queue_name => 'AQUSER.MY_QUEUE',
                 enqueue_options => enqopt ,
                 array_size => 10,
                 message_properties_array => msgproparr,
                 payload_array => payloadarr,
                 msgid_array => msgidarr ) ;
  commit;

  dbms_output.put_line('Enqueued ' || retval || ' messages') ;
  for i in 1..retval loop
   dbms_output.put_line ('Message ' || i || ' payload: ' || payloadarr(i).data) ;
  end loop ;

    
END;
/


