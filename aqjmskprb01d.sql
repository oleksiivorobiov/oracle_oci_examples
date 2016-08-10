Rem
Rem $Header: aqjmskprb01d.sql 05-jun-2007.15:02:54 aatam Exp $
Rem
Rem aqjmskprb01d.sql
Rem
Rem Copyright (c) 2002, 2007, Oracle. All rights reserved.  
Rem
Rem    NAME
Rem      aqjmskprb01d.sql - <one-line expansion of the name>
Rem
Rem    DESCRIPTION
Rem      <short description of component this file declares/defines>
Rem
Rem    NOTES
Rem      <other useful comments, qualifications, etc.>
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    aatam       06/05/07 - password need to be consistent
Rem    jleinawe    09/04/02 - jleinawe_new_aq_demos
Rem    jleinawe    08/30/02 - Created
Rem


connect jmsuser1/JMSUSER1;

execute dbms_aqadm.drop_Queue_table(queue_table => 'queue1', force => true);

drop package jmsdemo;

connect system/manager;
drop user jmsuser1 cascade;

quit;

