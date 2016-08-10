/* Copyright (c) 2003, 2009, Oracle and/or its affiliates. 
All rights reserved. */
/*
   NAME
     occiaqop.cpp - Demonstrating the Advanced queueing feature in OCCI 

   DESCRIPTION
     This program demonstates the advanced queueing feature
     in OCCI by creating a message, setting desired properties on it, 
     enqueuing it and dequeuing the same.
     It does the basic operational functions of sending and receiving messages
     In the AQ parlance, enqueuing refers to sending a message to a queue and 
     dequeuing refers to receiving one. 
     A client application can create a message, set the desired properties on it
     and enqueue whereby the message is stored in a queue (which is actually 
     a table on the database in case of persistent queue) 
     Now the application can receive the message by calling the receive methods 
     on the queue 
     Three types of messages can be enqueued and dequeued
     They are Raw messages, Anydata messages and ADT messages.
     The steps to enqueue and dequeue these 3 types of messages are as below:-

    * Create a message and set desired properties on it
    * Enqueue the message to the queue
    * Dequeue the message by calling receive methods on the queue

   MODIFIED   (MM/DD/YY)
   sudsrini    07/08/09 - Include string.h, iostream not including it on sles11
   sudsrini    10/22/06 - Username/Password lower case
   sudsrini    07/22/04 - Copyright Info
   sprabhak    03/05/03 - Creation
*/

#include <iostream>
#include <string.h>
#include <occi.h>
#include "occiaqopm.h"

using namespace oracle::occi;
using namespace std;
using namespace oracle::occi::aq;

Environment *env;

void test_raw(Connection *conn);
void test_raw_enqueue(Connection *conn);
void test_raw_dequeue(Connection *conn);
void test_anydata(Connection *conn);
void test_anydata_enqueue(Connection *conn);
void test_anydata_dequeue(Connection *conn);
void test_adt(Connection *conn);
void test_adt_enqueue(Connection *conn);
void test_adt_dequeue(Connection *conn);
  
int main (void)
{
   try
   {
     env = Environment::createEnvironment (Environment::OBJECT);
     cout<<"Demo for advanced queuing enqueue/dequeue"<<endl;
     Connection *conn=env->createConnection("hr","hr");
     //Testing AQ operations on raw message
     test_raw(conn);
     //Testing AQ operations on anydata message
     test_anydata(conn);
     //Testing AQ operations on adt message
     test_adt(conn);
     env->terminateConnection(conn);
     Environment::terminateEnvironment (env);
     cout<<"Demo for advanced queueing enqueue/dequeue done"<<endl;
   }
   catch(SQLException ex)
   {
     cout<<"Exception thrown "<<endl;
     cout<<"Error number: "<<  ex.getErrorCode()<<endl;
     cout<<ex.getMessage() << endl;
   }

}//End of main

void test_raw(Connection *conn)
{
  //Function to enqueue  raw message
  test_raw_enqueue(conn);
  //Function to dequeue raw message
  test_raw_dequeue(conn);
}
void test_raw_enqueue(Connection *conn)  
{ 
   Message m1(env);
   unsigned char by[] = "10001000";
   int bylen = 8;
   Bytes in(by, bylen, 0);

   //Set the message Payload
   m1.setBytes(in);
   cout << "Message before enqueue:";
   int length1 = in.length();
   unsigned char *c1 = new unsigned char [length1];
   memset (c1, 0, length1);
   in.getBytes(c1, length1, 0, 0);
   for (int i=0; i<length1; ++i)
     cout << c1[i];
   cout<<endl;

   //Set message property correlation id
   m1.setCorrelationId("SP");

   //Enqueue the message
   Producer prod(conn);
   prod.setVisibility(Producer::ENQ_IMMEDIATE);
   prod.send(m1, "hr.queue01");

   delete[] c1;
   cout<<"Enqueue of raw message done"<<endl;
}

void test_raw_dequeue(Connection *conn)
{
   Consumer cons(conn);
   //Single Consumer Queue
   cons.setCorrelationId("SP");

   //Dequeue the message
   cons.setQueueName("queue01");
   Message m2 = cons.receive(Message::RAW);
   Bytes out3 = m2.getBytes();
   cout << "Message after dequeue:" ;
   int length = out3.length();
   unsigned char *c = new unsigned char [length];
   memset (c, 0, length);
   out3.getBytes(c, length, 0, 0);
   for (int m=0; m<length; ++m)
     cout << c[m];
   cout << endl;

   delete[] c;
   cout<<"Dequeue of raw message done"<<endl;
}

void test_anydata(Connection *conn)
{
   //Function to enqueue anydata message
   test_anydata_enqueue(conn);
   //Function to dequeue anydata message
   test_anydata_dequeue(conn);
}

void test_anydata_enqueue(Connection *conn)
{
   Message m1(env);
   AnyData any1(conn);

   vector<Agent> list;
   Agent a1(env);
   a1.setName("AGT");
   a1.setAddress("hr.queue02");
   a1.setProtocol(0);

   //String functions
   string in_str1("ANYDATA-MSG");
   any1.setFromString(in_str1);
   cout << "Message before enqueue:" << in_str1 << endl;

   //Set the payload
   m1.setAnyData(any1);

   //Setting the Recipient list
   list.push_back(a1);
   m1.setRecipientList(list);

   //Enqueue the message
   Producer prod(conn);
   prod.setVisibility(Producer::ENQ_ON_COMMIT);
   Bytes by_enq=prod.send(m1, "hr.queue02");
   conn->commit();
   cout<<"Enqueue of anydata message done"<<endl;
}

void test_anydata_dequeue(Connection *conn)
{
   Consumer cons(conn);

   //Setting the consumer name since this is a multi consumer Queue
   cons.setConsumerName("AGT");

   //Dequeue the message
   cons.setQueueName("queue02");
   Message m2 = cons.receive(Message::ANYDATA);
   AnyData any2 = m2.getAnyData();
   string out_str = any2.getAsString();
   cout << "Message after dequeue:" << out_str << endl;
   cout<<"Dequeue of anydata message done"<<endl;
}

void test_adt(Connection *conn)
{
  //Function to enqueue adt message
  test_adt_enqueue(conn);
  //Function to dequeue adt message
  test_adt_dequeue(conn);
}

void test_adt_enqueue(Connection *conn)
{
   Message m1(env);
   occiaqopm (env);
   hr_obj *obj = new hr_obj ();
   Number a1 = Number (100);
   OCCI_STD_NAMESPACE::string a2 = OCCI_STD_NAMESPACE::string ("ADT-MSG");
   obj->setA1 (a1);
   obj->setA2 (a2);
   m1.setObject(obj);
   Producer prod(conn);
   cout << "Object data before enqueue " << endl;
   cout<<"A1: "<<(int)obj->getA1()<<endl;
   cout <<"A2: " << obj->getA2 () << endl;
   prod.send(m1, "hr.queue03");
   delete (obj);
   cout<<"Enqueue of adt message done"<<endl;
}

void test_adt_dequeue(Connection *conn)
{
   Consumer cons(conn);
   cons.setConsumerName("AGT1");
   cons.setQueueName("queue03");
   Message m2 = cons.receive(Message::OBJECT, "HR_OBJ", "HR");
    cout << "Object data after dequeue " << endl;
    hr_obj *obnew1=(hr_obj*)m2.getObject();
    cout<<"A1: "<<(int)obnew1->getA1()<<endl;
    cout <<"A2: " << obnew1->getA2 () << endl;
    delete(obnew1);
    cout<<"Dequeue of adt message done"<<endl;
}
