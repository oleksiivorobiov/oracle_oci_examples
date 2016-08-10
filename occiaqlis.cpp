/* Copyright (c) 2003, 2009, Oracle and/or its affiliates. 
All rights reserved. */
/*
   NAME
     occiaqlis.cpp - OCCI AQ listener functionality demo

   DESCRIPTION
     This demo program explains the use of listener class of advanced 
     queueing in occi.
     This functionality  aims at listening on behalf of a bunch of agents 
     which are specified on 1 or more queues, and return the agent 
     for whom a message has arrived.
     The listen() call listens on one or more queues on behalf 
     of a list of agents. 
     This call returns the agent for which there is a message. 
     Prior to this call, the list of agents on behalf to listen to must been set
     This is a blocking call that returns when there is a message ready for 
     consumption for an agent in the list. 
     If there are no messages found when the wait time expires, 
     an error is returned.

   MODIFIED   (MM/DD/YY)
   sudsrini    07/08/09 - Include string.h, iostream not including it on sles11
   sudsrini    10/22/06 - Username/Password lower case
   sudsrini    07/22/04 - Copyright info
   sprabhak    03/05/03 - Creation

*/

#include<iostream>
#include<string.h>
#include<occi.h>
#include<vector>

using namespace std;
using namespace oracle::occi;
using namespace oracle::occi::aq;

void test_enqueue(Environment *env, Connection *conn)
{
    cout<<"Enqueuing the message into the queue"<<endl;
    Message m(env);
    unsigned char by[] = "10001000";
    int bylen = 8;
    cout<<"Message before enqueue : ";
    Bytes in(by, bylen, 0);
    for (int k=0; k<bylen; ++k)
      cout << by[k];
    cout << endl;
    m.setBytes(in);
    Producer prod(conn);
    prod.send(m, "hr.queue04");
    conn->commit();
    cout<<"Enqueue done"<<endl;
}

void test_listener(Environment *env, Connection *conn)
{
    Listener l(conn);
    vector<Agent> agList;

    agList.push_back(Agent(env, "AGT1", "hr.queue04"));
    agList.push_back(Agent(env, "AGT2", "hr.queue04"));
    // The listener is to listen for the above 2 agents.
    l.setAgentList(agList);
    l.setTimeOutForListen(10);

    cout<<"Listening..."<<endl;
    // As soon as a message intended for any of the above agents
    // arrives at the queues being listened to, the listen call returns
    // with the agent for whom the message is intended for.  In this case
    // it is the agent(AGT1,"hr.queue04",0) since this is a subscriber
    // to the hr.queue04. The enqueuer can also specify the recipients
    // which will override the default subscriber list.
    Agent a=l.listen();

    cout<<"Message is for the following Agent: "<<endl;
    string aname=a.getName();
    string aadd=a.getAddress();
    int aprot=a.getProtocol();
    cout<<"name:"<<aname<<endl;
    cout<<"address:"<<aadd<<endl;
    cout<<"protocol:"<<aprot<<endl;

    //create a consumer object using the agent.
    Consumer cons(conn, a);
    cons.setWaitTime(20);
    // Need not specify consumer name and queuename. 
    // Will be inferred from the agent
    cout<<"Dequeuing of the message"<<endl;
    Message m2 = cons.receive(Message::RAW);
    Bytes out = m2.getBytes();
    cout << "Message after dequeue : ";
    int length = out.length();
    unsigned char *c = new unsigned char [length];
    memset (c, 0, length);
    out.getBytes(c, length, 0, 0);
    for (int k=0; k<length; ++k)
      cout << c[k];
    cout << endl;
    cout<<"Dequeue done"<<endl;
    delete[] c;
}

int main()
{
   cout<<"Demo for advanced queueing listener"<<endl;
   Environment *env = Environment::createEnvironment(Environment::OBJECT);
   try
   {
      Connection *con = env->createConnection("hr","hr");
      test_enqueue(env, con);
      test_listener(env, con);
      env->terminateConnection(con);
   }
   catch(SQLException &ex)
   {
     cout<<"Exception thrown "<<endl;
     cout<<"Error number: "<<  ex.getErrorCode()<<endl;
     cout<<ex.getMessage() << endl;
   }
   Environment::terminateEnvironment(env);
   cout<<"Demo for advanced queueing listener done"<<endl;
}
