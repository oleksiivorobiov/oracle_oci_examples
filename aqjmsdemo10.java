/* $Header: aqjmsdemo10.java 07-jun-2007.14:35:37 aatam Exp $ */

/* Copyright (c) 2003, 2007, Oracle. All rights reserved.  */

/*
   DESCRIPTION
    demonstrates usage of AQ/OJMS messaging with ANYDATA Queue Type

   PRIVATE CLASSES
    <list of private classes defined - with one-line descriptions>

   NOTES
    <other useful comments, qualifications, etc.>

   MODIFIED    (MM/DD/YY)
    aatam       06/05/07 - password need to be consistent
    jleinawe    07/11/05 - 
    jleinawe    03/13/03 - jleinawe_newaqdemos
    jleinawe    03/11/03 - Creation
 */

/**
 *  @version $Header: aqjmsdemo10.java 07-jun-2007.14:35:37 aatam Exp $
 *  @author  jleinawe
 *  @since   release specific (what release of product did this appear in)
 */

/*** This is a sample java file which shows messaging with SYS.ANYDATA queues
 * It will demonstrate a send/receive using each of the five JMS Message
 * Types (Text, Map, Object, Bytes, Stream) and with an AdtMessage.
 * 
 * Instructions for setting up and running this demo are found in
 * aqjmsREADME.txt
 *
 * Note: SYS.ANYDATA Queue Types are not supported for thin jdbc driver
 ***/

import oracle.AQ.*;
import oracle.jms.*;
import javax.jms.*;

public class aqjmsdemo10
{
  public static void main (String args[])
  throws Exception
  {
    javax.jms.QueueConnectionFactory qcf = null;
    javax.jms.QueueConnection qconn = null;
    javax.jms.QueueSession qsession = null;
    javax.jms.Queue queue = null;

    if (args.length < 3)
      System.out.println("Usage: java filename [SID] [HOST] [PORT]");
    else
    {
      //
      // setup
      //
      qcf = AQjmsFactory.getQueueConnectionFactory(
                      args[1], args[0], Integer.parseInt(args[2]), "oci8");
      qconn = qcf.createQueueConnection( "jmsuser","JMSUSER");
      qsession = qconn.createQueueSession(true, Session.CLIENT_ACKNOWLEDGE);
      qconn.start() ;
      queue = setup(qsession);
 
      //
      // run the demo
      //
      enqueueMessages(qsession, queue);
      dequeueMessages(qsession, queue);

      //
      // cleanup and exit
      //
      cleanup(qsession);
      qsession.close();
      qconn.close();
    }
    System.out.println("End of Demo");
  }


/**
  * Enqueue each type of JMS Message on the ANYDATA queue
  */
  public static void enqueueMessages(QueueSession session, javax.jms.Queue queue)
  throws javax.jms.JMSException, java.sql.SQLException
  {
    StringBuffer msgBody = null;;
    javax.jms.QueueSender sender = null;
    javax.jms.Message message = null;
    oracle.jms.AdtMessage adt_message = null;
    Cars car = null;
    int counter = 0;

    //
    // create the QueueSender
    //
    sender= session.createSender(queue);
    
    //
    // send a message for each message type
    //

    // TextMessage
    msgBody = new StringBuffer("This is a Text Message");
    message = session.createTextMessage();
    ((TextMessage)message).setText(msgBody.toString());
    sender.send(message);
    System.out.println("Sent TextMessage");
    counter++;

    // MapMessage
    msgBody = new StringBuffer("This is a Map Message");
    message = session.createMapMessage();
    ((MapMessage)message).setString("stringbuf", msgBody.toString());
    sender.send(message);
    System.out.println("Sent MapMessage");
    counter++;

    // ObjectMessage
    msgBody = new StringBuffer("This is a Object Message");
    message = session.createObjectMessage();
    ((ObjectMessage)message).setObject(msgBody);
    sender.send(message);
    System.out.println("Sent ObjectMessage");
    counter++;

    // BytesMessage
    msgBody = new StringBuffer("This is a Bytes Message");
    message = session.createBytesMessage();
    ((BytesMessage)message).writeBytes(msgBody.toString().getBytes());
    sender.send(message);
    System.out.println("Sent BytesMessage");
    counter++;

    // StreamMessage
    msgBody = new StringBuffer("This is a Stream Message");
    message = session.createStreamMessage();
    ((StreamMessage)message).writeBytes(msgBody.toString().getBytes());
    sender.send(message);
    System.out.println("Sent StreamMessage");
    counter++;

    // ADT Message
    car = new Cars();
    car.setCarno(new java.math.BigDecimal(11051963));
    car.setMake("CHEVROLET");
    car.setColor("BLUE");
    car.setPrice(new java.math.BigDecimal(21099));
    adt_message = ((AQjmsSession)session).createAdtMessage();
    adt_message.setAdtPayload(car);
    sender.send(adt_message);
    System.out.println("Sent AdtMessage");
    counter++;

    //
    // commit the sends
    //
    session.commit();
    System.out.println("Sent a total of "+counter+" messages");

    sender.close();
  }


/**
  * Dequeue each message enqueued on the ANYDATA Queue
  */
  public static void dequeueMessages(QueueSession session, javax.jms.Queue queue)
  throws javax.jms.JMSException, java.lang.ClassNotFoundException,
         java.sql.SQLException
  {
    javax.jms.QueueReceiver receiver = null;
    javax.jms.Message message = null;
    int counter = 0;
    java.util.Dictionary<Object, Object> map = null;
    
    //
    // setup mapping (needed because we have an AdtMessage)
    //
    map = (java.util.Dictionary<Object, Object>)((AQjmsSession)session).getTypeMap();
    map.put("JMSUSER.CARS", Class.forName("Cars"));

    //
    // create receiver
    //
    receiver = session.createReceiver(queue);
    
    //
    // receive all messages on the queue
    //
    boolean nullMsgReceive = false;
    while (!nullMsgReceive)
    {
      message = (javax.jms.Message) receiver.receive(20000);

      if (message == null)
      {
        System.out.println("Received a total of "+counter+" messages");
        nullMsgReceive = true;
      }
      else
      {
        counter++;
        if (message instanceof TextMessage)
        {
          String body = ((TextMessage)message).getText();
          System.out.println("Received TextMessage: "+body);
        }
        else if (message instanceof MapMessage)
        {
          String body = ((MapMessage)message).getString("stringbuf");
          System.out.println("Received MapMessage: "+body);
        }
        else if (message instanceof ObjectMessage)
        {
          StringBuffer strbuf 
                          = (StringBuffer)((ObjectMessage)message).getObject();
          System.out.println("Received ObjectMessage: "+strbuf.toString());
        }
        else if (message instanceof StreamMessage)
        {
          byte[] bdata = new byte[128];
          ((StreamMessage) message).readBytes(bdata);
          String body = new String(bdata);
          System.out.println("Received StreamMessage: "+body);
        }
        else if (message instanceof BytesMessage)
        {
          byte[] bdata = new byte[128];
          ((BytesMessage) message).readBytes(bdata);
          String body = new String(bdata);
          System.out.println("Received BytesMessage: "+body);
        }
        else if (message instanceof AdtMessage)
        {
          Cars car = (Cars) ((AdtMessage) message).getAdtPayload();
          System.out.print("Received AdtMessage for Cars: ");
          System.out.print(car.getCarno()+", ");
          System.out.print(car.getMake()+", ");
          System.out.print(car.getColor()+", ");
          System.out.println(car.getPrice());
        }
        else
        {
          System.out.println("Unknown Message Type Received!");
        }
      }
      session.commit();
    }
    receiver.close();
  }

/**
  * Creates and returns ANYDATA queue called jmsuser.jmsanydata10 
  */
  public static javax.jms.Queue setup(QueueSession session)
  throws javax.jms.JMSException, oracle.AQ.AQException
  {
    oracle.AQ.AQQueueTableProperty qtprop = null;
    oracle.AQ.AQQueueTable qtable = null;
    oracle.jms.AQjmsDestinationProperty dprop = null;
    javax.jms.Queue queue = null;

    //
    // drop queue table if it already exists
    //
    try
    {
      qtable =
             ((AQjmsSession)session).getQueueTable("jmsuser", "jmsqtable_any");
      if (qtable != null)
        qtable.drop(true);
    }
    catch (Exception e){}

    //
    // create ANYDATA queue table
    //
    qtprop = new AQQueueTableProperty ("SYS.ANYDATA");
    qtprop.setMultiConsumer(false);
    qtprop.setCompatible("9.2.0.0.0");
    qtable = ((AQjmsSession)session).createQueueTable("jmsuser", 
                                                   "jmsqtable_any", qtprop);

    //
    // create JMS Queue for ANYDATA queue table
    //
    dprop = new AQjmsDestinationProperty();
    queue = ((AQjmsSession)session).createQueue (qtable, "jmsanydata10", dprop);
    System.out.println("Created queue jmsanydata10");

    //
    // enable the ANYDATA JMS Queue for JMS Messaging
    //
    try
    { 
      java.sql.CallableStatement stmt;
      java.sql.Connection conn = ((AQjmsSession)session).getDBConnection();
      String sqlCall = 
                 "{call dbms_aqadm.enable_jms_types('jmsuser.jmsqtable_any')}";
      stmt = conn.prepareCall(sqlCall);
      stmt.execute();
      System.out.println("Queue is JMS enabled");
    }
    catch (java.sql.SQLException s)
    {
      s.printStackTrace();
    }
  
    // 
    // Start the queue
    // 
    ((AQjmsDestination)queue).start(session, true, true);

    return queue;
  }

/**
  * drops queue table "jmsuser.jmsqtable_any"
  */
  private static void cleanup(QueueSession session)
  {
    //
    // drop queue table used for test
    //
    try
    {
      oracle.AQ.AQQueueTable qtable =
             ((AQjmsSession)session).getQueueTable("jmsuser", "jmsqtable_any");
      if (qtable != null)
        qtable.drop(true);
    }
    catch (Exception e)
    {
      System.out.println("Error dropping jmsanydata10");
      e.printStackTrace(); 
    }
    System.out.println("Queue jmsanydata10 dropped successfully");
  }
}

