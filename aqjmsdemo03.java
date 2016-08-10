/* $Header: aqjmsdemo03.java 05-jun-2007.15:10:22 aatam Exp $ */

/* Copyright (c) 2000, 2007, Oracle. All rights reserved.  */

/*
   DESCRIPTION
    <short description of component this file declares/defines>

   PRIVATE CLASSES
    <list of private classes defined - with one-line descriptions>

   NOTES
    <other useful comments, qualifications, etc.>

   MODIFIED    (MM/DD/YY)
    aatam       06/05/07 - password need to be consistent
    jleinawe    07/11/05 - 
    jleinawe    12/20/02 - update instructions
    rbhyrava    03/01/02 - increase sleep
    rbhyrava    01/25/02 - increase sleep time
    lzhao       06/26/01 - comment
    rbhyrava    01/09/01 - bug 1419924
    rbhyrava    03/15/00 - AQ JMS demo - Message Listener Demo 
    rbhyrava    03/15/00 - Creation
 */

/**
 *  @version $Header: aqjmsdemo03.java 05-jun-2007.15:10:22 aatam Exp $
 *  @author  rbhyrava
 *  @since   release specific (what release of product did this appear in)
 */

/* Message Listener */
/***
 * This is a sample java file which uses Oracle JMS - Java Messaging Service 
 * API where Subscribers can Recieve messages asynchronously using 
 * MessageListener
 * Subscribers receiveing the messages asynchronously using Message Listener
 * aqjmsdemo02 - enqueues the messages to the queue 
 * aqjmsdemo03 - setup Message Listener and dequeue the messages 
 * This demo does the following:
 * -- Get the Queue already created in aqjmsdemo03.java 
 * -- Create Receiver for the Queue 
 * -- Setup the message listener for Queue Receiver 
 * -- Start the connection 
 * -- Recieve the messages 
 * -- Stop the connection 
 *
 * Instructions for setting up and running this demo are found in
 * aqjmsREADME.txt.
 ***/

/* import useful packages */

/* import useful packages */
import oracle.AQ.*;
import oracle.jms.*;
import javax.jms.*;
import java.lang.*;

public class aqjmsdemo03
{

  public static void main (String args [])
       throws java.sql.SQLException, ClassNotFoundException, JMSException
  {
     QueueSession  qsess = null;
     QueueConnectionFactory qcfact;
     QueueConnection qconn;
      try
      {
        if (args.length < 4 )
         System.out.println("Usage:java filename [SID] [HOST] [PORT] [DRIVER]");
        else {
          qcfact = AQjmsFactory.getQueueConnectionFactory(
                      args[1], args[0], Integer.parseInt(args[2]), args[3]);
          qconn = qcfact.createQueueConnection( "jmsuser","JMSUSER");
          /* Create a Queue Session */
          qsess = qconn.createQueueSession(true, Session.CLIENT_ACKNOWLEDGE);
          System.out.println("Successfully created QueueSession");  

          qconn.start() ;
          demoMessageListener(qsess);
          qsess.close() ;
          qconn.close() ;
          System.out.println("End of Demo") ;
        }
      }
      catch (Exception ex)
      {
         System.out.println("Exception-1: " + ex);
         ex.printStackTrace();
      }
  }

  public static void demoMessageListener(QueueSession qsess) throws Exception
  {
    javax.jms.Queue queue;
    QueueReceiver qrecv ; 
    MessageListener mLis ;

    try
    {
      System.out.println("Get the Queue") ;
      /* Get the Queue */
      /* Queue JMSMAPQ is created in aqjmsdemo02 */
      queue = ((AQjmsSession)qsess).getQueue("JMSUSER", "JMSMAPQ") ;

      System.out.println("Create Receiver...") ;

      /* Create a queue receiever */
      qrecv = qsess.createReceiver(queue, "JMSCorrelationID='BLUE'") ;
      
      /* Setup the message listener */
      System.out.println("Set Message Listener... and Sleep...") ;
      mLis = new MesgListener(qsess);
      qrecv.setMessageListener (mLis) ;

      try {
         Thread.sleep(250000) ;
      } catch (InterruptedException e) {} ;

      System.out.println("Successfully dequeued with MessageListener") ;

     } catch (JMSException e) {
      System.out.println("Error in demoMessageListener: " +e ) ;
      throw e;
     }
  } 
}
