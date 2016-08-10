/* $Header: aqjmsdemo02.java 05-jun-2007.15:10:20 aatam Exp $ */

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
    jleinawe    07/11/05 - jdk 1.5
    jleinawe    12/20/02 - update instructions
    rbhyrava    01/09/01 - bug 1419924
    rbhyrava    03/16/00 - AQ jms demos
    rbhyrava    03/15/00 - AQ JMS demo - Message Listener Demo - Enqueue Messag
    rbhyrava    03/15/00 - Creation
 */

/**
 *  @version $Header: aqjmsdemo02.java 05-jun-2007.15:10:20 aatam Exp $
 *  @author  rbhyrava
 *  @since   release specific (what release of product did this appear in)
 */



/***
 * This is a sample java file which uses Oracle JMS - Java Messaging Service 
 * API to Publish messages into topic. aqjmsdemo04 will demonstrate the 
 * Subscribers receiving the messages asynchronously using Message Listener
 * aqjmsdemo02 - enqueues the messages to the queue 
 * aqjmsdemo03 - setup Message Listener and dequeue the messages 

 * This demo does the following:
 * -- Create a Queue 
 * -- Send several MAP messages 
 *
 * Instructions for setting up and running this demo are found in
 * aqjmsREADME.txt.
 ***/

/* import useful packages */
import oracle.AQ.*;
import oracle.jms.*;
import javax.jms.*;
import java.lang.*;

public class aqjmsdemo02
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
         //args passed are sid,host,port,driver 
          qcfact = AQjmsFactory.getQueueConnectionFactory(
                      args[1], args[0], Integer.parseInt(args[2]), args[3]);

          /* Create Queue Connection */
          qconn = qcfact.createQueueConnection( "jmsuser","JMSUSER");

          /* Create a Queue Session */
          qsess = qconn.createQueueSession(true, Session.CLIENT_ACKNOWLEDGE);

          qconn.start() ;
          setupQueue(qsess) ;
          enqueueMessages(qsess);
          System.out.println("End of Demo") ;
          qsess.close() ;
          qconn.close() ;
        }
      }
      catch (Exception ex)
      {
         System.out.println("Exception-1: " + ex);
         ex.printStackTrace();
      }
  }

   public static void setupQueue(QueueSession qsess) throws Exception
   {
      AQQueueTableProperty qtprop ;
      AQQueueTable qtable;
      AQjmsDestinationProperty dprop;
      javax.jms.Queue q1;
      try {

         /* Create Queue Tables */
         System.out.println("Creating Queue Table...") ;
    
          qtprop = new AQQueueTableProperty ("SYS.AQ$_JMS_MAP_MESSAGE") ;
          qtprop.setCompatible("8.1") ;
          qtprop.setPayloadType("SYS.AQ$_JMS_MAP_MESSAGE") ;
    
           /* Drop if the queue table already exists */
           try { 
             qtable=((AQjmsSession)qsess).getQueueTable("JMSUSER", "JMSQTABLE");
             qtable.drop(true);
           } catch (Exception e) {} 
    
           qtable = ((AQjmsSession)qsess).createQueueTable("JMSUSER", 
             "jmsqtable", qtprop) ;
              
          System.out.println ("Creating Queue...");
          dprop = new AQjmsDestinationProperty() ;
          q1=((AQjmsSession)qsess).createQueue(qtable,"JMSMAPQ",dprop) ;
    
          /* Start the Queue */
          ((AQjmsDestination)q1).start(qsess, true, true);

         System.out.println("Successfully setup Queue");  
      } catch (Exception ex) {
         System.out.println("Error in setupTopic: " + ex);
         throw ex;
      }
   }

  public static void enqueueMessages(QueueSession qsess) throws Exception
  {
    javax.jms.Queue q1 ;
    MapMessage[] Messages;
    byte[] barray ;
    QueueSender qsender;

    TopicSubscriber tsub1,tsub2;
    TopicPublisher publisher;
    StringBuffer txtbuf ;
    TextMessage txtmsg, dtxtmsg;
    try
    {
      /* Get Queue */
      q1 = ((AQjmsSession)qsess).getQueue("JMSUSER", "JMSMAPQ") ;

      System.out.println("Send messages...") ;

      barray  = new byte[25] ;
      for ( int i=0 ; i< 25 ; i++) barray[i] = 67 ;
      
      Messages = new MapMessage[5] ;

      Messages[0] = qsess.createMapMessage() ;
      Messages[0].setIntProperty ("carno", 3355) ;
      Messages[0].setStringProperty ("color", "BLUE") ;
      Messages[0].setStringProperty ("make", "BMW") ;
      Messages[0].setDoubleProperty ("price", 20000) ;
      Messages[0].setIntProperty ("year", 1999) ;

      Messages[1] = qsess.createMapMessage() ;
      Messages[1].setIntProperty ("carno", 4444) ;
      Messages[1].setStringProperty ("color", "BLACK") ;
      Messages[1].setStringProperty ("make", "ACURA") ;
      Messages[1].setDoubleProperty ("price", 22995) ;
      Messages[1].setIntProperty ("year", 1998) ;

      Messages[2] = qsess.createMapMessage() ;
      Messages[2].setIntProperty ("carno", 1212) ;
      Messages[2].setStringProperty ("color", "BLUE") ;
      Messages[2].setStringProperty ("make", "MERCEDES") ;
      Messages[2].setDoubleProperty ("price", 30995) ;
      Messages[2].setIntProperty ("year", 2001) ;

      Messages[3] = qsess.createMapMessage() ;
      Messages[3].setIntProperty ("carno", 5345) ;
      Messages[3].setStringProperty ("color", "GREEN") ;
      Messages[3].setStringProperty ("make", "LEXUS") ;
      Messages[3].setDoubleProperty ("price", 21895) ;
      Messages[3].setIntProperty ("year", 1995) ;

      Messages[4] = qsess.createMapMessage() ;
      Messages[4].setIntProperty ("carno", 8909) ;
      Messages[4].setStringProperty ("color", "BLUE") ;
      Messages[4].setStringProperty ("make", "BMW") ;
      Messages[4].setDoubleProperty ("price", 40995) ;
      Messages[4].setIntProperty ("year", 2002) ;

      /* Create Queue Sender */ 
      qsender = qsess.createSender(q1) ;

      for ( int i = 0 ; i < 5 ; i++ ) 
      {
        Messages[i].setBytes("Picture" , barray) ;
        Messages[i].setJMSCorrelationID(Messages[i].getStringProperty("color"));
        System.out.println("Sending "+Messages[i].getStringProperty ("color") + 
                           " " + Messages[i].getStringProperty("make") + 
                           " " + Messages[i].getIntProperty("year") + 
                           " " + Messages[i].getDoubleProperty("price")) ; 

        qsender.send(q1, Messages[i], DeliveryMode.PERSISTENT, 
            1+ (i%10) , AQjmsConstants.EXPIRATION_NEVER ) ;
        try { 
           Thread.sleep(5000) ;
        } catch (InterruptedException e) {}  ;
        qsess.commit() ;
      }
      System.out.println("Successfully Sent Messages.");  

    } catch (JMSException e) {
      System.out.println("Error in Sending Messages : " + e) ;
      throw e;
    }
 } 
}
