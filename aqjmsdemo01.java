/* $Header: aqjmsdemo01.java 05-jun-2007.15:10:19 aatam Exp $ */

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
    rbhyrava    11/14/00 - remove pwd at the end
    rbhyrava    07/13/00 - fix compilation error
    rbhyrava    03/16/00 - AQ jms demos 
    rbhyrava    03/15/00 - AQ JMS demo - Enqueue,Dequeue Text Message
    rbhyrava    03/15/00 - Creation
 */

/**
 *  @version $Header: aqjmsdemo01.java 05-jun-2007.15:10:19 aatam Exp $
 *  @author  rbhyrava
 *  @since   release specific (what release of product did this appear in)
 */
/***
 * This is a sample java file which uses Oracle JMS - Java Messaging Service 
 * API to Enqueue/Dequeue text Message. Rule-based subscription on message properties and/or the message content of a topic. 
 *
 * This demo does the following:
 * -- Setup a topic 
 * -- Create two  Durable Subscribers.Specify a selector that represents 
 *    a specification (selects) for the messages that the subscriber wishes 
 *    to receive. 
 * -- Publish sereral Messages to the Topic 
 * -- Receive the Messages for each subscriber 
 *
 * Instructions for setting up and running this demo are found in 
 * aqjmsREADME.txt.
 *
 ***/
/* import useful packages */
import oracle.AQ.*;
import oracle.jms.*;
import javax.jms.*;
import java.lang.*;

public class aqjmsdemo01
{
  public static void main (String args [])
       throws java.sql.SQLException, ClassNotFoundException, JMSException
  {
     TopicSession  tsess = null;
     TopicConnectionFactory tcfact =null;
     TopicConnection tconn =null;
      try
      {
        if (args.length < 4 )
         System.out.println("Usage:java filename [SID] [HOST] [PORT] [DRIVER]");
        else {
          tcfact = AQjmsFactory.getTopicConnectionFactory(
                      args[1], args[0], Integer.parseInt(args[2]), args[3]);

          tconn = tcfact.createTopicConnection( "jmsuser","JMSUSER");

          /* Create a Topic Session */
          tsess = tconn.createTopicSession(true, Session.CLIENT_ACKNOWLEDGE);

          tconn.start() ;
          setupTopic(tsess) ;
          performJmsOperations(tsess);
          tsess.close() ;
          tconn.close() ;
          System.out.println("End of Demo") ;
        }
      }
      catch (Exception ex)
      {
         System.out.println("Exception-1: " + ex);
         ex.printStackTrace();
      }
  }

   public static void setupTopic(TopicSession tsess) throws Exception
   {
      AQQueueTableProperty qtprop ;
      AQQueueTable qtable;
      AQjmsDestinationProperty dprop;
      Topic topic;
      try {
         /* Create Queue Tables */
         System.out.println("Creating Input Queue Table...") ;

         /* Drop the queue if already exists */
         try {
           qtable=((AQjmsSession)tsess).getQueueTable("jmsuser", "jmsqtable" );
           qtable.drop(true);
         } catch (Exception e) {} ;

         qtprop = new AQQueueTableProperty ("SYS.AQ$_JMS_TEXT_MESSAGE") ;
         qtprop.setMultiConsumer(true) ;
         qtprop.setCompatible("8.1") ;
         qtprop.setPayloadType("SYS.AQ$_JMS_TEXT_MESSAGE") ;
         qtable = ((AQjmsSession)tsess).createQueueTable("JMSUSER", 
                "jmsqtable", qtprop) ;

         System.out.println ("Creating Topic input_queue...");
         dprop = new AQjmsDestinationProperty() ;
         topic=((AQjmsSession)tsess).createTopic( qtable,"jmstopic",dprop) ;

         /* Start the topic */
         ((AQjmsDestination)topic).start(tsess, true, true);
         System.out.println("Successfully setup Topic");  
      } catch (Exception ex) {
         System.out.println("Error in setupTopic: " + ex);
	 throw ex;
      }
   }

  public static void performJmsOperations(TopicSession tsess) 
  {
    Topic topic =null;
    TopicSubscriber tsub1,tsub2;
    TopicPublisher publisher;
    TextMessage txtmsg, dtxtmsg;

    try
    {
      System.out.println ("Get the Topic...");
      topic = ((AQjmsSession)tsess).getTopic("JMSUSER","jmstopic") ;

      System.out.println("Creating Topic Subscribers...") ;

      tsub1 = tsess.createDurableSubscriber(topic, "dallas",
      "(year = 1998 OR color NOT IN ('GREEN','RED','WHITE')) "+
        " AND make IN ('ACURA ', 'BMW', 'MERCEDES')", false);

      tsub2 = tsess.createDurableSubscriber(topic, "atlanta",
       "price < 20000", false );

      System.out.println("Publish messages...") ;
      publisher = tsess.createPublisher(topic); 


      txtmsg = tsess.createTextMessage() ;
      txtmsg.setText("Cars Distribution") ;
      txtmsg.setObjectProperty("carno", new Integer(12345)) ;
      txtmsg.setStringProperty("color", "BLUE") ;
      txtmsg.setIntProperty("year", 1999) ;
      txtmsg.setStringProperty("make", "BMW") ;
      txtmsg.setDoubleProperty("price", 25995) ;
      txtmsg.setJMSCorrelationID("dallas") ;
   
      publisher.publish(topic, txtmsg) ;
 
      txtmsg.clearProperties() ;
      txtmsg.setObjectProperty("carno", new Integer(55)) ;
      txtmsg.setStringProperty("color", "CYAN") ;
      txtmsg.setIntProperty("year", 2000) ;
      txtmsg.setStringProperty("make", "MERCEDES") ;
      txtmsg.setDoubleProperty("price", 19000) ;
      txtmsg.setJMSCorrelationID("atlanta") ;
      publisher.publish(topic, txtmsg) ;

      txtmsg.clearProperties() ;
      txtmsg.setObjectProperty("carno", new Integer(99099)) ;
      txtmsg.setStringProperty("color", "RED") ;
      txtmsg.setIntProperty("year", 1998) ;
      txtmsg.setStringProperty("make", "ACURA") ;
      txtmsg.setDoubleProperty("price", 19995) ;
      txtmsg.setJMSCorrelationID("atlanta") ;
      publisher.publish(topic, txtmsg) ;

      tsess.commit() ;

      /* Receive the text Message for two subscribers */
      boolean done=false ;
      System.out.println ("Dequeue Message for Subscriber 1") ;
      while (!done)  {
        dtxtmsg = (TextMessage) (tsub1.receiveNoWait() ) ;

        if (dtxtmsg == null) {
          done=true;
        } else
        {
          System.out.print(" Color: " + dtxtmsg.getStringProperty("color"));
          System.out.print(" Make: " + dtxtmsg.getStringProperty("make"));
          System.out.print(" Year: " + dtxtmsg.getStringProperty("year"));
          System.out.print(" Price: " + dtxtmsg.getStringProperty("price"));
          System.out.println(" Carno: " + dtxtmsg.getStringProperty("carno"));
        }
      }

      System.out.println ("Dequeue Message for Subscriber 2") ;
       
      done=false ;
      while (!done) {
        dtxtmsg = (TextMessage) (tsub2.receive(1) ) ;
        if ( dtxtmsg == null ) { 
           done=true ;
        } else {
          System.out.print(" Color: " + dtxtmsg.getStringProperty("color"));
          System.out.print(" Make: " + dtxtmsg.getStringProperty("make"));
          System.out.print(" Year: " + dtxtmsg.getStringProperty("year"));
          System.out.print(" Price: " + dtxtmsg.getStringProperty("price"));
          System.out.println(" Carno: " + dtxtmsg.getStringProperty("carno"));
        }
      }
      tsess.commit() ;
      ((AQjmsDestination)topic).stop(tsess, true, true, false);

    } catch (Exception e) {
      System.out.println("Error in performJmsOperations: " + e) ;
    }
 } 
}
