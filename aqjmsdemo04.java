/* $Header: aqjmsdemo04.java 05-jun-2007.15:10:23 aatam Exp $ */

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
    rbhyrava    03/01/02 - use ORAData
    rbhyrava    01/09/01 - bug 1419924
    rbhyrava    03/16/00 - AQ jms demos
    rbhyrava    03/15/00 - AQ JMS demo - Object Type Payload
    rbhyrava    03/15/00 - Creation
 */

/**
 *  @version $Header: aqjmsdemo04.java 05-jun-2007.15:10:23 aatam Exp $
 *  @author  rbhyrava
 *  @since   release specific (what release of product did this appear in)
 */
/***
 * This is a sample java file which uses Oracle JMS - Java Messaging Service 
 * API to Publish/Receive messages to/from topic. The Oracle Type Payload is 
 *  used. Dequeue is based on the Payload content 
 * This is an Oracle Extention to JMS.
 * This demo does the following:
 * -- Create a multi-consumer topic with Payload type as EMP
 *    Refer to aqjmsdemo.sql for definition of EMP and CARS Object Type 
      definitions
 * -- Create a Topic Publisher 
 * -- Create two durable subscribers based on the Payload content 
 * -- Publish messages 
 * -- Receive the messages for both the subscribers 
 *
 * Instructions for setting up and running this demo are found in
 * aqjmsREADME.txt.
 ***/

/* import useful packages */
import oracle.AQ.*;
import oracle.jms.*;
import javax.jms.*;
import java.lang.*;

public class aqjmsdemo04
{
  public static void main (String args [])
       throws java.sql.SQLException, ClassNotFoundException, JMSException
  {
      TopicSession  tsess = null;
      TopicConnectionFactory tcfact=null;
      TopicConnection tconn=null;
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
          publishReceiveMessages(tsess);
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
         System.out.println("Creating Queue Table...") ;
         /* Drop the queue if already exists */
         try {
           qtable=((AQjmsSession)tsess).getQueueTable("jmsuser", "jmsadtqueue");
           qtable.drop(true);
         } catch (Exception e) {} ;

         qtprop = new AQQueueTableProperty ("EMP") ;
         qtprop.setComment("EMP queue") ;
         qtprop.setMultiConsumer(true) ;
         qtprop.setCompatible("8.1") ;
         qtprop.setPayloadType("JMSUSER.EMP") ;

         qtable = ((AQjmsSession)tsess).createQueueTable("JMSUSER",
                         "JMSADTQUEUE", qtprop) ;

         System.out.println ("Creating Topic EMPCARS...");
         dprop = new AQjmsDestinationProperty() ;
         dprop.setComment("create topic") ;
         topic= ((AQjmsSession)tsess).createTopic( qtable,"EMPCARS",dprop);

         /* Start the topic */
         ((AQjmsDestination)topic).start(tsess, true, true);
         System.out.println("Successfully setup Topic");  
      } catch (Exception ex) {
         System.out.println("Error in setupTopic: " + ex);
         throw ex;
      }
   }

  public static void publishReceiveMessages(TopicSession tsess) 
     throws Exception
  {
    Topic topic =null;
    TopicSubscriber[] subs;
    TopicPublisher publisher;
    StringBuffer txtbuf ;
    AdtMessage adt_msg;
    Emp empmsg, rmsg;
    Cars c;
    try
    {
    
      /* Get the topic */
      System.out.println("Getting the Topic...") ;
      topic = ((AQjmsSession)tsess).getTopic("JMSUSER", "EMPCARS");

      System.out.println("Creating Topic Subscribers...") ;
      subs = new TopicSubscriber[2] ;

      subs[0] = ((AQjmsSession)tsess).createDurableSubscriber(topic, 
               "PROG1", "TAB.USER_DATA.CAROWN.COLOR = 'RED'", 
                   false, Emp.getORADataFactory()) ;
      subs[1] = ((AQjmsSession)tsess).createDurableSubscriber(topic, 
               "PROG2", "TAB.USER_DATA.RANK > 2", 
                   false, Emp.getORADataFactory()) ;

      System.out.println("Create a Publisher...") ;
      publisher = tsess.createPublisher(topic); 

      adt_msg = ((AQjmsSession)tsess).createAdtMessage() ;

      String[] names={"KING JOE","SCOTT RAY","ADITYA NELLORE","LORI MAXWELL" };
      int[] rank={1,2,3,4} ;
      String color[] = {"YELLOW", "RED", "BLUE","RED" } ;
      String make[] = {"BMW", "ACURA", "MERCEDES","LEXUS" } ;
      empmsg = new Emp() ;
      c = new Cars() ;
  
      System.out.println("Publish messages...") ;

      for ( int i = 1 ; i < 4 ; i++) 
      {
         java.math.BigDecimal id = new java.math.BigDecimal(i) ;
         empmsg.setId(id) ;
         empmsg.setName(names[i]) ;
         empmsg.setRank(new java.math.BigDecimal(rank[i])) ;
         c.setCarno(new java.math.BigDecimal(1000 + i)) ;
         c.setMake(make[i] );
         c.setColor(color[i] );
         c.setPrice(new java.math.BigDecimal(10000 * i) );
         empmsg.setCarown(c) ;

         adt_msg.clearProperties() ;
         //adt_msg.setJMSCorrelationID(new Integer(100+i) ) ;
         adt_msg.setAdtPayload(empmsg) ;
         adt_msg.setJMSPriority(1+(i%3)) ;
         publisher.publish(topic,adt_msg, DeliveryMode.PERSISTENT, 
              1+(i%3),AQjmsConstants.EXPIRATION_NEVER);
         
         System.out.print("Sent Message Pri: " +adt_msg.getJMSPriority()) ;
         System.out.print(" Id: " +empmsg.getId()) ;
         System.out.print(" Name: " +empmsg.getName()) ;
         System.out.print(" Rank: " +empmsg.getRank()) ;
         System.out.print(" Car: " +empmsg.getCarown().getColor()) ;
         System.out.println(" " +empmsg.getCarown().getMake()) ;
      }

      System.out.println("Commit") ;
      tsess.commit() ;

      Thread.sleep(500);

      System.out.println("Receive Messages...") ;

      /* Receive messages for each subscriber */

      for (int i=0; i< subs.length ; i++) 
      {
         System.out.println ("Messages for subscriber : " +i) ;
         boolean more = true;
         while(more) {
           try {
             adt_msg = (AdtMessage)( subs[i].receiveNoWait() );
             if (adt_msg != null) 
             {
               rmsg = (Emp) (adt_msg.getAdtPayload());
               System.out.print("Priority : " +adt_msg.getJMSPriority()) ;
               System.out.print(" Id: " +rmsg.getId()) ;
               System.out.print(" Name: " +rmsg.getName()) ;
               System.out.print(" Rank: " +rmsg.getRank()) ;
               System.out.print(" Car: " +rmsg.getCarown().getColor()) ;
               System.out.println(" " +rmsg.getCarown().getMake()) ;
             }
             else {
               System.out.println ("No more messages.") ;
               more=false;
             }
             tsess.commit();
           } catch (Exception e) {
               System.out.println ("Error in Receive: " + e) ;
               more=false;
           }
        } /* while loop*/
       }
    } catch (Exception e) {
      System.out.println("Error in publish receive Messages: " + e) ;
      throw e;
    }
 }
}
