/* $Header: aqjmsdemo06.java 05-jun-2007.15:10:24 aatam Exp $ */

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
    jleinawe    03/12/03 - add unschedulePropagation call
    jleinawe    12/20/02 - update instructions
    rbhyrava    01/09/01 - bug 1419924
    rbhyrava    07/13/00 - fix typo
    rbhyrava    03/16/00 - AQ jms demos
    rbhyrava    03/15/00 - AQ JMS demo - Propagation
    rbhyrava    03/15/00 - Creation
 */

/**
 *  @version $Header: aqjmsdemo06.java 05-jun-2007.15:10:24 aatam Exp $
 *  @author  rbhyrava
 *  @since   release specific (what release of product did this appear in)
 */

/***
 * This is a sample java file which uses Oracle JMS - Java Messaging Service 
 * API to schedule Propagation between Queues in Oracle database 
 *
 * This demo is functionally same a newaqdemo01.sql and does the following:
 * -- Create two queue tables - input_queue_table, prop_queue_table
 * -- Create two queues - input_queue belonging to input_queue_table,
 *    prop_queue belonging to prop_queue_table
 * -- Create two subscribers to input_queue - prog1, prog2
 * -- Create one subscribers to input_queue - prog3 at prop_queue
 * -- Schedule propagation between input_queue and other queues in
 *    the database
 *
 * Instructions for setting up and running this demo are found in
 * aqjmsREADME.txt.
 *
 ***/

/* Object Message */

/* import useful packages */
import oracle.AQ.*;
import oracle.jms.*;
import javax.jms.*;
import java.lang.*;

public class aqjmsdemo06
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
          performJmsOperations(tsess);
          tsess.close();
          tconn.close();
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
      AQQueueTableProperty qtprop1,qtprop2 ;
      AQQueueTable qtable,table1, table2;
      AQjmsDestinationProperty dprop;
      Topic topic1, topic2;
      try {
         /* Create Queue Tables */
         System.out.println("Creating Input Queue Table...") ;

         /* Drop the queue if already exists */
         try {
           qtable=((AQjmsSession)tsess).getQueueTable("jmsuser", 
                  "INPUT_QUEUE_TABLE" );
           qtable.drop(true);
         } catch (Exception e) {} ;
         try {
           qtable=((AQjmsSession)tsess).getQueueTable("jmsuser", 
                  "PROP_QUEUE_TABLE" );
           qtable.drop(true);
         } catch (Exception e) {} ;

         qtprop1 = new AQQueueTableProperty ("SYS.AQ$_JMS_OBJECT_MESSAGE") ;
         qtprop1.setComment("input queue") ;
         qtprop1.setMultiConsumer(true) ;
         qtprop1.setCompatible("8.1") ;
         qtprop1.setPayloadType("SYS.AQ$_JMS_OBJECT_MESSAGE") ;
         table1 = ((AQjmsSession)tsess).createQueueTable("JMSUSER", 
           "INPUT_QUEUE_TABLE", qtprop1) ;
  
         System.out.println("Creating Propagation Queue Table...") ;
         qtprop2 = new AQQueueTableProperty ("SYS.AQ$_JMS_OBJECT_MESSAGE") ;
         qtprop2.setComment("Popagation queue") ;
         qtprop2.setPayloadType("SYS.AQ$_JMS_OBJECT_MESSAGE") ;
         qtprop2.setMultiConsumer(true) ;
         qtprop2.setCompatible("8.1") ;
         table2 = ((AQjmsSession)tsess).createQueueTable("JMSUSER", 
           "PROP_QUEUE_TABLE", qtprop2) ;
  
        System.out.println ("Creating Topic input_queue...");
        dprop = new AQjmsDestinationProperty() ;
        dprop.setComment("create topic 1") ;
        topic1=((AQjmsSession)tsess).createTopic(table1,"INPUT_QUEUE",dprop) ;
  
        dprop.setComment("create topic 2") ;
        topic2=((AQjmsSession)tsess).createTopic( table2,"PROP_QUEUE",dprop) ;
  
        /* Start the topic */
        ((AQjmsDestination)topic1).start(tsess, true, true);
        ((AQjmsDestination)topic2).start(tsess, true, true);
        System.out.println("Successfully setup Topics");  

      } catch (Exception ex) {
         System.out.println("Error in setupTopic: " + ex);
         throw ex;
      }

   }


  public static void performJmsOperations(TopicSession tsess) 
  throws Exception
  {
    Topic topic1,topic2;
    TopicSubscriber[] subs;
    AQjmsAgent agt;
    ObjectMessage objmsg  = null, robjmsg=null;
    TopicPublisher publisher ;
    Message  sobj , rmsg;
    String[] cities={"BELMONT","REDWOOD SHORES", "SUNNYVALE", "BURLINGAME" };
    try
    {
      
      System.out.println("Get Topics...") ;
      topic1 = ((AQjmsSession)tsess).getTopic("JMSUSER", "INPUT_QUEUE") ;
      topic2 = ((AQjmsSession)tsess).getTopic("JMSUSER", "PROP_QUEUE") ;

      System.out.println("Creating Topic Subscribers...") ;

      subs = new TopicSubscriber[3] ;
      subs[0] = ((AQjmsSession)tsess).createDurableSubscriber(
                                  topic1, "PROG1", null, false);
      subs[1] = ((AQjmsSession)tsess).createDurableSubscriber(
                                  topic1, "PROG2", "JMSPriority > 2", false);
      subs[2] = tsess.createDurableSubscriber(
                                  topic2, "PROG3", null , false) ;

      agt = new AQjmsAgent("PROG3", "PROP_QUEUE" ) ;

      System.out.println("Creating Remote Subscriber...") ;
      ((AQjmsSession)tsess).createRemoteSubscriber(
                                  topic1, agt,"JMSPriority = 2");

      /* Schedule Propagation with latency 0 */ 
      System.out.println("Schedule Propagation...") ;
      ((AQjmsDestination)topic1).schedulePropagation(
                                tsess, null, null, null, null, new Double(0)) ;

      System.out.println("Publish messages...") ;
      objmsg = ((AQjmsSession)tsess).createObjectMessage() ;

      publisher = tsess.createPublisher(topic1); 

      /*publish 100 messages*/

      for ( int i = 1 ; i <= 100 ; i++) 
      {
         objmsg.setIntProperty("Id",i) ;
         if ( ( i % 3 ) == 0 )  { 
           objmsg.setStringProperty("City",cities[0]) ;
         } 
         else if ((i % 4 ) == 0) { 
           objmsg.setStringProperty("City",cities[1]) ;
         }
         else if (( i % 2) == 0) {
           objmsg.setStringProperty("City",cities[2]) ;
         }
         else {
           objmsg.setStringProperty("City",cities[3]) ;
         }

         objmsg.setIntProperty("Priority",(1+ (i%3))) ;

         sobj = new Message() ;
         sobj.setId(i) ;
         sobj.setName("message# "+i) ;
         sobj.setData(500);
         objmsg.setObject(sobj) ;
         objmsg.setJMSCorrelationID(""+i) ;
         objmsg.setJMSPriority(1+(i%3)) ;
         publisher.publish(topic1,objmsg, DeliveryMode.PERSISTENT, 
             1 +(i%3), AQjmsConstants.EXPIRATION_NEVER);
      }
      System.out.println("Commit now...") ;
      tsess.commit() ;

      Thread.sleep(50000);

      /* Receive messages for each subscriber */
      System.out.println("Receive Messages...") ;
      for (int i=0; i< subs.length ; i++) 
      {
         System.out.println ("Messages for subscriber : " +i) ;
         if (subs[i].getMessageSelector() != null)
         {
            System.out.println("  with selector: " +
                             subs[i].getMessageSelector());
         }

         boolean done = false;
         while(!done) {
         try {
           robjmsg = (ObjectMessage)( subs[i].receiveNoWait() );
           if (robjmsg != null) 
           {
             rmsg = (Message)robjmsg.getObject();
             System.out.print("Name : " +rmsg.getName()) ;
             System.out.print(" Pri: " +robjmsg.getJMSPriority()) ;
             System.out.print(" Message: " +robjmsg.getIntProperty("Id")) ;
             System.out.print(" " +robjmsg.getStringProperty("City")) ;
             System.out.println(" " +robjmsg.getIntProperty("Priority")) ;
           }
           else {
             System.out.println ("No more messages.") ;
             done=true;
           }
           tsess.commit();
         } catch (Exception e) {
           System.out.println("Error in performJmsOperations: " + e) ;
           done=true;
         }
        } /* while loop*/
       }
       ((AQjmsDestination)topic1).unschedulePropagation(tsess,null);
    } catch (Exception e) {
      System.out.println("Error in performJmsOperations: " + e) ;
      throw e;
    }
 } /* end of demo06 */
}
