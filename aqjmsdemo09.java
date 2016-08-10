/* $Header: aqjmsdemo09.java 05-jun-2007.15:10:27 aatam Exp $ */

/* Copyright (c) 2002, 2007, Oracle. All rights reserved.  */

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
    jleinawe    11/19/02 - queue name conflict
    vmaganty    11/05/02 - OJMS bulk enqueue/dequeue changes
    vmaganty    10/30/02 - Creation
 */

/**
 *  @version $Header: aqjmsdemo09.java 05-jun-2007.15:10:27 aatam Exp $
 *  @author  vmaganty
 *  @since   release specific (what release of product did this appear in)
 */


/*** This is a sample java file which uses Oracle JMS - Oracle
 * specific bulk array enqueue dequeue messaging APIs to
 * Enqueue/Dequeue JMS Messages from both queues and topics in the
 * same transaction using the same session and connection using JMS
 * 1.1 domain unfied APIs.
 *
 * Instructions for setting up and running this demo are found in
 * aqjmsREADME.txt.
 *
 ***/

/* import useful packages */
import oracle.AQ.*;
import javax.jms.*;
import oracle.jms.*;
import java.lang.*;

public class aqjmsdemo09
{
  public static void main (String args [])
       throws java.sql.SQLException, ClassNotFoundException, JMSException
  {
     Session  sess = null;
     ConnectionFactory cfact = null;
     Connection conn = null;
     try {
       if (args.length < 4 )
         System.out.println("Usage:java filename [SID] [HOST] [PORT] [DRIVER]");
       else {
	 System.out.println("Create connection factory, connection and session");
	 cfact = 
	   AQjmsFactory.getConnectionFactory(args[1], args[0], 
					     Integer.parseInt(args[2]), 
					     args[3]);

	 conn = cfact.createConnection( "jmsuser","JMSUSER");

          /* Create a Topic Session */
	 sess = conn.createSession(true, Session.CLIENT_ACKNOWLEDGE);
	 
	 System.out.println("Start the connection");
	 conn.start() ;
	 
	 System.out.println("Setup a queue and a topic");
	 setupQueue(sess) ;
	 setupTopic(sess) ;
	 System.out.println("Perform bulk queue and topic operations on the");
	 System.out.println("same connection and session in the same transaction");
	 System.out.println("Begin transaction");
	 enqueueMessages(sess);
	 dequeueMessages(sess);	 
	  
	 performTopicOperations(sess);
	 System.out.println("Committing transaction");
	 sess.commit();
	 System.out.println("Cleaning up");
	 sess.unsubscribe("atlanta");
	 sess.close() ;
	 conn.close() ;
	 System.out.println("End of Demo") ;
       }
     }
     catch (Exception ex) {
       System.out.println("Exception-1: " + ex);
       ex.printStackTrace();
     }
  }

  public static void setupTopic(Session sess) throws Exception
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
	qtable=((AQjmsSession)sess).getQueueTable("jmsuser", "jmst9table" );
	qtable.drop(true);
      } catch (Exception e) {} ;
      
      qtprop = new AQQueueTableProperty ("SYS.AQ$_JMS_MESSAGE") ;
      qtprop.setMultiConsumer(true) ;
      qtprop.setCompatible("8.1") ;
      qtprop.setPayloadType("SYS.AQ$_JMS_MESSAGE") ;
      qtable = ((AQjmsSession)sess).createQueueTable("JMSUSER", 
						     "jmst9table", qtprop) ;
      
      System.out.println ("Creating Topic ...");
      dprop = new AQjmsDestinationProperty() ;
      topic=((AQjmsSession)sess).createTopic( qtable,"JMSDemoTopic9",dprop) ;
      
      /* Start the topic */
      ((AQjmsDestination)topic).start(sess, true, true);
      System.out.println("Successfully setup Topic");  
    } catch (Exception ex) {
      System.out.println("Error in setupTopic: " + ex);
      throw ex;
    }
  }

  public static void performTopicOperations(Session sess) 
  {
    Topic topic = null;
    MessageConsumer mcon;
    TopicSubscriber tsub;
    MessageProducer mprod;
    javax.jms.Message[] send_messages = null;
    javax.jms.Message[] receive_messages = null;
    
    try {
      System.out.println ("Get the Topic...");
      topic = ((AQjmsSession)sess).getTopic("JMSUSER","JMSDemoTopic9") ;
      
      System.out.println("Creating a nondurable MessageConsumer with selector...") ;

      mcon = 
	sess.createConsumer(topic, 
			    "(year = 1998 OR color NOT IN ('GREEN','RED','WHITE')) "
			    + " AND make IN ('ACURA ', 'BMW', 'MERCEDES')", false);

      System.out.println("Creating a durable TopicSubscriber with selector...") ;

      tsub = sess.createDurableSubscriber(topic, "atlanta",
					  "price < 20000", false );
      
      
      System.out.println("Produce and send assorted messages to topic...") ;
      
      send_messages = new javax.jms.Message[4];

      mprod = sess.createProducer(topic); 

      
      send_messages[0] = (javax.jms.Message) (sess.createMapMessage()); 
      send_messages[0].setObjectProperty("carno", new Integer(55)) ;
      send_messages[0].setStringProperty("color", "CYAN") ;
      send_messages[0].setIntProperty("year", 2000) ;
      send_messages[0].setStringProperty("make", "MERCEDES") ;
      send_messages[0].setDoubleProperty("price", 19000) ;
      send_messages[0].setJMSCorrelationID("atlanta") ;
      
      send_messages[1] = (javax.jms.Message) (sess.createBytesMessage()) ;
      ((BytesMessage) send_messages[1]).writeBytes("Cars Distribution".getBytes()) ;
      send_messages[1].setObjectProperty("carno", new Integer(12345)) ;
      send_messages[1].setStringProperty("color", "BLUE") ;
      send_messages[1].setIntProperty("year", 1999) ;
      send_messages[1].setStringProperty("make", "BMW") ;
      send_messages[1].setDoubleProperty("price", 15995) ;
      send_messages[1].setJMSCorrelationID("dallas") ;
      
      
      send_messages[2] = (javax.jms.Message) (sess.createStreamMessage()); 
      send_messages[2].setObjectProperty("carno", new Integer(99099)) ;
      send_messages[2].setStringProperty("color", "RED") ;
      send_messages[2].setIntProperty("year", 1998) ;
      send_messages[2].setStringProperty("make", "ACURA") ;
      send_messages[2].setDoubleProperty("price", 19995) ;
      send_messages[2].setJMSCorrelationID("seattle") ;

      send_messages[3] = (javax.jms.Message) (sess.createTextMessage()) ;
      ((TextMessage) send_messages[3]).setText("Cars WholeSale") ;
      send_messages[3].setObjectProperty("carno", new Integer(5678)) ;
      send_messages[3].setStringProperty("color", "RED") ;
      send_messages[3].setIntProperty("year", 2003) ;
      send_messages[3].setStringProperty("make", "Subaru") ;
      send_messages[3].setDoubleProperty("price", 18488) ;
      send_messages[3].setJMSCorrelationID("portland") ;

      
      ((AQjmsProducer) mprod).bulkSend((Destination) topic, send_messages) ;
      
      // sess.commit() ;
      
      System.out.println ("Consume messages in bulk from topic using non durable consumer") ;
      boolean done = false ;
      while (!done)  {
	receive_messages = ((AQjmsConsumer) mcon).bulkReceiveNoWait(2);
	
        if (receive_messages == null) {
          done=true;
        } else {
	  for ( int i=0; i<receive_messages.length; i++) {
	    System.out.print(" Color: " + receive_messages[i].getStringProperty("color"));
	    System.out.print(" Make: " + receive_messages[i].getStringProperty("make"));
	    System.out.print(" Year: " + receive_messages[i].getStringProperty("year"));
	    System.out.print(" Price: " + receive_messages[i].getStringProperty("price"));
	    System.out.println(" Carno: " + receive_messages[i].getStringProperty("carno"));
	  }
	}
      }
      
      System.out.println ("Consume messages in bulk from topic using durable subscriber") ;
      done=false ;
      while (!done)  {
        receive_messages =  ((AQjmsConsumer) tsub).bulkReceiveNoWait(2) ;
	
        if (receive_messages == null) {
          done=true;
        } else {
	  for ( int i=0; i<receive_messages.length; i++) {
	    System.out.print(" Color: " + receive_messages[i].getStringProperty("color"));
	    System.out.print(" Make: " + receive_messages[i].getStringProperty("make"));
	    System.out.print(" Year: " + receive_messages[i].getStringProperty("year"));
	    System.out.print(" Price: " + receive_messages[i].getStringProperty("price"));
	    System.out.println(" Carno: " + receive_messages[i].getStringProperty("carno"));
	    if (receive_messages[i] instanceof BytesMessage)
	      System.out.println(" Text: " + new String(((AQjmsBytesMessage) receive_messages[i]).getBytesData()));
	    else if (receive_messages[i] instanceof TextMessage)
	      System.out.println(" Text: " + new String(((TextMessage) receive_messages[i]).getText()));
	  }
        }
      }
      
      // sess.commit() ;

      ((AQjmsDestination)topic).stop(sess, true, true, false);
      
    } catch (Exception e) {
      System.out.println("Error in performJmsOperations: " + e) ;
    }
  } 
  

  public static void setupQueue(Session sess) throws Exception
  {
    AQQueueTableProperty qtprop ;
    AQQueueTable qtable;
    AQjmsDestinationProperty dprop;
    javax.jms.Queue q1;
    
    try {
      
      /* Create Queue Tables */
      System.out.println("Creating Queue Table...") ;
      
      qtprop = new AQQueueTableProperty ("SYS.AQ$_JMS_MESSAGE") ;
      qtprop.setCompatible("8.1") ;
      qtprop.setPayloadType("SYS.AQ$_JMS_MESSAGE") ;
      
      /* Drop if the queue table already exists */
      try { 
	qtable=((AQjmsSession)sess).getQueueTable("JMSUSER", "JMSQ9TABLE");
	qtable.drop(true);
      } catch (Exception e) {} 
      
      qtable = ((AQjmsSession)sess).createQueueTable("JMSUSER", 
						      "jmsq9table", qtprop) ;
      
      System.out.println ("Creating Queue...");
      dprop = new AQjmsDestinationProperty() ;
      q1=((AQjmsSession)sess).createQueue(qtable,"JMSDemoQueue9",dprop) ;
      
      /* Start the Queue */
      ((AQjmsDestination)q1).start(sess, true, true);
      
      System.out.println("Successfully setup Queue");  
    } catch (Exception ex) {
      System.out.println("Error in setupQueue: " + ex);
      throw ex;
    }
  }
  
  public static void enqueueMessages(Session sess) throws Exception
  {
    javax.jms.Queue q1 ;
    MapMessage[] Messages;
    byte[] barray ;
    MessageProducer mprod;
    
    try
    {
      /* Get Queue */
      q1 = ((AQjmsSession)sess).getQueue("JMSUSER", "JMSDemoQueue9") ;

      System.out.println("Produce and send text messages to queue...") ;

      barray  = new byte[25] ;
      for ( int i=0 ; i< 25 ; i++) barray[i] = 67 ;
      
      Messages = new MapMessage[5] ;

      Messages[0] = sess.createMapMessage() ;
      Messages[0].setIntProperty ("carno", 3355) ;
      Messages[0].setStringProperty ("color", "BLUE") ;
      Messages[0].setStringProperty ("make", "BMW") ;
      Messages[0].setDoubleProperty ("price", 20000) ;
      Messages[0].setIntProperty ("year", 1999) ;

      Messages[1] = sess.createMapMessage() ;
      Messages[1].setIntProperty ("carno", 4444) ;
      Messages[1].setStringProperty ("color", "BLACK") ;
      Messages[1].setStringProperty ("make", "ACURA") ;
      Messages[1].setDoubleProperty ("price", 22995) ;
      Messages[1].setIntProperty ("year", 1998) ;

      Messages[2] = sess.createMapMessage() ;
      Messages[2].setIntProperty ("carno", 1212) ;
      Messages[2].setStringProperty ("color", "BLUE") ;
      Messages[2].setStringProperty ("make", "MERCEDES") ;
      Messages[2].setDoubleProperty ("price", 30995) ;
      Messages[2].setIntProperty ("year", 2001) ;

      Messages[3] = sess.createMapMessage() ;
      Messages[3].setIntProperty ("carno", 5345) ;
      Messages[3].setStringProperty ("color", "GREEN") ;
      Messages[3].setStringProperty ("make", "LEXUS") ;
      Messages[3].setDoubleProperty ("price", 21895) ;
      Messages[3].setIntProperty ("year", 1995) ;

      Messages[4] = sess.createMapMessage() ;
      Messages[4].setIntProperty ("carno", 8909) ;
      Messages[4].setStringProperty ("color", "BLUE") ;
      Messages[4].setStringProperty ("make", "BMW") ;
      Messages[4].setDoubleProperty ("price", 40995) ;
      Messages[4].setIntProperty ("year", 2002) ;

      /* Create Queue Sender */ 
      mprod = sess.createProducer(q1) ;

      for ( int i = 0 ; i < 5 ; i++ ) 
      {
        Messages[i].setBytes("Picture" , barray) ;
        Messages[i].setJMSCorrelationID(Messages[i].getStringProperty("color"));
        System.out.println("Sending "+Messages[i].getStringProperty ("color") + 
                           " " + Messages[i].getStringProperty("make") + 
                           " " + Messages[i].getIntProperty("year") + 
                           " " + Messages[i].getDoubleProperty("price")) ; 
      }
      
      ((AQjmsProducer)mprod).bulkSend((javax.jms.Destination) q1, (javax.jms.Message[]) Messages) ;
      try { 
	Thread.sleep(5000) ;
      } catch (InterruptedException e) {}  ;
      // sess.commit() ;
      
      System.out.println("Successfully Sent Messages.");  
      
    } catch (JMSException e) {
      System.out.println("Error in Sending Messages : " + e) ;
      throw e;
    }
 }

  public static void dequeueMessages(Session sess) throws Exception
  {
    MapMessage mm;
    javax.jms.Message[] messages = null;
    int mesgno = 0;
    javax.jms.Queue queue;
    MessageConsumer mcon; 

    try
    {
      System.out.println("Get the Destination") ;
      queue = ((AQjmsSession)sess).getQueue("JMSUSER", "JMSDemoQueue9") ;
      
      System.out.println("Create message consumer with selector...") ;      
      mcon = sess.createConsumer((Destination) queue, "JMSCorrelationID='BLUE'") ;
    
      System.out.println("Receive messages using the message consumer...") ;      
      boolean done = false ;
      while (!done)  {
	messages = ((AQjmsConsumer) mcon).bulkReceiveNoWait(2);
	
	if (messages == null) {
	  done=true;
	} else {
	  for (int i=0; i < messages.length; i++) {
	    mm = (MapMessage) (messages[i]) ;
	    mesgno++;
	    System.out.println("Retrieved message " + 
			       mesgno + " with correlation: " + mm.getJMSCorrelationID());
	     System.out.println (" "+ mm.getStringProperty ("color") +
				" " + mm.getStringProperty("make") +
				" " + mm.getIntProperty("year") +
				 " " + mm.getDoubleProperty("price")) ; 
	  }
	}
      }
      
      /*
	try {
	sess.commit() ;
	} catch (Exception e) {
	System.out.println("Exception on Session Commit " + e) ;
	} 
      */
    } catch (JMSException e) {
      System.out.println("Exception while dequeueing:" + e) ;
    }
  }
}


