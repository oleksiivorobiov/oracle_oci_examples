/* $Header: aqjmsdemo08.java 05-jun-2007.15:10:25 aatam Exp $ */

/* Copyright (c) 2002, 2007, Oracle. All rights reserved.  */

/*
   DESCRIPTION
   OJMS JMS 1.1 domain unification demo

   PRIVATE CLASSES
     None

   NOTES
     This sample performs JMS topic and queue operations using the same JMS
     connection and session and in the context of the same transaction using
     JMS 1.1 domain unified apis.

   MODIFIED    (MM/DD/YY)
    aatam       06/05/07 - password need to be consistent
    jleinawe    07/11/05 - 
    jleinawe    09/19/03 - no destination for Producer.send(...)
    jleinawe    12/20/02 - update instructions
    jleinawe    11/01/02 - 
    vmaganty    10/04/02 - checkin OJMS JMS 1.1 compliance changes
    vmaganty    09/20/02 - Creation
 */

/**
 *  @version $Header: aqjmsdemo08.java 05-jun-2007.15:10:25 aatam Exp $
 *  @author  vmaganty
 *  @since   release specific (what release of product did this appear in)
 */


/*** This is a sample java file which uses Oracle JMS - Java Messaging
 * Service API to Enqueue/Dequeue JMS Messages from both queues and
 * topics in the same transaction using the same session and
 * connection using the JMS 1.1 domain unfied APIs.
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

public class aqjmsdemo08
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

	 System.out.println("Perform queue and topic operations on the same connection and session in the same transaction");
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
	qtable=((AQjmsSession)sess).getQueueTable("jmsuser", "jmstxtqtable" );
	qtable.drop(true);
      } catch (Exception e) {} ;
      
      qtprop = new AQQueueTableProperty ("SYS.AQ$_JMS_TEXT_MESSAGE") ;
      qtprop.setMultiConsumer(true) ;
      qtprop.setCompatible("8.1") ;
      qtprop.setPayloadType("SYS.AQ$_JMS_TEXT_MESSAGE") ;
      qtable = ((AQjmsSession)sess).createQueueTable("JMSUSER", 
						     "jmstxtqtable", qtprop) ;
      
      System.out.println ("Creating Topic ...");
      dprop = new AQjmsDestinationProperty() ;
      topic=((AQjmsSession)sess).createTopic( qtable,"jmstopic",dprop) ;
      
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
    TextMessage txtmsg, dtxtmsg;
    
    try {
      System.out.println ("Get the Topic...");
      topic = ((AQjmsSession)sess).getTopic("JMSUSER","jmstopic") ;
      
      System.out.println("Creating a nondurable MessageConsumer with selector...") ;

      mcon = 
	sess.createConsumer(topic, 
			    "(year = 1998 OR color NOT IN ('GREEN','RED','WHITE')) "
			    + " AND make IN ('ACURA ', 'BMW', 'MERCEDES')", false);

      System.out.println("Creating a durable TopicSubscriber with selector...") ;

      tsub = sess.createDurableSubscriber(topic, "atlanta",
					  "price < 20000", false );
      
      
      System.out.println("Produce and send text messages to topic...") ;
      
      mprod = sess.createProducer(topic); 

      txtmsg = sess.createTextMessage() ;
      txtmsg.setText("Cars Distribution") ;
      txtmsg.setObjectProperty("carno", new Integer(12345)) ;
      txtmsg.setStringProperty("color", "BLUE") ;
      txtmsg.setIntProperty("year", 1999) ;
      txtmsg.setStringProperty("make", "BMW") ;
      txtmsg.setDoubleProperty("price", 25995) ;
      txtmsg.setJMSCorrelationID("dallas") ;
      
      mprod.send(txtmsg) ;
      
      txtmsg.clearProperties() ;
      txtmsg.setObjectProperty("carno", new Integer(55)) ;
      txtmsg.setStringProperty("color", "CYAN") ;
      txtmsg.setIntProperty("year", 2000) ;
      txtmsg.setStringProperty("make", "MERCEDES") ;
      txtmsg.setDoubleProperty("price", 19000) ;
      txtmsg.setJMSCorrelationID("atlanta") ;
      
      mprod.send(txtmsg) ;

      txtmsg.clearProperties() ;
      txtmsg.setObjectProperty("carno", new Integer(99099)) ;
      txtmsg.setStringProperty("color", "RED") ;
      txtmsg.setIntProperty("year", 1998) ;
      txtmsg.setStringProperty("make", "ACURA") ;
      txtmsg.setDoubleProperty("price", 19995) ;
      txtmsg.setJMSCorrelationID("atlanta") ;
      
      mprod.send(txtmsg) ;

      // sess.commit() ;

      System.out.println ("Consume messages from topic using non durable consumer") ;
      boolean done = false ;
      while (!done)  {
        dtxtmsg = (TextMessage) (mcon.receiveNoWait() ) ;
	
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

      System.out.println ("Consume messages from topic using durable subscriber") ;
      done=false ;
      while (!done)  {
        dtxtmsg = (TextMessage) (tsub.receiveNoWait() ) ;

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
      
      qtprop = new AQQueueTableProperty ("SYS.AQ$_JMS_MAP_MESSAGE") ;
      qtprop.setCompatible("8.1") ;
      qtprop.setPayloadType("SYS.AQ$_JMS_MAP_MESSAGE") ;
      
      /* Drop if the queue table already exists */
      try { 
	qtable=((AQjmsSession)sess).getQueueTable("JMSUSER", "JMSMAPQTABLE");
	qtable.drop(true);
      } catch (Exception e) {} 
      
      qtable = ((AQjmsSession)sess).createQueueTable("JMSUSER", 
						      "jmsmapqtable", qtprop) ;
      
      System.out.println ("Creating Queue...");
      dprop = new AQjmsDestinationProperty() ;
      q1=((AQjmsSession)sess).createQueue(qtable,"JMS_MAPQ",dprop) ;
      
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
    
    StringBuffer txtbuf ;
    TextMessage txtmsg, dtxtmsg;
    try
    {
      /* Get Queue */
      q1 = ((AQjmsSession)sess).getQueue("JMSUSER", "JMS_MAPQ") ;

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

        mprod.send(Messages[i], DeliveryMode.PERSISTENT, 
            1+ (i%10) , AQjmsConstants.EXPIRATION_NEVER ) ;
        try { 
           Thread.sleep(5000) ;
        } catch (InterruptedException e) {}  ;
	// sess.commit() ;
      }
      System.out.println("Successfully Sent Messages.");  

    } catch (JMSException e) {
      System.out.println("Error in Sending Messages : " + e) ;
      throw e;
    }
 }

  public static void dequeueMessages(Session sess) throws Exception
  {
    MapMessage mm;
    int mesgno = 0;
    javax.jms.Queue queue;
    MessageConsumer mcon; 

    try
    {
      System.out.println("Get the Destination") ;
      queue = ((AQjmsSession)sess).getQueue("JMSUSER", "JMS_MAPQ") ;
      
      System.out.println("Create message consumer with selector...") ;      
      mcon = sess.createConsumer((Destination) queue, "JMSCorrelationID='BLUE'") ;
    
      System.out.println("Receive messages using the message consumer...") ;      
      boolean done = false ;
      while (!done)  {
	mm = (MapMessage) (mcon.receiveNoWait() ) ;
	
	if (mm == null) {
	  done=true;
	} else {
	  mesgno++;
	  System.out.println("Retrieved message " + 
			     mesgno + " with correlation: " + mm.getJMSCorrelationID());
	  System.out.println (" "+ mm.getStringProperty ("color") +
			      " " + mm.getStringProperty("make") +
			      " " + mm.getIntProperty("year") +
			      " " + mm.getDoubleProperty("price")) ;
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
      System.out.println("Exception onMessage:" + e) ;
    }
  }
}

