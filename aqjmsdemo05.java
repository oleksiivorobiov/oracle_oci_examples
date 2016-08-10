/* $Header: aqjmsdemo05.java 05-jun-2007.15:10:24 aatam Exp $ */

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
    aatam       06/27/06 - fix Queue is ambiguous error 
    jleinawe    07/11/05 - 
    jleinawe    12/20/02 - update instructions
    rbhyrava    01/09/01 - bug 1419924
    rbhyrava    03/15/00 - AQ JMS demo - Queue Browser
    rbhyrava    03/15/00 - Creation
 */

/**
 *  @version $Header: aqjmsdemo05.java 05-jun-2007.15:10:24 aatam Exp $
 *  @author  rbhyrava
 *  @since   release specific (what release of product did this appear in)
 */

/***
 * This is a sample java file which uses Oracle JMS - Java Messaging Service 
 * API to demonstrate QueueBrowser. 
 * Client uses a QueueBrowser to view messages on a queue without removing them.
 *
 * This demo does the following:
 * -- Create a single consumer queue of Payload type as CARS
 *    Refer to aqjmsdemo.sql for definition of CARS Object Type 
 * -- Create a Queue Sender
 * -- Create  a Queue Browser with Message Selector ;
       messages delivered to the browser to those that match the selector. 
 * -- Save the message Ids 
 * -- Dequeue the messages based on the Message ids
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
import java.util.* ;
public class aqjmsdemo05
{
  public static void main (String args [])
       throws java.sql.SQLException, ClassNotFoundException, JMSException
  {
     QueueSession  qsess = null;
     QueueConnectionFactory qcfact=null;
     QueueConnection qconn=null;
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

         qconn.start() ;
         setupQueue(qsess) ;
         sendMessages(qsess) ;
         queueBrowserDemo(qsess);
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

  public static void setupQueue(QueueSession qsess) throws Exception
  {
      AQQueueTable qtable=null;
      AQQueueTableProperty qtprop;
      AQjmsDestinationProperty dprop;
      javax.jms.Queue queue;
      try {
         /* Create Queue Tables */
         System.out.println("Creating Queue Table...") ;

         /* Drop the queue if already exists */
         try {
           qtable=((AQjmsSession)qsess).getQueueTable("jmsuser", "jmsqbtable" );
           qtable.drop(true);
         } catch (Exception e) {} ;

         qtprop = new AQQueueTableProperty ("SYS.AQ$_JMS_TEXT_MESSAGE") ;
         qtprop.setPayloadType("SYS.AQ$_JMS_TEXT_MESSAGE") ;

         qtable = ((AQjmsSession)qsess).createQueueTable("JMSUSER", 
                "jmsqbtable", qtprop) ;

         System.out.println ("Creating Queue jmsqb...");
         dprop = new AQjmsDestinationProperty() ;
         queue=((AQjmsSession)qsess).createQueue( qtable,"jmsqb",dprop) ;

         /* Start the Queue */
         ((AQjmsDestination)queue).start(qsess, true, true);
         System.out.println("Successfully setup Queue");  
      } catch (Exception ex) {
         System.out.println("Error in setupQueue: " + ex);
         throw ex;
      }
  }

  public static void sendMessages(QueueSession qsess) throws Exception
  {
    javax.jms.Queue q1;
    QueueSender qsender ;
    TextMessage txtmsg;
    StringBuffer txtbuf;
    String color[] = {"YELLOW", "RED", "BLUE","RED" } ;
    String make[] = {"BMW", "ACURA", "MERCEDES","LEXUS" } ;
    try
    {
      System.out.println ("get Queue JMSQB");
      q1=((AQjmsSession)qsess).getQueue("JMSUSER","JMSQB") ;

      System.out.println("Send messages...") ;
      /* Create Queue Sender */ 
      qsender = qsess.createSender(q1) ;

      txtbuf = new StringBuffer() ;
      txtbuf.append ("Favorite Cars") ;
      txtmsg = ((AQjmsSession)qsess).createTextMessage(txtbuf) ;

      for (int i =0; i < 4 ; i++) {
        txtmsg.setIntProperty("carno",1000 + i) ;
        txtmsg.setStringProperty("make", make[i]) ;
        txtmsg.setIntProperty ("year",1999 + i) ;
        txtmsg.setDoubleProperty("price",10000*(i+1));
        txtmsg.setStringProperty("color",color[i]) ;
        txtmsg.setJMSCorrelationID(color[i]+"-"+make[i]) ;

        qsender.send(q1, txtmsg, DeliveryMode.PERSISTENT, 1+ (i%10),
             AQjmsConstants.EXPIRATION_NEVER) ;

        byte[] msgid =((AQjmsMessage)txtmsg).getJMSMessageIDAsBytes() ;
        System.out.println("Sent "+ txtmsg.getStringProperty("color") + 
                              " " + txtmsg.getStringProperty("make")  +
                              " " + txtmsg.getIntProperty("year") ) ;
      }  
      qsess.commit();

    }catch (Exception e) {
      System.out.println("Error in SendMessages : " + e) ;
      throw e;
    }
  }

  public static void queueBrowserDemo(QueueSession qsess)  throws JMSException
  {
    javax.jms.Queue q1;
    QueueBrowser qb;
    Enumeration q1_enum ;
    String[] msgids  ;
    int cnt=0;
    TextMessage tmsg;
    QueueReceiver qr ;
    try
    {
      /* Get the queue */
      System.out.println("Get the Queue..") ;
      q1= ((AQjmsSession)qsess).getQueue("JMSUSER", "JMSQB") ;
      /* Create Queue Browser */
      System.out.println("Create Queue Browser..") ;
      qb = ((AQjmsSession)qsess).createBrowser(q1, 
                      "JMSCorrelationID LIKE 'RED%'", null, true);

      q1_enum = qb.getEnumeration() ;

      /* Browse the messages now */
      msgids  = new String[10];
      while(q1_enum.hasMoreElements() == true )
      {
         try {
           tmsg = (TextMessage) (q1_enum.nextElement()) ;
           msgids[cnt] = tmsg.getJMSMessageID() ;
           System.out.println("MessageID is "+ msgids[cnt]) ;
           cnt++ ;
           System.out.println("Browser "+ tmsg.getStringProperty("color") + 
                              " " + tmsg.getStringProperty("make")  +
                              " " + tmsg.getIntProperty("year")) ;

           System.out.print(" Correlation id "+ tmsg.getJMSCorrelationID());
           System.out.println(" Priority "+ tmsg.getJMSPriority());

         } catch (Exception e) {
           System.out.println("Browser raised exception " + e) ;
         }
      }
      
      System.out.println("Now Dequeue Messages using message Ids") ;
      for (int i =0; i< cnt; i++) {

         qr = ((AQjmsSession)qsess).createReceiver(q1, 
            "JMSMessageID = '"+msgids[i]+"'",null ) ;
         
         tmsg= (TextMessage)qr.receiveNoWait() ;
             
         System.out.println("Dequeued "+ tmsg.getStringProperty("color") + 
                              " " + tmsg.getStringProperty("make")  +
                              " " + tmsg.getIntProperty("year")) ;
         qr.close() ;
      }
      qsess.commit();
     } catch (JMSException e) {
       System.out.println("Error in QueueBrowserDemo : "+ e) ;
       throw e ;
     }
 } /* end of demo06 */
}
