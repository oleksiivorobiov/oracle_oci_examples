/* $Header: aqjmskprb01.java 11-jul-2005.13:27:45 jleinawe Exp $ */

/* Copyright (c) 2002, 2005, Oracle. All rights reserved.  */

/*
   DESCRIPTION
    AQ/OJMS KPRB driver demo

   PRIVATE CLASSES
    <list of private classes defined - with one-line descriptions>

   NOTES
    see below

   MODIFIED    (MM/DD/YY)
    jleinawe    07/11/05 - 
    rbhyrava    10/07/02 - import oracle.jdbc.*
    jleinawe    09/04/02 - jleinawe_new_aq_demos
    jleinawe    08/30/02 - Creation
 */

/**
 *  @version $Header: aqjmskprb01.java 11-jul-2005.13:27:45 jleinawe Exp $
 *  @author  jleinawe
 *  @since   release specific (what release of product did this appear in)
 */

/****
 * This is a sample java file which uses Oracle JMS - Java Message service
 * API.  This file helps demonstrate how to send and receive a message 
 * from within an Oracle Database.
 * 
 * This demo runs within the database and does the following
 * - gets the database connection
 * - creates a QueueReceiver
 * - creates a QueueSender
 * - sends and receives a text message
 *
 * Compilation
 * ===========
 * 1) The client machine should have JDK 1.1.x or JDK 1.2 or higher installed.
 * 2) The following jar/zip files should be in the CLASSPATH
 *     For JDK1.2 or higher
 *       classes12.zip
 *       aqapi.jar
 *       jmscommon.jar
 *       nls_charset12.zip
 *
 *     For JDK1.1.x
 *       classes111.zip
 *       aqapi11.jar
 *       jmscommon.jar
 *       nls_charset11.zip
 *
 *  3) Compile by executing "javac aqjmskprb01.java"
 *
 * This demo cannot be executed standalone.  For execution, see the 
 * instructions for the kprb driver demo in the aqjmsREADME.txt file.  
 *
 */

import oracle.jms.*;
import oracle.jdbc.*;
import javax.jms.*;
import java.sql.*;

public class aqjmskprb01
{

  public static void main(String[] args)
  throws Exception
  {
    runTest(args[0]);
  }

 // send and receive one message
  public static void runTest(String msgBody)
  {
    OracleDriver ora_drv = null;
    java.sql.Connection db_conn = null;


    QueueConnection s_conn = null;
    QueueSession s_session = null;
    QueueSender sender = null;
    javax.jms.Queue s_queue = null;
    TextMessage s_msg = null;

    QueueConnection r_conn = null;
    QueueSession r_session = null;
    QueueReceiver receiver = null;
    javax.jms.Queue r_queue = null;
    TextMessage r_msg = null;

    try
    {
     //
     // get database connection
     //
      ora_drv = new OracleDriver();
      db_conn = ora_drv.defaultConnection();

     //
     // setup receiver
     //
      r_conn = AQjmsQueueConnectionFactory.createQueueConnection(db_conn);
      r_conn.start();
      r_session = r_conn.createQueueSession(true, Session.CLIENT_ACKNOWLEDGE);
      r_queue = ((AQjmsSession) r_session).getQueue("jmsuser1", "queue1");
      receiver = r_session.createReceiver(r_queue);
      System.out.println("receiver created");

     //
     // setup sender
     //
      s_conn = AQjmsQueueConnectionFactory.createQueueConnection(db_conn);
      s_conn.start();
      s_session = s_conn.createQueueSession(true, Session.CLIENT_ACKNOWLEDGE);
      s_queue = ((AQjmsSession) s_session).getQueue("jmsuser1", "queue1");
      sender = r_session.createSender(s_queue);
      System.out.println("sender created");

     //
     // create message
     //
      s_msg = s_session.createTextMessage(msgBody);
      System.out.println("message created");

     //
     // send message
     //
      sender.send(s_msg);
      s_session.commit();
      System.out.println("message sent");

     //
     // receive message
     //
      r_msg = (TextMessage) receiver.receive();
      r_session.commit();
      System.out.println("message received");

     //
     // output message text
     //
      String body = r_msg.getText();
      System.out.println("message was '"+body+"'");

     //
     // cleanup
     //
      s_session.close();
      s_conn.close();
      r_session.close();
      r_conn.close();
    }
    catch (java.sql.SQLException sql_ex)
    {
      System.out.println("Exception-2: " + sql_ex);
      sql_ex.printStackTrace();
    }
    catch (JMSException aq_ex)
    {
      System.out.println("Exception-2: " + aq_ex);
      aq_ex.printStackTrace();

      if(aq_ex.getLinkedException() != null)
      {
        aq_ex.getLinkedException().printStackTrace();
      }
    }
  }
}
