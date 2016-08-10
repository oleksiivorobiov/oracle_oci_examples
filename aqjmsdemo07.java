/* $Header: aqjmsdemo07.java 05-jun-2007.15:10:25 aatam Exp $ */

/* Copyright (c) 2002, 2007, Oracle. All rights reserved.  */

/*
   DESCRIPTION
    AQ/OJMS XML payload demo

   PRIVATE CLASSES
    <list of private classes defined - with one-line descriptions>

   NOTES
    see below

   MODIFIED    (MM/DD/YY)
    aatam       06/05/07 - password need to be consistent
    jleinawe    07/11/05 - 
    jleinawe    04/18/05 - update lcr format
    jleinawe    12/20/02 - update instructions
    rbhyrava    10/07/02 - import oracle.jdbc.*
    jleinawe    09/04/02 - jleinawe_new_aq_demos
    jleinawe    08/30/02 - Creation
 */

/**
 *  @version $Header: aqjmsdemo07.java 05-jun-2007.15:10:25 aatam Exp $
 *  @author  jleinawe
 *  @since   release specific (what release of product did this appear in)
 */

/****
 * This is a sample java file which uses Oracle JMS - Java Message Service
 * API to Enqueue/Dequeue an XML message using a sys.Anydata queue.
 *
 * This demo does the following:
 * - creates an Anydata Queue
 * - creates a QueueSender
 * - sends a message containing an XML message body
 * - create a QueueReceiver
 * - receives and prints the XML message
 * - drops the Anydata Queue
 * 
 * Instructions for setting up and running this demo are found in
 * aqjmsREADME.txt.
 *
 ***/

import javax.jms.*;
import java.sql.*;
import oracle.AQ.*;
import oracle.jms.*;
import oracle.xdb.*;

public class aqjmsdemo07 
{

  public static void main (String args [])
       throws java.sql.SQLException, ClassNotFoundException, JMSException
  {
    try
    {
      String ora_sid;
      String host;
      int    port;
      try
      {
	ora_sid = args[0];
	host    = args[1];
	port    = Integer.parseInt(args[2]);

	System.out.println ("ora_sid: " + ora_sid);
	System.out.println ("host: " + host);
	System.out.println ("port: " + port);

        init(ora_sid, host, port);
	EnqueueAnydata(ora_sid, host, port);
	DequeueAnydata(ora_sid, host, port);
        tearDown(ora_sid, host, port);
        System.out.println("End of Demo");
      }
      catch (ArrayIndexOutOfBoundsException ex) 
      {
	System.out.println ("Not enough parameters. " +
		   "Usage: java filename [SID] [HOST] [PORT]\n" +
		    "\tExample: java aqjmsdemo07 orcl myserver 1521");
	return;
      }
    }
    catch (Exception ex)
    {
      System.out.println("Exception:  " + ex);
      ex.printStackTrace();
    } 
  }


  public static void EnqueueAnydata(String ora_sid, String host, int port)
  {
    QueueConnectionFactory    qc_fact   = null;
    QueueConnection           q_conn    = null;
    QueueSession              q_sess    = null;
    java.sql.Connection       db_conn   = null;
    AQQueueTableProperty      qt_prop   = null;
    AQQueueTable              q_table   = null;
    AQjmsDestinationProperty  dest_prop = null;
    javax.jms.Queue           queue     = null;
    AdtMessage                adt_msg   = null;
    QueueSender               q_sender  = null;
    Message                   jms_msg   = null;
    oracle.xdb.XMLType        xtype     = null; 
    String                    data      = null;


    try
    {
      qc_fact = AQjmsFactory.getQueueConnectionFactory(host, 
						       ora_sid, port, "oci8");

      q_conn = qc_fact.createQueueConnection("jmsuser", "JMSUSER");
      q_sess = q_conn.createQueueSession(true, Session.CLIENT_ACKNOWLEDGE);
   
      q_conn.start(); 
      db_conn = ((AQjmsSession)q_sess).getDBConnection();

      queue = ((AQjmsSession)q_sess).getQueue("jmsuser", "xmlq");

     //
     // create sender
     //
      q_sender = q_sess.createSender(queue);

     //
     // create ade message
     //
      adt_msg = ((AQjmsSession)q_sess).createAdtMessage();

      System.out.println("start xml convert");

     data = "<ROW_LCR " +
      "xmlns=\"http://xmlns.oracle.com/streams/schemas/lcr\" \n" +
      "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"  \n" +
      "xsi:schemaLocation=\"http://xmlns.oracle.com/streams/schemas/lcr http://xmlns.oracle.com/streams/schemas/lcr/streamslcr.xsd\"" + 
      "> \n" +
      "<source_database_name>source_dbname</source_database_name> \n" +
      "<command_type>INSERT</command_type> \n" +
      "<object_owner>RAM</object_owner> \n" +
      "<object_name>EMP</object_name> \n" +
      "<tag>0ABC</tag> \n" +
      "<transaction_id>0.0.0</transaction_id> \n" +
      "<scn>0</scn> \n" +
      "<old_values> \n" +
      "<old_value> \n" +
      "<column_name>C01</column_name> \n" +
      "<data><varchar2>Clob old</varchar2></data> \n" +
      "</old_value> \n" +
      "<old_value> \n" +
      "<column_name>C02</column_name> \n" +
      "<data><varchar2>A123FF</varchar2></data> \n" +
      "</old_value> \n" +
      "<old_value> \n" +
      "<column_name>C03</column_name> \n" +
      "<data> \n" +
      "<date><value>1997/11/24 00:00:00</value><format>SYYYY/MM/DD HH24:MI:SS</format></date> \n" +
      "</data> \n" +
      "</old_value> \n" +
      "<old_value> \n" +
      "<column_name>C04</column_name> \n" +
      "<data> \n" +
      "<timestamp><value>1999/05/31 13:20:00.000000000</value><format>SYYYY/MM/DD HH24:MI:SSXFF9</format></timestamp> \n" +
      "</data> \n" +
      "</old_value> \n" +
      "<old_value> \n" +
      "<column_name>C05</column_name> \n" +
      "<data><raw>0ABCDE</raw></data> \n" +
      "</old_value> \n" +
      "</old_values> \n" +
      "<new_values> \n" +
      "<new_value> \n" +
      "<column_name>C01</column_name> \n" +
      "<data><varchar2>A123FF</varchar2></data> \n" +
      "</new_value> \n" +
      "<new_value> \n" +
      "<column_name>C02</column_name> \n" +
      "<data><number>35.23</number></data> \n" +
      "</new_value> \n" +
      "<new_value> \n" +
      "<column_name>C03</column_name> \n" +
      "<data><number>-100000</number></data> \n" +
      "</new_value> \n" +
      "<new_value> \n" +
      "<column_name>C04</column_name> \n" +
      "<data><varchar2>Hel lo</varchar2></data> \n" +
      "</new_value> \n" +
      "<new_value> \n" +
      "<column_name>C05</column_name> \n" +
      "<data><char>wor ld</char></data> \n" +
      "</new_value> \n" +
      "</new_values> \n" +
      "</ROW_LCR>";
    

      xtype = oracle.xdb.XMLType.createXML(db_conn, data);

      adt_msg.setAdtPayload(xtype);

      System.out.println("Enqueue anydata(xmltype) message");

     //
     // send the message
     //
      ((AQjmsQueueSender)q_sender).send(queue, adt_msg, 
				     DeliveryMode.PERSISTENT, 1, 
				     AQjmsConstants.EXPIRATION_NEVER);

      q_sess.commit();
      q_sess.close();
      q_conn.close();

    }
    catch (java.sql.SQLException sql_ex)
    {
      System.out.println("Exception: " + sql_ex);
      sql_ex.printStackTrace();
    }
    catch (JMSException aq_ex)
    {
      System.out.println("Exception: " + aq_ex);
      aq_ex.printStackTrace();

      if(aq_ex.getLinkedException() != null)
      {
	aq_ex.getLinkedException().printStackTrace();
      }
    }
    
  }


  public static void DequeueAnydata(String ora_sid, String host, int port) 
  {
    QueueConnectionFactory    qc_fact   = null;
    QueueConnection           q_conn    = null;
    QueueSession              q_sess    = null;
    java.sql.Connection       db_conn   = null;
    AQQueueTableProperty      qt_prop   = null;
    AQQueueTable              q_table   = null;
    AQjmsDestinationProperty  dest_prop = null;
    javax.jms.Queue           queue     = null;
    AdtMessage                adt_msg   = null;
    AdtMessage                adt_msg2  = null;
    QueueReceiver             q_receiver= null;
    TextMessage               t_msg     = null;
    javax.jms.Message         jms_msg   = null;
    oracle.xdb.XMLType        xtype2    = null; 

    try
    {
      qc_fact = AQjmsFactory.getQueueConnectionFactory(host, 
						       ora_sid, port, "oci8");

      q_conn = qc_fact.createQueueConnection("jmsuser", "JMSUSER");
      q_sess = q_conn.createQueueSession(true, Session.CLIENT_ACKNOWLEDGE);
    
      q_conn.start(); 
      db_conn = ((AQjmsSession)q_sess).getDBConnection();

     //
     // get queue xmlq
     //
      queue = ((AQjmsSession)q_sess).getQueue("jmsuser", "xmlq");

     //
     // Create receiver
     //
      q_receiver = ((AQjmsSession)q_sess).createReceiver(queue);

     //
     // Receive message
     //
      jms_msg = (javax.jms.Message) q_receiver.receive();

     //
     // output message contents
     //
      if(jms_msg == null)
          System.out.println("Null message received");
      else
      {
        if(jms_msg instanceof AdtMessage)
        {
          adt_msg2 = (AdtMessage)jms_msg;
          System.out.println("Retrieved message: " + adt_msg2.getAdtPayload());

          if(adt_msg2.getAdtPayload() instanceof oracle.xdb.XMLType)
          {
            xtype2 = (XMLType)adt_msg2.getAdtPayload();
            System.out.println("Data: \n" + xtype2.getStringVal());
          }

          System.out.println("Msg id: " + adt_msg2.getJMSMessageID());
          q_sess.commit();
        }
        else
          System.out.println("Invalid message type");
      }
      q_conn.close();
    }
    catch (java.sql.SQLException sql_ex)
    {
      System.out.println("Exception: " + sql_ex);
      sql_ex.printStackTrace();
    }
    catch (JMSException aq_ex)
    {
      System.out.println("Exception: " + aq_ex);
      aq_ex.printStackTrace();

      if(aq_ex.getLinkedException() != null)
      {
	aq_ex.getLinkedException().printStackTrace();
      }
    }
    catch (Exception ex)
    {
      System.out.println("Exception: " + ex);
      ex.printStackTrace();
    }
  }


  public static void init(String ora_sid, String host, int port)
  {
    java.sql.Connection conn = null;
    String url = null;
    String jmsCall = null;
    CallableStatement stmt = null;
    try
    {
     //
     // setup connection (either thin or oci8)
     //
      url = new String ("jdbc:oracle:oci8:");
      url = url.concat("@(DESCRIPTION=(ADDRESS=(PROTOCOL=tcp)(PORT=" +port);
      url = url.concat(")(HOST=" +host+ "))(CONNECT_DATA=(SID=" +
               ora_sid+ ")))");
      DriverManager.registerDriver(new oracle.jdbc.OracleDriver());
      conn = DriverManager.getConnection(url, "jmsuser", "JMSUSER");

     //
     // create queue table xmlq_t and enable it for jms types
     //
      jmsCall ="{call dbms_aqadm.create_queue_table( queue_table => 'xmlq_t',"+
              " multiple_consumers => false, "+
              "queue_payload_type => 'sys.ANYDATA',compatible => '8.1.3')}";
      stmt = conn.prepareCall(jmsCall);
      stmt.execute();

     // enable jms types
      jmsCall = "{call dbms_aqadm.enable_jms_types('xmlq_t')}";
      stmt = conn.prepareCall(jmsCall);
      stmt.execute();

     //
     // create queue xmlq
     //
      jmsCall = "{call dbms_aqadm.create_queue(queue_name => 'xmlq',"+
                " queue_table => 'xmlq_t')}";
      stmt = conn.prepareCall(jmsCall);
      stmt.execute();

     //
     // start queue xmlq
     //
      jmsCall = "{call dbms_aqadm.start_queue(queue_name => 'xmlq')}";
      stmt = conn.prepareCall(jmsCall);
      stmt.execute();

      conn.close();
    }
    catch (SQLException s)
    {
      System.out.println("Exception: "+s);
      s.printStackTrace();
    }
  }


  public static void tearDown(String ora_sid, String host, int port)
  {
    java.sql.Connection conn = null;
    String url = null;
    String jmsCall = null;
    CallableStatement stmt = null;
    try
    {
     //
     // Drop queue table 'xmlq_t'
     //
      url = new String ("jdbc:oracle:oci8:");
      url = url.concat("@(DESCRIPTION=(ADDRESS=(PROTOCOL=tcp)(PORT=" +port);
      url = url.concat(")(HOST=" +host+ "))(CONNECT_DATA=(SID=" +
               ora_sid+ ")))");
      DriverManager.registerDriver(new oracle.jdbc.OracleDriver());
      conn = DriverManager.getConnection(url, "jmsuser", "JMSUSER");

      jmsCall = "{call dbms_aqadm.drop_queue_table( queue_table => 'xmlq_t',"+
                " force => TRUE)}";
      stmt = conn.prepareCall(jmsCall);
      stmt.execute();

      conn.close();
    }
    catch (SQLException s)
    {
      s.printStackTrace();
    }
  }
}
