/* $Header: aqorademo01.java 12-aug-2003.12:51:11 rbhyrava Exp $ */

/* Copyright (c) 2000, 2003, Oracle Corporation.  All rights reserved.  */

/*
   DESCRIPTION
    <short description of component this file declares/defines>

   PRIVATE CLASSES
    <list of private classes defined - with one-line descriptions>

   NOTES
    <other useful comments, qualifications, etc.>

   MODIFIED    (MM/DD/YY)
    rbhyrava    08/12/03 - stop/drop queue/queuetables 
    rbhyrava    10/07/02 - import oracle.jdbc.*
    rbhyrava    03/20/00 - use driver arg
    rbhyrava    03/16/00 - AQ API demos
    rbhyrava    03/15/00 - AQ API demo - Enqueue,Dequeue RAW Message
    rbhyrava    03/15/00 - Creation
 */

/**
 *  @version $Header: aqorademo01.java 12-aug-2003.12:51:11 rbhyrava Exp $
 *  @author  rbhyrava
 *  @since   release specific (what release of product did this appear in)
 */
/***
 * This is a sample java file which uses Oracle AQ API to enqueue and 
 * dequeue  RAW messages

 * -- Create a Queue 
 * -- Enqueue RAW Message
 *    Dequeue RAW message 
 *
 * The following instructions describe how to compile and execute 
 * this sample on the client machine. 
 *
 * System requirements:
 * ====================
 *  1) Oracle 8.1.6 database or higher 
 *  2) The client machine should have JDK 1.1.x or JDK1.2 or higher installed
 *  3) The following jar/zip files should be in the CLASSPATH on the client 
 *     machine. 
 *     For JDK1.2.x 
 *        classes12.zip 
 *        aqapi.jar
 *     For JDK1.1.x 
 *        classes111.zip 
 *        aqapi11.jar
 *  Set up CLASSPATH, PATH, LD_LIBRARY_PATH based on JDK version and platform.
 * Compilation and Running:
 *  ========================
 *  4) If you already have the jars in step 3) in classpath 
 *      javac aqorademo01.java 
 *
 *  5) java aqorademo01  <SID> <HOST> <PORT> <DRIVER>
 *     Example usage: 
 *       java aqorademo01 orcl82 dlsun666 1521 thin
 *
 *  Thin driver is used in the demo. Modify the connect string to use oci8 
 *  jdbc driver
 ***/

/* Set up main class from which we will call subsequent examples and handle 
   exceptions: */
import java.sql.*;
import oracle.AQ.*;

public class aqorademo01
{
   public static void main(String args[]) 
   {
      AQSession  aq_sess = null;
      try 
      {
        if (args.length < 4 )
         System.out.println("Usage:java filename [SID] [HOST] [PORT] [DRIVER]");
        else {
          aq_sess = createSession(args);
          /* now Enqueue and Dequeue Messages  */
          createQTable(aq_sess) ;
          enqRawMsg(aq_sess);     
          deqRawMsg(aq_sess);     
          dropQTable(aq_sess);
          System.out.println("End of Demo") ;
        }
      }
      catch (Exception ex)
      {
         System.out.println("Exception-1: " + ex); 
         ex.printStackTrace();      
      }  
   }

/* Create an Java AQ Session for the 'aqjava' user as shown in the 
   AQDriverManager section above: */

   public static AQSession createSession(String args[]) throws Exception
   {
      Connection db_conn;
      AQSession  aq_sess = null;

      try 
      {
         Class.forName("oracle.jdbc.OracleDriver");

         String drv,url;
         drv=args[3];

         if ( drv.toLowerCase().compareTo("thin") == 0)
            url="jdbc:oracle:"+args[3]+":@"+args[1]+":"+args[2]+":"+args[0];
         else {
            url= new String ("jdbc:oracle:oci8:");
            url= url.concat("@(DESCRIPTION=(ADDRESS=(PROTOCOL=tcp)" +
                       "(PORT=" + args[2] +
                       ")(HOST=" + args[1] + "))(CONNECT_DATA=(SID=" +
                       args[0] + ")))");

         }
         //"jdbc:oracle:thin:@host:port:sid"
          System.out.println("Connect String is :"+url) ;

         db_conn = DriverManager.getConnection(url, "aqjava", "aqjava");

         System.out.println("JDBC Connection opened "); 
         db_conn.setAutoCommit(false);
                 
         /* Load the Oracle8i AQ driver: */
         Class.forName("oracle.AQ.AQOracleDriver");

         /* Create an AQ Session: */
         aq_sess = AQDriverManager.createAQSession(db_conn);
         System.out.println("Successfully created AQSession ");  
      }
      catch (Exception ex)
      {
         System.out.println("Exception: " + ex); 
         throw ex;
      }  
      return aq_sess;
   }


   public static void createQTable(AQSession aq_sess) throws Exception
   {
       AQQueueTableProperty     qtable_prop;
       AQQueueProperty          queue_prop;
       AQQueueTable             q_table;
       AQQueue                  queue;
      
       try {
         /* Create a AQQueueTableProperty object (payload type - RAW): */
         qtable_prop = new AQQueueTableProperty("RAW"); 
        
         /* Drop the queue if already exists */
         try {
           q_table = aq_sess.getQueueTable ("aqjava", "aq_table4" );
           q_table.drop(true);
         } catch (Exception e) {} ;
     
         /* Create a queue table called aq_table4 in aqjava schema: */
         q_table=aq_sess.createQueueTable ("aqjava", "aq_table4", qtable_prop);
         System.out.println("Successfully created aq_table4 in aqjava schema");  
     
         /* Create a new AQQueueProperty object */
         queue_prop = new AQQueueProperty();
        
         /* Create a queue called aq_queue4 in aq_table4: */
         queue = aq_sess.createQueue (q_table, "aq_queue4", queue_prop);
         System.out.println("Successfully created aq_queue4 in aq_table4");  
     
         /* Enable enqueue/dequeue on this queue: */
         queue.start();
         System.out.println("Successful start queue");
       } catch (Exception e) { 
         System.out.println("Error in createQTable:"+ e);
         throw e;
       }
   }



   public static void enqRawMsg(AQSession aq_sess) throws Exception
   {
     AQQueueTable             q_table;
     AQQueue                  queue;
     AQMessage                message;
     AQRawPayload             raw_payload;
     AQEnqueueOption          enq_option;
     String                   test_data = "new message";
     byte[]                   b_array;
     Connection               db_conn;

     try {
        db_conn = ((AQOracleSession)aq_sess).getDBConnection();
   
        /* Get a handle to queue table - aq_table4 in aqjava schema: */
        q_table = aq_sess.getQueueTable ("aqjava", "aq_table4");
        System.out.println("Successful getQueueTable");  
   
        /* Get a handle to a queue - aq_queue4 in aquser schema: */
        queue = aq_sess.getQueue ("aqjava", "aq_queue4");
        System.out.println("Successful getQueue");  
   
        /* Create a message to contain raw payload: */
        message = queue.createMessage();
   
        /* Get handle to the AQRawPayload object and populate it with raw data: */
        b_array = test_data.getBytes();
   
        raw_payload = message.getRawPayload();
   
        raw_payload.setStream(b_array, b_array.length);
   
        /* Create a AQEnqueueOption object with default options: */
        enq_option = new AQEnqueueOption();
        /* Enqueue the message: */
        queue.enqueue(enq_option, message);
   
        db_conn.commit();
     } catch (Exception e) {
       System.out.println("Exception during Enqueue: " + e) ;
       throw e;
     }
   }



   public static void deqRawMsg(AQSession aq_sess) throws Exception
   {
     AQQueueTable             q_table;
     AQQueue                  queue;
     AQMessage                message;
     AQRawPayload             raw_payload;
     AQEnqueueOption          enq_option;
     String                   test_data = "new message";
     AQDequeueOption          deq_option;
     byte[]                   b_array;
     Connection               db_conn;
     try {
        db_conn = ((AQOracleSession)aq_sess).getDBConnection();
   
        /* Get a handle to queue table - aq_table4 in aqjava schema: */
        q_table = aq_sess.getQueueTable ("aqjava", "aq_table4");
        System.out.println("Successful getQueueTable");  
   
        /* Get a handle to a queue - aq_queue4 in aquser schema: */
        queue = aq_sess.getQueue ("aqjava", "aq_queue4");
        System.out.println("Successful getQueue");  
   
        /* Create a message to contain raw payload: */
        message = queue.createMessage();
   
        /* Get handle to the AQRawPayload object and populate it with raw data: */
        b_array = test_data.getBytes();
   
        raw_payload = message.getRawPayload();
   
        raw_payload.setStream(b_array, b_array.length);
   
        /* Create a AQEnqueueOption object with default options: */
        enq_option = new AQEnqueueOption();
   
        /* Enqueue the message: */
        queue.enqueue(enq_option, message);
        System.out.println("Successful enqueue");  
   
        db_conn.commit();
   
        /* Create a AQDequeueOption object with default options: */
        deq_option = new AQDequeueOption();
   
        /* Dequeue a message: */
        message = queue.dequeue(deq_option);
        System.out.println("Successful dequeue"); 
       
        /* Retrieve raw data from the message: */
        raw_payload = message.getRawPayload();
    
        b_array = raw_payload.getBytes();
   
        db_conn.commit();
     } catch (Exception e) {
       System.out.println("Exception during Dequeue: " + e) ;
       throw e;
     }
  }
  public static void dropQTable(AQSession aq_sess) throws Exception
  {
       AQQueueTable             q_table;
       AQQueue                  queue ;
       try {
           q_table = aq_sess.getQueueTable ("aqjava", "aq_table4" );
           queue = aq_sess.getQueue("aqjava", "aq_queue4");
           queue.stop(true, true, true) ;
           q_table.dropQueue("aq_queue4");
           q_table.drop(true);
       } catch (Exception e) {
           System.out.println("Error in dropQTable:"+ e);
       } ;
     
       System.out.println("Successfully dropped aq_table4 in aqjava schema");  
  }
}
