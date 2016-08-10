/* $Header: MesgListener.java 16-mar-00.13:11:30 rbhyrava Exp $ */

/* Copyright (c) Oracle Corporation 2000. All Rights Reserved. */

/*
   DESCRIPTION
    <short description of component this file declares/defines>

   PRIVATE CLASSES
    <list of private classes defined - with one-line descriptions>

   NOTES
    <other useful comments, qualifications, etc.>

   MODIFIED    (MM/DD/YY)
    rbhyrava    03/15/00 - AQ jms demo -Message Listener
    rbhyrava    03/15/00 - Creation
 */

/**
 *  @version $Header: MesgListener.java 16-mar-00.13:11:30 rbhyrava Exp $
 *  @author  rbhyrava
 *  @since   release specific (what release of product did this appear in)
 */
/* 
 * Message Listener to listen messages asynchronously.
 * Setup Message Listener for a Queue Receiver to receive the messages
 * Refer to aqjmsdemo03.java 
 */
import oracle.AQ.*;
import oracle.jms.*;
import javax.jms.*;
import java.lang.*;
import java.math.*;
import java.util.*;

public class MesgListener implements MessageListener
{
   int mesgno=0;

   Session  mysess;
   String              myname;

   MesgListener(Session sess) {
      mysess = sess;
   }
   public void onMessage(javax.jms.Message m)
   {
      MapMessage mm = (MapMessage) m ;
      try {
         mesgno++;
         System.out.println("Retrieved message " + 
               mesgno + " with correlation: " + mm.getJMSCorrelationID());
         System.out.println (" "+ mm.getStringProperty ("color") +
                           " " + mm.getStringProperty("make") +
                           " " + mm.getIntProperty("year") +
                           " " + mm.getDoubleProperty("price")) ;
         try {
           mysess.commit() ;
         } catch (Exception e) {
           System.out.println("Exception on Message Listener Commit " + e) ;
         }

      } catch (JMSException e) {
        System.out.println("Exception onMessage:" + e) ;
      }
   }
}
