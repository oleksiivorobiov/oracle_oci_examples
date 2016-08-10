/* $Header: AQDemoServlet.java 27-oct-2004.17:29:35 rbhyrava Exp $ */

/* Copyright (c) 2001, 2004, Oracle. All rights reserved.  */

/*
   DESCRIPTION
     AQ Demo Servlet for OC4J configuration

   PRIVATE CLASSES
    <list of private classes defined - with one-line descriptions>

   NOTES
    <other useful comments, qualifications, etc.>

   MODIFIED    (MM/DD/YY)
    rbhyrava    10/27/04 - enable tracing ; oc4j support 
    rbhyrava    09/13/04 - Pass host,port,sid as arguments 
    rbhyrava    03/11/02 - fix typo
    rbhyrava    02/20/02 - set host/port etc
    rbhyrava    04/12/01 - Merged rbhyrava_aqxmldemos
    rbhyrava    04/02/01 - Creation
 */

/**
 *  @version $Header: AQDemoServlet.java 27-oct-2004.17:29:35 rbhyrava Exp $
 *  @author  rbhyrava
 *  @since   release specific (what release of product did this appear in)
 */

import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;
import oracle.AQ.*;
import oracle.AQ.xml.*;
import java.sql.*;
import oracle.jms.*;
import javax.jms.*;
import java.io.*;
import oracle.jdbc.pool.*;

/**
 * This is a sample AQ Servlet for OC4J webserver configuration.
 */
public class AQDemoServlet extends oracle.AQ.xml.AQxmlServlet
{

  public  FileOutputStream ostream = null;
  //========================================================================
  /*
   *  getDBDrv - specify the database to which the servlet will connect
   */

  public AQxmlDataSource createAQDataSource() throws AQxmlException
  {

    //String sid = "aq2";
    //String port = "1521";
    //String host = "stacj31";

    AQxmlDataSource  db_drv = null;
    String sid = System.getProperty("MYDB_SID");
    String port = System.getProperty("MYDB_PORT");
    String host = System.getProperty("MYDB_HOST");

    System.out.println("AQDemoServlet - setting db driver using");
    System.out.println("sid:" + sid + " port:" + port + " host:" + host ) ;

    db_drv = new AQxmlDataSource("scott", "tiger", sid, host, port);
    System.out.print("sid: "+ db_drv.getSid() ) ;
    System.out.print(" host: "+ db_drv.getHost() ) ;
    System.out.print(" port: "+ db_drv.getPort() ) ;
    System.out.print(" cachesize: "+ db_drv.getCacheSize() ) ;
    System.out.println(" cachescheme: "+ db_drv.getCacheScheme() ) ;

    return db_drv;
  }

 //========================================================================
  public void init(ServletConfig cfg)
  {
      AQxmlDataSource  db_drv = null;

      try
      {
          super.init(cfg);

          // Remove the comment below to enable tracing 
          /*
          ostream = new FileOutputStream(fname) ;
          AQxmlDebug.setTraceLevel(5);
          AQxmlDebug.setDebug(true);

          //to write debug output to a file 
          String fname = "/tmp/wsdemoservletdebug.log" ;
          File f = new File(fname) ;
          AQxmlDebug.setLogStream(ostream);
 
          // or to write debug output to standard err
          AQxmlDebug.setLogStream(System.err);
          */

          /* 
          AQjmsOracleDebug.setLogStream(System.err);
          AQjmsOracleDebug.setTraceLevel(5);
          AQjmsOracleDebug.setDebug(true);
          */

          db_drv = this.createAQDataSource();
          setAQDataSource(db_drv);

          setManualInvalidation(false);
          setSessionMaxInactiveTime(30);
      }
      catch (AQxmlException aq_ex)
      {
        System.out.println("AQ exception: " + aq_ex);
        aq_ex.printStackTrace();

        aq_ex.getNextException().printStackTrace();
      }
      catch (Exception ex)
      {
        System.out.println("Exception: " + ex);
        ex.printStackTrace();
      }
  }
}
