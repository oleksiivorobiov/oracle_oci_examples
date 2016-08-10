/* $Header: AQPropServlet.java 27-oct-2004.17:30:12 rbhyrava Exp $ */

/* Copyright (c) 2001, 2004, Oracle. All rights reserved.  */

/*
   DESCRIPTION
    AQ propagation using http/https.

   PRIVATE CLASSES
    <list of private classes defined - with one-line descriptions>

   NOTES
    <other useful comments, qualifications, etc.>

   MODIFIED    (MM/DD/YY)
    rbhyrava    10/27/04 - oc4j support;enable tracing 
    rbhyrava    10/26/04 - DBPort
    rbhyrava    09/13/04 - Pass host,port,sid as arguments 
    rbhyrava    02/20/02 - set host,port,sid
    rbhyrava    05/09/01 - Init for Jserv
    rbhyrava    04/12/01 - Merged rbhyrava_aqxmldemos
    rbhyrava    04/10/01 - comments
    rbhyrava    03/30/01 - Creation
 */

/**
 *  @version $Header: AQPropServlet.java 27-oct-2004.17:30:12 rbhyrava Exp $
 *  @author  rbhyrava
 *  @since   release specific (what release of product did this appear in)
 */

/*
  AQ Propagation Servlet
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
 * This is a  AQ Propagation Servlet.
 */
public class AQPropServlet extends oracle.AQ.xml.AQxmlServlet
{

  /*====================================================================*
   *  getDBDrv - specify the database to which the servlet will connect *
   *====================================================================*/
  public AQxmlDataSource createAQDataSource() throws AQxmlException
  {
    AQxmlDataSource  db_drv = null;

    //String sid = "%s_oracleSid%";
    //String port = "1521";
    //String host = "%s_oracleHost%";

    String sid = System.getProperty("MYDB_SID");
    String port = System.getProperty("MYDB_PORT");
    String host = System.getProperty("MYDB_HOST");

    System.err.println("AQPropServlet - setting db driver using");
    System.err.println("sid:" + sid + " port:" + port + " host:" + host ) ;

    System.err.println("AQPropServlet - setting db driver");
    db_drv = new AQxmlDataSource("scott", "tiger", sid, host, port);
    System.err.print("sid: "+ db_drv.getSid() ) ;
    System.err.print(" host: "+ db_drv.getHost() ) ;
    System.err.print(" port: "+ db_drv.getPort() ) ;
    System.err.print(" cachesize: "+ db_drv.getCacheSize() ) ;
    System.err.println(" cachescheme: "+ db_drv.getCacheScheme() ) ;
    return db_drv;
  }

 public void init(ServletConfig cfg)
  {
      try
      {
          super.init(cfg) ;
          
          AQxmlDebug.setTraceLevel(5);
          AQxmlDebug.setDebug(true);
          AQxmlDebug.setLogStream(System.err);
          
          AQxmlDataSource dt_src = this.createAQDataSource();
          setAQDataSource(dt_src);
          setSessionMaxInactiveTime(180) ;
      }
      catch (Exception ex)
      {
          System.err.println("Exception in init: " + ex);
      }
  }
}

