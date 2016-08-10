/* $Header: xmltype3.java 09-dec-2001.18:51:27 bkhaladk Exp $ */

/* Copyright (c) 2001, Oracle Corporation.  All rights reserved.  */

/*
   DESCRIPTION
    This demo shows how to access/modify XMLType inside java/jdbc:
    - retrieve PurchaseOrder(pono#2001) xml document from database
    - store this PurchaseOrder in po_xml_hist table
    - update "DISCOUNT" element using java DOM API
    - update modified PurchaseOrder xml documnet in database
    - delete PurchaseOrder 1001

   PRIVATE CLASSES
    None

   NOTES
    Need to have classes12.zip, xmlparserv2.jar, and xdb_g.jar in CLASSPATH
    And execute xmltype3.sql first to setup schema:
    sqlplus /nolog @xmltype3.sql

   MODIFIED    (MM/DD/YY)
    bkhaladk    12/09/01 - .
    bkhaladk    12/05/01 - .
    bkhaladk    12/03/01 - .
    rbooredd    04/29/01 - Merged rbooredd_xml_demo1
    rbooredd    04/27/01 - Creation
 */

import java.sql.*;
import java.io.*;

import oracle.xml.parser.v2.*;
import org.xml.sax.*;
import org.w3c.dom.*;

import oracle.jdbc.driver.*;
import oracle.sql.*;

import oracle.xdb.XMLType;

public class xmltype3
{

  //static String conStr = "jdbc:oracle:thin:@energize:1521:m3";
  static String conStr = "jdbc:oracle:oci8:@";
  static String user = "xmltype3";
  static String pass = "xmltype3";
  /* static String qryStr = 
        "select x.xpo from po_xml x "+
        "where x.xpo.extract('/PurchaseOrder/@PONO').getNumberVal()=2001";*/
  static String qryStr = 
  "select x.xpo from po_xml x ";


 static String updateXML(String xmlTypeStr)
  {
     System.out.println("\n===============================");
     System.out.println("xmlType.getStringVal():");
     System.out.println(xmlTypeStr);
     System.out.println("===============================");
     String outXML = null;
     try{
        DOMParser parser  = new DOMParser();
        parser.setValidationMode(parser.PARTIAL_VALIDATION);
        parser.setPreserveWhitespace (true);    


        parser.parse(new StringReader(xmlTypeStr));
        System.out.println("xmlType.getStringVal(): xml String is well-formed");

        XMLDocument doc = parser.getDocument();

        NodeList nl = doc.getElementsByTagName("DISCOUNT");

        for(int i=0;i<nl.getLength();i++){
           XMLElement discount = (XMLElement)nl.item(i);
           XMLNode textNode = (XMLNode)discount.getFirstChild();
           textNode.setNodeValue("10");
        }

       StringWriter sw = new StringWriter();
       doc.print(new PrintWriter(sw));

       outXML = sw.toString();
       //print modified xml
       System.out.println("\n===============================");
       System.out.println("Updated PurchaseOrder:");
       System.out.println(outXML);
       System.out.println("===============================");
      }
    catch ( Exception e )
    {
      e.printStackTrace(System.out);
    }
    return outXML;
  }

 
 public static void main(String args[]) throws Exception
  {
    try{

	System.out.println("qryStr="+ qryStr);

	DriverManager.registerDriver(new oracle.jdbc.driver.OracleDriver());


	Connection conn = 
	    DriverManager.getConnection(conStr, user, pass);
        Statement s = conn.createStatement();
        System.out.println("after create statement");
        OraclePreparedStatement stmt;

        ResultSet rset = s.executeQuery(qryStr);
        System.out.println("after exec query");
        OracleResultSet orset = (OracleResultSet) rset;
        OPAQUE xml;
        while(orset.next()){

         //retrieve PurchaseOrder xml documnet from database
        System.out.println("after exec query1");
         XMLType xt = XMLType.createXML(orset.getOPAQUE(1));
        System.out.println("after exec query2");

        /*System.out.println(xt.getStringVal());*/

         //store this PurchaseOrder in po_xml_hist table
         stmt = (OraclePreparedStatement)conn.prepareStatement(
                   "insert into po_xml_hist values(?)");
        System.out.println("after exec query31");
         stmt.setObject(1,xt);
        System.out.println("after exec query32");
         stmt.execute();
        System.out.println("after exec query3");


         //update "DISCOUNT" element
         /*String newXML = updateXML(xt.getStringVal());
           xt = XMLType.createXML(conn,newXML);*/

         // update PurchaseOrder xml documnet in database
         stmt = (OraclePreparedStatement)conn.prepareStatement(
           "update po_xml x set x.xpo=? "+
           "where x.xpo.extract('/PurchaseOrder/@PONO').getNumberVal()=2001");
         stmt.setObject(1,xt);
         stmt.execute();

         conn.commit();
         System.out.println("PurchaseOrder 2001 Updated!");
        }
        //delete PurchaseOrder 1001
        /*s.execute("delete from po_xml x "+
          "where x.xpo.extract('/PurchaseOrder/@PONO').getNumberVal()=1001");*/
        System.out.println("PurchaseOrder 1001 deleted!");
    }
    catch( Exception e )
    {
      e.printStackTrace(System.out);
    }
  }
} 
