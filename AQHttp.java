/* $Header: AQHttp.java 02-nov-2004.13:47:08 rbhyrava Exp $ */

/* Copyright (c) 2001, 2004, Oracle. All rights reserved.  */

/*
   DESCRIPTION
     AQxml HTTP/HTTPS POST an AQ XML Request using HTTP Client

   PRIVATE CLASSES
    <list of private classes defined - with one-line descriptions>

   NOTES
    <other useful comments, qualifications, etc.>

   MODIFIED    (MM/DD/YY)
    rbhyrava    11/02/04 - demossl
    lzhao       08/28/01 - merge rbhyrava's changes-setContentType
    rbhyrava    07/23/01 - use s_oracleHome as per bug1895434
    rbhyrava    07/20/01 - 
    rbhyrava    07/20/01 - https
    rbhyrava    04/12/01 - Merged rbhyrava_aqxmldemos
    rbhyrava    04/09/01 - Creation
 */

/**
 *  @version $Header: AQHttp.java 02-nov-2004.13:47:08 rbhyrava Exp $
 *  @author  rbhyrava
 *  @since   release specific (what release of product did this appear in)
 */

/* Helper class to Post AQ XML request */
import HTTPClient.*;
import java.util.*;
import java.io.*;
import java.lang.reflect.*;

public abstract class AQHttp {
   private String server_name; // server name 
   private int port_number; // port number 
   private String protocol_type ;
   private String fs = File.separator ;
   private String demossl_crt_file = System.getProperty("CERT");
   private HTTPConnection con = null; // HTTP connection 
   private String userPass = null;

   
   String oc4jcert =
"MIIB0jCCATsCBEAIDP0wDQYJKoZIhvcNAQEEBQAwMDELMAkGA1UEBhMCVVMxDzANBgNVBAoTBk9y"+
"YWNsZTEQMA4GA1UEAxMHc29tZW9uZTAeFw0wNDAxMTYxNjEwMzdaFw0xNDAxMTMxNjEwMzdaMDAx"+
"CzAJBgNVBAYTAlVTMQ8wDQYDVQQKEwZPcmFjbGUxEDAOBgNVBAMTB3NvbWVvbmUwgZ8wDQYJKoZI"+
"hvcNAQEBBQADgY0AMIGJAoGBAMfTBEKasCvuWVLQZocEACMt7S03j64NBCAGBG8XAKq5gbAOAyat"+
"fOnoV/fSeNVgBv7XuNmtysOCBCGCrNnrdGdup8CjTkfrHxFrq5cWjfLZammrFjlebIY6BPkTX4I4"+
"zBkEyCjyFpTh1y/SzRzdsiGSPIIML2OuCpLsZCet1i67AgMBAAEwDQYJKoZIhvcNAQEEBQADgYEA"+
"MUQy8siUiFhXJ7q7W3HThQ0pi3jO+ryJz/TjfH+RCjPPwh2Ipq5oMDoXsoBoUzc7oiYj8GM+angE"+
"0XSkLLayqidmcXYCki92D4Wqemx90bhxVvCz6wUyG4yPm9VhKBwqiwKrw3np8RtDjD5HFEvk2Icb"+
"3OoWB2Z/MxN0Upl/Ki4=";


     
   public AQHttp() {} 
   public void setProtocolType(String prot) {
      try {
         protocol_type = prot.toLowerCase() ;
         
      } catch (Exception e) {
         e.printStackTrace();
         System.out.println("setProtocolType:" + e);
      }
   }
   public void setServerPort(String sname, String pnumber ) {
      try {
         server_name = sname ;
         port_number = Integer.parseInt(pnumber);
         
      } catch (Exception e) {
         e.printStackTrace();
         System.out.println("setServerPort:" + e);
      }
   }

  // make connection 
   public void connect(String username, String password) {
      if (con != null) con = null;
      userPass = username + ":" + password ;
      con = newConnection(username, password);
   }
   // close connection 
   public void close() {
      try {
         if (con != null) 
            con.stop() ;
      } catch (Exception e ) { 
         System.out.println("Close Connection : "+ e) ;
      }
   }

    //  This method returns a new instance of HTTPConnection
   public  HTTPConnection newConnection(String username, String password) {
      HTTPConnection returnValue = null;
      
      try {
         returnValue = 
             new HTTPConnection(protocol_type, server_name, port_number);
         returnValue.addBasicAuthorization("aqxmldemo_web", username, password);
          if ( protocol_type.compareTo("https") == 0) {
              //cert=readFile(demossl_crt_file) ;
              //System.out.println(cert) ;
              returnValue.addTrustPoint(oc4jcert);
          }

          NVPair[] hdrs = returnValue.getDefaultHeaders() ;
          int idx =0;
          for (idx=0; idx< hdrs.length ; idx++) {
            System.out.println(hdrs[idx].getName());
            if (hdrs[idx].getName().equalsIgnoreCase("content-type")) {
             hdrs[idx] = new NVPair("Content-Type", "text/xml") ;
            }
          }
          if (hdrs.length == 0) {
             hdrs = new NVPair[1];
             hdrs[0] = new NVPair("Content-Type", "text/xml") ;
          }
          returnValue.setDefaultHeaders(hdrs) ;

      } catch (Exception e) {
         e.printStackTrace();
         System.out.println("Fail to connect to " + server_name + ":" + port_number);
      }
      return returnValue;
   }

   // get response from webserver 
   public int getStatusCode(String urlPath) {
      int code = 0;
      try {
         HTTPResponse resp = con.Get(urlPath);
         code = resp.getStatusCode();
      } catch (Exception e) {
         e.printStackTrace();
         System.out.println("Fail to get the status code");
      }
      return code;
   }

   public String getStringResponse(String urlPath) {
      String str = null;
      try {
         HTTPResponse resp = con.Get(urlPath);
         str = new String(resp.getData());
      } catch (Exception e) {
         e.printStackTrace();
         System.out.println("Fail to get a String response ");
      }
      return str;
   }

   public String postStringResponse(String urlPath, String[] xmlfiles) {
      String str = "";
      String data = null;
      String cookie = null;
      String ret_cookie = null;
      try {
       for ( int i =0 ; i < xmlfiles.length ; i++) {

        if (cookie != null )  {
        AuthorizationInfo au1 = AuthorizationInfo.getAuthorization(
               server_name, port_number, "Basic", "aqxmldemo_web") ;
         au1.setCookie(cookie) ;
        }
        if (xmlfiles[i] != null )
        {
        data = readFile(xmlfiles[i]) ;
        HTTPResponse resp = con.Post(urlPath, data.getBytes(), null);
        if (i == 0) {
          str = str +"URL: " +protocol_type +"://" + server_name + ":"+
               port_number+ urlPath  + "\n";
        } else {
          str = str + "\n" ;
        }
        str = str + "Setting User::  "  + userPass + "\n";
        str = str + "Setting Encoded::  "  + resp.getHeader("Content-Encoding") + "\n";
        if ( i != 0 ) {
          str = str + "Setting cookie "  + cookie  + "\n";
        }
   
        str = str + "POSTing file: " + xmlfiles[i] + "\n";
        AuthorizationInfo au = AuthorizationInfo.getAuthorization(server_name, port_number, "Basic", "aqxmldemo_web") ;
        ret_cookie = au.getCookie() ;
        if (ret_cookie != null) {
           cookie = ret_cookie ;
        }   
        str = str + "Cookie " + cookie  + "\n";
        str = str + "HTTP response code is " + resp.getStatusCode() + "\n";
        String con_len = resp.getHeader("Content-Length") ;
        str = str+"HTTP Content Length: "+ ((con_len != null) ? con_len : "-1"  ) +"\n";
        str = str+"HTTP Content Type: " + resp.getHeader("Content-Type") + "\n";
        str = str + "HTTP Content: " +  "\n";
        String respdata = new String(resp.getData()) ;
        str = str + respdata  ;
        
        }
      }
      } catch (Exception e) {
        e.printStackTrace();
        System.out.println("Fail to POST a String response ");
      }
      return str;
   }

   /**
    * This class defines a CookiePolicyHandler that
    * accepts and sends all cookie without dicretion.
    *@see setCookieAcceptance
    */
   protected class MaxPolicyHandler implements HTTPClient.CookiePolicyHandler {
      public boolean acceptCookie(Cookie cookie, RoRequest req, RoResponse resp)
      { return true; }

      public boolean sendCookie(Cookie cookie, RoRequest req)
      { return true; }
   }

   /**
    * This class defines a CookiePolicyHandler that
    * rejects all cookie without dicretion.
    *@see setCookieAcceptance
    */
   protected class MinPolicyHandler implements HTTPClient.CookiePolicyHandler {
      public boolean acceptCookie(Cookie cookie, RoRequest req, RoResponse resp)
      { return false; }

      public boolean sendCookie(Cookie cookie, RoRequest req)
      { return false; }
   }
  
   protected void setCookieAcceptance(boolean accept) {
      setCookieAcceptance(con, accept);
   }

   protected void setCookieAcceptance(HTTPConnection con, boolean accept) {
      try {
         Class []c = con.getModules();
         int i;
         for (i=0; i < c.length; i++) {
	    //System.out.println("module "+ i +" : " + c[i]);
	    if (c[i].equals(CookieModule.class)) break;
         }
         Method m = c[i].getMethod("setCookiePolicyHandler", new Class[] {CookiePolicyHandler.class});
         if (accept)
	    m.invoke(null, new Object[] {new MaxPolicyHandler()});
         else 
	    m.invoke(null, new Object[] {new MinPolicyHandler()});
      } catch (Exception e) {
         e.printStackTrace();
         System.out.println("Exception thrown in setCookieAcceptance");
      }
   }

   public static String readFile(String fname) throws Exception
  {
    FileReader fReader;
    BufferedReader    bReader;
    String            line= null;
    StringBuffer      data;

    fReader = new FileReader(fname);
    bReader = new BufferedReader(fReader);
    data = new StringBuffer();
    while ((line = bReader.readLine()) != null)
    {
      data.append(line + "\n");
    }
    return data.toString();
  }
}
