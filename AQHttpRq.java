/* $Header: AQHttpRq.java 20-jul-2001.16:50:36 rbhyrava Exp $ */

/* Copyright (c) 2001, Oracle Corporation.  All rights reserved.  */

/*
   DESCRIPTION

    AQXml-HTTP/HTTPS POST an AQ XML Request using HTTP Client


   PRIVATE CLASSES
    <list of private classes defined - with one-line descriptions>

   NOTES
    <other useful comments, qualifications, etc.>

   MODIFIED    (MM/DD/YY)
    rbhyrava    07/20/01 - 
    rbhyrava    07/20/01 - Protocol
    rbhyrava    04/12/01 - Merged rbhyrava_aqxmldemos
    rbhyrava    04/04/01 - Creation
 */

/**
 *  @version $Header: AQHttpRq.java 20-jul-2001.16:50:36 rbhyrava Exp $
 *  @author  rbhyrava
 *  @since   release specific (what release of product did this appear in)
 */

//Post an AQ XML Request 
public class AQHttpRq extends AQHttp {

    static String URL = null;
    static String[] xmlfiles = new String[3];
    static String username = null;
    static String password = null;
    static String server = null;
    static String method = null;
    static String port_number = null;
    static String protocol = null;

    public static void main(String args[])
    {
      try
      {
        if(args.length < 6)
        {
           System.out.println ("Not enough parameters. " +
          "Usage: java AQHttpRq [SERVER] [PORT] [GET/POST] [PROTOCOL] [URL] [USERNAME] [PASSWORD] [XMLFILE1] [XMLFILE2] ..\n");

           return;
         }
         server = args[0] ; 
         port_number = args[1] ;
         method = args[2] ;       
         protocol = args[3];
         URL = args[4] ;       
         username = args[5] ;
         password = args[6] ;
         
         if (method.equals("POST") ) {
            xmlfiles = new String [args.length -7] ;
            for ( int i= 7 ; i < args.length ; i++) {
                xmlfiles[i -7 ] = args[i] ;
            }
            if (xmlfiles.length == 0) {
              System.out.println("Error:XMLFiles should be specified for POST");
            }
         }
         System.out.println("server: " + server ) ;
         System.out.println("port : " + port_number ) ;
         System.out.println("method " + method ) ;
         System.out.println("protocol " + protocol ) ;
         System.out.println("URL " + URL ) ;
         System.out.println("username " + username ) ;
         System.out.println("password " + password ) ;
         for ( int i= 0 ; i < xmlfiles.length ; i++) {
              System.out.println("xmlfiles " + xmlfiles[i] ) ;
         }
          new AQHttpRq().performAction() ;
       }
       catch (Exception ex)
       {
         System.out.println("MAIN : " + ex);
       }
       finally {}
       
    }

    public void performAction() {
        setProtocolType(protocol) ;
        setServerPort(server, port_number) ;
	connect(username, password);
	setCookieAcceptance(true);
        if (method.equals("POST")) {
	    System.out.println(postStringResponse(URL, xmlfiles));
         } else {
	System.out.println(getStringResponse(URL)) ;
        close() ;
        }
    }
}
