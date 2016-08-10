/* $Header: extdemo3.java 30-mar-2006.18:37:53 yhu Exp $ */

/* Copyright (c) 1999, 2006, Oracle. All rights reserved.  */

/*
   DESCRIPTION
	extdemo3.java - class that implements the ODCIIndex methods.
	 	This class was originally generated using JPUB based
		on the type that extensible index is based on. The 
		methods were implemented after the class was generated.

   PRIVATE CLASSES

   NOTES

   MODIFIED    (MM/DD/YY)
    yhu         03/30/06 - remove dependencies on classes12 
    hdnguyen    08/10/01 - case sensitive fixes due to 9.0.2 regen of ODCIs
    rshaikh     06/23/99 - create
    rshaikh     06/23/99 - Creation
 */

/**
 *  @version $Header: extdemo3.java 30-mar-2006.18:37:53 yhu Exp $
 *  @author  rshaikh 
 *  @since   release specific 8.1.6
 */
import java.sql.SQLException;
import java.sql.Connection;
import oracle.jdbc.OracleTypes;
import oracle.sql.ORAData;
import oracle.sql.ORADataFactory;
import oracle.sql.Datum;
import oracle.sql.STRUCT;
import oracle.jpub.runtime.MutableStruct;

import java.lang.*;
import java.sql.*;
import oracle.*;
import oracle.sql.*;
import oracle.jdbc.*;
import sqlj.runtime.ref.DefaultContext;
import sqlj.runtime.ConnectionContext;
import oracle.ODCI.*;
import oracle.CartridgeServices.*;

public class extdemo3 implements ORAData, ORADataFactory
{
  public static final String _SQL_NAME = "EXTDEMO.EXTDEMO3";
  public static final int _SQL_TYPECODE = OracleTypes.STRUCT;

  final static java.math.BigDecimal SUCCESS = new java.math.BigDecimal("0");
  final static java.math.BigDecimal ERROR = new java.math.BigDecimal("1");
  final static int TRUE = 1;
  final static int FALSE = 0;

  /* connection management */
  protected DefaultContext __tx = null;
  protected Connection __onn = null;
  public void setConnectionContext(DefaultContext ctx) throws SQLException
  { release(); __tx = ctx; }
  public DefaultContext getConnectionContext() throws SQLException
  { if (__tx==null)
    { __tx = (__onn==null) ? DefaultContext.getDefaultContext() : new DefaultContext(__onn); }
    return __tx;
  };
  public Connection getConnection() throws SQLException
  { return (__onn==null) ? ((__tx==null) ? null : __tx.getConnection()) : __onn ; }
  public void release() throws SQLException
  { if (__tx!=null && __onn!=null) __tx.close(ConnectionContext.KEEP_CONNECTION );
    __onn = null; __tx = null;
  }

  protected MutableStruct _struct;

  private static int[] _sqlType =  { 4 };
  private static ORADataFactory[] _factory = new ORADataFactory[1];
  protected static final extdemo3 _extdemo3Factory = new extdemo3(false);

  public static ORADataFactory getORADataFactory()
  { return _extdemo3Factory; }
 
  /* constructor */
  protected extdemo3(boolean init)
  { if (init) _struct = new MutableStruct(new Object[1], _sqlType, _factory); }
  public extdemo3()
  { this(true); __tx = DefaultContext.getDefaultContext(); }
  public extdemo3(DefaultContext c) throws SQLException
  { this(true); __tx = c; }
  public extdemo3(Connection c) throws SQLException
  { this(true); __onn = c; }

  /* ORAData interface */
  public Datum toDatum(Connection c) throws SQLException
  {
    if (__tx!=null && __onn!=c) release();
    __onn = c;
    return _struct.toDatum(c, _SQL_NAME);
  }

  /* ORADataFactory interface */
  public ORAData create(Datum d, int sqlType) throws SQLException
  { return create(null, d, sqlType); }
  public void setFrom(extdemo3 o) throws SQLException
  { release(); _struct = o._struct; __tx = o.__tx; __onn = o.__onn; }
  protected void setValueFrom(extdemo3 o) { _struct = o._struct; }
  protected ORAData create(extdemo3 o, Datum d, int sqlType) throws SQLException
  {
    if (d == null) { if (o!=null) { o.release(); }; return null; }
    if (o == null) o = new extdemo3(false);
    o._struct = new MutableStruct((STRUCT) d, _sqlType, _factory);
    o.__onn = ((STRUCT) d).getJavaSqlConnection();
    return o;
  }
    
  /* accessor methods */
  public Integer getScanctx() throws SQLException
  { return (Integer) _struct.getAttribute(0); }

  public void setScanctx(Integer scanctx) throws SQLException
  { _struct.setAttribute(0, scanctx); }

  //  ODCIIndexStart 
  public static java.math.BigDecimal ODCIStart(extdemo3 sctx[], 
		ODCIIndexInfo ia, ODCIPredInfo op, 
		ODCIQueryInfo qi, 
		java.math.BigDecimal strt, java.math.BigDecimal stop, 
		String cmpval) 
    throws java.sql.SQLException
  {
    String relop;
    String selstmt;
    int key;
    extdemo3a sbtctx;   // cntxt obj that holds the ResultSet and Statement
    PreparedStatement ps;
    OracleResultSet rset;    

    Connection conn = 	
        sqlj.runtime.RuntimeContext.getRuntime().getDefaultConnection();
 

    CallableStatement cstmt = conn.prepareCall
      ("{CALL dbms_output.put_line(\'Start \')}");
    cstmt.executeUpdate();
   
    //***********************************
    //* Check that the bounds are valid *
    //***********************************
    // verify that strtval/stopval are both either 0 or 1 
    if (!(((strt.intValue() == 0) && (stop.intValue() == 0)) || 
	  ((strt.intValue() == 1) && (stop.intValue() == 1))))
      {
	// throw Application_Error
	System.out.println("incorrect predicate for btree operator");
	return ERROR;
      }

    String s = new String("start key:  "+ strt.intValue() + " stop key: " + 
	stop.intValue() + " compare value: " + cmpval);
    cstmt = conn.prepareCall
      ("{CALL dbms_output.put_line(?)}");
    cstmt.setString(1, s);
    cstmt.executeUpdate();

    //*********************************************
    //* Generate the SQL statement to be executed *
    //*********************************************
    if ((op.getObjectName()).equals("EQ")){
      if (strt.intValue() == 1)
	relop = new String("=");
      else
	relop = new String("!=");
    }else if ((op.getObjectName()).equals("LT")){
      if (strt.intValue() == 1)
	relop = new String("<");
      else
	relop = new String(">=");
    }else{ 
      if (strt.intValue() == 1)
	relop = new String(">");
      else
	relop = new String("<=");
    }
    
    selstmt = new String("select ROWIDTOCHAR(f2) from "
         + ia.getIndexSchema()
	 + "." 
         + ia.getIndexName()
	 + "_sbtree where f1 "
	 + relop + " '" + cmpval + "'");
    cstmt = conn.prepareCall
      ("{CALL dbms_output.put_line(?)}");
    cstmt.setString(1, selstmt);
    cstmt.executeUpdate();


    ps = conn.prepareStatement(selstmt);
    rset = (OracleResultSet) ps.executeQuery();
  
    // set result set in ContextManager.  This stores away the 
    // ResultSet and the Statement handle so that they can
    // be used to fetch the rowids and cleanup at a later time.
    sbtctx = new extdemo3a(rset, ps);
    sctx[0] = new extdemo3();
    
    try{
	key = ContextManager.setContext((Object)sbtctx);
    }catch (CountException ce) {
	System.out.println("ContextManager CountException error");
	return ERROR;
    }

    System.out.println("ContextManager key=" + key);

    // set the key into the self argument so that we can retrieve the
    // context with this key later.
    sctx[0].setScanctx(new Integer(key));

    return SUCCESS;
  }
  
  // ODCIIndexFetch
  public java.math.BigDecimal ODCIFetch(
	java.math.BigDecimal nrows, 
	ODCIRidList rids[]) 
    throws java.sql.SQLException
  {
    extdemo3a sbtctx;  // cntxt obj that holds the ResultSet and Statement
    OracleResultSet rset;
    String rid;
    int idx = 1;
    int done = FALSE;
    String[] rlist = new String[nrows.intValue()];
    int key = getScanctx().intValue();

    Connection conn = 	
        sqlj.runtime.RuntimeContext.getRuntime().getDefaultConnection();
    CallableStatement cstmt = conn.prepareCall
      ("{CALL dbms_output.put_line(\'Fetch \')}");
    cstmt.executeUpdate();

    String s = new String("nrows : " + nrows);
    cstmt = conn.prepareCall
      ("{CALL dbms_output.put_line(?)}");
    cstmt.setString(1, s);
    cstmt.executeUpdate();
   
    System.out.println("ContextManager key=" + key);

    // Get the resultSet back from the ContextManager using the key
    try{
    	sbtctx= (extdemo3a)ContextManager.getContext(key);
    }catch(InvalidKeyException ike){
 	System.out.println("ContextManager InvalidKeyException");
	return ERROR;
    }
    rset = (OracleResultSet)(sbtctx.getRs());

    //***************
    // Fetch rowids *
    //***************
    for(int i=0; done != TRUE; i++)
      {
	if (idx > nrows.intValue()){
	  done = TRUE;
	}else {
	  if (rset.next()){
	    // append rowid to collection 
	    rid =  rset.getString(1);
	    rlist[i] = new String(rid);
	    idx++; 
	  }else{
	    // append null rowid to collection
	    rlist[i] = null;
	    done = TRUE; 
	  }
	}
      }

    // Since rids is an out parameter we need to set the ODCIRidList
    // object into the first position to be passed out.
    rids[0] = new ODCIRidList(rlist);

    return SUCCESS;
  }
  
  // ODCIIndexClose
  public java.math.BigDecimal ODCIClose() 
		throws java.sql.SQLException
  {
    extdemo3a sbtctx;   // contxt obj that holds the ResultSet and Statement
    OracleResultSet rset;
    PreparedStatement ps;
    System.out.println("in odciclose");

    int key = getScanctx().intValue();
    System.out.println("in odciclose2");

    Connection conn = 	
        sqlj.runtime.RuntimeContext.getRuntime().getDefaultConnection();
    CallableStatement  cstmt = conn.prepareCall
      ("{CALL dbms_output.put_line(\'Close\')}");
    cstmt.executeUpdate();

    System.out.println("key=" + key);

    // Get the resultSet and statement back from the ContextManager
    // so that we can close them.
    try{
        sbtctx = (extdemo3a)ContextManager.clearContext(key);
    }catch(InvalidKeyException ike){
 	System.out.println("ContextManager InvalidKeyException");
	return ERROR;
    }

    rset = (OracleResultSet)sbtctx.getRs();
    ps = (PreparedStatement)sbtctx.getStmt();
    rset.close();    
    ps.close();

    return SUCCESS;
  }
  
  // ODCIIndexInsert
  public static java.math.BigDecimal ODCIInsert(
	ODCIIndexInfo ia, String rid, String newval)
    throws java.sql.SQLException
  {
    String insstmt;
    
    Connection conn = 	
      sqlj.runtime.RuntimeContext.getRuntime().getDefaultConnection();
    CallableStatement cstmt = conn.prepareCall
      ("{CALL dbms_output.put_line(\'Insert\')}");
    cstmt.executeUpdate();

    /******************************
     * Construct insert Statement *
     ******************************/
    insstmt = new String("INSERT into "
        + ia.getIndexSchema() + "." + ia.getIndexName() 
	+"_sbtree values ('" + newval + "','" + rid + "')" );

    Statement stmt = conn.createStatement();
    stmt.executeUpdate(insstmt);
    stmt.close();
    
    return SUCCESS;
  }
  
  // ODCIIndexDelete 
  public static java.math.BigDecimal ODCIDelete(
	ODCIIndexInfo ia, String rid, String oldval)
    throws java.sql.SQLException
  {
    
    String delstmt;
    
    Connection conn = 	
      sqlj.runtime.RuntimeContext.getRuntime().getDefaultConnection();
    CallableStatement cstmt = conn.prepareCall
      ("{CALL dbms_output.put_line(\'Delete\')}");
    cstmt.executeUpdate();
    
    /******************************
     * Construct delete Statement *
     ******************************/
    delstmt = new String("DELETE from "
           + ia.getIndexSchema() + "." + ia.getIndexName() 
	   +"_sbtree where f1= '" + oldval +"'" );

    Statement stmt = conn.createStatement();
    stmt.executeUpdate(delstmt);
    stmt.close();
    
    return SUCCESS;
  }
  
  // ODCIIndexUpdate 
  public static java.math.BigDecimal ODCIUpdate(
	ODCIIndexInfo ia, String rid, String oldval, 
	String newval) 
    throws java.sql.SQLException
  {
    String updstmt;
    
    Connection conn = 	
      sqlj.runtime.RuntimeContext.getRuntime().getDefaultConnection();
    CallableStatement cstmt = conn.prepareCall
      ("{CALL dbms_output.put_line(\'Update\')}");
    cstmt.executeUpdate();
    
    /******************************
     * Construct update Statement *
     ******************************/
    updstmt = new String("UPDATE "
            + ia.getIndexSchema() + "." + ia.getIndexName() 
	    +"_sbtree SET f1= '" + newval + "' WHERE f1 = '" 
            + oldval +"'");
    
    Statement stmt = conn.createStatement();
    stmt.executeUpdate(updstmt);
    stmt.close();
    
    return SUCCESS;
  }
 
}
