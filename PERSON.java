/* $Header: PERSON.java 15-mar-00.09:06:53 rbhyrava Exp $ */

/* Copyright (c) Oracle Corporation 2000. All Rights Reserved. */

/*
   DESCRIPTION
    <short description of component this file declares/defines>

   PRIVATE CLASSES
    <list of private classes defined - with one-line descriptions>

   NOTES
    <other useful comments, qualifications, etc.>

   MODIFIED    (MM/DD/YY)
    rbhyrava    03/15/00 - AQ API demo -Jpub generated class for PERSON Type
    rbhyrava    03/15/00 - Creation
 */

/**
 *  @version $Header: PERSON.java 15-mar-00.09:06:53 rbhyrava Exp $
 *  @author  rbhyrava
 *  @since   release specific (what release of product did this appear in)
 */

/****
 * Jpub generated class for for ADDRESS object type 
 * Used in aqorademo02.java                     
 ***/

import java.sql.SQLException;
import java.sql.Connection;
import oracle.jdbc.OracleTypes;
import oracle.sql.ORAData;
import oracle.sql.ORADataFactory;
import oracle.sql.Datum;
import oracle.sql.STRUCT;
import oracle.jpub.runtime.MutableStruct;

public class PERSON implements ORAData, ORADataFactory
{
  public static final String _SQL_NAME = "AQJAVA.PERSON";
  public static final int _SQL_TYPECODE = OracleTypes.STRUCT;

  protected MutableStruct _struct;

  private static int[] _sqlType =  { 12,2002 };
  private static ORADataFactory[] _factory = new ORADataFactory[2];
  static
  {
    _factory[1] = ADDRESS.getORADataFactory();
  }
protected static final PERSON _PERSONFactory = new PERSON(false);

  public static ORADataFactory getORADataFactory()
  { return _PERSONFactory; }
  /* constructor */
  protected PERSON(boolean init)
  { if(init) _struct = new MutableStruct(new Object[2], _sqlType, _factory); }
  public PERSON()
  { this(true); }
  public PERSON(String name, ADDRESS home) throws SQLException
  { this(true);
    setName(name);
    setHome(home);
  }

  /* ORAData interface */
  public Datum toDatum(Connection c) throws SQLException
  {
    return _struct.toDatum(c, _SQL_NAME);
  }


  /* ORADataFactory interface */
  public ORAData create(Datum d, int sqlType) throws SQLException
  { return create(null, d, sqlType); }
  protected ORAData create(PERSON o, Datum d, int sqlType) throws SQLException
  {
    if (d == null) return null; 
    if (o == null) o = new PERSON(false);
    o._struct = new MutableStruct((STRUCT) d, _sqlType, _factory);
    return o;
  }
  /* accessor methods */
  public String getName() throws SQLException
  { return (String) _struct.getAttribute(0); }

  public void setName(String name) throws SQLException
  { _struct.setAttribute(0, name); }


  public ADDRESS getHome() throws SQLException
  { return (ADDRESS) _struct.getAttribute(1); }

  public void setHome(ADDRESS home) throws SQLException
  { _struct.setAttribute(1, home); }

}
