/* $Header: ADDRESS.java 15-mar-00.09:06:06 rbhyrava Exp $ */

/* Copyright (c) Oracle Corporation 2000. All Rights Reserved. */

/*
   DESCRIPTION
    <short description of component this file declares/defines>

   PRIVATE CLASSES
    <list of private classes defined - with one-line descriptions>

   NOTES
    <other useful comments, qualifications, etc.>

   MODIFIED    (MM/DD/YY)
    rbhyrava    03/15/00 - AQ API demo -Jpub generated class for ADDRESS Type
    rbhyrava    03/15/00 - Creation
 */

/**
 *  @version $Header: ADDRESS.java 15-mar-00.09:06:06 rbhyrava Exp $
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

public class ADDRESS implements ORAData, ORADataFactory
{
  public static final String _SQL_NAME = "AQJAVA.ADDRESS";
  public static final int _SQL_TYPECODE = OracleTypes.STRUCT;

  protected MutableStruct _struct;

  private static int[] _sqlType =  { 12,12 };
  private static ORADataFactory[] _factory = new ORADataFactory[2];
protected static final ADDRESS _ADDRESSFactory = new ADDRESS(false);

  public static ORADataFactory getORADataFactory()
  { return _ADDRESSFactory; }
  /* constructor */
  protected ADDRESS(boolean init)
  { if(init) _struct = new MutableStruct(new Object[2], _sqlType, _factory); }
  public ADDRESS()
  { this(true); }
  public ADDRESS(String street, String city) throws SQLException
  { this(true);
    setStreet(street);
    setCity(city);
  }

  /* ORAData interface */
  public Datum toDatum(Connection c) throws SQLException
  {
    return _struct.toDatum(c, _SQL_NAME);
  }


  /* ORADataFactory interface */
  public ORAData create(Datum d, int sqlType) throws SQLException
  { return create(null, d, sqlType); }
  protected ORAData create(ADDRESS o, Datum d, int sqlType) throws SQLException
  {
    if (d == null) return null; 
    if (o == null) o = new ADDRESS(false);
    o._struct = new MutableStruct((STRUCT) d, _sqlType, _factory);
    return o;
  }
  /* accessor methods */
  public String getStreet() throws SQLException
  { return (String) _struct.getAttribute(0); }

  public void setStreet(String street) throws SQLException
  { _struct.setAttribute(0, street); }


  public String getCity() throws SQLException
  { return (String) _struct.getAttribute(1); }

  public void setCity(String city) throws SQLException
  { _struct.setAttribute(1, city); }

}
