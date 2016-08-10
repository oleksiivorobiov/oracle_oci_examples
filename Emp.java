/* $Header: Emp.java 15-mar-00.09:08:33 rbhyrava Exp $ */

/* Copyright (c) Oracle Corporation 2000. All Rights Reserved. */

/*
   DESCRIPTION
    <short description of component this file declares/defines>

   PRIVATE CLASSES
    <list of private classes defined - with one-line descriptions>

   NOTES
    <other useful comments, qualifications, etc.>

   MODIFIED    (MM/DD/YY)
    rbhyrava    03/15/00 - AQ jms demo -Jpub generated class for Emp Type
    rbhyrava    03/15/00 - Creation
 */

/**
 *  @version $Header: Emp.java 15-mar-00.09:08:33 rbhyrava Exp $
 *  @author  rbhyrava
 *  @since   release specific (what release of product did this appear in)
 */

/****
 * Jpub generated class for for EMP object type 
 * Used in aqjmsdemo05.java                     
 ***/
import java.sql.SQLException;
import java.sql.Connection;
import oracle.jdbc.OracleTypes;
import oracle.sql.ORAData;
import oracle.sql.ORADataFactory;
import oracle.sql.Datum;
import oracle.sql.STRUCT;
import oracle.jpub.runtime.MutableStruct;

public class Emp implements ORAData, ORADataFactory
{
  public static final String _SQL_NAME = "JMSUSER.EMP";
  public static final int _SQL_TYPECODE = OracleTypes.STRUCT;

  protected MutableStruct _struct;

  private static int[] _sqlType =  { 2,12,2002,2,12 };
  private static ORADataFactory[] _factory = new ORADataFactory[5];
  static
  {
    _factory[2] = Cars.getORADataFactory();
  }
protected static final Emp _EmpFactory = new Emp(false);

  public static ORADataFactory getORADataFactory()
  { return _EmpFactory; }
  /* constructor */
  protected Emp(boolean init)
  { if(init) _struct = new MutableStruct(new Object[5], _sqlType, _factory); }
  public Emp()
  { this(true); }
  public Emp(java.math.BigDecimal id, String name, Cars carown, java.math.BigDecimal rank, String zip) throws SQLException
  { this(true);
    setId(id);
    setName(name);
    setCarown(carown);
    setRank(rank);
    setZip(zip);
  }

  /* ORAData interface */
  public Datum toDatum(Connection c) throws SQLException
  {
    return _struct.toDatum(c, _SQL_NAME);
  }


  /* ORADataFactory interface */
  public ORAData create(Datum d, int sqlType) throws SQLException
  { return create(null, d, sqlType); }
  protected ORAData create(Emp o, Datum d, int sqlType) throws SQLException
  {
    if (d == null) return null; 
    if (o == null) o = new Emp(false);
    o._struct = new MutableStruct((STRUCT) d, _sqlType, _factory);
    return o;
  }
  /* accessor methods */
  public java.math.BigDecimal getId() throws SQLException
  { return (java.math.BigDecimal) _struct.getAttribute(0); }

  public void setId(java.math.BigDecimal id) throws SQLException
  { _struct.setAttribute(0, id); }


  public String getName() throws SQLException
  { return (String) _struct.getAttribute(1); }

  public void setName(String name) throws SQLException
  { _struct.setAttribute(1, name); }


  public Cars getCarown() throws SQLException
  { return (Cars) _struct.getAttribute(2); }

  public void setCarown(Cars carown) throws SQLException
  { _struct.setAttribute(2, carown); }


  public java.math.BigDecimal getRank() throws SQLException
  { return (java.math.BigDecimal) _struct.getAttribute(3); }

  public void setRank(java.math.BigDecimal rank) throws SQLException
  { _struct.setAttribute(3, rank); }


  public String getZip() throws SQLException
  { return (String) _struct.getAttribute(4); }

  public void setZip(String zip) throws SQLException
  { _struct.setAttribute(4, zip); }

}
