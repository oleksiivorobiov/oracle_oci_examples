/* $Header: Cars.java 01-mar-2002.14:56:41 rbhyrava Exp $ */

/* Copyright (c) 2000, 2002, Oracle Corporation.  All rights reserved.  */

/*
   DESCRIPTION
    <short description of component this file declares/defines>

   PRIVATE CLASSES
    <list of private classes defined - with one-line descriptions>

   NOTES
    <other useful comments, qualifications, etc.>

   MODIFIED    (MM/DD/YY)
    rbhyrava    03/01/02 - 
    rbhyrava    03/15/00 - AQ jms demo -Jpub generated class for CARS Type
    rbhyrava    03/15/00 - Creation
 */

/**
 *  @version $Header: Cars.java 01-mar-2002.14:56:41 rbhyrava Exp $
 *  @author  rbhyrava
 *  @since   release specific (what release of product did this appear in)
 */

/****
 * Jpub generated class for for CARS object type 
 * Used in aqjmsdemo05.java, aqjmsdemo06.java                      
 ***/
import java.sql.SQLException;
import java.sql.Connection;
import oracle.jdbc.OracleTypes;
import oracle.sql.ORAData;
import oracle.sql.ORADataFactory;
import oracle.sql.Datum;
import oracle.sql.STRUCT;
import oracle.jpub.runtime.MutableStruct;

public class Cars implements ORAData, ORADataFactory
{
  public static final String _SQL_NAME = "JMSUSER.CARS";
  public static final int _SQL_TYPECODE = OracleTypes.STRUCT;

  protected MutableStruct _struct;

  private static int[] _sqlType =  { 2,12,2,2,12 };
  private static ORADataFactory[] _factory = new ORADataFactory[5];
protected static final Cars _CarsFactory = new Cars(false);

  public static ORADataFactory getORADataFactory()
  { return _CarsFactory; }
  /* constructor */
  protected Cars(boolean init)
  { if(init) _struct = new MutableStruct(new Object[5], _sqlType, _factory); }
  public Cars()
  { this(true); }
  public Cars(java.math.BigDecimal carno, String make, java.math.BigDecimal year, java.math.BigDecimal price, String color) throws SQLException
  { this(true);
    setCarno(carno);
    setMake(make);
    setYear(year);
    setPrice(price);
    setColor(color);
  }

  /* ORAData interface */
  public Datum toDatum(Connection c) throws SQLException
  {
    return _struct.toDatum(c, _SQL_NAME);
  }


  /* ORADataFactory interface */
  public ORAData create(Datum d, int sqlType) throws SQLException
  { return create(null, d, sqlType); }
  protected ORAData create(Cars o, Datum d, int sqlType) throws SQLException
  {
    if (d == null) return null; 
    if (o == null) o = new Cars(false);
    o._struct = new MutableStruct((STRUCT) d, _sqlType, _factory);
    return o;
  }
  /* accessor methods */
  public java.math.BigDecimal getCarno() throws SQLException
  { return (java.math.BigDecimal) _struct.getAttribute(0); }

  public void setCarno(java.math.BigDecimal carno) throws SQLException
  { _struct.setAttribute(0, carno); }


  public String getMake() throws SQLException
  { return (String) _struct.getAttribute(1); }

  public void setMake(String make) throws SQLException
  { _struct.setAttribute(1, make); }


  public java.math.BigDecimal getYear() throws SQLException
  { return (java.math.BigDecimal) _struct.getAttribute(2); }

  public void setYear(java.math.BigDecimal year) throws SQLException
  { _struct.setAttribute(2, year); }


  public java.math.BigDecimal getPrice() throws SQLException
  { return (java.math.BigDecimal) _struct.getAttribute(3); }

  public void setPrice(java.math.BigDecimal price) throws SQLException
  { _struct.setAttribute(3, price); }


  public String getColor() throws SQLException
  { return (String) _struct.getAttribute(4); }

  public void setColor(String color) throws SQLException
  { _struct.setAttribute(4, color); }

}
