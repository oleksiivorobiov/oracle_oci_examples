/* $Header: extdemo3a.java 30-mar-2006.18:38:05 yhu Exp $ */

/* Copyright (c) 1999, 2006, Oracle. All rights reserved.  */

/*
   DESCRIPTION
    extdemo3a.java - class that holds the ResultSet and PreparedStatment
                     to be stored in CartridgeServices context

   MODIFIED    (MM/DD/YY)
    yhu         03/30/06 - remove dependencies on classes12 
    rshaikh     06/24/99 -
    rshaikh     06/23/99 - create
    rshaikh     06/23/99 - Creation
 */

/**
 *  @version $Header: extdemo3a.java 30-mar-2006.18:38:05 yhu Exp $
 *  @author  rshaikh 
 *  @since   release specific (what release of product did this appear in)
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

public class extdemo3a 
{
  OracleResultSet rs;
  PreparedStatement stmt;

  public extdemo3a(OracleResultSet r, PreparedStatement s)
  {
	rs=r;
	stmt=s;
  }

  public OracleResultSet getRs(){ return rs;}
  public PreparedStatement getStmt() {return stmt;}
  public void setRs(OracleResultSet r) {rs=r;}
  public void setStmt(PreparedStatement s) {stmt=s;}

}

