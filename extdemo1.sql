rem
rem $Header: extdemo1.sql 16-oct-2002.11:53:50 ayoaz Exp $
rem
rem extdemo1.sql
rem
rem Copyright (c) 1998, 2002, Oracle Corporation.  All rights reserved.  
rem
rem    NAME
rem      extdemo1.sql - A power company example
rem
rem    DESCRIPTION
rem      This is an example of a data cartridge which uses
rem      object types and the extensible indexing framework.
rem
rem    NOTES
rem
rem    MODIFIED   (MM/DD/YY)
rem    ayoaz       10/16/02 - add cardinality arg to ODCIArgDesc
rem    ddas        03/12/01 - use 9i interfaces
rem    rmurthy     03/09/01 - bug 1676437 - change call to extdemo0
rem    hdnguyen    02/15/01 - added order by to selects  
rem    ddas        02/10/01 - modify table data and cost functions
rem    ddas        05/25/00 - 8.2 interface change
rem    rmurthy     01/12/00 - add path name while calling utlxplan
rem    mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
rem    hdnguyen    07/26/99 - sqlplus conversion
rem    ddas        06/25/99 - extend example to use extensible optimizer
rem    rmurthy     09/25/98 - add explain plans
rem    rmurthy     07/15/98 - extensibility demo
rem    rmurthy     07/15/98 - Created

SET ECHO ON
CONNECT sys/knl_test7 AS sysdba;

-- Clean up from any previous running of this procedure.
DROP USER PowerCartUser CASCADE;

-- Create the user for schema objects.
CREATE USER PowerCartUser IDENTIFIED BY PowerCartUser;

-------------------------------------------------------------------
-- INITIAL SET-UP
-------------------------------------------------------------------
-- Grant privileges --
GRANT connect, resource to PowerCartUser;
GRANT create table to PowerCartUser;

-------------------------------------------------------------------
-- CREATE POWERTYPE --
-------------------------------------------------------------------
CONNECT PowerCartUser/PowerCartUser
set echo off
@'?/rdbms/admin/utlxplan.sql'
set echo on

-- Create table for user-defined statistics
CREATE TABLE PowerCartUserStats (
  -- Table for which statistics are collected
  tab VARCHAR2(30),
  -- Column for which statistics are collected
  col VARCHAR2(30),
  -- Cell position
  cpos NUMBER,
  -- Minimum power demand for the given cell
  lo NUMBER,
  -- Maximum power demand for the given cell
  hi NUMBER,
  -- Number of (non-null) power demands for the given cell
  nrows NUMBER
);

-- Type used in the definition of PowerDemand_Typ

CREATE OR REPLACE TYPE PowerGrid_Typ as VARRAY(100) of NUMBER;
/
CREATE OR REPLACE TYPE NumTab_Typ as TABLE of NUMBER;
/
-- The object type used for this example. Contains 3
-- attributes that will be set by the 3 member procedures;
-- also a power demand grid (array) and the date/time for
-- the samplings (readings).

CREATE OR REPLACE TYPE PowerDemand_Typ AS OBJECT (
  -- Total power demand for the grid
  TotGridDemand NUMBER,
  -- Call with maximum/minimum power demand for the grid
  MaxCellDemand NUMBER,
  MinCellDemand NUMBER,
  -- Power grid: 10X10 array represented as Varray(100)
  -- using previously defined PowerGrid_Typ
  CellDemandValues PowerGrid_Typ,
  -- Date/time for power-demand samplings: Every hour,
  -- 100 power stations transmit their power demand
  -- readings.
  SampleTime DATE,
  --
  -- Methods (Set...) for this type:
  -- Total demand for the entire power grid for a
  -- SampleTime: sets the value of TotGridDemand.
  Member Procedure SetTotalDemand,
  -- Maximum demand for the entire power grid for a
  -- SampleTime: sets the value of MaxCellDemand.
  Member Procedure SetMaxDemand,
  -- Minimum demand for the entire power grid for a
  -- SampleTime: sets the value of MinCellDemand.
  Member Procedure SetMinDemand
);
/
show errors;

CREATE OR REPLACE TYPE BODY PowerDemand_Typ
IS
  --
  -- Methods (Set...) for this type:
  -- Total demand for the entire power grid for a
  -- SampleTime: sets the value of TotGridDemand.
  Member Procedure SetTotalDemand
  IS
  I BINARY_INTEGER;
  Total NUMBER;
  BEGIN
    Total :=0;
    I := CellDemandValues.FIRST;
    WHILE I IS NOT NULL LOOP
        Total := Total + CellDemandValues(I);
        I := CellDemandValues.NEXT(I);
    END LOOP;
    TotGridDemand := Total;
  END;

  -- Maximum demand for the entire power grid for a
  -- SampleTime: sets the value of MaxCellDemand.
  Member Procedure SetMaxDemand
  IS
  I BINARY_INTEGER;
  Temp NUMBER;
  BEGIN
    I := CellDemandValues.FIRST;
    Temp := CellDemandValues(I);
    WHILE I IS NOT NULL LOOP
        IF Temp < CellDemandValues(I) THEN
           Temp := CellDemandValues(I);
        END IF;
        I := CellDemandValues.NEXT(I);
    END LOOP;
    MaxCellDemand := Temp;
  END;

  -- Minimum demand for the entire power grid for a
  -- SampleTime: sets the value of MinCellDemand.
  Member Procedure SetMinDemand
  IS
  I BINARY_INTEGER;
  Temp NUMBER;
  BEGIN
    I := CellDemandValues.FIRST;
    Temp := CellDemandValues(I);
    WHILE I IS NOT NULL LOOP
        IF Temp > CellDemandValues(I) THEN
           Temp := CellDemandValues(I);
        END IF;
        I := CellDemandValues.NEXT(I);
    END LOOP;
    MinCellDemand := Temp;
  END;
END;
/
show errors;

-------------------------------------------------------------------
-- CREATE FUNCTIONS AND OPERATORS
-------------------------------------------------------------------

CREATE FUNCTION Power_EqualsSpecific_Func(
  object PowerDemand_Typ, cell NUMBER, value NUMBER)
RETURN NUMBER AS
  BEGIN
  IF cell <= object.CellDemandValues.LAST
  THEN
     IF (object.CellDemandValues(cell) = value) THEN
        RETURN 1;
     ELSE
        RETURN 0;
     END IF;
  ELSE
     RETURN NULL;
  END IF;
  END;
/

CREATE FUNCTION Power_GreaterThanSpecific_Func(
  object PowerDemand_Typ, cell NUMBER, value NUMBER)
RETURN NUMBER AS
  BEGIN
  IF cell <= object.CellDemandValues.LAST
  THEN
     IF (object.CellDemandValues(cell) > value) THEN
        RETURN 1;
     ELSE
        RETURN 0;
     END IF;
  ELSE
     RETURN NULL;
  END IF;
  END;
/

CREATE FUNCTION Power_LessThanSpecific_Func(
  object PowerDemand_Typ, cell NUMBER, value NUMBER)
RETURN NUMBER AS
  BEGIN
  IF cell <= object.CellDemandValues.LAST
  THEN
     IF (object.CellDemandValues(cell) < value) THEN
        RETURN 1;
     ELSE
        RETURN 0;
     END IF;
  ELSE
     RETURN NULL;
  END IF;
  END;
/

CREATE FUNCTION Power_EqualsAny_Func(
  object PowerDemand_Typ, value NUMBER)
RETURN NUMBER AS
   idx NUMBER;
  BEGIN
    FOR idx IN object.CellDemandValues.FIRST..object.CellDemandValues.LAST LOOP
      IF (object.CellDemandValues(idx) = value) THEN
        RETURN 1;
      END IF;
    END LOOP;

   RETURN 0;
  END;
/

CREATE FUNCTION Power_GreaterThanAny_Func(
  object PowerDemand_Typ, value NUMBER)
RETURN NUMBER AS
   idx NUMBER;
  BEGIN
    FOR idx IN object.CellDemandValues.FIRST..object.CellDemandValues.LAST LOOP
      IF (object.CellDemandValues(idx) > value) THEN
        RETURN 1;
      END IF;
    END LOOP;

   RETURN 0;
  END;
/

CREATE FUNCTION Power_LessThanAny_Func(
  object PowerDemand_Typ, value NUMBER)
RETURN NUMBER AS
   idx NUMBER;
  BEGIN
    FOR idx IN object.CellDemandValues.FIRST..object.CellDemandValues.LAST LOOP
      IF (object.CellDemandValues(idx) < value) THEN
        RETURN 1;
      END IF;
    END LOOP;

   RETURN 0;
  END;
/


CREATE OPERATOR Power_Equals BINDING(PowerDemand_Typ, NUMBER, NUMBER)
  RETURN NUMBER USING Power_EqualsSpecific_Func;
CREATE OPERATOR Power_GreaterThan BINDING(PowerDemand_Typ, NUMBER, NUMBER)
  RETURN NUMBER USING Power_GreaterThanSpecific_Func;
CREATE OPERATOR Power_LessThan BINDING(PowerDemand_Typ, NUMBER, NUMBER)
  RETURN NUMBER USING Power_LessThanSpecific_Func;

CREATE OPERATOR Power_EqualsAny BINDING(PowerDemand_Typ, NUMBER)
  RETURN NUMBER USING Power_EqualsAny_Func;
CREATE OPERATOR Power_GreaterThanAny BINDING(PowerDemand_Typ, NUMBER)
  RETURN NUMBER USING Power_GreaterThanAny_Func;
CREATE OPERATOR Power_LessThanAny BINDING(PowerDemand_Typ, NUMBER)
  RETURN NUMBER USING Power_LessThanAny_Func;

-------------------------------------------------------------------
-- Create package used by ODCIIndexGetMetadata
-------------------------------------------------------------------
CREATE OR REPLACE PACKAGE power_pkg AS
  FUNCTION getversion(idxschema IN VARCHAR2, idxname IN VARCHAR2,
        newblock OUT PLS_INTEGER) RETURN VARCHAR2;
  PROCEDURE checkversion (version IN VARCHAR2);
END power_pkg;
/
SHOW ERRORS;

CREATE OR REPLACE PACKAGE BODY power_pkg AS

-- iterate is a package level variable used to maintain state across calls
-- by export in this session.

iterate NUMBER := 0;

FUNCTION getversion(idxschema IN VARCHAR2, idxname IN VARCHAR2,
        newblock OUT PLS_INTEGER) RETURN VARCHAR2 IS

BEGIN

-- We are generating only one PL/SQL block consisting of one line of code.
  newblock := 1;

  IF iterate = 0
  THEN
-- Increment iterate so we'll know we're done next time we're called.
    iterate := iterate + 1;

-- Return a string that calls checkversion with a version 'V1.0'
-- Note that export adds the surrounding BEGIN/END pair to form the anon.
-- block... we don't have to.

    RETURN 'power_pkg.checkversion(''V1.0'');';
  ELSE
-- reset iterate for next index
    iterate := 0;
-- Return a 0-length string; we won't be called again for this index.
    RETURN '';
  END IF;
END getversion;

PROCEDURE checkversion (version IN VARCHAR2) IS

  wrong_version EXCEPTION;

BEGIN
  IF version != 'V1.0' THEN
     RAISE wrong_version;
  END IF;
END checkversion;

END power_pkg;
/
SHOW ERRORS;

-------------------------------------------------------------------
-- CREATE INDEXTYPE IMPLEMENTATION TYPE
-------------------------------------------------------------------
CREATE OR REPLACE TYPE power_idxtype_im AS OBJECT
(
  curnum NUMBER,
  STATIC FUNCTION ODCIGetInterfaces(ifclist OUT sys.ODCIObjectList)
     RETURN NUMBER,
  STATIC FUNCTION ODCIIndexCreate (ia sys.odciindexinfo, parms VARCHAR2,
                                   env sys.ODCIEnv)
     RETURN NUMBER,
  STATIC FUNCTION ODCIIndexDrop(ia sys.odciindexinfo,
                                env sys.ODCIEnv) RETURN NUMBER,
  STATIC FUNCTION ODCIIndexStart(sctx IN OUT power_idxtype_im,
     ia sys.odciindexinfo,
     op sys.odciPredInfo, qi sys.ODCIQueryInfo,
     strt NUMBER, stop NUMBER,
     cmppos NUMBER, cmpval NUMBER, env sys.ODCIEnv) RETURN NUMBER,
  STATIC FUNCTION ODCIIndexStart(sctx IN OUT power_idxtype_im,
     ia sys.odciindexinfo,
     op sys.odciPredInfo, qi sys.ODCIQueryInfo,
     strt NUMBER, stop NUMBER,
     cmpval NUMBER, env sys.ODCIEnv) RETURN NUMBER,
  MEMBER FUNCTION ODCIIndexFetch(nrows NUMBER,
     rids OUT sys.odciridlist, env sys.ODCIEnv)
     RETURN NUMBER,
  MEMBER FUNCTION ODCIIndexClose(env sys.ODCIEnv) RETURN NUMBER,
  STATIC FUNCTION ODCIIndexInsert(ia sys.odciindexinfo, rid VARCHAR2,
     newval PowerDemand_Typ, env sys.ODCIEnv) RETURN NUMBER,
  STATIC FUNCTION ODCIIndexDelete(ia sys.odciindexinfo, rid VARCHAR2,
     oldval PowerDemand_Typ, env sys.ODCIEnv) RETURN NUMBER,
  STATIC FUNCTION ODCIIndexUpdate(ia sys.odciindexinfo, rid VARCHAR2,
     oldval PowerDemand_Typ, newval PowerDemand_Typ, env sys.ODCIEnv)
     RETURN NUMBER,
  STATIC FUNCTION ODCIIndexGetMetadata(ia sys.odciindexinfo,
        expversion VARCHAR2, newblock OUT PLS_INTEGER, env sys.ODCIEnv)
     RETURN VARCHAR2
);
/
show errors;

create or replace type body power_idxtype_im
is
  STATIC FUNCTION ODCIGetInterfaces(ifclist OUT sys.ODCIObjectList)
       return number is
   BEGIN
       ifclist := sys.ODCIObjectList(sys.ODCIObject('SYS','ODCIINDEX2'));
       return ODCIConst.Success;
   END ODCIGetInterfaces;

  STATIC FUNCTION ODCIIndexCreate (ia sys.odciindexinfo,
                                   parms VARCHAR2, env sys.ODCIEnv)
    RETURN NUMBER
  is
   i INTEGER;
   r ROWID;
   p NUMBER;
   v NUMBER;
   stmt1 VARCHAR2(1000);
   stmt2 VARCHAR2(1000);
   stmt3 VARCHAR2(1000);
   cnum1 INTEGER;
   cnum2 INTEGER;
   cnum3 INTEGER;
   junk NUMBER;
  BEGIN
   -- Construct the SQL statement.
   stmt1 := 'CREATE TABLE ' ||
            ia.IndexSchema || '.' || ia.IndexName || '_pidx' ||
            '( r ROWID, cpos NUMBER, cval NUMBER)';

   -- Dump the SQL statement.
   dbms_output.put_line('ODCIIndexCreate>>>>>');
   sys.ODCIIndexInfoDump(ia);
   dbms_output.put_line('ODCIIndexCreate>>>>>'||stmt1);

   -- Execute the statement.
   cnum1 := dbms_sql.open_cursor;
   dbms_sql.parse(cnum1, stmt1, dbms_sql.native);
   junk := dbms_sql.execute(cnum1);
   dbms_sql.close_cursor(cnum1);

   -- Now populate the table.
   stmt2 := ' INSERT INTO '||
            ia.IndexSchema || '.' || ia.IndexName || '_pidx' ||
            ' SELECT :rr, ROWNUM, column_value FROM THE' ||
            ' (SELECT CAST (P.'|| ia.IndexCols(1).ColName ||
                                  '.CellDemandValues AS NumTab_Typ)' ||
            ' FROM ' || ia.IndexCols(1).TableSchema || '.' ||
                        ia.IndexCols(1).TableName || ' P' ||
            ' WHERE P.ROWID = :rr)';

   -- Execute the statement.
   dbms_output.put_line('ODCIIndexCreate>>>>>'||stmt2);

   -- Parse the statement.
   cnum2 := dbms_sql.open_cursor;
   dbms_sql.parse(cnum2, stmt2, dbms_sql.native);

   stmt3 := 'SELECT ROWID FROM '||
            ia.IndexCols(1).TableSchema || '.' || ia.IndexCols(1).TableName;
   dbms_output.put_line('ODCIIndexCreate>>>>>'||stmt3);
   cnum3 := dbms_sql.open_cursor;
   dbms_sql.parse(cnum3, stmt3, dbms_sql.native);
   dbms_sql.define_column_rowid(cnum3, 1, r);
   junk := dbms_sql.execute(cnum3);

   WHILE dbms_sql.fetch_rows(cnum3) > 0 LOOP
      -- Get column values of the row. --
      dbms_sql.column_value_rowid(cnum3, 1, r);
      -- Bind the row into the cursor for the next insert. --
      dbms_sql.bind_variable_rowid(cnum2, ':rr', r);
      junk := dbms_sql.execute(cnum2);
   END LOOP;

   dbms_sql.close_cursor(cnum2);
   dbms_sql.close_cursor(cnum3);
   RETURN ODCICONST.SUCCESS;
  END;

  STATIC FUNCTION ODCIIndexDrop(ia sys.odciindexinfo, env sys.ODCIEnv)
    RETURN NUMBER is
   stmt VARCHAR2(1000);
   cnum INTEGER;
   junk INTEGER;
  BEGIN
    -- Construct the SQL statement.
   stmt := 'drop table ' || ia.IndexSchema || '.' || ia.IndexName || '_pidx';

   dbms_output.put_line('ODCIIndexDrop>>>>>');
   sys.ODCIIndexInfoDump(ia);
   dbms_output.put_line('ODCIIndexDrop>>>>>'||stmt);

   -- Execute the statement.
   cnum := dbms_sql.open_cursor;
   dbms_sql.parse(cnum, stmt, dbms_sql.native);
   junk := dbms_sql.execute(cnum);
   dbms_sql.close_cursor(cnum);

   RETURN ODCICONST.SUCCESS;
  END;

  -- This definition of ODCIIndexStart is for queries on a specific
  -- cell. (The next definition is for queries on any cell.)
  STATIC FUNCTION ODCIIndexStart(sctx IN OUT power_idxtype_im,
        ia sys.odciindexinfo,
        op sys.odciPredInfo, qi sys.ODCIQueryInfo,
        strt NUMBER, stop NUMBER,
        cmppos NUMBER, cmpval NUMBER, env sys.ODCIEnv) RETURN NUMBER is
    cnum INTEGER;
    rid ROWID;
    nrows INTEGER;
    relop VARCHAR2(2);
    stmt VARCHAR2(1000);
  BEGIN
    dbms_output.put_line('ODCIIndexStart>>>>>');
    sys.ODCIIndexInfoDump(ia);
    sys.ODCIPredInfoDump(op);
    dbms_output.put_line('start key : '||strt);
    dbms_output.put_line('stop key : '||stop);
    dbms_output.put_line('compare position : '||cmppos);
    dbms_output.put_line('compare value : '||cmpval);

    -- Take care of some error cases.
    -- The only predicates in which operators can appear are
    --    op() = 1     OR    op() = 0
    if (strt != 1) and (strt != 0) then
    raise_application_error(-20101, 'Incorrect predicate for operator');
    END if;

    if (stop != 1) and (stop != 0) then
    raise_application_error(-20101, 'Incorrect predicate for operator');
    END if;

    -- Generate the SQL statement to be executed.
    -- First, figure out the relational operator needed for the statement.
    -- Take into account the operator name and the start and stop keys.
    -- For now, the start and stop keys can both be 1 (= TRUE) or
    -- both be 0 (= FALSE).
    if op.ObjectName = 'POWER_EQUALS' then
      if strt = 1 then
        relop := '=';
      else
        relop := '!=';
      end if;
    elsif op.ObjectName = 'POWER_LESSTHAN' then
      if strt = 1 then
        relop := '<';
      else
        relop := '>=';
      end if;
    elsif op.ObjectName = 'POWER_GREATERTHAN' then
      if strt = 1 then
        relop := '>';
      else
        relop := '<=';
      end if;
    else
      raise_application_error(-20101, 'Unsupported operator');
    end if;

    stmt := 'select r from '||ia.IndexSchema||'.'||ia.IndexName||'_pidx'||
              ' where cpos '|| '=' ||''''||cmppos||''''||
              ' and cval '||relop||''''||cmpval||'''';

    dbms_output.put_line('ODCIIndexStart>>>>>' || stmt);
    cnum := dbms_sql.open_cursor;
    dbms_sql.parse(cnum, stmt, dbms_sql.native);
    dbms_sql.define_column_rowid(cnum, 1, rid);
    nrows := dbms_sql.execute(cnum);

    -- Set context as the cursor number.
    sctx := power_idxtype_im(cnum);

    -- Return success.
    RETURN ODCICONST.SUCCESS;
  END;

  -- This definition of ODCIIndexStart is for queries on any
  -- cell. (The preceding definition was for queries on a
  -- specific cell.)
  STATIC FUNCTION ODCIIndexStart(sctx IN OUT power_idxtype_im,
        ia sys.odciindexinfo,
        op sys.odciPredInfo, qi sys.ODCIQueryInfo,
        strt NUMBER, stop NUMBER,
        cmpval NUMBER, env sys.ODCIEnv) RETURN NUMBER is
    cnum INTEGER;
    rid ROWID;
    nrows INTEGER;
    relop VARCHAR2(2);
    stmt VARCHAR2(1000);
  BEGIN
    dbms_output.put_line('ODCIIndexStart>>>>>');
    sys.ODCIIndexInfoDump(ia);
    sys.ODCIPredInfoDump(op);
    dbms_output.put_line('start key : '||strt);
    dbms_output.put_line('stop key : '||stop);
    dbms_output.put_line('compare value : '||cmpval);

    -- Take care of some error cases.
    -- The only predicates in which btree operators can appear are
    --    op() = 1     OR    op() = 0
    if (strt != 1) and (strt != 0) then
    raise_application_error(-20101, 'Incorrect predicate for operator');
    END if;

    if (stop != 1) and (stop != 0) then
    raise_application_error(-20101, 'Incorrect predicate for operator');
    END if;

    -- Generate the SQL statement to be executed.
    -- First, figure out the relational operator needed for the statement.
    -- Take into account the operator name and the start and stop keys.
    -- For now, the start and stop keys can both be 1 (= TRUE) or
    -- both be 0 (= FALSE).
    if op.ObjectName = 'POWER_EQUALSANY' then
      relop := '=';
    elsif op.ObjectName = 'POWER_LESSTHANANY' then
        relop := '<';
    elsif op.ObjectName = 'POWER_GREATERTHANANY' then
        relop := '>';
    else
      raise_application_error(-20101, 'Unsupported operator');
    end if;

    -- This statement returns the qualifying rows for the TRUE case.
    stmt := 'select distinct r from ' ||
            ia.IndexSchema || '.' || ia.IndexName || '_pidx' ||
            ' where cval ' || relop || '''' || cmpval || '''';
    -- In the FALSE case, we need to find the  complement of the rows.
    if (strt = 0) then
      stmt := 'select distinct r from ' ||
              ia.IndexSchema || '.' || ia.IndexName||'_pidx' ||
              ' minus '||stmt;
    end if;

    dbms_output.put_line('ODCIIndexStart>>>>>' || stmt);
    cnum := dbms_sql.open_cursor;
    dbms_sql.parse(cnum, stmt, dbms_sql.native);
    dbms_sql.define_column_rowid(cnum, 1, rid);
    nrows := dbms_sql.execute(cnum);

    -- Set context as the cursor number.
    sctx := power_idxtype_im(cnum);

    -- Return success.
    RETURN ODCICONST.SUCCESS;
  END;

  MEMBER FUNCTION ODCIIndexFetch(nrows NUMBER, rids OUT sys.odciridlist,
                                 env sys.ODCIEnv)
   RETURN NUMBER is
    cnum INTEGER;
    idx INTEGER := 1;
    rlist sys.odciridlist := sys.odciridlist();
    done boolean := FALSE;
  BEGIN
    dbms_output.put_line('ODCIIndexFetch>>>>>');
    dbms_output.put_line('Nrows : '||round(nrows));

    cnum := self.curnum;

    WHILE not done LOOP
      if idx > nrows then
         done := TRUE;
      else
         rlist.extend;
         if dbms_sql.fetch_rows(cnum) > 0 then
            dbms_sql.column_value_rowid(cnum, 1, rlist(idx));
            idx := idx + 1;
         else
            rlist(idx) := null;
            done := TRUE;
         END if;
      END if;
    END LOOP;

    rids := rlist;
    RETURN ODCICONST.SUCCESS;
  END;

  MEMBER FUNCTION ODCIIndexClose(env sys.ODCIEnv) RETURN NUMBER is
    cnum INTEGER;
  BEGIN
    dbms_output.put_line('ODCIIndexClose>>>>>');

    cnum := self.curnum;
    dbms_sql.close_cursor(cnum);
    RETURN ODCICONST.SUCCESS;
  END;

  STATIC FUNCTION ODCIIndexInsert(ia sys.odciindexinfo, rid VARCHAR2,
        newval PowerDemand_Typ, env sys.ODCIEnv)
       RETURN NUMBER as
       cid INTEGER;
       i BINARY_INTEGER;
       nrows INTEGER;
       stmt VARCHAR2(1000);
   BEGIN
     dbms_output.put_line(' ');
     dbms_output.put_line('ODCIIndexInsert>>>>>'||
      ' TotGridDemand= '||newval.TotGridDemand ||
      ' MaxCellDemand= '||newval.MaxCellDemand ||
      ' MinCellDemand= '||newval.MinCellDemand) ;
     sys.ODCIIndexInfoDump(ia);

     -- Construct the statement.
     stmt := ' INSERT INTO ' ||
             ia.IndexSchema || '.' || ia.IndexName || '_pidx' ||
             ' VALUES (:rr, :pos, :val)';

     -- Execute the statement.
     dbms_output.put_line('ODCIIndexInsert>>>>>'||stmt);
     -- Parse the statement.
     cid := dbms_sql.open_cursor;
     dbms_sql.parse(cid, stmt, dbms_sql.native);
     dbms_sql.bind_variable_rowid(cid, ':rr', rid);

     -- Iterate over the rows of the Varray and insert them.
     i := newval.CellDemandValues.FIRST;
     WHILE i IS NOT NULL LOOP
         -- Bind the row into the cursor for insert.
         dbms_sql.bind_variable(cid, ':pos', i);
         dbms_sql.bind_variable(cid, ':val', newval.CellDemandValues(i));
         -- Execute.
         nrows := dbms_sql.execute(cid);
         dbms_output.put_line('ODCIIndexInsert>>>>>('||
                               'RID' ||' , '||
                               i   || ' , '||
                               newval.CellDemandValues(i)|| ')');
         i := newval.CellDemandValues.NEXT(i);
      END LOOP;
     dbms_sql.close_cursor(cid);
     RETURN ODCICONST.SUCCESS;
   END ODCIIndexInsert;

  STATIC FUNCTION ODCIIndexDelete(ia sys.odciindexinfo, rid VARCHAR2,
         oldval PowerDemand_Typ, env sys.ODCIEnv)
       RETURN NUMBER as
       cid INTEGER;
       stmt VARCHAR2(1000);
       nrows INTEGER;
   BEGIN
     dbms_output.put_line(' ');
     dbms_output.put_line('ODCIIndexDelete>>>>>'||
      ' TotGridDemand= '||oldval.TotGridDemand ||
      ' MaxCellDemand= '||oldval.MaxCellDemand ||
      ' MinCellDemand= '||oldval.MinCellDemand) ;
     sys.ODCIIndexInfoDump(ia);

     -- Construct the statement.
     stmt := ' DELETE FROM ' ||
             ia.IndexSchema || '.' || ia.IndexName || '_pidx' ||
             ' WHERE r=:rr';
     dbms_output.put_line('ODCIIndexDelete>>>>>'||stmt);

     -- Parse and execute the statement.
     cid := dbms_sql.open_cursor;
     dbms_sql.parse(cid, stmt, dbms_sql.native);
     dbms_sql.bind_variable_rowid(cid, ':rr', rid);
     nrows := dbms_sql.execute(cid);
     dbms_sql.close_cursor(cid);

     RETURN ODCICONST.SUCCESS;
   END ODCIIndexDelete;

  STATIC FUNCTION ODCIIndexUpdate(ia sys.odciindexinfo, rid VARCHAR2,
        oldval PowerDemand_Typ, newval PowerDemand_Typ, env sys.ODCIEnv)
       RETURN NUMBER as
       cid INTEGER;
       cid2 INTEGER;
       stmt VARCHAR2(1000);
       stmt2 VARCHAR2(1000);
       nrows INTEGER;
       i NUMBER;
   BEGIN
     dbms_output.put_line(' ');
     dbms_output.put_line('ODCIIndexUpdate>>>>> Old'||
      ' TotGridDemand= '||oldval.TotGridDemand ||
      ' MaxCellDemand= '||oldval.MaxCellDemand ||
      ' MinCellDemand= '||oldval.MinCellDemand) ;
     dbms_output.put_line('ODCIIndexUpdate>>>>> New'||
      ' TotGridDemand= '||newval.TotGridDemand ||
      ' MaxCellDemand= '||newval.MaxCellDemand ||
      ' MinCellDemand= '||newval.MinCellDemand) ;
     sys.ODCIIndexInfoDump(ia);

     -- Delete old entries.
     stmt := ' DELETE FROM ' ||
             ia.IndexSchema || '.' || ia.IndexName || '_pidx' ||
             ' WHERE r=:rr';
     dbms_output.put_line('ODCIIndexUpdate>>>>>'||stmt);

     -- Parse and execute the statement.
     cid := dbms_sql.open_cursor;
     dbms_sql.parse(cid, stmt, dbms_sql.native);
     dbms_sql.bind_variable_rowid(cid, ':rr', rid);
     nrows := dbms_sql.execute(cid);
     dbms_sql.close_cursor(cid);

     -- Insert new entries.
     stmt2 := ' INSERT INTO ' ||
              ia.IndexSchema || '.' || ia.IndexName || '_pidx' ||
              ' VALUES (:rr, :pos, :val)';
     dbms_output.put_line('ODCIIndexUpdate>>>>>'||stmt2);

     -- Parse and execute the statement.
     cid2 := dbms_sql.open_cursor;
     dbms_sql.parse(cid2, stmt2, dbms_sql.native);
     dbms_sql.bind_variable_rowid(cid2, ':rr', rid);

     -- Iterate over the rows of the Varray and insert them.
     i := newval.CellDemandValues.FIRST;
     WHILE i IS NOT NULL LOOP
         -- Bind the row into the cursor for insert.
         dbms_sql.bind_variable(cid2, ':pos', i);
         dbms_sql.bind_variable(cid2, ':val', newval.CellDemandValues(i));
         nrows := dbms_sql.execute(cid2);
         dbms_output.put_line('ODCIIndexUpdate>>>>>('||
                               'RID' || ' , '||
                               i   || ' , '||
                               newval.CellDemandValues(i)|| ')');
         i := newval.CellDemandValues.NEXT(i);
      END LOOP;
     dbms_sql.close_cursor(cid2);

     RETURN ODCICONST.SUCCESS;
   END ODCIIndexUpdate;

  STATIC FUNCTION ODCIIndexGetMetadata(ia sys.odciindexinfo,
        expversion VARCHAR2, newblock OUT PLS_INTEGER, env sys.ODCIEnv)
       RETURN VARCHAR2 is
  BEGIN
    -- Let getversion do all the work since it has to maintain
    -- state across calls.

    RETURN power_pkg.getversion (ia.IndexSchema, ia.IndexName, newblock);

    EXCEPTION
      WHEN OTHERS THEN
        RAISE;

  END ODCIIndexGetMetaData;

END;
/

show errors;
-------------------------------------------------------------------
-- CREATE INDEXTYPE
-------------------------------------------------------------------
CREATE OR REPLACE INDEXTYPE power_idxtype
FOR
   Power_Equals(PowerDemand_Typ, NUMBER, NUMBER),
   Power_GreaterThan(PowerDemand_Typ, NUMBER, NUMBER),
   Power_LessThan(PowerDemand_Typ, NUMBER, NUMBER),
   Power_EqualsAny(PowerDemand_Typ, NUMBER),
   Power_GreaterThanAny(PowerDemand_Typ, NUMBER),
   Power_LessThanAny(PowerDemand_Typ, NUMBER)
USING power_idxtype_im;

-------------------------------------------------------------------
-- Create table and populate it --
-------------------------------------------------------------------
CREATE TABLE PowerDemand_Tab (
  -- Region for which these power demand readings apply
  region NUMBER,
  -- Values for each "sampling" time (for a given hour)
  sample PowerDemand_Typ
);

-- The next INSERT statements "cheat" by supplying
-- only 5 grid values (instead of 100).

-- INSERT statements are for region 1 to get enough timestamps
-- for a moving average using the Time Series cartridge.
-- (Time Series cartridge tests are in a separate file.)

declare
  i integer;
begin
  i := 0;

  while i < 2000 loop
    INSERT INTO PowerDemand_Tab VALUES(
      1,
      PowerDemand_Typ(NULL, NULL, NULL,
                      PowerGrid_Typ(i,i+1,i+2,i+3,i+4),SYSDATE));

    i := i+5;
 END LOOP;
end;
/

-- Also insert some rows for region 2.

INSERT INTO PowerDemand_Tab VALUES(2,
   PowerDemand_Typ(NULL, NULL, NULL, PowerGrid_Typ(9,8,11,16,5),
   to_date('02-01-1998 01','MM-DD-YYYY HH'))
);

INSERT INTO PowerDemand_Tab VALUES(2,
   PowerDemand_Typ(NULL, NULL, NULL, PowerGrid_Typ(9,8,11,20,5),
   to_date('02-01-1998 02','MM-DD-YYYY HH'))
);


DECLARE
CURSOR c1 IS SELECT Sample FROM PowerDemand_Tab FOR UPDATE;
s PowerDemand_Typ;
BEGIN
  OPEN c1;
  LOOP
     FETCH c1 INTO s;
     EXIT WHEN c1%NOTFOUND;
     s.SetTotalDemand;
     s.SetMaxDemand;
     s.SetMinDemand;
     dbms_output.put_line(s.TotGridDemand);
     dbms_output.put_line(s.MaxCellDemand);
     dbms_output.put_line(s.MinCellDemand);
     UPDATE PowerDemand_Tab SET Sample = s WHERE CURRENT of c1;
  END LOOP;
  CLOSE c1;
END;
/

-------------------------------------------------------------------
-- CREATE GENERIC FUNCTION TO COMPUTE SELECTIVITY OF PREDICATE
-------------------------------------------------------------------

CREATE FUNCTION get_selectivity(relop VARCHAR2, value NUMBER,
                                lo NUMBER, hi NUMBER, ndv NUMBER)
  RETURN NUMBER AS
  sel NUMBER := NULL;
BEGIN
  -- This function computes the selectivity (as a percentage)
  -- of a predicate
  --             col <relop> <value>
  -- where <relop> is one of: =, !=, <, <=, >, >=
  --       <value> is one of: 0, 1
  -- lo and hi are the minimum and maximum values of the column in
  -- the table.  This function performs a simplistic estimation of the
  -- selectivity by assulog that the range of distinct values of
  -- the column is distributed uniformly in the range lo..hi and that
  -- each distinct value occurs nrows/(hi-lo+1) times (where nrows is
  -- the number of rows).

  IF ndv IS NULL OR ndv <= 0 THEN
    RETURN 0;
  END IF;

  -- col != <value>
  IF relop = '!=' THEN
    IF value between lo and hi THEN
      sel := 1 - 1/ndv;
    ELSE
      sel := 1;
    END IF;

  -- col = <value>
  ELSIF relop = '=' THEN
    IF value between lo and hi THEN
      sel := 1/ndv;
    ELSE
      sel := 0;
    END IF;

  -- col >= <value>
  ELSIF relop = '>=' THEN
    IF lo = hi THEN
      IF value <= lo THEN
        sel := 1;
      ELSE
        sel := 0;
      END IF;
    ELSIF value between lo and hi THEN
      sel := (hi-value)/(hi-lo) + 1/ndv;
    ELSIF value < lo THEN
      sel := 1;
    ELSE
      sel := 0;
    END IF;

  -- col < <value>
  ELSIF relop = '<' THEN
    IF lo = hi THEN
      IF value > lo THEN
        sel := 1;
      ELSE
        sel := 0;
      END IF;
    ELSIF value between lo and hi THEN
      sel := (value-lo)/(hi-lo);
    ELSIF value < lo THEN
      sel := 0;
    ELSE
      sel := 1;
    END IF;

  -- col <= <value>
  ELSIF relop = '<=' THEN
    IF lo = hi THEN
      IF value >= lo THEN
        sel := 1;
      ELSE
        sel := 0;
      END IF;
    ELSIF value between lo and hi THEN
      sel := (value-lo)/(hi-lo) + 1/ndv;
    ELSIF value < lo THEN
      sel := 0;
    ELSE
      sel := 1;
    END IF;

  -- col > <value>
  ELSIF relop = '>' THEN
    IF lo = hi THEN
      IF value < lo THEN
        sel := 1;
      ELSE
        sel := 0;
      END IF;
    ELSIF value between lo and hi THEN
      sel := (hi-value)/(hi-lo);
    ELSIF value < lo THEN
      sel := 1;
    ELSE
      sel := 0;
    END IF;

  END IF;

  RETURN least(100, ceil(100*sel));

END;
/

-------------------------------------------------------------------
-- CREATE STATISTICS IMPLEMENTATION TYPE
-------------------------------------------------------------------
CREATE OR REPLACE TYPE power_statistics AS OBJECT
(
  curnum NUMBER,
  STATIC FUNCTION ODCIGetInterfaces(ifclist OUT sys.ODCIObjectList)
     RETURN NUMBER,
  STATIC FUNCTION ODCIStatsCollect(col sys.ODCIColInfo,
     options sys.ODCIStatsOptions, rawstats OUT RAW, env sys.ODCIEnv)
     RETURN NUMBER,
  STATIC FUNCTION ODCIStatsDelete(col sys.ODCIColInfo,
     statistics OUT RAW, env sys.ODCIEnv)
     RETURN NUMBER,
  STATIC FUNCTION ODCIStatsCollect(ia sys.ODCIIndexInfo,
     options sys.ODCIStatsOptions, rawstats OUT RAW, env sys.ODCIEnv)
     RETURN NUMBER,
  STATIC FUNCTION ODCIStatsDelete(ia sys.ODCIIndexInfo,
     statistics OUT RAW, env sys.ODCIEnv)
     RETURN NUMBER,
  STATIC FUNCTION ODCIStatsSelectivity(pred sys.ODCIPredInfo,
     sel OUT NUMBER, args sys.ODCIArgDescList, strt NUMBER, stop NUMBER,
     object PowerDemand_Typ, cell NUMBER, value NUMBER, env sys.ODCIEnv)
     RETURN NUMBER,
     PRAGMA restrict_references(ODCIStatsSelectivity, WNDS, WNPS),
  STATIC FUNCTION ODCIStatsSelectivity(pred sys.ODCIPredInfo,
     sel OUT NUMBER, args sys.ODCIArgDescList, strt NUMBER, stop NUMBER,
     object PowerDemand_Typ, value NUMBER, env sys.ODCIEnv)
     RETURN NUMBER,
     PRAGMA restrict_references(ODCIStatsSelectivity, WNDS, WNPS),
  STATIC FUNCTION ODCIStatsIndexCost(ia sys.ODCIIndexInfo,
     sel NUMBER, cost OUT sys.ODCICost, qi sys.ODCIQueryInfo,
     pred sys.ODCIPredInfo, args sys.ODCIArgDescList,
     strt NUMBER, stop NUMBER, cmppos NUMBER, cmpval NUMBER, env sys.ODCIEnv)
     RETURN NUMBER,
     PRAGMA restrict_references(ODCIStatsIndexCost, WNDS, WNPS),
  STATIC FUNCTION ODCIStatsIndexCost(ia sys.ODCIIndexInfo,
     sel NUMBER, cost OUT sys.ODCICost, qi sys.ODCIQueryInfo,
     pred sys.ODCIPredInfo, args sys.ODCIArgDescList,
     strt NUMBER, stop NUMBER, cmpval NUMBER, env sys.ODCIEnv) RETURN NUMBER,
     PRAGMA restrict_references(ODCIStatsIndexCost, WNDS, WNPS),
  STATIC FUNCTION ODCIStatsFunctionCost(func sys.ODCIFuncInfo,
     cost OUT sys.ODCICost, args sys.ODCIArgDescList,
     object PowerDemand_Typ, cell NUMBER, value NUMBER, env sys.ODCIEnv)
     RETURN NUMBER,
     PRAGMA restrict_references(ODCIStatsFunctionCost, WNDS, WNPS),
  STATIC FUNCTION ODCIStatsFunctionCost(func sys.ODCIFuncInfo,
     cost OUT sys.ODCICost, args sys.ODCIArgDescList,
     object PowerDemand_Typ, value NUMBER, env sys.ODCIEnv) RETURN NUMBER,
     PRAGMA restrict_references(ODCIStatsFunctionCost, WNDS, WNPS)
);
/
show errors;

CREATE OR REPLACE TYPE BODY power_statistics
IS
  STATIC FUNCTION ODCIGetInterfaces(ifclist OUT sys.ODCIObjectList)
     RETURN NUMBER IS
  BEGIN
     ifclist := sys.ODCIObjectList(sys.ODCIObject('SYS','ODCISTATS2'));
     RETURN ODCIConst.Success;
  END ODCIGetInterfaces;

  STATIC FUNCTION ODCIStatsCollect(col sys.ODCIColInfo,
                                   options sys.ODCIStatsOptions,
                                   rawstats OUT RAW, env sys.ODCIEnv)
     RETURN NUMBER IS
     cnum                INTEGER;
     stmt                VARCHAR2(1000);
     junk                INTEGER;

     cval                NUMBER;
     colname             VARCHAR2(30) := rtrim(ltrim(col.colName, '"'), '"');
     statsexists         BOOLEAN := FALSE;
     pdemands            PowerDemand_Tab%ROWTYPE;
     user_defined_stats  PowerCartUserStats%ROWTYPE;
     CURSOR c1(tname VARCHAR2, cname VARCHAR2) IS
       SELECT * FROM PowerCartUserStats
       WHERE tab = tname
         AND col = cname;
     CURSOR c2 IS
       SELECT * FROM PowerDemand_Tab;

  BEGIN
    sys.ODCIColInfoDump(col);
    sys.ODCIStatsOptionsDump(options);

    IF (col.TableSchema IS NULL OR col.TableName IS NULL
        OR col.ColName IS NULL) THEN
      RETURN ODCIConst.Error;
    END IF;

    dbms_output.put_line('ODCIStatsCollect>>>>>');
    dbms_output.put_line('**** Analyzing column '
                         || col.TableSchema
                         || '.' || col.TableName
                         || '.' || col.ColName);

    -- Check if statistics exist for this column
    FOR user_defined_stats IN c1(col.TableName, colname) LOOP
      statsexists := TRUE;
      EXIT;
    END LOOP;

    IF not statsexists THEN
      -- column statistics don't exist; create entries for
      -- each of the 100 cells
      cnum := dbms_sql.open_cursor;
      FOR i in 1..100 LOOP
        stmt := 'INSERT INTO PowerCartUserStats VALUES( '
             || '''' || col.TableName || ''', '
             || '''' || colname || ''', '
             || to_char(i) || ', '
             || 'NULL, NULL, NULL)';
        dbms_sql.parse(cnum, stmt, dbms_sql.native);
        junk := dbms_sql.execute(cnum);
      END LOOP;
      dbms_sql.close_cursor(cnum);
    ELSE
      -- column statistics exist; initialize to NULL
      cnum := dbms_sql.open_cursor;
      stmt := 'UPDATE PowerCartUserStats'
           || ' SET lo = NULL, hi = NULL, nrows = NULL'
           || ' WHERE tab = ' || col.TableName
           || ' AND col = ' || colname;
      dbms_sql.parse(cnum, stmt, dbms_sql.native);
      junk := dbms_sql.execute(cnum);
      dbms_sql.close_cursor(cnum);
    END IF;

    -- For each cell position, the following statistics are collected:
    --   maximum value
    --   minimum value
    --   number of rows (excluding NULLs)
    cnum := dbms_sql.open_cursor;
    FOR i in 1..100 LOOP
      FOR pdemands IN c2 LOOP
        IF i BETWEEN pdemands.sample.CellDemandValues.FIRST AND
                     pdemands.sample.CellDemandValues.LAST THEN
          cval := pdemands.sample.CellDemandValues(i);
          stmt := 'UPDATE PowerCartUserStats SET '
               || 'lo = least(' || 'NVL(' || to_char(cval) || ', lo), '
               || 'NVL(' || 'lo, ' || to_char(cval) || ')), '
               || 'hi = greatest(' || 'NVL(' || to_char(cval) || ', hi), '
               || 'NVL(' || 'hi, ' || to_char(cval) || ')), '
               || 'nrows = decode(nrows, NULL, decode('
               || to_char(cval) || ', NULL, NULL, 1), decode('
               || to_char(cval) || ', NULL, nrows, nrows+1)) '
               || 'WHERE cpos = ' || to_char(i)
               || ' AND tab = ''' || col.TableName || ''''
               || ' AND col = ''' || colname || '''';
          dbms_sql.parse(cnum, stmt, dbms_sql.native);
          junk := dbms_sql.execute(cnum);
        END IF;
      END LOOP;
    END LOOP;
    dbms_sql.close_cursor(cnum);

    rawstats := NULL;

    return ODCIConst.Success;

  END;

  STATIC FUNCTION ODCIStatsDelete(col sys.ODCIColInfo, statistics OUT RAW,
                                  env sys.ODCIEnv)
     RETURN NUMBER IS
     cnum                INTEGER;
     stmt                VARCHAR2(1000);
     junk                INTEGER;

     colname             VARCHAR2(30) := rtrim(ltrim(col.colName, '"'), '"');
     statsexists         BOOLEAN := FALSE;
     user_defined_stats  PowerCartUserStats%ROWTYPE;
     CURSOR c1(tname VARCHAR2, cname VARCHAR2) IS
       SELECT * FROM PowerCartUserStats
       WHERE tab = tname
         AND col = cname;
  BEGIN
    sys.ODCIColInfoDump(col);

    IF (col.TableSchema IS NULL OR col.TableName IS NULL
        OR col.ColName IS NULL) THEN
      RETURN ODCIConst.Error;
    END IF;

    dbms_output.put_line('ODCIStatsDelete>>>>>');
    dbms_output.put_line('**** Analyzing (delete) column '
                         || col.TableSchema
                         || '.' || col.TableName
                         || '.' || col.ColName);

    -- Check if statistics exist for this column
    FOR user_defined_stats IN c1(col.TableName, colname) LOOP
      statsexists := TRUE;
      EXIT;
    END LOOP;

    -- If user-defined statistics exist, delete them
    IF statsexists THEN
      stmt := 'DELETE FROM PowerCartUserStats'
           || ' WHERE tab = ''' || col.TableName || ''''
           || ' AND col = ''' || colname || '''';
      cnum := dbms_sql.open_cursor;
      dbms_output.put_line('ODCIStatsDelete>>>>>');
      dbms_output.put_line('ODCIStatsDelete>>>>>' || stmt);
      dbms_sql.parse(cnum, stmt, dbms_sql.native);
      junk := dbms_sql.execute(cnum);
      dbms_sql.close_cursor(cnum);
    END IF;

    RETURN ODCIConst.Success;
  END;

  STATIC FUNCTION ODCIStatsCollect (ia sys.ODCIIndexInfo,
     options sys.ODCIStatsOptions, rawstats OUT RAW, env sys.ODCIEnv)
     RETURN NUMBER IS
     cnum                INTEGER;
     stmt                VARCHAR2(1000);
     junk                INTEGER;
  BEGIN
    -- To analyze a domain index, simply analyze the table that
    -- implements the index

    sys.ODCIIndexInfoDump(ia);
    sys.ODCIStatsOptionsDump(options);

    stmt := 'ANALYZE TABLE '
         || ia.IndexSchema || '.' || ia.IndexName || '_pidx'
         || ' COMPUTE STATISTICS';

    dbms_output.put_line('**** Analyzing index '
                         || ia.IndexSchema || '.' || ia.IndexName);
    dbms_output.put_line('SQL Statement: ' || stmt);

    cnum := dbms_sql.open_cursor;
    dbms_sql.parse(cnum, stmt, dbms_sql.native);
    junk := dbms_sql.execute(cnum);
    dbms_sql.close_cursor(cnum);

    rawstats := NULL;

    RETURN ODCIConst.Success;
  END;

  STATIC FUNCTION ODCIStatsDelete(ia sys.ODCIIndexInfo,
                                  statistics OUT RAW, env sys.ODCIEnv)
     RETURN NUMBER IS
     cnum                INTEGER;
     stmt                VARCHAR2(1000);
     junk                INTEGER;
  BEGIN
    -- To delete statistics for a domain index, simply delete the
    -- statistics for the table implementing the index

    sys.ODCIIndexInfoDump(ia);

    stmt := 'ANALYZE TABLE '
         || ia.IndexSchema || '.' || ia.IndexName || '_pidx'
         || ' DELETE STATISTICS';

    dbms_output.put_line('**** Analyzing (delete) index '
                         || ia.IndexSchema || '.' || ia.IndexName);
    dbms_output.put_line('SQL Statement: ' || stmt);

    cnum := dbms_sql.open_cursor;
    dbms_sql.parse(cnum, stmt, dbms_sql.native);
    junk := dbms_sql.execute(cnum);
    dbms_sql.close_cursor(cnum);

    RETURN ODCIConst.Success;
  END;

  STATIC FUNCTION ODCIStatsSelectivity(pred sys.ODCIPredInfo,
     sel OUT NUMBER, args sys.ODCIArgDescList, strt NUMBER, stop NUMBER,
     object PowerDemand_Typ, cell NUMBER, value NUMBER, env sys.ODCIEnv)
     RETURN NUMBER IS
     fname               varchar2(30);
     relop               varchar2(2);
     lo                  NUMBER;
     hi                  NUMBER;
     nrows               NUMBER;
     colname             VARCHAR2(30);
     statsexists         BOOLEAN := FALSE;
     stats               PowerCartUserStats%ROWTYPE;
     CURSOR c1(cell NUMBER, tname VARCHAR2, cname VARCHAR2) IS
       SELECT * FROM PowerCartUserStats
       WHERE cpos = cell
         AND tab = tname
         AND col = cname;
  BEGIN
    -- compute selectivity only when predicate is of the form:
    --      fn(col, <cell>, <value>) <relop> <val>
    -- In all other cases, return an error and let the optimizer
    -- make a guess.  We also assume that the function "fn" has
    -- a return value of 0, 1, or NULL.

    -- start value
    IF (args(1).ArgType != ODCIConst.ArgLit AND
        args(1).ArgType != ODCIConst.ArgNull) THEN
      RETURN ODCIConst.Error;
    END IF;

    -- stop value
    IF (args(2).ArgType != ODCIConst.ArgLit AND
        args(2).ArgType != ODCIConst.ArgNull) THEN
      RETURN ODCIConst.Error;
    END IF;

    -- first argument of function
    IF (args(3).ArgType != ODCIConst.ArgCol) THEN
      RETURN ODCIConst.Error;
    END IF;

    -- second argument of function
    IF (args(4).ArgType != ODCIConst.ArgLit AND
        args(4).ArgType != ODCIConst.ArgNull) THEN
      RETURN ODCIConst.Error;
    END IF;

    -- third argument of function
    IF (args(5).ArgType != ODCIConst.ArgLit AND
        args(5).ArgType != ODCIConst.ArgNull) THEN
      RETURN ODCIConst.Error;
    END IF;

    colname := rtrim(ltrim(args(3).colName, '"'), '"');

    -- Check if the statistics table exists (we are using a
    -- user-defined table to store the user-defined statistics).
    -- Get user-defined statistics: MIN, MAX, NROWS
    FOR stats IN c1(cell, args(3).TableName, colname) LOOP
      -- Get user-defined statistics: MIN, MAX, NROWS
      lo := stats.lo;
      hi := stats.hi;
      nrows := stats.nrows;
      statsexists := TRUE;
      EXIT;
    END LOOP;

    -- If no user-defined statistics were collected, return error
    IF not statsexists THEN
      RETURN ODCIConst.Error;
    END IF;

    -- selectivity is 0 for "fn(col, <cell>, <value>) < 0"
    IF (stop = 0 AND
        bitand(pred.Flags, ODCIConst.PredIncludeStop) = 0) THEN
      sel := 0;
      RETURN ODCIConst.Success;
    END IF;

    -- selectivity is 0 for "fn(col, <cell>, <value>) > 1"
    IF (strt = 1 AND
        bitand(pred.Flags, ODCIConst.PredIncludeStart) = 0) THEN
      sel := 0;
      RETURN ODCIConst.Success;
    END IF;

    -- selectivity is 100% for "fn(col, <cell>, <value>) >= 0"
    IF (strt = 0 AND
        bitand(pred.Flags, ODCIConst.PredExactMatch) = 0 AND
        bitand(pred.Flags, ODCIConst.PredIncludeStart) > 0) THEN
      sel := 100;
      RETURN ODCIConst.Success;
    END IF;

    -- selectivity is 100% for "fn(col, <cell>, <value>) <= 1"
    IF (stop = 1 AND
        bitand(pred.Flags, ODCIConst.PredExactMatch) = 0 AND
        bitand(pred.Flags, ODCIConst.PredIncludeStop) > 0) THEN
      sel := 100;
      RETURN ODCIConst.Success;
    END IF;

    -- get function name
    IF bitand(pred.Flags, ODCIConst.PredObjectFunc) > 0 THEN
      fname := pred.ObjectName;
    ELSE
      fname := pred.MethodName;
    END IF;

    -- convert prefix relational operator to infix;
    -- e.g., "Power_EqualsSpecific_Func(col, <cell>, <value>) = 1"
    -- becomes "col[<cell>] = <value>"

    --   Power_EqualsSpecific_Func(col, <cell>, <value>) = 0
    --   Power_EqualsSpecific_Func(col, <cell>, <value>) <= 0
    --   Power_EqualsSpecific_Func(col, <cell>, <value>) < 1
    -- can be transformed to
    --   col[<cell>] != <value>
    IF (fname LIKE upper('Power_Equals%') AND
        (stop = 0 OR
         (stop = 1 AND
          bitand(pred.Flags, ODCIConst.PredIncludeStop) = 0))) THEN
      relop := '!=';

    --   Power_LessThanSpecific_Func(col, <cell>, <value>) = 0
    --   Power_LessThanSpecific_Func(col, <cell>, <value>) <= 0
    --   Power_LessThanSpecific_Func(col, <cell>, <value>) < 1
    -- can be transformed to
    --   col[<cell>] >= <value>
    ELSIF (fname LIKE upper('Power_LessThan%') AND
           (stop = 0 OR
            (stop = 1 AND
             bitand(pred.Flags, ODCIConst.PredIncludeStop) = 0))) THEN
      relop := '>=';

    --   Power_GreaterThanSpecific_Func(col, <cell>, <value>) = 0
    --   Power_GreaterThanSpecific_Func(col, <cell>, <value>) <= 0
    --   Power_GreaterThanSpecific_Func(col, <cell>, <value>) < 1
    -- can be transformed to
    --   col[<cell>] <= <value>
    ELSIF (fname LIKE upper('Power_GreaterThan%') AND
           (stop = 0 OR
            (stop = 1 AND
             bitand(pred.Flags, ODCIConst.PredIncludeStop) = 0))) THEN
      relop := '<=';

    --   Power_EqualsSpecific_Func(col, <cell>, <value>) = 1
    --   Power_EqualsSpecific_Func(col, <cell>, <value>) >= 1
    --   Power_EqualsSpecific_Func(col, <cell>, <value>) > 0
    -- can be transformed to
    --   col[<cell>] = <value>
    ELSIF (fname LIKE upper('Power_Equals%') AND
           (strt = 1 OR
            (strt = 0 AND
             bitand(pred.Flags, ODCIConst.PredIncludeStart) = 0))) THEN
      relop := '=';

    --   Power_LessThanSpecific_Func(col, <cell>, <value>) = 1
    --   Power_LessThanSpecific_Func(col, <cell>, <value>) >= 1
    --   Power_LessThanSpecific_Func(col, <cell>, <value>) > 0
    -- can be transformed to
    --   col[<cell>] < <value>
    ELSIF (fname LIKE upper('Power_LessThan%') AND
           (strt = 1 OR
            (strt = 0 AND
             bitand(pred.Flags, ODCIConst.PredIncludeStart) = 0))) THEN
      relop := '<';

    --   Power_GreaterThanSpecific_Func(col, <cell>, <value>) = 1
    --   Power_GreaterThanSpecific_Func(col, <cell>, <value>) >= 1
    --   Power_GreaterThanSpecific_Func(col, <cell>, <value>) > 0
    -- can be transformed to
    --   col[<cell>] > <value>
    ELSIF (fname LIKE upper('Power_GreaterThan%') AND
           (strt = 1 OR
            (strt = 0 AND
             bitand(pred.Flags, ODCIConst.PredIncludeStart) = 0))) THEN
      relop := '>';

    ELSE
      RETURN ODCIConst.Error;

    END IF;

    sel := get_selectivity(relop, value, lo, hi, nrows);
    RETURN ODCIConst.Success;
  END;

  STATIC FUNCTION ODCIStatsSelectivity(pred sys.ODCIPredInfo,
     sel OUT NUMBER, args sys.ODCIArgDescList, strt NUMBER, stop NUMBER,
     object PowerDemand_Typ, value NUMBER, env sys.ODCIEnv)
     RETURN NUMBER IS
     cellsel             NUMBER;
     i                   NUMBER;
     specsel             NUMBER;
     newargs             sys.ODCIArgDescList
                           := sys.ODCIArgDescList(NULL, NULL, NULL,
                                                  NULL, NULL);
  BEGIN
    -- To compute selectivity for the ANY functions, call the
    -- selectivity function for the SPECIFIC functions.  For example,
    -- the selectivity of the ANY predicate
    --
    --     Power_EqualsAnyFunc(object, value) = 1
    --
    -- is computed as
    --
    --     1 - (1-s[1])(1-s[2])...(1-s[100])
    --
    -- where s[i] is the selectivity of the SPECIFIC predicate
    --
    --     Power_EqualsSpecific_Func(object, i, value) = 1
    --

    sel := 1;
    newargs(1) := args(1);
    newargs(2) := args(2);
    newargs(3) := args(3);
    newargs(4) := sys.ODCIArgDesc(ODCIConst.ArgLit, NULL, NULL, NULL,
                                  NULL, NULL, NULL);
    newargs(5) := args(4);
    FOR i in 1..100 LOOP
      cellsel := NULL;
      specsel := power_statistics.ODCIStatsSelectivity(pred, cellsel,
                   newargs, strt, stop, object, i, value, env);
      IF specsel = ODCIConst.Success THEN
        sel := sel * (1 - cellsel/100);
      END IF;
    END LOOP;

    sel := (1 - sel)*100;
    RETURN ODCIConst.Success;
  END;

  STATIC FUNCTION ODCIStatsIndexCost(ia sys.ODCIIndexInfo,
     sel NUMBER, cost OUT sys.ODCICost, qi sys.ODCIQueryInfo,
     pred sys.ODCIPredInfo, args sys.ODCIArgDescList,
     strt NUMBER, stop NUMBER, cmppos NUMBER, cmpval NUMBER, env sys.ODCIEnv)
     RETURN NUMBER IS
  BEGIN
    -- This is the cost for queries on a specific cell; simply
    -- use the cost for queries on any cell.
    RETURN ODCIStatsIndexCost(ia, sel, cost, qi, pred, args,
                              strt, stop, cmpval, env);
  END;

  STATIC FUNCTION ODCIStatsIndexCost(ia sys.ODCIIndexInfo,
     sel NUMBER, cost OUT sys.ODCICost, qi sys.ODCIQueryInfo,
     pred sys.ODCIPredInfo, args sys.ODCIArgDescList,
     strt NUMBER, stop NUMBER, cmpval NUMBER, env sys.ODCIEnv)
     RETURN NUMBER IS
     ixtable             VARCHAR2(40);
     numblocks           NUMBER := NULL;
     get_table           user_tables%ROWTYPE;
     CURSOR c1(tab VARCHAR2) IS
       SELECT * FROM user_tables WHERE table_name = tab;
  BEGIN
    -- This is the cost for queries on any cell.

    -- To compute the cost of a domain index, multiply the
    -- number of blocks in the table implementing the index
    -- with the selectivity

    -- Return if we don't have predicate selectivity
    IF sel IS NULL THEN
      RETURN ODCIConst.Error;
    END IF;

    cost := sys.ODCICost(NULL, NULL, NULL, NULL);

    -- Get name of table implementing the domain index
    ixtable := ia.IndexName || '_pidx';

    -- Get number of blocks in domain index
    FOR get_table IN c1(upper(ixtable)) LOOP
      numblocks := get_table.blocks;
      EXIT;
    END LOOP;

    IF numblocks IS NULL THEN
      -- Exit if there are no user-defined statistics for the index
      RETURN ODCIConst.Error;
    END IF;

    cost.CPUCost := ceil(400*(sel/100)*numblocks);
    cost.IOCost := ceil(1.5*(sel/100)*numblocks);
    RETURN ODCIConst.Success;
  END;

  STATIC FUNCTION ODCIStatsFunctionCost(func sys.ODCIFuncInfo,
     cost OUT sys.ODCICost, args sys.ODCIArgDescList,
     object PowerDemand_Typ, cell NUMBER, value NUMBER, env sys.ODCIEnv)
     RETURN NUMBER IS
  BEGIN
    -- This is the cost for functions on a specific cell; simply
    -- use the cost for functions on any cell.
    RETURN ODCIStatsFunctionCost(func, cost, args, object, value, env);
  END;

  STATIC FUNCTION ODCIStatsFunctionCost(func sys.ODCIFuncInfo,
     cost OUT sys.ODCICost, args sys.ODCIArgDescList,
     object PowerDemand_Typ, value NUMBER, env sys.ODCIEnv)
     RETURN NUMBER IS
     fname               VARCHAR2(30);
  BEGIN
    cost := sys.ODCICost(NULL, NULL, NULL, NULL);

    -- Get function name
    IF  bitand(func.Flags, ODCIConst.ObjectFunc) > 0 THEN
      fname := func.ObjectName;
    ELSE
      fname := func.MethodName;
    END IF;

    IF fname LIKE upper('Power_LessThan%') THEN
      cost.CPUCost := 5000;
      cost.IOCost := 0;
      RETURN ODCIConst.Success;
    ELSIF fname LIKE upper('Power_Equals%') THEN
      cost.CPUCost := 7000;
      cost.IOCost := 0;
      RETURN ODCIConst.Success;
    ELSIF fname LIKE upper('Power_GreaterThan%') THEN
      cost.CPUCost := 5000;
      cost.IOCost := 0;
      RETURN ODCIConst.Success;
    ELSE
      RETURN ODCIConst.Error;
    END IF;
  END;

END;
/
show errors;

-- Associate statistics type with types, indextypes, and functions
ASSOCIATE STATISTICS WITH TYPES PowerDemand_Typ USING power_statistics;
ASSOCIATE STATISTICS WITH INDEXTYPES power_idxtype USING power_statistics;
ASSOCIATE STATISTICS WITH FUNCTIONS
  Power_EqualsSpecific_Func,
  Power_GreaterThanSpecific_Func,
  Power_LessThanSpecific_Func,
  Power_EqualsAny_Func,
  Power_GreaterThanAny_Func,
  Power_LessThanAny_Func
  USING power_statistics;

-- Analyze the table
ANALYZE TABLE PowerDemand_Tab COMPUTE STATISTICS;

-- Verify that user-defined statistics were collected
SELECT tab tablename, col colname, cpos, lo, hi, nrows
FROM PowerCartUserStats
WHERE nrows IS NOT NULL
ORDER BY cpos;

-- Delete the statistics
ANALYZE TABLE PowerDemand_Tab DELETE STATISTICS;

-- Verify that user-defined statistics were deleted
SELECT tab tablename, col colname, cpos, lo, hi, nrows
FROM PowerCartUserStats
WHERE nrows IS NOT NULL
ORDER BY cpos;

-- Re-analyze the table
ANALYZE TABLE PowerDemand_Tab COMPUTE STATISTICS;

-- Verify that user-defined statistics were re-collected
SELECT tab tablename, col colname, cpos, lo, hi, nrows
FROM PowerCartUserStats
WHERE nrows IS NOT NULL
ORDER BY cpos;

-- Examine the values.
SELECT region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
   P.Sample.MinCellDemand  
 FROM PowerDemand_Tab P
 ORDER BY 1,4,3;


-------------------------------------------------------------------
-- Query, referencing the functions.
-------------------------------------------------------------------

SELECT P.Region, P.Sample.TotGridDemand ,P.Sample.MaxCellDemand,
     P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_EqualsSpecific_Func(P.Sample,2,10) = 1;

SELECT P.Region, P.Sample.TotGridDemand ,P.Sample.MaxCellDemand,
     P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_EqualsSpecific_Func(P.Sample,2,8) = 1 
     ORDER BY 1,2;

SET SERVEROUTPUT ON SIZE 999999

-------------------------------------------------------------------
-- Query, referencing the operators (without index)
-------------------------------------------------------------------

EXPLAIN PLAN FOR
SELECT P.Region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_Equals(P.Sample,2,211) = 1;
set echo off
@@extdemo0
set echo on

SELECT P.Region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_Equals(P.Sample,2,211) = 1;

EXPLAIN PLAN FOR
SELECT P.Region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_Equals(P.Sample,1,10) = 1;
set echo off
@@extdemo0
set echo on

SELECT P.Region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_Equals(P.Sample,1,10) = 1;

EXPLAIN PLAN FOR
SELECT P.Region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_Equals(P.Sample,2,9) = 1;
set echo off
@@extdemo0
set echo on

SELECT P.Region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_Equals(P.Sample,2,9) = 1;

EXPLAIN PLAN FOR
SELECT P.Region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_Equals(P.Sample,2,8) = 1;
set echo off
@@extdemo0
set echo on

SELECT P.Region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_Equals(P.Sample,2,8) = 1 
     ORDER BY 1,2;

EXPLAIN PLAN FOR
SELECT P.Region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_EqualsAny(P.Sample,9) = 1;
set echo off
@@extdemo0
set echo on

SELECT P.Region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_EqualsAny(P.Sample,9) = 1  
     ORDER BY 1,2;

EXPLAIN PLAN FOR
SELECT P.Region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_GreaterThanAny(P.Sample,10) = 0;
set echo off
@@extdemo0
set echo on

SELECT P.Region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_GreaterThanAny(P.Sample,10) = 0  
     ORDER BY 1,2;

EXPLAIN PLAN FOR
SELECT P.Region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_LessThanAny(P.Sample,10) = 1;
set echo off
@@extdemo0
set echo on

SELECT P.Region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_LessThanAny(P.Sample,10) = 1  
     ORDER BY 1,2;

-------------------------------------------------------------------
-- CREATE INDEX
-------------------------------------------------------------------
CREATE INDEX PowerIndex ON PowerDemand_Tab(Sample)
   INDEXTYPE IS power_idxtype parameters('test');

-- Analyze the index
ANALYZE INDEX PowerIndex COMPUTE STATISTICS;

-------------------------------------------------------------------
-- Query, referencing the operators (with index)
-------------------------------------------------------------------
EXPLAIN PLAN FOR
SELECT P.Region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_Equals(P.Sample,2,211) = 1;
set echo off
@@extdemo0
set echo on

SELECT P.Region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_Equals(P.Sample,2,211) = 1;

EXPLAIN PLAN FOR
SELECT P.Region, P.Sample.TotGridDemand ,P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_Equals(P.Sample,1,10) = 1;
set echo off
@@extdemo0
set echo on

SELECT P.Region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_Equals(P.Sample,1,10) = 1;

EXPLAIN PLAN FOR
SELECT P.Region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_EqualsAny(P.Sample,9) = 1;
set echo off
@@extdemo0
set echo on

SELECT P.Region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_EqualsAny(P.Sample,9) = 1  
     ORDER BY 1,2;

EXPLAIN PLAN FOR
SELECT P.Region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_GreaterThanAny(P.Sample,4) = 0;
set echo off
@@extdemo0
set echo on

SELECT P.Region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_GreaterThanAny(P.Sample,4) = 0;

EXPLAIN PLAN FOR
SELECT P.Region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_LessThanAny(P.Sample,5) = 1;
set echo off
@@extdemo0
set echo on

SELECT P.Region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_LessThanAny(P.Sample,5) = 1;

----------------------------------------------------------------
-- Test incremental inserts. --
----------------------------------------------------------------
INSERT INTO PowerDemand_Tab VALUES(
   3,
   PowerDemand_Typ(NULL, NULL, NULL, PowerGrid_Typ(9,8,10,6,5),
      to_date('02-01-1998 01','MM-DD-YYYY HH'))
);

SET ECHO ON
SET SERVEROUTPUT ON SIZE 999999

declare
CURSOR c1 IS SELECT Sample FROM PowerDemand_Tab WHERE Region=3
   FOR UPDATE;
s PowerDemand_Typ;
BEGIN
  OPEN c1;
  LOOP
     FETCH c1 INTO s;
     EXIT WHEN c1%NOTFOUND;
     s.SetTotalDemand;
     s.SetMaxDemand;
     s.SetMinDemand;
     dbms_output.put_line(s.TotGridDemand);
     dbms_output.put_line(s.MaxCellDemand);
     dbms_output.put_line(s.MinCellDemand);
     UPDATE PowerDemand_Tab SET Sample = s WHERE CURRENT OF c1;
  END LOOP;
  CLOSE c1;
END;
/

-- This should return one more row.
SELECT P.Region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_Equals(P.Sample,2,8) = 1  
     ORDER BY 1,2;

----------------------------------------------------------------
-- Test incremental deletes. --
----------------------------------------------------------------
DELETE FROM PowerDemand_Tab WHERE Region=3;

-- This should return one less row than the preceding SELECT.
SELECT P.Region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_Equals(P.Sample,2,8) = 1  
     ORDER BY 1,2;

----------------------------------------------------------------
-- Test incremental updates. --
----------------------------------------------------------------
INSERT INTO PowerDemand_Tab VALUES(4,
   PowerDemand_Typ(NULL, NULL, NULL, PowerGrid_Typ(61,8,12,9,3),
   to_date('02-01-1998 01','MM-DD-YYYY HH'))
);

declare
CURSOR c1 IS SELECT Sample FROM PowerDemand_Tab WHERE Region=4
   FOR UPDATE;
s PowerDemand_Typ;
BEGIN
  OPEN c1;
  LOOP
     FETCH c1 INTO s;
     EXIT WHEN c1%NOTFOUND;
     s.SetTotalDemand;
     s.SetMaxDemand;
     s.SetMinDemand;
     dbms_output.put_line(s.TotGridDemand);
     dbms_output.put_line(s.MaxCellDemand);
     dbms_output.put_line(s.MinCellDemand);
     UPDATE PowerDemand_Tab SET Sample = s WHERE CURRENT OF c1;
  END LOOP;
  CLOSE c1;
END;
/

-- This SELECT should return one more row than the next SELECT.
SELECT P.Region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_Equals(P.Sample,2,8) = 1  
     ORDER BY 1,2;

-- Now change cell 2's value to 7 (was 8) for the row that was
-- just inserted (region 4, 1:00 AM on 02-Feb-1998).

UPDATE PowerDemand_Tab t
SET
 t.Sample = PowerDemand_Typ(NULL, NULL, NULL,
                            PowerGrid_Typ(54,7,12,9,3),
                            to_date('02-01-1998 01','MM-DD-YYYY HH'))
WHERE
 t.Region=4 and
 t.sample.sampletime = to_date('02-01-1998 01','MM-DD-YYYY HH');

declare
CURSOR c1 IS SELECT Sample FROM PowerDemand_Tab WHERE Region=4
   FOR UPDATE;
s PowerDemand_Typ;
BEGIN
  OPEN c1;
  LOOP
     FETCH c1 INTO s;
     EXIT WHEN c1%NOTFOUND;
     s.SetTotalDemand;
     s.SetMaxDemand;
     s.SetMinDemand;
     dbms_output.put_line(s.TotGridDemand);
     dbms_output.put_line(s.MaxCellDemand);
     dbms_output.put_line(s.MinCellDemand);
     UPDATE PowerDemand_Tab SET Sample = s WHERE CURRENT OF c1;
  END LOOP;
  CLOSE c1;
END;
/

-- This SELECT should return one less row than the preceding SELECT.
SELECT P.Region, P.Sample.TotGridDemand, P.Sample.MaxCellDemand,
       P.Sample.MinCellDemand
   FROM PowerDemand_Tab P
   WHERE Power_Equals(P.Sample,2,8) = 1  
     ORDER BY 1,2;

-- Delete the row that was added for region 4.
DELETE FROM PowerDemand_Tab WHERE Region=4;

-- Cleanup
CONNECT sys/knl_test7 AS sysdba;
DROP USER PowerCartUser CASCADE;

