Rem
Rem $Header: extdemo4.sql 08-feb-2001.18:14:55 ayoaz Exp $
Rem
Rem extdemo4.sql
Rem
Rem  Copyright (c) Oracle Corporation 1998, 2000. All Rights Reserved.
Rem
Rem    NAME
Rem      extdemo4.sql - user defined aggregate implemented using C functions.
Rem
Rem    DESCRIPTION
Rem      This file demonstrates the creation and use of a simple user
Rem      defined aggregate function called SumVector, which calculates
Rem      the sum of a set of vectors.
Rem
Rem      The following steps should be taken before running this script:
Rem
Rem      1. Create the shared library extdemo4.so:
Rem
Rem    make extproc_with_context SHARED_LIBNAME=extdemo4.so OBJS="extdemo4.o"
Rem
Rem      2. Change the CREATE LIBRARY command in this script to reflect
Rem         the path of the shared library created in step 1.
Rem         For example:
Rem
Rem    CREATE LIBRARY VectorLib is '/oracle_home/rdbms/demo/extdemo4.so';
Rem    /
Rem
Rem    MODIFIED   (MM/DD/YY)
Rem    ayoaz      02/08/01 - Merged ayoaz_udag_demo
Rem    ayoaz      02/06/01 - Created
Rem

SET FEEDBACK 1
SET ECHO ON

CONNECT system/manager
DROP USER extdemo4 CASCADE;
GRANT RESOURCE, CONNECT TO extdemo4 IDENTIFIED BY extdemo4;
GRANT CREATE TYPE TO extdemo4;
GRANT CREATE PROCEDURE TO extdemo4;
GRANT CREATE TABLE TO extdemo4;
GRANT CREATE LIBRARY TO extdemo4;

CONNECT extdemo4/extdemo4

CREATE LIBRARY VectorLib is '/ade/ayoaz_ade0/oracle/rdbms/demo/extdemo4.so';
/

-- Vector type

CREATE TYPE Vector_t AS OBJECT (
  length NUMBER,
  angle NUMBER,
  MAP MEMBER FUNCTION GetLength RETURN NUMBER
);
/

CREATE TYPE BODY Vector_t IS
  MAP MEMBER FUNCTION GetLength RETURN NUMBER IS
  BEGIN
    RETURN length;
  END;
END;
/

-- Create the context type

CREATE TYPE AggCtx_t AS OBJECT (
  x NUMBER,
  y NUMBER
);
/

-- Create the implementation type

CREATE TYPE Imp_t AS OBJECT
(
  key RAW(4),
  aggCtx aggCtx_t,

  STATIC FUNCTION ODCIAggregateInitialize(sctx IN OUT Imp_t) 
    RETURN PLS_INTEGER
    AS LANGUAGE C
    LIBRARY VectorLib
    NAME "Initialize"
    WITH CONTEXT
    PARAMETERS (
      CONTEXT,
      sctx,
      sctx INDICATOR STRUCT,
      RETURN INT
    ),

  MEMBER FUNCTION ODCIAggregateIterate(self IN OUT Imp_t, arg IN Vector_t) 
    RETURN PLS_INTEGER
    AS LANGUAGE C
    LIBRARY VectorLib
    NAME "Iterate"
    WITH CONTEXT
    PARAMETERS (
      CONTEXT,
      self,
      self INDICATOR STRUCT,
      arg,
      arg INDICATOR STRUCT,
      RETURN INT
    ),

  MEMBER FUNCTION ODCIAggregateTerminate(self IN Imp_t, result OUT Vector_t, 
    flags IN NUMBER) RETURN PLS_INTEGER
    AS LANGUAGE C
    LIBRARY VectorLib
    NAME "Terminate"
    WITH CONTEXT
    PARAMETERS (
      CONTEXT,
      self,
      self INDICATOR STRUCT,
      result,
      result INDICATOR STRUCT,
      flags,
      flags INDICATOR,
      RETURN INT
    ),

  MEMBER FUNCTION ODCIAggregateMerge(self IN OUT Imp_t, sctx2 IN Imp_t) 
    RETURN PLS_INTEGER
    AS LANGUAGE C
    LIBRARY VectorLib
    NAME "Merge"
    WITH CONTEXT
    PARAMETERS (
      CONTEXT,
      self,
      self INDICATOR STRUCT,
      sctx2,
      sctx2 INDICATOR STRUCT,
      RETURN INT
    ),

  member function ODCIAggregateDelete(self IN OUT Imp_t, arg IN Vector_t) 
    RETURN PLS_INTEGER
    AS LANGUAGE C
    LIBRARY VectorLib
    NAME "Delete"
    WITH CONTEXT
    PARAMETERS (
      CONTEXT,
      self,
      self INDICATOR STRUCT,
      arg,
      arg INDICATOR STRUCT,
      RETURN INT
    ),

  MEMBER FUNCTION ODCIAggregateWrapContext(self IN OUT Imp_t) 
    RETURN PLS_INTEGER
    AS LANGUAGE C
    LIBRARY VectorLib
    NAME "WrapContext"
    WITH CONTEXT
    PARAMETERS (
      CONTEXT,
      self,
      self INDICATOR STRUCT,
      RETURN INT
    )

);
/

-- Create user aggregate function

CREATE FUNCTION SumVector(arg Vector_t) RETURN Vector_t
PARALLEL_ENABLE AGGREGATE USING Imp_t;
/

-- Create sample data

CREATE TABLE t1 (c1 NUMBER, c2 Vector_t);
INSERT INTO T1 VALUES(1, Vector_t(5,0));
INSERT INTO T1 VALUES(1, Vector_t(3,0));
INSERT INTO T1 VALUES(2, Vector_t(1,2));
INSERT INTO T1 VALUES(2, Vector_t(2,1));
INSERT INTO T1 VALUES(2, Vector_t(3,0.5));
INSERT INTO T1 VALUES(2, Vector_t(3,2.5));

CREATE TABLE t2 PARALLEL 2 AS SELECT * FROM t1;

COMMIT;

-- Sample queries

-- serial cases
SELECT SumVector(c2) sum FROM t1;
SELECT SumVector(DISTINCT c2) sum FROM t1;
SELECT c1, SumVector(c2) sum FROM t1 GROUP BY c1 ORDER BY c1;
SELECT c1, SumVector(distinct c2) sum FROM t1 GROUP BY c1 ORDER BY c1;

-- parallel cases
SELECT SumVector(c2) sum FROM t2;
SELECT SumVector(DISTINCT c2) sum FROM t2;
SELECT c1, SumVector(c2) sum FROM t2 GROUP BY c1 ORDER BY c1;
SELECT c1, SumVector(distinct c2) sum FROM t2 GROUP BY c1 ORDER BY c1;

