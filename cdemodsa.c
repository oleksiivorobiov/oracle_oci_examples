#ifdef RCSID
static char *RCSid =
   "$Header: cdemodsa.c 10-oct-2006.14:39:59 azhao Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1997, 2006, Oracle. All rights reserved.  
*/

/*

   NAME
     cdemodsa.c - <one-line expansion of the name>

   DESCRIPTION
     <short description of component this file declares/defines>

   PUBLIC FUNCTION(S)
     <list of external functions declared/defined - with one-line descriptions>

   PRIVATE FUNCTION(S)
     <list of static functions defined in .c file - with one-line descriptions>

   RETURNS
     <function return values, for .c file with single function>

   NOTES
     <other useful comments, qualifications, etc.>

   MODIFIED   (MM/DD/YY)
   azhao       10/10/06 - case-senstive password change
   stsun       06/18/04 - bug3698329: check null str for strncpy 
   mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
   svedala     09/09/98 - lines longer than 79 chars reformatted - bug 722491
   svedala     05/22/98 - modify describe_typeattr
   azhao       06/03/97 - fix lint errors
   sgollapu    05/30/97 - Creation

*/

/*
 *      -- cdemodsa.c --
 *  An example program which describes the table EMPNML created by running
 *  the sql script CDEMODSA.SQL as SCOTT/TIGER. The table EMPNML consists of
 *  two columns, one of which is an ADT PERSON defined as containing a name
 *  (CHAR(20)), age (NUMBER), and address (an ADT).
 *
 *  When successfully executed, the program will print the user defined type
 *  PERSON and its attributes, and then the table EMPNML.  Note that the
 *  schema name and type name are printed for user-defined types only.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <oratypes.h>
#include <oci.h>

static text *username = (text *) "SCOTT";                /* username */
static text *password = (text *) "tiger";                /* password */
static text *tablename = (text *) "EMPNML";              /* tablename */

static OCIEnv       *envhp = (OCIEnv *)0;
static OCIServer    *srvhp = (OCIServer *)0;
static OCIError     *errhp = (OCIError *)0;
static OCISvcCtx    *svchp = (OCISvcCtx *)0;
static OCIStmt      *stmhp = (OCIStmt *)0;
static OCIDescribe  *dschp = (OCIDescribe *)0;
static OCISession   *authp = (OCISession *)0;

static void   initialize(/*_ void _*/);
static void   logon(/*_ void _*/);
static void   describe_table(/*_ void _*/);
static void   describe_column(/*_ OCIParam *parmp, ub4 parmcnt _*/);
static void   describe_type(/*_ OCIParam *parmp _*/);
static void   describe_typeattr(/*_ OCIParam *parmp, ub4 num_attr _*/);
static void   describe_typecoll(/*_ OCIParam *parmp, sword typecode _*/);
static void   describe_typemethodlist(/*_  OCIParam *parmp, ub4 num_meth,
                              text *comment _*/);
static void   describe_typemethod(/*_ OCIParam *parmp, text *comment _*/);
static void   describe_typearg(/*_ OCIParam *parmp, ub1 type, ub4 start,
                                                                ub4 end _*/);
static void   logout(/*_ void _*/);
static void   cleanup(/*_ void _*/);
static void   checkerr(/*_ OCIError *errhp, sword status _*/);
int main(/*_ int argc, char *argv[] _*/);

static sb4 status;

#define NPOS    30                                 /* max number of columns */
#define MAXLEN 128                            /* max length of column names */


int main(argc, argv)
int argc;
char *argv[];
{
  initialize ();
  logon ();

  checkerr(errhp, OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &stmhp,
                                  OCI_HTYPE_STMT, (size_t) 0, (dvoid **) 0));
  describe_table ();
  logout ();
  cleanup ();
}

static void initialize ()
{
  printf ("\nInitializing the environment..\n");
  (void) OCIInitialize (OCI_OBJECT, (dvoid *)0,
                       (dvoid * (*)(dvoid *, size_t)) 0,
                       (dvoid * (*)(dvoid *, dvoid *, size_t))0,
                       (void (*)(dvoid *, dvoid *)) 0 );

  (void) OCIEnvInit ((OCIEnv **) &envhp, OCI_DEFAULT, (size_t) 0,
                                                               (dvoid **) 0);

                                                            /* error handle */
  (void) OCIHandleAlloc ((dvoid *) envhp, (dvoid **) &errhp, OCI_HTYPE_ERROR,
                         (size_t) 0, (dvoid **) 0);

                                                         /* server contexts */
  (void) OCIHandleAlloc ((dvoid *) envhp, (dvoid **) &srvhp, OCI_HTYPE_SERVER,
                         (size_t) 0, (dvoid **) 0);

  (void) OCIHandleAlloc ((dvoid *) envhp, (dvoid **) &svchp, OCI_HTYPE_SVCCTX,
                         (size_t) 0, (dvoid **) 0);

  (void) OCIServerAttach (srvhp, errhp, (text *)"",
                          strlen(""), 0);

                     /* set attribute server context in the service context */
  (void) OCIAttrSet ((dvoid *) svchp, OCI_HTYPE_SVCCTX, (dvoid *)srvhp,
                     (ub4) 0, OCI_ATTR_SERVER, (OCIError *) errhp);
}

static void logon ()
{
  printf ("Logging on as %s/%s..", username, password);
  (void) OCIHandleAlloc ((dvoid *) envhp, (dvoid **)&authp,
                         (ub4) OCI_HTYPE_SESSION, (size_t) 0, (dvoid **) 0);

  (void) OCIAttrSet ((dvoid *)authp, (ub4)OCI_HTYPE_SESSION,
                     (dvoid *)username, (ub4)strlen((char *)username),
                     OCI_ATTR_USERNAME, errhp);

  (void) OCIAttrSet ((dvoid *)authp, (ub4)OCI_HTYPE_SESSION,
                     (dvoid *)password, (ub4)strlen((char *)password),
                     OCI_ATTR_PASSWORD, errhp);

  checkerr (errhp, OCISessionBegin (svchp,  errhp, authp, OCI_CRED_RDBMS,
                                    (ub4) OCI_DEFAULT));
  printf ("Logged on\n");

  (void) OCIAttrSet ((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX,
                     (dvoid *) authp, (ub4) 0,
                     (ub4) OCI_ATTR_SESSION, errhp);
}


static void describe_table ()
{
  sword     retval;
  OCIParam *parmp, *collst;
  ub4       parmcnt;
  ub2       numcols;
  ub4       objid = 0;

  checkerr (errhp, OCIHandleAlloc((dvoid *) envhp, (dvoid **) &dschp,
                           (ub4) OCI_HTYPE_DESCRIBE,
                           (size_t) 0, (dvoid **) 0));

  if ((retval = OCIDescribeAny(svchp, errhp, (dvoid *)tablename,
                               (ub4) strlen((char *) tablename),
                               OCI_OTYPE_NAME, (ub1)1,
                               OCI_PTYPE_TABLE, dschp)) != OCI_SUCCESS)
  {
    if (retval == OCI_NO_DATA)
    {
      printf("NO DATA: OCIDescribeAny on %s\n", tablename);
    }
    else                                                      /* OCI_ERROR */
    {
      printf( "ERROR: OCIDescribeAny on %s\n", tablename);
      checkerr(errhp, retval);
      return;
    }
  }
  else
  {
                                           /* get the parameter descriptor */
    checkerr (errhp, OCIAttrGet((dvoid *)dschp, (ub4)OCI_HTYPE_DESCRIBE,
                         (dvoid *)&parmp, (ub4 *)0, (ub4)OCI_ATTR_PARAM,
                         (OCIError *)errhp));

                                        /* Get the attributes of the table */
    checkerr (errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                         (dvoid*) &objid, (ub4 *) 0,
                         (ub4) OCI_ATTR_OBJID, (OCIError *)errhp));
                                               /* column list of the table */
    checkerr (errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                         (dvoid*) &collst, (ub4 *) 0,
                         (ub4) OCI_ATTR_LIST_COLUMNS, (OCIError *)errhp));
                                                      /* number of columns */
    checkerr (errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                         (dvoid*) &numcols, (ub4 *) 0,
                         (ub4) OCI_ATTR_NUM_COLS, (OCIError *)errhp));
                                               /* now describe each column */
    describe_column(collst, numcols);
  }
                                               /* free the describe handle */
  OCIHandleFree((dvoid *) dschp, (ub4) OCI_HTYPE_DESCRIBE);
}

static void describe_column(parmp, parmcnt)
OCIParam *parmp;
ub4 parmcnt;
{
  text      colname1[NPOS][30], colname2[NPOS][30], colname3[NPOS][30];
  text     *namep;
  ub4       sizep;
  ub2       collen[NPOS];
  ub2       coltyp[NPOS];
  OCIParam *parmdp;
  ub4       i, pos;
  sword     retval;
  ub1       precision[NPOS];
  sb1       scale[NPOS];

  for (pos = 1; pos <= parmcnt; pos++)
  {
                            /* get the parameter descriptor for each column */
    checkerr (errhp, OCIParamGet((dvoid *)parmp, (ub4)OCI_DTYPE_PARAM, errhp,
                       (dvoid *)&parmdp, (ub4) pos));
                                                           /* column length */
    checkerr (errhp, OCIAttrGet((dvoid*) parmdp, (ub4) OCI_DTYPE_PARAM,
                      (dvoid*) &collen[pos-1], (ub4 *) 0,
                      (ub4) OCI_ATTR_DATA_SIZE, (OCIError *)errhp));
                                                             /* column name */
    checkerr (errhp, OCIAttrGet((dvoid*) parmdp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &namep, (ub4 *) &sizep,
                    (ub4) OCI_ATTR_NAME, (OCIError *)errhp));

    if(sizep){
      strncpy((char *)colname1[pos-1], (char *)namep, (size_t) sizep);
    }
    colname1[pos-1][sizep] = '\0';
                                                            /* schema name */
    checkerr (errhp, OCIAttrGet((dvoid*) parmdp, (ub4) OCI_DTYPE_PARAM,
                         (dvoid*) &namep, (ub4 *) &sizep,
                         (ub4) OCI_ATTR_SCHEMA_NAME, (OCIError *)errhp));

    if(sizep){
      strncpy((char *)colname2[pos-1], (char *)namep, (size_t) sizep);
    }    
    colname2[pos-1][sizep] = '\0';
                                                              /* type name */
    checkerr (errhp, OCIAttrGet((dvoid*) parmdp, (ub4) OCI_DTYPE_PARAM,
                         (dvoid*) &namep, (ub4 *) &sizep,
                         (ub4) OCI_ATTR_TYPE_NAME, (OCIError *)errhp));

    if(sizep){
      strncpy((char *)colname3[pos-1], (char *)namep, (size_t) sizep);
    }    
    colname3[pos-1][sizep] = '\0';
                                                              /* data type */
    checkerr (errhp, OCIAttrGet((dvoid*) parmdp, (ub4) OCI_DTYPE_PARAM,
                         (dvoid*) &coltyp[pos-1], (ub4 *) 0,
                         (ub4) OCI_ATTR_DATA_TYPE, (OCIError *)errhp));
                                                              /* precision */
    checkerr (errhp, OCIAttrGet ((dvoid*) parmdp, (ub4) OCI_DTYPE_PARAM,
                          (dvoid*) &precision[pos-1], (ub4 *) 0,
                          (ub4) OCI_ATTR_PRECISION, (OCIError *)errhp));
                                                                  /* scale */
    checkerr (errhp, OCIAttrGet ((dvoid*) parmdp, (ub4) OCI_DTYPE_PARAM,
                        (dvoid*) &scale[pos-1], (ub4 *) 0,
                        (ub4) OCI_ATTR_SCALE, (OCIError *)errhp));

    /* if column or attribute is type OBJECT/COLLECTION, describe it by ref */
    if (coltyp[pos-1] == OCI_TYPECODE_OBJECT ||
        coltyp[pos-1] == OCI_TYPECODE_NAMEDCOLLECTION)
    {
      OCIDescribe *deshp;
      OCIParam    *parmhp;
      OCIRef      *typeref;

                                        /* get ref to attribute/column type */
      checkerr (errhp, OCIAttrGet ((dvoid *)parmdp, (ub4)OCI_DTYPE_PARAM,
                            (dvoid *)&typeref, (ub4 *)0,
                            (ub4)OCI_ATTR_REF_TDO, (OCIError *)errhp));
                                                             /* describe it */
      checkerr (errhp, OCIHandleAlloc((dvoid *)envhp, (dvoid **)&deshp,
                            (ub4)OCI_HTYPE_DESCRIBE, (size_t)0, (dvoid **)0));

      checkerr (errhp, OCIDescribeAny(svchp, errhp, (dvoid *)typeref, (ub4)0,
                               OCI_OTYPE_REF, (ub1)1, OCI_PTYPE_TYPE, deshp));
                                            /* get the parameter descriptor */
      checkerr (errhp, OCIAttrGet((dvoid *)deshp, (ub4)OCI_HTYPE_DESCRIBE,
                           (dvoid *)&parmhp, (ub4 *)0, (ub4)OCI_ATTR_PARAM,
                           (OCIError *)errhp));
                                                       /* describe the type */
      describe_type (parmhp);

                                                    /* free describe handle */
      OCIHandleFree((dvoid *) deshp, (ub4) OCI_HTYPE_DESCRIBE);
    }
  }

  printf ("\n------------------\n");
  printf ("TABLE : %s \n", tablename);
  printf ("------------------\n");
  printf (
    "\nColumn Name    Schema  Length   Type    Datatype  Precision   Scale\n");
  printf (
    "_____________________________________________________________________\n");
  for (i = 1; i <= parmcnt; i++)
    printf( "%10s%10s%6d%10s%10d%10d%10d\n", colname1[i-1], colname2[i-1],
          collen[i-1], colname3[i-1], coltyp[i-1], precision[i-1], scale[i-1]);
}


static void describe_type(type_parmp)
OCIParam    *type_parmp;
{
  sword         retval;
  OCITypeCode   typecode,
                collection_typecode;
  text          schema_name[MAXLEN],
                version_name[MAXLEN],
                type_name[MAXLEN];
  text         *namep;
  ub4           size;                                           /* not used */
  OCIRef       *type_ref;                                       /* not used */
  ub2           num_attr,
                num_method;
  ub1           is_incomplete,
                has_table;
  OCIParam     *list_attr,
               *list_method,
               *map_method,
               *order_method,
               *collection_elem;

  printf ("\n\n-----------------\n");
  printf ("USED-DEFINED TYPE\n-----------------\n");

  checkerr (errhp, OCIAttrGet((dvoid*) type_parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &namep, (ub4 *) &size,
                    (ub4) OCI_ATTR_SCHEMA_NAME, (OCIError *) errhp));
  if(size){
    strncpy((char *)schema_name, (char *)namep, (size_t) size);
  }
  schema_name[size] = '\0';
  printf ( "Schema:            %s\n", schema_name);

  checkerr (errhp, OCIAttrGet((dvoid*) type_parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &namep, (ub4 *) &size,
                    (ub4) OCI_ATTR_NAME, (OCIError *) errhp));
  if(size){
    strncpy ((char *)type_name, (char *)namep, (size_t) size);
  }
  type_name[size] = '\0';
  printf ( "Name:              %s\n", type_name);

                      /* get ref of type, although we are not using it here */
  checkerr (errhp, OCIAttrGet((dvoid *)type_parmp, (ub4)OCI_DTYPE_PARAM,
                    (dvoid *)&type_ref, (ub4 *)0,
                    (ub4)OCI_ATTR_REF_TDO, (OCIError *)errhp));

  checkerr (errhp, OCIAttrGet((dvoid*) type_parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &typecode, (ub4 *) 0,
                    (ub4) OCI_ATTR_TYPECODE, (OCIError *) errhp));
  printf ( "Oracle Typecode:   %d\n", typecode);

  checkerr (errhp, OCIAttrGet((dvoid*) type_parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &namep, (ub4 *) &size,
                    (ub4) OCI_ATTR_VERSION, (OCIError *) errhp));
  if(size){
    strncpy ((char *)version_name, (char *)namep, (size_t) size);
  }
  version_name[size] = '\0';
  printf ( "Version:           %s\n", version_name);

  checkerr (errhp, OCIAttrGet((dvoid*) type_parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &is_incomplete, (ub4 *) 0,
                    (ub4) OCI_ATTR_IS_INCOMPLETE_TYPE, (OCIError *)errhp));
  printf ( "Is incomplete:     %d\n", is_incomplete);

  checkerr (errhp, OCIAttrGet((dvoid*) type_parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &has_table, (ub4 *) 0,
                    (ub4) OCI_ATTR_HAS_NESTED_TABLE, (OCIError *)errhp));
  printf ( "Has nested table:  %d\n", has_table);

                                         /* describe type attributes if any */
  checkerr (errhp, OCIAttrGet((dvoid*) type_parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &num_attr, (ub4 *) 0,
                    (ub4) OCI_ATTR_NUM_TYPE_ATTRS, (OCIError *) errhp));
  printf ( "Number of attrs:   %d\n", num_attr);
  if (num_attr > 0)
  {
                               /* get the list of attributes and pass it on */
    checkerr (errhp, OCIAttrGet((dvoid *)type_parmp, (ub4)OCI_DTYPE_PARAM,
                    (dvoid *)&list_attr, (ub4 *)0,
                    (ub4)OCI_ATTR_LIST_TYPE_ATTRS, (OCIError *)errhp));
    describe_typeattr(list_attr, num_attr);
  }

            /* describe the collection element if this is a collection type */
  if (typecode == OCI_TYPECODE_NAMEDCOLLECTION)
  {
    checkerr (errhp, OCIAttrGet((dvoid *)type_parmp, (ub4)OCI_DTYPE_PARAM,
                      (dvoid *)&collection_typecode, (ub4 *)0,
                      (ub4)OCI_ATTR_COLLECTION_TYPECODE, (OCIError *)errhp));
    printf ( "Collection typecode: %d\n", collection_typecode);

    checkerr (errhp, OCIAttrGet((dvoid *)type_parmp, (ub4)OCI_DTYPE_PARAM,
                      (dvoid *)&collection_elem, (ub4 *)0,
                      (ub4)OCI_ATTR_COLLECTION_ELEMENT, (OCIError *)errhp));

    describe_typecoll(collection_elem, collection_typecode);
  }

                                          /* describe the MAP method if any */
  checkerr (errhp, OCIAttrGet((dvoid*) type_parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &map_method, (ub4 *) 0,
                    (ub4) OCI_ATTR_MAP_METHOD, (OCIError *)errhp));
  if (map_method != (dvoid *)0)
    describe_typemethod(map_method,(text *)"TYPE MAP METHOD\n---------------");

  /* describe the ORDER method if any; note that this is mutually exclusive */
  /* with MAP                                                               */
  checkerr (errhp, OCIAttrGet((dvoid *)type_parmp, (ub4)OCI_DTYPE_PARAM,
                    (dvoid *)&order_method, (ub4 *)0,
                    (ub4)OCI_ATTR_ORDER_METHOD, (OCIError *)errhp));
  if (order_method != (dvoid *)0)
    describe_typemethod(order_method,
                        (text *)"TYPE ORDER METHOD\n-----------------");

                              /* describe all methods (including MAP/ORDER) */
  checkerr (errhp, OCIAttrGet((dvoid *)type_parmp, (ub4)OCI_DTYPE_PARAM,
                    (dvoid *)&num_method, (ub4 *)0,
                    (ub4)OCI_ATTR_NUM_TYPE_METHODS, (OCIError *)errhp));
  printf("Number of methods: %d\n", num_method);
  if (num_method > 0)
  {
    checkerr (errhp, OCIAttrGet((dvoid *)type_parmp, (ub4)OCI_DTYPE_PARAM,
                      (dvoid *)&list_method, (ub4 *)0,
                      (ub4)OCI_ATTR_LIST_TYPE_METHODS, (OCIError *)errhp));

    describe_typemethodlist(list_method, num_method,
                           (text *)"TYPE METHOD\n-----------");
  }
}

static void   describe_typeattr(attrlist_parmp, num_attr)
OCIParam      *attrlist_parmp;
ub4            num_attr;
{
  OCIParam     *attr_parmp;
  sword         retval;
  text         *attr_name,
               *schema_name,
               *type_name;
  ub4           namesize, snamesize, tnamesize;
  ub4           size;
  ub2           datasize;
  OCITypeCode   typecode;
  ub2           datatype;
  ub1           precision;
  sb1           scale;
  ub4           i,
                pos;

  printf(
     "\nAttr Name      Schema      Type        Length Typ Datatyp Pre Scal\n");
  printf(
     "____________________________________________________________________\n");

  for (pos = 1; pos <= num_attr; pos++)
  {
                  /* get the attribute's describe handle from the parameter */
    checkerr (errhp, OCIParamGet((dvoid *)attrlist_parmp, (ub4)OCI_DTYPE_PARAM,
                       errhp, (dvoid *)&attr_parmp, (ub4)pos));

                       /* obtain attribute values for the type's attributes */
    checkerr (errhp, OCIAttrGet((dvoid *)attr_parmp, (ub4)OCI_DTYPE_PARAM,
                      (dvoid *)&attr_name, (ub4 *)&namesize,
                      (ub4)OCI_ATTR_NAME, (OCIError *)errhp));
    checkerr (errhp, OCIAttrGet((dvoid *)attr_parmp, (ub4)OCI_DTYPE_PARAM,
                      (dvoid *)&schema_name, (ub4 *)&snamesize,
                      (ub4)OCI_ATTR_SCHEMA_NAME, (OCIError *)errhp));
    checkerr (errhp, OCIAttrGet((dvoid *)attr_parmp, (ub4)OCI_DTYPE_PARAM,
                      (dvoid *)&type_name, (ub4 *)&tnamesize,
                      (ub4)OCI_ATTR_TYPE_NAME, (OCIError *)errhp));
    checkerr (errhp, OCIAttrGet((dvoid *)attr_parmp, (ub4)OCI_DTYPE_PARAM,
                      (dvoid *)&datasize, (ub4 *)0,
                      (ub4)OCI_ATTR_DATA_SIZE, (OCIError *)errhp));
    checkerr (errhp, OCIAttrGet((dvoid *)attr_parmp, (ub4)OCI_DTYPE_PARAM,
                      (dvoid *)&typecode, (ub4 *)0,
                      (ub4)OCI_ATTR_TYPECODE, (OCIError *)errhp));
    checkerr (errhp, OCIAttrGet((dvoid *)attr_parmp, (ub4)OCI_DTYPE_PARAM,
                      (dvoid *)&datatype, (ub4 *)0,
                      (ub4)OCI_ATTR_DATA_TYPE, (OCIError *)errhp));
    checkerr (errhp, OCIAttrGet((dvoid *)attr_parmp, (ub4)OCI_DTYPE_PARAM,
                      (dvoid *)&precision, (ub4 *)0,
                      (ub4)OCI_ATTR_PRECISION, (OCIError *)errhp));
    checkerr (errhp, OCIAttrGet((dvoid *)attr_parmp, (ub4)OCI_DTYPE_PARAM,
                      (dvoid *)&scale, (ub4 *)0,
                      (ub4)OCI_ATTR_SCALE, (OCIError *)errhp));

    /* if typecode == OCI_TYPECODE_OBJECT, you can proceed to describe it
       recursively by calling describe_type() with its name; or you can
       obtain its OCIRef by using OCI_ATTR_REF_TDO, and then describing the
       type by REF                                                          */

                                          /* print values for the attribute */
    printf("%10.*s%10.*s%16.*s%8d%4d%8d%4d%5d\n", namesize, attr_name,
                snamesize, schema_name, tnamesize, type_name, datasize,
                typecode, datatype, precision, scale);
  }
  printf("\n");
}


static void  describe_typecoll(collelem_parmp, coll_typecode)
OCIParam  *collelem_parmp;
sword      coll_typecode;      /* OCI_TYPECODE_VARRAY or OCI_TYPECODE_TABLE */
{
  text         *attr_name,
               *schema_name,
               *type_name;
  ub4           size;
  ub2           datasize;
  ub4           num_elems;
  OCITypeCode   typecode;
  ub2           datatype;
  sword         retval;

  checkerr (errhp, OCIAttrGet((dvoid *)collelem_parmp, (ub4)OCI_DTYPE_PARAM,
                    (dvoid *)&schema_name, (ub4 *)&size,
                    (ub4)OCI_ATTR_SCHEMA_NAME, (OCIError *)errhp));
  checkerr (errhp, OCIAttrGet((dvoid *)collelem_parmp, (ub4)OCI_DTYPE_PARAM,
                    (dvoid *)&type_name, (ub4 *)&size,
                    (ub4)OCI_ATTR_TYPE_NAME, (OCIError *)errhp));
  checkerr (errhp, OCIAttrGet((dvoid *)collelem_parmp, (ub4)OCI_DTYPE_PARAM,
                    (dvoid *)&datasize, (ub4 *)0,
                    (ub4)OCI_ATTR_DATA_SIZE, (OCIError *)errhp));
  checkerr (errhp, OCIAttrGet((dvoid *)collelem_parmp, (ub4)OCI_DTYPE_PARAM,
                    (dvoid *)&typecode, (ub4 *)0,
                    (ub4)OCI_ATTR_TYPECODE, (OCIError *)errhp));
  checkerr (errhp, OCIAttrGet((dvoid *)collelem_parmp, (ub4)OCI_DTYPE_PARAM,
                    (dvoid *)&datatype, (ub4 *)0,
                    (ub4)OCI_ATTR_DATA_TYPE, (OCIError *)errhp));
  if (coll_typecode == OCI_TYPECODE_VARRAY)
    checkerr (errhp, OCIAttrGet((dvoid *)collelem_parmp, (ub4)OCI_DTYPE_PARAM,
                      (dvoid *)&num_elems, (ub4 *)0,
                      (ub4)OCI_ATTR_NUM_ELEMS, (OCIError *)errhp));
  else num_elems = 0;

  printf("Schema    Type            Length   Type Datatype Elements\n");
  printf("_________________________________________________________\n");

  printf("%10s%16s%9d%5d%9d%8d\n", schema_name, type_name,
         datasize, typecode, datatype, num_elems);
}

static void   describe_typemethodlist(methodlist_parmp, num_method, comment)
OCIParam      *methodlist_parmp;
ub4            num_method;
text          *comment;
{
  sword      retval;
  OCIParam  *method_parmp;
  ub4        i, pos;
                                                /* traverse the method list */
  for (pos = 1; pos <= num_method; pos++)
  {
    checkerr (errhp, OCIParamGet((dvoid *)methodlist_parmp,
                                 (ub4)OCI_DTYPE_PARAM, errhp,
                                 (dvoid *)&method_parmp, (ub4)pos));
    describe_typemethod(method_parmp, comment);
  }
}

static void   describe_typemethod(method_parmp, comment)
OCIParam      *method_parmp;
text          *comment;
{
  sword          retval;
  text          *method_name;
  ub4            size;
  ub2            ovrid;
  ub4            num_arg;
  ub1            has_result,
                 is_map,
                 is_order;
  OCITypeEncap   encap;
  OCIParam      *list_arg;

                                                            /* print header */
  printf("\n%s\n", comment);

  checkerr (errhp, OCIAttrGet((dvoid *)method_parmp, (ub4)OCI_DTYPE_PARAM,
                    (dvoid *)&method_name, (ub4 *)&size,
                    (ub4)OCI_ATTR_NAME, (OCIError *)errhp));
  printf("Method Name:       %s\n", method_name);

  checkerr (errhp, OCIAttrGet((dvoid *)method_parmp, (ub4)OCI_DTYPE_PARAM,
                    (dvoid *)&ovrid, (ub4 *)0,
                    (ub4)OCI_ATTR_OVERLOAD_ID, (OCIError *)errhp));
  printf("Overload ID:       %d\n", ovrid);

  checkerr (errhp, OCIAttrGet((dvoid *)method_parmp, (ub4)OCI_DTYPE_PARAM,
                    (dvoid *)&encap, (ub4 *)0,
                    (ub4)OCI_ATTR_ENCAPSULATION, (OCIError *)errhp));
  printf("Encapsulation:     %s\n",
         (encap == OCI_TYPEENCAP_PUBLIC) ? "public" : "private");

  checkerr (errhp, OCIAttrGet((dvoid *)method_parmp, (ub4)OCI_DTYPE_PARAM,
                    (dvoid *)&is_map, (ub4 *)0,
                    (ub4)OCI_ATTR_IS_MAP, (OCIError *)errhp));
  printf("Is map:            %d\n", is_map);

  checkerr (errhp, OCIAttrGet((dvoid *)method_parmp, (ub4)OCI_DTYPE_PARAM,
                    (dvoid *)&is_order, (ub4 *)0,
                    (ub4)OCI_ATTR_IS_ORDER, (OCIError *)errhp));
  printf("Is order:          %d\n", is_order);

                             /* retrieve the argument list, includes result */
  checkerr (errhp, OCIAttrGet((dvoid *)method_parmp, (ub4)OCI_DTYPE_PARAM,
                    (dvoid *)&list_arg, (ub4 *)0,
                    (ub4)OCI_ATTR_LIST_ARGUMENTS, (OCIError *)errhp));

               /* if this is a function (has results, then describe results */
  checkerr (errhp, OCIAttrGet((dvoid *)method_parmp, (ub4)OCI_DTYPE_PARAM,
                    (dvoid *)&has_result, (ub4 *)0,
                    (ub4)OCI_ATTR_HAS_RESULT, (OCIError *)errhp));
  printf("Has result:        %d\n", has_result);
  if (has_result)
  {
    describe_typearg(list_arg, OCI_PTYPE_TYPE_RESULT, 0, 1);
  }

                                                  /* describe each argument */
  checkerr (errhp, OCIAttrGet((dvoid *)method_parmp, (ub4)OCI_DTYPE_PARAM,
                    (dvoid *)&num_arg, (ub4 *)0,
                    (ub4)OCI_ATTR_NUM_ARGS, (OCIError *)errhp));
  printf("Number of args:    %d\n", num_arg);
  if (num_arg > 0)
  {
    describe_typearg(list_arg, OCI_PTYPE_TYPE_ARG, 1, num_arg+1);
  }
}

static void   describe_typearg (arglist_parmp, type, start, end)
OCIParam      *arglist_parmp;
ub1            type;
ub4            start;
ub4            end;
{
  OCIParam          *arg_parmp;
  sword              retval;
  text              *arg_name,
                    *schema_name,
                    *type_name;
  ub2                position;
  ub2                level;
  ub1                has_default;
  OCITypeParamMode   iomode;
  ub4                size;
  OCITypeCode        typecode;
  ub2                datatype;
  ub4                i,
                     pos;

  /* print header */
  printf("Name    Pos   Type Datatype Lvl Def Iomode SchName TypeName\n");
  printf(
      "________________________________________________________________\n");

  for (pos = start; pos < end; pos++)
  {
                  /* get the attribute's describe handle from the parameter */
    checkerr (errhp, OCIParamGet((dvoid *)arglist_parmp, (ub4)OCI_DTYPE_PARAM,
                       errhp, (dvoid *)&arg_parmp, (ub4)pos));

                       /* obtain attribute values for the type's attributes */
                  /* if this is a result, it has no name, so we give it one */
    if (type == OCI_PTYPE_TYPE_RESULT)
    {
      arg_name = (text *)"RESULT";
    }
    else if (type == OCI_PTYPE_TYPE_ARG)
    {
      checkerr (errhp, OCIAttrGet((dvoid *)arg_parmp, (ub4)OCI_DTYPE_PARAM,
                        (dvoid *)&arg_name, (ub4 *)&size,
                        (ub4)OCI_ATTR_NAME, (OCIError *)errhp));
    }
    checkerr (errhp, OCIAttrGet((dvoid *)arg_parmp, (ub4)OCI_DTYPE_PARAM,
                      (dvoid *)&position, (ub4 *)0,
                      (ub4)OCI_ATTR_POSITION, (OCIError *)errhp));
    checkerr (errhp, OCIAttrGet((dvoid *)arg_parmp, (ub4)OCI_DTYPE_PARAM,
                      (dvoid *)&typecode, (ub4 *)0,
                      (ub4)OCI_ATTR_TYPECODE, (OCIError *)errhp));
    checkerr (errhp, OCIAttrGet((dvoid *)arg_parmp, (ub4)OCI_DTYPE_PARAM,
                      (dvoid *)&datatype, (ub4 *)0,
                      (ub4)OCI_ATTR_DATA_TYPE, (OCIError *)errhp));
    checkerr (errhp, OCIAttrGet((dvoid *)arg_parmp, (ub4)OCI_DTYPE_PARAM,
                      (dvoid *)&level, (ub4 *)0,
                      (ub4)OCI_ATTR_LEVEL, (OCIError *)errhp));
    checkerr (errhp, OCIAttrGet((dvoid *)arg_parmp, (ub4)OCI_DTYPE_PARAM,
                      (dvoid *)&has_default, (ub4 *)0,
                      (ub4)OCI_ATTR_HAS_DEFAULT, (OCIError *)errhp));
    checkerr (errhp, OCIAttrGet((dvoid *)arg_parmp, (ub4)OCI_DTYPE_PARAM,
                      (dvoid *)&iomode, (ub4 *)0,
                      (ub4)OCI_ATTR_IOMODE, (OCIError *)errhp));
    checkerr (errhp, OCIAttrGet((dvoid *)arg_parmp, (ub4)OCI_DTYPE_PARAM,
                      (dvoid *)&schema_name, (ub4 *)&size,
                      (ub4)OCI_ATTR_SCHEMA_NAME, (OCIError *)errhp));
    checkerr (errhp, OCIAttrGet((dvoid *)arg_parmp, (ub4)OCI_DTYPE_PARAM,
                      (dvoid *)&type_name, (ub4 *)&size,
                      (ub4)OCI_ATTR_TYPE_NAME, (OCIError *)errhp));

    /* if typecode == OCI_TYPECODE_OBJECT, you can proceed to describe it
       recursively by calling describe_type() with its name; or you can
       obtain its OCIRef by using OCI_ATTR_REF_TDO, and then describing the
       type by REF                                                          */

                                           /* print values for the argument */
    printf ("%8s%5d%5d%9d%4d%3c%7d%8s%14s\n", arg_name, position,
                   typecode, datatype, level, has_default ? 'y' : 'n',
                   iomode, schema_name, type_name);
  }
}

static void logout()
{
  printf ("\n\nFreeing statement handle..\n");
  OCIHandleFree ((dvoid *) stmhp, (ub4) OCI_HTYPE_STMT);

  printf ("Logging off...\n");
  OCISessionEnd (svchp, errhp, authp, (ub4) 0);
}

static void cleanup()
{
  printf ("\nFreeing global structures..\n");
  if (errhp) OCIServerDetach (srvhp, errhp, (ub4) OCI_DEFAULT );
  if (srvhp) OCIHandleFree((dvoid *) srvhp, (CONST ub4) OCI_HTYPE_SERVER);
  if (svchp) OCIHandleFree((dvoid *) svchp, (CONST ub4) OCI_HTYPE_SVCCTX);
  if (errhp) OCIHandleFree((dvoid *) errhp, (CONST ub4) OCI_HTYPE_ERROR);
  if (authp) OCIHandleFree((dvoid *) authp, (CONST ub4) OCI_HTYPE_SESSION);
}


static void checkerr(errhp, status)
OCIError *errhp;
sword status;
{
  text errbuf[512];
  sb4 errcode = 0;

  switch (status)
  {
  case OCI_SUCCESS:
    break;
  case OCI_SUCCESS_WITH_INFO:
    (void) printf("Error - OCI_SUCCESS_WITH_INFO\n");
    break;
  case OCI_NEED_DATA:
    (void) printf("Error - OCI_NEED_DATA\n");
    break;
  case OCI_NO_DATA:
    (void) printf("Error - OCI_NODATA\n");
    break;
  case OCI_ERROR:
    (void) OCIErrorGet ((dvoid *)errhp, (ub4) 1, (text *) NULL, &errcode,
                    errbuf, (ub4) sizeof(errbuf), OCI_HTYPE_ERROR);
    (void) printf("Error - %.*s\n", 512, errbuf);
    break;
  case OCI_INVALID_HANDLE:
    (void) printf("Error - OCI_INVALID_HANDLE\n");
    break;
  case OCI_STILL_EXECUTING:
    (void) printf("Error - OCI_STILL_EXECUTE\n");
    break;
  case OCI_CONTINUE:
    (void) printf("Error - OCI_CONTINUE\n");
    break;
  default:
    break;
  }
}

/* end of file cdemodsa.c */

