/* Copyright (c) 2001, 2002, Oracle Corporation.  All rights reserved.  */

/*

   NAME
     cdemoAnyData2.c - OCI demo program for ANYDATA.

   DESCRIPTION
     An example program which creates an TYPE piecewise using OCITypeBeginCreate()
     and then describe the new type created. Three types are created and described.
     An OCIANYDATA is constructed piecewise using OCIAnyDataBeginCreate() and 
     accessed piecewise.

   EXPORT FUNCTION(S)
     <external functions defined for use outside package - one-line descriptions>

   INTERNAL FUNCTION(S)
     <other external functions defined - one-line descriptions>

   STATIC FUNCTION(S)
     <static functions defined - one-line descriptions>

   NOTES
     Dependent Files:
       cdemoAnyData2.sql  - SQL script to be run before execution.

   MODIFIED   (MM/DD/YY)
   jchai       06/13/02 - fix bug 2360431
   ani         09/17/01 - Change access interface for DATE,NUMBER
   jchai       05/04/01 - Merged jchai_add_oci_demos_to_shiphome
   ani         04/30/01 - Merged ani_ocidemo
   ani         04/24/01 - Creation

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef OCI_ORACLE
# include<oci.h>
#endif

/*---------------------------------------------------------------------------
                     PRIVATE TYPES AND CONSTANTS
  ---------------------------------------------------------------------------*/
#define MAXNAME       30

typedef struct cdemoctx
{
  OCIEnv *envhp;
  OCIError *errhp;
  OCISvcCtx *svchp;
  OCIServer *svrhp;
  OCISession *usrhp;
} cdemoctx;

/* boolean IS_CHARTYPE(OCITypeCode tc) */
#define IS_CHARTYPE(tc) ((tc == OCI_TYPECODE_CHAR) || \
(tc == OCI_TYPECODE_VARCHAR) || (tc == OCI_TYPECODE_VARCHAR2) || \
(tc == OCI_TYPECODE_CLOB) || (tc == OCI_TYPECODE_CFILE))


static text *username = (text *) "scott";
static text *password = (text *) "tiger";


/*---------------------------------------------------------------------------
                     STATIC FUNCTION DECLARATIONS 
  ---------------------------------------------------------------------------*/
static void chk_typecoll (/*_ cdemoctx *ctx, OCIParam  *parmp, boolean is_array _*/);
static void describe_type(/*_ cdemoctx *ctx, OCIType *tdo _*/);
static void checkerr(/*_ OCIError *errhp, sword status _*/);
static void initialize(cdemoctx *ctx);
static void setup_param(/*_ OCIError *errhp, OCIParam *parm, OCITypeCode tc, 
                        ub1 prec, sb1 scale, ub4 len, ub2 csid, ub1 csfrm,
                         OCIType *attr_tdo, OraText *schm, OraText *typn _*/);
static OCIType *create_builtin(/*_ OCISvcCtx *svchp, OCIEnv *envhp, OCIError *errhp, 
                                 OCIParam *parm, OCIDuration dur _*/);
static OCIType *create_usertype(/*_ OCISvcCtx *svchp, OCIEnv *envhp, OCIError *errhp, 
                                OCITypeCode tc, ub4 cnt, OCIParam *parm1, OCIParam *parm2,
                         OCIParam *parm3, OCIParam *parm4, OCIParam *parm5, OCIDuration dur _*/);
static void build_data(/*_ OCIEnv *envhp, OCIError *errhp, OCISvcCtx *svchp _*/);
void main(/*_ int argc, char *argv[] _*/);


/* check status and print error information  */ 
void checkerr(errhp, status)
OCIError *errhp;
sword status;
{
  text errbuf[512];
  sb4 errcode = OCI_SUCCESS;

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
    (void) printf("Error - OCI_NO_DATA\n");
    break;
  case OCI_ERROR:
    OCIErrorGet ((dvoid *) errhp, (ub4) 1, (text *) NULL, &errcode,
                    errbuf, (ub4) sizeof(errbuf), (ub4) OCI_HTYPE_ERROR);
    (void) printf("Error - %s\n", errbuf);
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
 if (errcode == 1034)
    exit(1);
  else
    return;
}


/* initialize envionment and handlers */
void initialize(ctx)
cdemoctx *ctx;
{
  OCIInitialize((ub4) OCI_THREADED | OCI_OBJECT, (dvoid *)0,  
                (dvoid * (*)(dvoid *, size_t)) 0,
                (dvoid * (*)(dvoid *, dvoid *, size_t)) 0,  
                (void (*)(dvoid *, dvoid *)) 0 );

  OCIHandleAlloc( (dvoid *) NULL, (dvoid **) &ctx->envhp,
                   (ub4) OCI_HTYPE_ENV,
                   0, (dvoid **)0);

  OCIEnvInit( &ctx->envhp, (ub4) OCI_DEFAULT, 0, (dvoid **) 0
                    );

  OCIHandleAlloc( (dvoid *) ctx->envhp,
        (dvoid **) &ctx->errhp, (ub4) OCI_HTYPE_ERROR, 0, (dvoid **)0);

  /* Allocate server handle and attach to server */
  OCIHandleAlloc( (dvoid *) ctx->envhp,
                  (dvoid **) &ctx->svrhp,
                  (ub4) OCI_HTYPE_SERVER, 0, (dvoid **) 0);
  checkerr(ctx->errhp, OCIServerAttach( ctx->svrhp,
                           ctx->errhp, (text *) 0,
                           (sb4) 0, (ub4) OCI_DEFAULT));

  /* Allocate and setup service context */
  OCIHandleAlloc( (dvoid *) ctx->envhp,
                  (dvoid **) &ctx->svchp,
                  (ub4) OCI_HTYPE_SVCCTX, 0, (dvoid **)0);
  checkerr(ctx->errhp, OCIAttrSet( (dvoid *) ctx->svchp,
                    (ub4) OCI_HTYPE_SVCCTX,
                    (dvoid *) ctx->svrhp, (ub4) 0,
                    (ub4) OCI_ATTR_SERVER, (OCIError *) ctx->errhp));
 
  /* allocate a user context handle */
  OCIHandleAlloc((dvoid *)ctx->envhp,
                   (dvoid **)&ctx->usrhp,
                   (ub4) OCI_HTYPE_SESSION, (size_t) 0, (dvoid **) 0);
  checkerr(ctx->errhp, OCIAttrSet((dvoid *)ctx->usrhp,
                   (ub4)OCI_HTYPE_SESSION,
                   (dvoid *) username, (ub4)strlen((char *) username),
                   (ub4)OCI_ATTR_USERNAME, ctx->errhp));
  checkerr(ctx->errhp, OCIAttrSet((dvoid *)ctx->usrhp,
                   (ub4)OCI_HTYPE_SESSION,
                   (dvoid *) password, (ub4)strlen((char *) password),
                   (ub4)OCI_ATTR_PASSWORD, ctx->errhp));
  checkerr(ctx->errhp,
        OCISessionBegin (ctx->svchp, ctx->errhp,
                         ctx->usrhp, (ub4)OCI_CRED_RDBMS,
                         (ub4)OCI_DEFAULT));

  /* Set up user context inside service context */
  checkerr(ctx->errhp, OCIAttrSet((dvoid *)ctx->svchp,
                   (ub4)OCI_HTYPE_SVCCTX,
                   (dvoid *)ctx->usrhp, (ub4)0,
                   (ub4)OCI_ATTR_SESSION, ctx->errhp));  
}


/* Setup param handle with type info */
void setup_param(errhp, parm, tc, prec, scale, len, csid, csfrm,
                         attr_tdo, schm, typn)
OCIError *errhp;
OCIParam *parm;
OCITypeCode tc;
ub1 prec;
sb1 scale;
ub4 len;
ub2 csid;
ub1 csfrm;
OCIType *attr_tdo;            /* attribute TDO or schema, typename specified */
OraText *schm;
OraText *typn;
{  

  /* Setup type code */
  OCIAttrSet((dvoid *)parm, (ub4)OCI_DTYPE_PARAM, (dvoid *)&tc,
             (ub4) sizeof(ub2), (ub4)OCI_ATTR_TYPECODE, errhp);
  
  switch (tc)
  {
  case OCI_TYPECODE_NUMBER:                     /* Setup precision and scale */
     OCIAttrSet((dvoid *)parm, (ub4)OCI_DTYPE_PARAM, (dvoid *)&prec,
             (ub4) sizeof(ub1), (ub4)OCI_ATTR_PRECISION, errhp);
     OCIAttrSet((dvoid *)parm, (ub4)OCI_DTYPE_PARAM, (dvoid *)&scale,
             (ub4) sizeof(sb1), (ub4)OCI_ATTR_SCALE, errhp);
     break;
  case OCI_TYPECODE_RAW:                              /* Setup length of raw */
     OCIAttrSet((dvoid *)parm, (ub4)OCI_DTYPE_PARAM, (dvoid *)&len,
             (ub4) sizeof(ub4), (ub4)OCI_ATTR_DATA_SIZE, errhp);
     break;
  case OCI_TYPECODE_VARCHAR2:         /* Set Len, csid, csfrm for varchar2's */
  case OCI_TYPECODE_VARCHAR:
  case OCI_TYPECODE_CHAR:
     OCIAttrSet((dvoid *)parm, (ub4)OCI_DTYPE_PARAM, (dvoid *)&len,
             (ub4) sizeof(ub4), (ub4)OCI_ATTR_DATA_SIZE, errhp);
     /* FALLTHROUGH */
  case OCI_TYPECODE_CLOB:
  case OCI_TYPECODE_CFILE:
     OCIAttrSet((dvoid *)parm, (ub4)OCI_DTYPE_PARAM, (dvoid *)&csid,
             (ub4) sizeof(ub2), (ub4)OCI_ATTR_CHARSET_ID, errhp);
     OCIAttrSet((dvoid *)parm, (ub4)OCI_DTYPE_PARAM, (dvoid *)&csfrm,
             (ub4) sizeof(ub1), (ub4)OCI_ATTR_CHARSET_FORM, errhp);
     break;
  case OCI_TYPECODE_REF:  
  case OCI_TYPECODE_OBJECT:                                  /* handle later */
  case OCI_TYPECODE_VARRAY:
  case OCI_TYPECODE_TABLE:
     OCIAttrSet((dvoid *)parm, (ub4)OCI_DTYPE_PARAM, (dvoid *)attr_tdo,
             (ub4) sizeof(OCIType *), (ub4)OCI_ATTR_TDO, errhp);
     break;
  default:
    break;  
  }
}


/* create OCIType of built in types */
OCIType *create_builtin(svchp, envhp, errhp, parm, dur)
OCISvcCtx *svchp;
OCIEnv *envhp;
OCIError *errhp;
OCIParam *parm;
OCIDuration dur;
{
  ub2 tc;
  OCIType *builtin;
  ub4 siz;

  /* Get Typecode */
  OCIAttrGet((dvoid *)parm, (ub4)OCI_DTYPE_PARAM, (dvoid *)&tc,
             &siz, (ub4)OCI_ATTR_TYPECODE, errhp);

  /* Create transient builtin based on the typecode */
  checkerr(errhp, OCITypeBeginCreate(svchp, errhp, tc, dur, &builtin));

  checkerr(errhp, OCITypeSetBuiltin(svchp, errhp, builtin, parm));
  
  checkerr(errhp, OCITypeEndCreate(svchp, errhp, builtin));

  return(builtin);
}


/* Create a user defined type of given typecode - OBJECT, VARRAY etc. */
OCIType *create_usertype(svchp, envhp, errhp, tc, cnt, parm1, parm2,
                         parm3, parm4, parm5, dur)
OCISvcCtx *svchp;
OCIEnv *envhp;
OCIError *errhp;
OCITypeCode tc;
ub4 cnt;
OCIParam *parm1;  /* MUST - param for collection info / 1st object attribute */
OCIParam *parm2;                             /* OPTIONAL - for 2nd attribute */
OCIParam *parm3;                             /* OPTIONAL - for 3rd attribute */
OCIParam *parm4;                             /* OPTIONAL - for 4th attribute */
OCIParam *parm5;                             /* OPTIONAL - for 5th attribute */
OCIDuration dur;
{
  OCIType *usertype;
  ub4 siz;
  sword status;
  /* Create transient usertype based on the typecode */
  status = OCITypeBeginCreate(svchp, errhp, tc, dur, &usertype);
  checkerr(errhp, status);
  if(status!=0){
    printf("exiting.... \n");
    exit(1);
  }

  switch (tc)
  {
    case OCI_TYPECODE_OBJECT:
      checkerr(errhp, OCITypeAddAttr(svchp, errhp, usertype, (text *)"attr1",
                                     (ub4)strlen("attr1"), parm1));
      if (parm2)
        checkerr(errhp, OCITypeAddAttr(svchp, errhp, usertype, (text *)"attr2",
                                       (ub4)strlen("attr2"), parm2));
      if (parm3)
        checkerr(errhp, OCITypeAddAttr(svchp, errhp, usertype, (text *)"attr3",
                                       (ub4)strlen("attr3"), parm3));
      if (parm4)
        checkerr(errhp, OCITypeAddAttr(svchp, errhp, usertype, (text *)"attr4",
                                       (ub4)strlen("attr4"), parm4));
      if (parm5)
        checkerr(errhp, OCITypeAddAttr(svchp, errhp, usertype, (text *)"attr5",
                                       (ub4)strlen("attr5"), parm5));
      break;
    case OCI_TYPECODE_VARRAY:
      checkerr(errhp,OCITypeSetCollection(svchp, errhp, usertype, parm1, cnt));
      break;
    default:
      printf ("Incorrect parameters to create_usertype\n");
      return((OCIType *)0);
  }

  checkerr(errhp, OCITypeEndCreate(svchp, errhp, usertype));

  return(usertype);
}


/* Using OCIDescribeAny to describe type */
static void chk_type (ctx, parmp)
cdemoctx *ctx;
OCIParam  *parmp;
{
  OCITypeCode typecode,
         collection_typecode;
  ub4    size;
  OCIType *tdo;
  ub2    num_attr;
  ub1    is_incomplete,
         is_predefined,
         is_transient,
         has_table;
  OCIParam *collection_parmp;
  ub1    prec;
  sb1    scale;
  ub2    len;
  ub2    csid;
  ub1    csfrm;
  ub2    sqltype;

  checkerr(ctx->errhp, OCIAttrGet((dvoid*) parmp,
                    (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &tdo, (ub4 *) 0,
                    (ub4) OCI_ATTR_TDO, (OCIError *) ctx->errhp));
    
  checkerr(ctx->errhp,
         OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &typecode, (ub4 *) 0,
                    (ub4) OCI_ATTR_TYPECODE, (OCIError *) ctx->errhp));

  if (typecode == OCI_TYPECODE_NAMEDCOLLECTION)
  {
    checkerr(ctx->errhp,
         OCIAttrGet((dvoid *)parmp, (ub4)OCI_DTYPE_PARAM,
                     (dvoid *)&collection_typecode, (ub4 *)0,
                     (ub4)OCI_ATTR_COLLECTION_TYPECODE,
                     (OCIError *)ctx->errhp));
    checkerr(ctx->errhp,
         OCIAttrGet((dvoid *)parmp, (ub4)OCI_DTYPE_PARAM,
           (dvoid *)&collection_parmp, (ub4 *)0,
           (ub4)OCI_ATTR_COLLECTION_ELEMENT, (OCIError *)ctx->errhp));
  }
    
  checkerr(ctx->errhp,
    OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
               (dvoid*) &is_incomplete, (ub4 *) 0,
        (ub4) OCI_ATTR_IS_INCOMPLETE_TYPE, (OCIError *) ctx->errhp));
 
  checkerr(ctx->errhp, 
       OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                (dvoid*) &is_predefined, (ub4 *) 0,
         (ub4) OCI_ATTR_IS_PREDEFINED_TYPE, (OCIError *) ctx->errhp));

  checkerr(ctx->errhp,
     OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
               (dvoid*) &is_transient, (ub4 *) 0,
          (ub4) OCI_ATTR_IS_TRANSIENT_TYPE, (OCIError *) ctx->errhp));

  checkerr(ctx->errhp,
        OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &has_table, (ub4 *) 0,
                    (ub4) OCI_ATTR_HAS_NESTED_TABLE,
                    (OCIError *) ctx->errhp));
 
  checkerr(ctx->errhp,
      OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
             (dvoid*) &num_attr, (ub4 *) 0,
              (ub4) OCI_ATTR_NUM_TYPE_ATTRS, (OCIError *) ctx->errhp));

  /* Get constraint info for predefined transient types */
  if (is_predefined && is_transient)
  {
    checkerr(ctx->errhp,
              OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                      (dvoid*) &prec, (ub4 *) 0, 
                      (ub4) OCI_ATTR_PRECISION,
                      (OCIError *) ctx->errhp));
    checkerr(ctx->errhp,
              OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                      (dvoid*) &scale, (ub4 *) 0, 
                      (ub4) OCI_ATTR_SCALE,
                      (OCIError *) ctx->errhp));
    if (IS_CHARTYPE(typecode))
    {
      checkerr(ctx->errhp,
                OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                      (dvoid*) &csid, (ub4 *) 0, 
                      (ub4) OCI_ATTR_CHARSET_ID,
                      (OCIError *) ctx->errhp));
      checkerr(ctx->errhp,
                OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                      (dvoid*) &csfrm, (ub4 *) 0, 
                      (ub4) OCI_ATTR_CHARSET_FORM,
                      (OCIError *) ctx->errhp));
    }

    checkerr(ctx->errhp,
              OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                      (dvoid*) &len, (ub4 *) 0, 
                      (ub4) OCI_ATTR_DATA_SIZE,
                      (OCIError *) ctx->errhp));
    checkerr(ctx->errhp,
              OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                      (dvoid*) &sqltype, (ub4 *) 0, 
                      (ub4) OCI_ATTR_DATA_TYPE,
                      (OCIError *) ctx->errhp));
  }

  printf ( "TYPE\n");
  printf ( "Typecode:          %d\n", typecode);
  if (typecode == OCI_TYPECODE_NAMEDCOLLECTION)
    printf ( "Collection typecode: %d\n", collection_typecode);
  printf ( "Number of attrs:   %d\n", num_attr);
  printf ( "Is incomplete:     %d\n", is_incomplete);
  printf ( "Is predefined:     %d\n", is_predefined);
  printf ( "Is transient:      %d\n", is_transient);
  printf ( "Has nested table:  %d\n", has_table);

  if (is_predefined && is_transient)
  {
    printf( "Predefined transient type information :\n");
    printf( "Precision        : %d\n", prec );
    printf( "Scale            : %d\n", scale);
    printf( "Length           : %d\n", len);
    printf( "SQLT code        : %d\n", sqltype);
    if (IS_CHARTYPE(typecode))
    {
      printf( "Charset id       : %d\n", csid );
      printf( "Charset form     : %d\n", csfrm);
    }
  }

  if (typecode == OCI_TYPECODE_NAMEDCOLLECTION)
    chk_typecoll(ctx, collection_parmp,
                   collection_typecode == OCI_TYPECODE_VARRAY);
}


/*describe collection type */
static void chk_typecoll (ctx, parmp, is_array)
cdemoctx *ctx;
OCIParam  *parmp;
boolean    is_array;
{
  text         schema[MAXNAME],
               type[MAXNAME],
              *namep;
  ub4          size;         
  ub2          len = 0;
  ub4          num_elems;
  OCITypeCode  typecode;
  ub2          datatype;

  checkerr(ctx->errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM, 
                    (dvoid*) &len, (ub4 *) 0, 
                    (ub4) OCI_ATTR_DATA_SIZE, (OCIError *) ctx->errhp));

  checkerr(ctx->errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &namep, (ub4 *) &size, 
                    (ub4) OCI_ATTR_TYPE_NAME, (OCIError *) ctx->errhp));

  strncpy((char *)type, (char *)namep, (size_t) size);
  type[size] = '\0';
    
  checkerr(ctx->errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &namep, (ub4 *) &size, 
                    (ub4) OCI_ATTR_SCHEMA_NAME, (OCIError *) ctx->errhp));

  strncpy((char *)schema, (char *)namep, (size_t) size);
  schema[size] = '\0';
    
  checkerr(ctx->errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &typecode, (ub4 *) 0, (ub4) OCI_ATTR_TYPECODE,
                    (OCIError *) ctx->errhp));
  
  checkerr(ctx->errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &datatype, (ub4 *) 0, (ub4) OCI_ATTR_DATA_TYPE, 
                    (OCIError *) ctx->errhp));
  
  num_elems = 0;
  if (is_array)
    checkerr(ctx->errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                      (dvoid*) &num_elems, (ub4 *) 0,
                      (ub4) OCI_ATTR_NUM_ELEMS, (OCIError *) ctx->errhp));

  
    printf ( "Schema    Type            Length   Type Datatype Elements\n");  
    printf ( "_________________________________________________________\n");

  printf("%10s%16s%9d%5d%9d%8d\n", schema, type,
                 len, typecode, datatype, num_elems);

}           


/* Describe a type given the OCIType */
static void describe_type(ctx, tdo)
cdemoctx *ctx;
OCIType *tdo;
{
  OCIDescribe *dschp;
  OCIParam *parmp;
 
  OCIHandleAlloc((dvoid *)ctx->envhp, (dvoid **) &dschp,
                         (ub4) OCI_HTYPE_DESCRIBE, (size_t) 0, (dvoid **) 0);

  checkerr(ctx->errhp,
         OCIDescribeAny(ctx->svchp, ctx->errhp, (dvoid *)tdo,
                        (ub4)0, OCI_OTYPE_PTR, (ub1)1, OCI_PTYPE_TYPE, dschp));

  checkerr(ctx->errhp, OCIAttrGet((dvoid *)dschp, (ub4)OCI_HTYPE_DESCRIBE,
           (dvoid *)&parmp, (ub4 *)0, (ub4)OCI_ATTR_PARAM,
           ctx->errhp));

  chk_type(ctx, parmp);
}


/* constructs an ANYDATA piecewise using OCIAnyDataBeginCreate() 
   and access the data piecewise */
static void build_data(envhp, errhp, svchp)
OCIEnv *envhp;
OCIError *errhp;
OCISvcCtx *svchp;
{
  OCIAnyData *addressimgh = (OCIAnyData *) 0;
  OCIAnyData *addressimgh2 = (OCIAnyData *) 0;
  text *addressimg_buf;
  OCITypeCode       typecode;
  OCIInd   indp;
  ub4      zip_size, state_size;
  ub2      len;
  OCIRef  *Ref, *Ref2;
  OCIType *tdo = (OCIType *) 0;
  sword  error;

  OCIString *str = (OCIString *) 0;
  OCIRaw *Raw = (OCIRaw *)0;
  OCIRaw *Raw2 = (OCIRaw *)0;
  ub1  raw_data[5] = {1, 1, 1, 1, 1};
  OCIDate date1, *date2 = (OCIDate *)0;
  OCINumber num1, *num2 = (OCINumber *)0; 
  OCIInd ind1 = OCI_IND_NOTNULL;

  printf("\nBeginning piecewise construction and access ANYDATA\n");

  checkerr(errhp, OCITypeByName(envhp, errhp, svchp, (const text *) "",
                   (ub4) strlen((const char *) ""),
                   (const text *) "BASIC_OBJECT",
                   (ub4) strlen((const char *) "BASIC_OBJECT"),
                   (CONST text *) 0, (ub4) 0,
                   OCI_DURATION_SESSION, OCI_TYPEGET_ALL, &tdo));

  printf("Constructing image\n");

  /* begin to construct the data piecewise */
  OCIAnyDataBeginCreate(svchp, errhp, OCI_TYPECODE_OBJECT, tdo, 
        OCI_DURATION_SESSION, &addressimgh);

  printf("Adding scalar values for the attributes in the image handle\n");

  OCIStringAssignText(envhp, errhp, (text *)"CA", 2, &str);
  printf("state is %s \n", OCIStringPtr(envhp, str)); 
  OCIAnyDataAttrSet(svchp, errhp, addressimgh, OCI_TYPECODE_CHAR,
                (OCIType *)0, (dvoid *)&ind1, str, 2, FALSE);
  OCIStringAssignText(envhp, errhp, (text *)"94065", 5, &str);
  printf("zip is %s \n", OCIStringPtr(envhp, str)); 
  OCIAnyDataAttrSet(svchp, errhp, addressimgh, OCI_TYPECODE_CHAR,
                (OCIType *)0, (dvoid *)&ind1, str, 5, FALSE);

  OCIRawAssignBytes(envhp, errhp, raw_data, 5, &Raw);
  OCIAnyDataAttrSet(svchp, errhp, addressimgh, OCI_TYPECODE_RAW,
                        (OCIType *)0, (dvoid *)&ind1, Raw, (ub4) 5, FALSE);

  error = OCIDateSysDate(errhp, &date1);
  OCIAnyDataAttrSet(svchp, errhp, addressimgh, OCI_TYPECODE_DATE,
                        (OCIType *)0, (dvoid *)&ind1, &date1, (ub4)0, FALSE);

  zip_size = 10;
  error = OCINumberFromInt(errhp, &zip_size, sizeof(zip_size), 
                OCI_NUMBER_UNSIGNED, &num1);
  OCIAnyDataAttrSet(svchp, errhp, addressimgh, OCI_TYPECODE_NUMBER,
                        (OCIType *)0, (dvoid *)&ind1, &num1, (ub4)0, FALSE);

  printf("Generating Image\n\n");
  OCIAnyDataEndCreate(svchp, errhp, addressimgh);

  /* Access the image */
  printf("Accessing image\n");

  zip_size = 11;
  state_size = 3;

  OCIAnyDataAttrGet(svchp, errhp, addressimgh,
                      OCI_TYPECODE_CHAR, (OCIType *)0, (dvoid *)&indp,
                (dvoid **)&str, &state_size, FALSE);
  printf("state is %s \n", OCIStringPtr(envhp, str)); 
  OCIAnyDataAttrGet(svchp, errhp, addressimgh,
                      OCI_TYPECODE_CHAR, (OCIType *)0, (dvoid *)&indp,
                (dvoid **)&str, &zip_size, FALSE);
  printf("zip is %s \n", OCIStringPtr(envhp, str)); 

  checkerr(errhp, OCIObjectNew(envhp, errhp, svchp, OCI_TYPECODE_RAW, 
                (OCIType *) 0, (dvoid *) 0, OCI_DURATION_TRANS, FALSE,
                (dvoid **) &Raw2));
  OCIAnyDataAttrGet(svchp, errhp, addressimgh,
                      OCI_TYPECODE_RAW, (OCIType *)0, (dvoid *)&indp,
                (dvoid **)&Raw2, &state_size, FALSE);
  if (!memcmp((dvoid *)OCIRawPtr(envhp, Raw), (dvoid *)OCIRawPtr(envhp, Raw2),
                        OCIRawSize(envhp, Raw)))
    printf("RAWs are same \n");
  else 
    printf("Error! RAWs are diff \n");
   
  error =  OCIObjectNew(envhp, errhp, svchp, OCI_TYPECODE_DATE,
          (OCIType *) 0, (dvoid *) 0, OCI_DURATION_DEFAULT, FALSE,
          (dvoid **) &date2);
  /*OCIAnyDataAttrGet(svchp, errhp, addressimgh,
                      OCI_TYPECODE_DATE, (OCIType *)0, (dvoid *)&indp,
                (dvoid **)date2, &state_size, FALSE); */
  OCIAnyDataAttrGet(svchp, errhp, addressimgh,
                      OCI_TYPECODE_DATE, (OCIType *)0, (dvoid *)&indp,
                (dvoid **)&date2, &state_size, FALSE);
  (void) OCIDateCompare(errhp, &date1, date2, &error);


  if (!error)
    printf("Dates are same \n");
  else
    printf("Error! dates are diff \n");

  checkerr(errhp, OCIObjectNew(envhp, errhp, svchp, OCI_TYPECODE_NUMBER,
                (OCIType *) 0, (dvoid *) 0, OCI_DURATION_TRANS, FALSE,
                (dvoid **) &num2));
  /*OCIAnyDataAttrGet(svchp, errhp, addressimgh,
                      OCI_TYPECODE_NUMBER, (OCIType *)0, (dvoid *)&indp,
                (dvoid **)num2, &state_size, FALSE);*/
  OCIAnyDataAttrGet(svchp, errhp, addressimgh,
                      OCI_TYPECODE_NUMBER, (OCIType *)0, (dvoid *)&indp,
                (dvoid **)&num2, &state_size, FALSE);
  OCINumberCmp(errhp, &num1, num2, &error);
  if (!error)
    printf("Numbers are same \n");
  else
    printf("Error! Numbers are diff \n");
  OCINumberToInt(errhp, num2, sizeof(zip_size), 0,
                        &zip_size);
  printf("Number (num2) is %d \n", zip_size);

  /*
  ** Free the handles
  */

  printf("Freeing FDO, TDS and Image structures\n\n\n");
  OCIAnyDataDestroy(svchp, errhp, addressimgh);
}


void main(int argc, char *argv[])
{
  cdemoctx ctx;
  OCIType *tdo;                                                   /* AnyType */
  OCIParam *parm = (OCIParam *)0;                        /* parameter handle */
  OCIParam *parm2 = (OCIParam *)0;
  OCIParam *parm3 = (OCIParam *)0;
  OCIDuration dur;
  OCIType *attr_tdo;
  OCIType *obj_tdo;
  OCIType *coll_tdo;
  ub1 t_precision;
  sb1 t_scale;
  ub2 t_charset_id;
  ub1 t_charset_form;
  ub4 t_data_size;
  ub2 t_typecode;
  OCIType *t_tdo;

  initialize(&ctx);

  /* Allocate parameter handle for transient type creation */
  OCIDescriptorAlloc((dvoid *)ctx.envhp, (dvoid **)&parm,
                     (ub4)OCI_DTYPE_PARAM, 0, (dvoid **)0);
  OCIDescriptorAlloc((dvoid *)ctx.envhp, (dvoid **)&parm2,
                     (ub4)OCI_DTYPE_PARAM, 0, (dvoid **)0);
  OCIDescriptorAlloc((dvoid *)ctx.envhp, (dvoid **)&parm3,
                     (ub4)OCI_DTYPE_PARAM, 0, (dvoid **)0);

  OCIDurationBegin(ctx.envhp, ctx.errhp, ctx.svchp,
                   OCI_DURATION_CALL, &dur);


  /* Create transient built-in types and descrie types*/

  /* NUMBER(5,2) */
  setup_param(ctx.errhp, parm, OCI_TYPECODE_NUMBER, (ub1)5, (sb1)2,
              (ub4)0, (ub2)0,
              (ub1)0, (OCIType *)0, (OraText *)0, (OraText *)0);

  tdo = create_builtin(ctx.svchp, ctx.envhp, ctx.errhp,
                       parm, dur);

  describe_type(&ctx, tdo);

  /* CHAR(20), csid = OCI_UCS2ID, csfrm = SQLCS_NCHAR */
  setup_param(ctx.errhp, parm, OCI_TYPECODE_CHAR, (ub1)0, (sb1)0,
              (ub4)20, (ub2)OCI_UCS2ID,
              (ub1)SQLCS_NCHAR, (OCIType *)0, (OraText *)0, (OraText *)0);
  tdo = create_builtin(ctx.svchp,  ctx.envhp,
                       ctx.errhp, parm, dur);
  describe_type(&ctx, tdo);

  OCIDurationEnd(ctx.envhp, ctx.errhp, ctx.svchp,
                 dur);


  OCITypeByName(ctx.envhp, ctx.errhp,
                        ctx.svchp, (text *)0, (ub4)0,
                        (text *)"FOO", (ub4)3, (text *)0, (ub4)0,
                        OCI_DURATION_SESSION, OCI_TYPEGET_ALL, &attr_tdo);

  /* create a transient collection - VARRAY(100) of SCOTT.FOO */
  setup_param(ctx.errhp, parm, OCI_TYPECODE_OBJECT, (ub1)0, (sb1)0,
              (ub4)0, (ub2)0,
              (ub1)SQLCS_IMPLICIT, attr_tdo, (OraText *)0, (OraText *)0);
  coll_tdo = create_usertype(ctx.svchp, ctx.envhp,
                        ctx.errhp, OCI_TYPECODE_VARRAY, (ub4)100,
                        parm, (OCIParam *)0, (OCIParam *)0, (OCIParam *)0,
                        (OCIParam *)0, dur);
  describe_type(&ctx, coll_tdo);

  /* piecewise construct an ANYDATA and access it piecewise */
  build_data(ctx.envhp, ctx.errhp, ctx.svchp);

  /* Cleanup */
  OCIHandleFree((dvoid *)ctx.envhp, (ub4) OCI_HTYPE_ENV);
  OCITerminate((ub4)OCI_OBJECT);
}

/* end of file cdemoAnyData2.c */
