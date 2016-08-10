#ifdef RCSID
static char *RCSid =
   "$Header: cdemort.c 14-jul-99.13:21:07 mjaeger Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1995, 1999,, 2000 Oracle Corporation.  All rights reserved.
*/

/*
   NAME
     cdemort.c
   DESCRIPTION
     Tests ORT user interface type access.

     cdemo_ort takes the username, password and a type name
     (created in the database) as a command line argument and
     dumps all the information about the type -- its attribute
     types, methods, method parameters, etc.

   NOTES
     See routines.

   MODIFIED   (MM/DD/YY)
    svedala    01/24/00 -  4th argument to OCITypeMethodOverload should be
                           "CONST text *" - bug 1078623
    mjaeger    07/14/99 -  bug 808870: OCCS: convert tabs, no long lines
    svedala    09/09/98 -  lines longer than 79 chars reformatted - bug 722491
    cchau      05/19/97 -  change OCITypeByName to OCIDescribeAny
    echen      03/05/97 -  remove unnecessary code
    cxcheng    02/20/97 -  fix oro names
    cxcheng    02/10/97 -  remove short ORO names
    cxcheng    01/15/97 -  change prototype of OCITypeElemParameterizedType()
    echen      01/03/97 -  modify the test
    echen      11/15/96 -  oci beautification
    echen      07/25/96 -  enhance the demo
    dchatter   07/18/96 -  remove Oracle specific code
    echen      07/16/96 -  Creation
*/

#ifndef CDEMO_ORT_ORACLE
#include "cdemort.h"
#endif

/*****************************************************************************/
static void checkerr(errhp, status)
OCIError *errhp;
sword status;
{
  text errbuf[512];
  sb4 errcode;

  switch (status)
  {
  case OCI_SUCCESS:
    break;
  case OCI_SUCCESS_WITH_INFO:
    break;
  case OCI_NEED_DATA:
    break;
  case OCI_NO_DATA:
    break;
  case OCI_ERROR:
    (void) OCIErrorGet ((dvoid *) errhp, (ub4) 1, (text *) NULL, &errcode,
                    errbuf, (ub4) sizeof(errbuf), (ub4) OCI_HTYPE_ERROR);
    (void) printf("Error - %s\n", errbuf);
    break;
  case OCI_INVALID_HANDLE:
    break;
  case OCI_STILL_EXECUTING:
    break;
  case OCI_CONTINUE:
    break;
  default:
    break;
  }
}

/*****************************************************************************/
static void print_method_positions(meth_name, methpos, om_count)
text *meth_name;
ub4 methpos[10];
ub4 om_count;
{
  ub4 i;

  (void) printf("The position(s) for method %s -- %d",
                 meth_name, methpos[0]);
  for (i = 1; i < om_count; i++)
    (void) printf(", %d", methpos[i]);

  (void) printf(".\n");
}


/*****************************************************************************/
static void unit_test_type_access(envhp, errhp, svchp, type_name)
OCIEnv *envhp;
OCIError *errhp;
OCISvcCtx *svchp;
char *type_name;
{
  ub1      meth_flags;
  ub4      i, j, k;
  text    *text_value, *ret_param;
  ub4      text_len, len;
  ub4      count, pcount, pos, methpos[10], om_count;
  OCITypeCode    typecode;
  OCIType   *at_tdo = (OCIType *) 0,
           *tdo_stored, *param_tdo, *attr_tdo, *non_collection,
           *return_tdo;
  OCITypeElem   *rdo, *pdo;
  OCITypeIter   *iterator;
  OCITypeMethod   *mdoPtr_array = (OCITypeMethod *) 0;
  OCITypeElem   *ado = (OCITypeElem *) 0, **bad_ado = (OCITypeElem **) 0,
           *ado1 = (OCITypeElem *) 0;
  OCITypeMethod   *mdo, **bad_mdo = (OCITypeMethod **) 0;
  OCITypeElem   *ms_ado;
  ub2      sqtcode, sqttype;
  ub4      num_elems;
  OCITypeElem   *element;
  OCIType   *element_type;
  OCIType   *table_type;
  text     *method, *pname, *meth_name;
  ub4      mname_len;
  OCIRef *type_ref = (OCIRef *) 0;
  OCIDescribe *dschp = (OCIDescribe *) 0;
  OCIParam *parmp;



  /* ----------------- GET INFORMATION ABOUT A TYPE ----------------- */

  /* allocate describe handle for OCIDescribeAny */
  checkerr(errhp, OCIHandleAlloc((dvoid *) envhp, (dvoid **) &dschp,
                        (ub4) OCI_HTYPE_DESCRIBE,
                        (size_t) 0, (dvoid **) 0));

  /* if (OCITypeByName(envhp, errhp, svchp, (const text *)"",
              (ub4) strlen(""), (const text *) type_name,
              (ub4) strlen(type_name),
              (CONST text *) 0, (ub4) 0,
              OCI_DURATION_SESSION, OCI_TYPEGET_HEADER,
              &at_tdo) != OCI_SUCCESS  || !at_tdo)
  {
    (void) printf("Can not get type descriptor.\n");
    return;
  } */

  checkerr(errhp, OCIDescribeAny(svchp, errhp, (text *)type_name,
                  (ub4) strlen(type_name), OCI_OTYPE_NAME, (ub1)1,
                  (ub1) OCI_PTYPE_TYPE, dschp));

  checkerr(errhp, OCIAttrGet((dvoid *) dschp, (ub4) OCI_HTYPE_DESCRIBE,
                  (dvoid *)&parmp, (ub4 *)0, (ub4)OCI_ATTR_PARAM, errhp));

  checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &type_ref, (ub4 *) 0,
                    (ub4) OCI_ATTR_REF_TDO, (OCIError *) errhp));

  checkerr(errhp, OCIObjectPin(envhp, errhp, type_ref, (OCIComplexObject *) 0,
               OCI_PIN_ANY, OCI_DURATION_SESSION, OCI_LOCK_NONE,
               (dvoid **)&at_tdo));

  if (!at_tdo)
  {
    (void) printf("Can not get type descriptor.\n");
    return;
  }

  if (OCITypeTypeCode(envhp, errhp, at_tdo) != OCI_TYPECODE_OBJECT)
    (void) printf("The type is not name type.\n");

  /* version name will be "$8.0" for future label */
  if (memcmp((const void *) OCITypeVersion(envhp, errhp, at_tdo, &text_len),
            (const void *) "$8.0", text_len))
    (void) printf("return version is wrong.\n");

  /* ---------- GET IMMEDIATE ATTRIBUTES IN A TYPE ---------- */

  /* loop through all attributes in the type */
  count = OCITypeAttrs(envhp, errhp, at_tdo);

  if (OCITypeIterNew(envhp, errhp, at_tdo, &iterator) != OCI_SUCCESS)
    (void) printf("BUG -- OCITypeIterNew, test return code OCI_SUCCESS.\n");

  for (i = 0; OCITypeAttrNext(envhp, errhp,
                                        iterator, &ado) != OCI_NO_DATA; i++)
  {
    /* get the attribute's name */
    (void) printf("Attribute # %d -- %s\n", i + 1,
                   OCITypeElemName(envhp, errhp, ado, &text_len));

    /* get information about the attribute by name */
    if (OCITypeAttrByName(envhp, errhp, at_tdo,
                          OCITypeElemName(envhp, errhp, ado, &text_len),
                text_len, &ado1) != OCI_SUCCESS)
      (void) printf("Can not get attribute by name.\n");

    /* get the attribute's type code */
    sqtcode = OCITypeElemExtTypeCode(envhp, errhp, ado);
    (void) printf("The SQT code is %d.\n", sqtcode);

    /* get the attribute's type code */
    typecode = OCITypeElemTypeCode(envhp, errhp, ado);

    switch (typecode)
    {
      /* scalar types */
      case OCI_TYPECODE_DATE:                                        /* date */
        (void) printf(" TYPE CODE -- OCI_TYPECODE_DATE.\n");
        break;
      case OCI_TYPECODE_SIGNED8:                                     /* byte */
        (void) printf(" TYPE CODE -- OCI_TYPECODE_SIGNED8.\n");
        break;
      case OCI_TYPECODE_SIGNED16:                                   /* short */
        (void) printf(" TYPE CODE -- OCI_TYPECODE_SIGNED16.\n");
        break;
      case OCI_TYPECODE_UNSIGNED8:                          /* unsigned byte */
       (void) printf(" TYPE CODE -- OCI_TYPECODE_UNSIGNED8.\n");
        break;
      case OCI_TYPECODE_UNSIGNED16:                        /* unsigned short */
        (void) printf(" TYPE CODE -- OCI_TYPECODE_UNSIGNED16.\n");
        break;
      case OCI_TYPECODE_OCTET:                                      /* octet */
        (void) printf(" TYPE CODE -- OCI_TYPECODE_OCTET.\n");
        break;
      case OCI_TYPECODE_MLSLABEL:                         /* oracle mlslabel */
        (void) printf(" TYPE CODE -- OCI_TYPECODE_MLSLABEL.\n");
        break;
      case OCI_TYPECODE_CLOB:                                        /* clob */
        (void) printf(" TYPE CODE -- OCI_TYPECODE_CLOB.\n");
        break;
      case OCI_TYPECODE_BLOB:                                        /* blob */
        (void) printf(" TYPE CODE -- OCI_TYPECODE_BLOB.\n");
        break;
      case OCI_TYPECODE_CFILE:                                      /* cfile */
        (void) printf(" TYPE CODE -- OCI_TYPECODE_CFILE.\n");
        break;
      case OCI_TYPECODE_BFILE:                                      /* bfile */
        (void) printf(" TYPE CODE -- OCI_TYPECODE_BFILE.\n");
        break;
      /* number types */
      case OCI_TYPECODE_INTEGER:
        (void) printf(" TYPE CODE -- OCI_TYPECODE_INTEGER.\n");
        break;
      case OCI_TYPECODE_NUMBER:                             /* oracle number */
        (void) printf(" TYPE CODE -- OCI_TYPECODE_NUMBER.\n");
        (void) printf(" Scale -- %d\n",
                                  OCITypeElemNumScale(envhp, errhp, ado));
        break;
      case OCI_TYPECODE_DECIMAL:                                  /* decimal */
        (void) printf(" TYPE CODE -- OCI_TYPECODE_DECIMAL.\n");
        (void) printf(" Scale -- %d\n",
                                  OCITypeElemNumScale(envhp, errhp, ado));
        break;

      /* fall through to get the precision */
      case OCI_TYPECODE_FLOAT:                                      /* float */
        (void) printf(" TYPE CODE -- OCI_TYPECODE_FLOAT.\n");
        (void) printf(" Scale -- %d, Precision %d\n",
                         OCITypeElemNumScale(envhp, errhp, ado),
                         OCITypeElemNumPrec(envhp, errhp, ado) );
        break;
      case OCI_TYPECODE_SIGNED32:                                    /* long */
        (void) printf(" TYPE CODE -- OCI_TYPECODE_SIGNED32.\n");
        (void) printf(" Scale -- %d, Precision %d\n",
                         OCITypeElemNumScale(envhp, errhp, ado),
                         OCITypeElemNumPrec(envhp, errhp, ado) );
        break;
      case OCI_TYPECODE_UNSIGNED32:                         /* unsigned long */
        (void) printf(" TYPE CODE -- OCI_TYPECODE_UNSIGNED32.\n");
        (void) printf(" Scale -- %d, Precision %d\n",
                         OCITypeElemNumScale(envhp, errhp, ado),
                         OCITypeElemNumPrec(envhp, errhp, ado) );
        break;
      case OCI_TYPECODE_REAL:                                        /* real */
        (void) printf(" TYPE CODE -- OCI_TYPECODE_REAL.\n");
        (void) printf(" Scale -- %d, Precision %d\n",
                         OCITypeElemNumScale(envhp, errhp, ado),
                         OCITypeElemNumPrec(envhp, errhp, ado) );
        break;
      case OCI_TYPECODE_DOUBLE:                                    /* double */
        (void) printf(" TYPE CODE -- OCI_TYPECODE_DOUBLE.\n");
        (void) printf(" Scale -- %d, Precision %d\n",
                         OCITypeElemNumScale(envhp, errhp, ado),
                         OCITypeElemNumPrec(envhp, errhp, ado) );
        break;

      /* string types */
      case OCI_TYPECODE_CHAR:                         /* fixed length string */
        (void) printf(" TYPE CODE -- OCI_TYPECODE_CHAR.\n");
        (void) printf(" Character set id -- %d\n",
                         OCITypeElemCharSetID(envhp, errhp, ado));
        (void) printf(" String length -- %d\n",
                                     OCITypeElemLength(envhp, errhp, ado));
        break;
      case OCI_TYPECODE_VARCHAR2:                  /* variable length string */
        (void) printf(" TYPE CODE -- OCI_TYPECODE_VARCHAR2.\n");
        (void) printf(" Character set id -- %d\n",
                         OCITypeElemCharSetID(envhp, errhp, ado));
        (void) printf(" String length -- %d\n",
                                     OCITypeElemLength(envhp, errhp, ado));
        break;
      case OCI_TYPECODE_VARCHAR:               /* variable length string old */
        (void) printf(" TYPE CODE -- OCI_TYPECODE_VARCHAR.\n");
        (void) printf(" Character set id -- %d\n",
                         OCITypeElemCharSetID(envhp, errhp, ado));
        (void) printf(" String length -- %d\n",
                                       OCITypeElemLength(envhp, errhp, ado));
        break;
      case OCI_TYPECODE_RAW:                                          /* raw */
        (void) printf(" TYPE CODE -- OCI_TYPECODE_RAW.\n");
        (void) printf(" String length -- %d\n",
                                       OCITypeElemLength(envhp, errhp, ado));
        break;

      /* parameterized types */
      case OCI_TYPECODE_VARRAY:                            /* variable array */
        (void) printf(" TYPE CODE -- OCI_TYPECODE_VARRAY.\n");
        if ( OCITypeElemParameterizedType(envhp, errhp,
                                           ado, &tdo_stored) != OCI_SUCCESS )
           (void) printf(
                  "Error -- can not get parameterized types's descriptor.\n");
        (void) printf(" Array elements type code -- %d\n",
                         OCITypeTypeCode(envhp, errhp, tdo_stored));
        break;

      case OCI_TYPECODE_REF:                                    /* reference */
        (void) printf(" TYPE CODE -- OCI_TYPECODE_REF.\n");
        if ( OCITypeElemParameterizedType(envhp, errhp,
                                           ado, &tdo_stored) != OCI_SUCCESS )
           (void) printf(
                  "Error -- can not get parameterized types's descriptor.\n");
        (void) printf(" Array elements type code -- %d\n",
                         OCITypeTypeCode(envhp, errhp, tdo_stored));
        break;
      case OCI_TYPECODE_PTR:                                      /* pointer */
        (void) printf(" TYPE CODE -- OCI_TYPECODE_PTR.\n");
        if ( OCITypeElemParameterizedType(envhp, errhp,
                                           ado, &tdo_stored) != OCI_SUCCESS )
           (void) printf(
                  "Error -- can not get parameterized types's descriptor.\n");
        (void) printf(" Array elements type code -- %d\n",
                         OCITypeTypeCode(envhp, errhp, tdo_stored));
        break;

      case OCI_TYPECODE_NAMEDCOLLECTION:                      /* domain type */
        (void) printf(" TYPE CODE -- OCI_TYPECODE_NAMEDCOLLECTION.\n");
        if ( OCITypeElemType( envhp, errhp, ado, &attr_tdo) == OCI_SUCCESS )
        {
        switch (OCITypeCollTypeCode(envhp, errhp, attr_tdo))
        {
          case OCI_TYPECODE_VARRAY:                        /* variable array */

            (void) printf("   SUBTYPE CODE -- OCI_TYPECODE_VARRAY.\n");

            if (OCITypeCollSize(envhp, errhp,
                                    attr_tdo, &num_elems) != OCI_SUCCESS)
               (void) printf(
                     "Error -- can not get collectio type element count.\n");
            (void) printf("The number of elements are %d.\n", num_elems);

            /* test error code for OCITypeCollElem */
            /* get the ado of the array */
            element = (OCITypeElem *)0;
            if (OCITypeCollElem(envhp, errhp,
                                      attr_tdo, &element) != OCI_SUCCESS)
              (void) printf(
              "Error -- can not get descriptor to an element's descriptor.\n");

            /* get the type of the array */
            if (ortgcty(envhp, errhp, attr_tdo, &element_type) != OCI_SUCCESS)
              (void) printf(
                       "Error -- can not get element's type descriptor.\n");
            (void) printf(" The type code for array is %d.\n",
                             OCITypeTypeCode(envhp, errhp, element_type) );

            if (OCITypeCollExtTypeCode(envhp, errhp,
                                         attr_tdo, &sqtcode) != OCI_SUCCESS)
              (void) printf("Error -- can not get element's sql code.\n");
            (void) printf(" The SQT code for array is %d.\n", sqtcode );
            break;

          case OCI_TYPECODE_TABLE:                               /* multiset */

            (void) printf("   SUBTYPE CODE -- OCI_TYPECODE_TABLE.\n");
            /* get the type of the multiset */
            if (ortgcty(envhp, errhp, attr_tdo, &table_type) != OCI_SUCCESS)
              (void) printf("Error -- can not get the type of a multiset.\n");
            (void) printf(" The type code for the multiset is %d.\n",
                             OCITypeTypeCode(envhp, errhp, table_type) );

            break;
        }
        }
        else
          (void) printf(
                " Error -- OCITypeElemType, test return code OCI_SUCCESS.\n");
        break;

      /* abstract type */
      case OCI_TYPECODE_OBJECT:                        /* abstract data type */
        if ( OCITypeElemType( envhp, errhp, ado, &attr_tdo) == OCI_SUCCESS )
        {
          if ( OCITypeTypeCode(envhp, errhp, attr_tdo)!=OCI_TYPECODE_OBJECT )
            (void) printf(" Error -- expect type code OCI_TYPECODE_OBJECT.\n");
        }
        else
          (void) printf(" Error -- can not get attribute tdo.\n");
        (void) printf(" TYPE CODE -- OCI_TYPECODE_OBJECT.\n");
        break;

      default:
        (void) printf("Error:  invalid type code\n");
        break;
    } /* end of typecode switch */

  }


  /* ------------ GET THE IMMEDIATE METHODS OF A TYPE ------------ */

  count = OCITypeMethods(envhp, errhp, at_tdo);

  /* ----------------- GET THE MAP FUNCTION ----------------- */
  /* MAP method cannot be created yet */
  if ( OCITypeMethodMap(envhp, errhp, at_tdo, &mdo) == OCI_SUCCESS )
    (void) printf("Error -- can not get the map function.\n");
  if (mdo != (OCITypeMethod *)0)
    (void) printf("Map method name -- %s.\n",
                   OCITypeMethodName(envhp, errhp, mdo, &text_len));

  /* ----------------- GET THE ORDER FUNCTION ----------------- */
  /* MAP method cannot be created yet */
  if ( OCITypeMethodOrder(envhp, errhp, at_tdo, &mdo) == OCI_SUCCESS )
    (void) printf("Error -- can not get map method's mdo.\n");
  if (mdo != (OCITypeMethod *)0)
    (void) printf("Order method name -- %s.\n",
                   OCITypeMethodName(envhp, errhp, mdo, &text_len));


  /* ----------- LOOP THROUGH ALL METHODS IN A TYPE ----------- */

  (void) printf("Number of methods -- %d\n", count);
  for (i = 0; OCITypeMethodNext(envhp, errhp,
                                         iterator, &mdo) != OCI_NO_DATA; i++)
  {
    if (OCITypeResult(envhp, errhp, mdo, &rdo) != OCI_SUCCESS)
      (void) printf("BUG -- OCITypeResult, test return code OCI_SUCCESS.\n");


    /* get the typecode of the method's result */
    typecode = OCITypeElemTypeCode(envhp, errhp, rdo);

    meth_name = OCITypeMethodName(envhp, errhp, mdo, &text_len);
    (void) printf("\nMethod name -- %s, Type code: %d, Position: %d.\n",
                   meth_name, typecode, i);

    if (OCITypeMethodByName(envhp, errhp, at_tdo, (CONST text *) meth_name,
                           (ub4) strlen((const char *) meth_name),
                           &mdoPtr_array) != OCI_SUCCESS)
      (void) printf(
             "Error -- OCITypeMethodByName, test return code OCI_SUCCESS.\n");

    /* Get method's return parameter */
    if (OCITypeResult(envhp, errhp, mdo, &rdo) != OCI_SUCCESS)
      (void) printf("Error -- ortgrbp, test return code OCI_SUCCESS.\n");

    /* find out how many methods exist with this name */
    om_count = OCITypeMethodOverload(envhp, errhp, at_tdo,
                    (CONST text *) meth_name,
                    (ub4) strlen((const char *) meth_name));
    (void) printf("Number of overloaded methods for %s -- %d\n",
                   meth_name, om_count);

    print_method_positions(meth_name, methpos, om_count);

    /* get the method's encapsulation */
    if (OCITypeMethodEncap(envhp, errhp, mdo) != OCI_TYPEENCAP_PUBLIC)
      (void) printf("Error -- wrong method's encapsulation.\n");

    /* ------------ GET THE PARAMETERS IN A METHOD ------------ */

    /* loop through all parameters in the method */
    pcount = OCITypeMethodParams(envhp, errhp, mdo);
    (void) printf("Number of parameter's in method -- %d\n", pcount);

    for (j = 1; j <= pcount; j++)
    {
      /* get the parameter information by position */
      if (OCITypeParamByPos(envhp, errhp, mdo, j, &pdo) != OCI_SUCCESS)
        (void) printf(
                "Error -- OCITypeParamByPos, test return code OCI_SUCCESS.\n");

      /* put the parameter's number */
      (void) printf("Parameter's number -- %d\n.", j);

      /* get the parameter's name */
      pname = OCITypeElemName(envhp, errhp, pdo, &text_len);
      (void) printf("Parameter's name -- %s\n.", pname);

      /* get a parameter in a method by name */
      if (OCITypeParamByName(envhp, errhp, mdo, (CONST text *) pname, (ub4)
                  strlen((const char *) pname), &pdo) != OCI_SUCCESS)
        (void) printf(
               "Error -- OCITypeParamByName, test return code OCI_SUCCESS.\n");

      if (OCITypeParamPos(envhp, errhp, mdo, (CONST text *) pname,
                  (ub4) strlen((const char *)pname),
                  &pos, &pdo) != OCI_SUCCESS)
        (void) printf(
               "Error -- OCITypeParamPos, test return code OCI_SUCCESS.\n");
      /* check the position */
      if (pos != j)
        (void) printf("Error -- wrong attribute position.\n");

      /* get the parameter's mode */
      (void) printf(
         "Parameter's mode -- %d\n.", OCITypeElemParamMode(envhp, errhp, pdo));

      /* get the parameter's required flag */
      OCI_TYPEPARAM_IS_REQUIRED(OCITypeElemFlags(envhp, errhp, pdo)) ?
        (void) printf("parameter's required flag is TRUE  -- %d\n.",
                       OCITypeElemParamMode(envhp, errhp, pdo)):
        (void) printf("parameter's required flag is FALSE  -- %d\n.",
                        OCITypeElemParamMode(envhp, errhp, pdo));
      if ( OCITypeElemType(envhp, errhp, pdo, &param_tdo) == OCI_SUCCESS )
        (void) printf(" Parameter type code -- %d\n",
                         OCITypeTypeCode(envhp, errhp, param_tdo));

      /* got to verify with Zona if default value is working */
      text_value = OCITypeElemDefaultValue( envhp, errhp, pdo, &text_len);
      if (text_len)
        (void) printf(" Parameter default value -- %s\n", text_value);
    }

  }    /* end methods for loop */


}

/*****************************************************************************/
int main(int argc, char *argv[])
{
  OCIEnv *envhp;
  OCIServer *srvhp;
  OCIError *errhp;
  OCISvcCtx *svchp;
  OCISession *usrhp;
  dvoid *tmp;
  int i;


  if (argc < 4)
  {
    (void) printf(
           "Usage -- cdemort <username> <password> <upper case typename>\n");
    return (0);
  }
  /* initialize the process */
  OCIInitialize((ub4) OCI_THREADED | OCI_OBJECT,
                (dvoid *)0,  (dvoid * (*)()) 0,
                (dvoid * (*)()) 0,  (void (*)()) 0 );
  /* initialize the environment handle */
  (void) OCIEnvInit( &envhp, (ub4) OCI_DEFAULT, 21, (dvoid **) &tmp  );
  /* get the error handle */
  (void) OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &errhp,
                   (ub4) OCI_HTYPE_ERROR,
                   52, (dvoid **) &tmp);
  /* two server contexts */
  (void) OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &srvhp,
                   (ub4) OCI_HTYPE_SERVER,
                   52, (dvoid **) &tmp);
  /* attach to the server */
  (void) OCIServerAttach( srvhp, errhp, (text *) "",
                                               (sb4) 0, (ub4) OCI_DEFAULT);
  /* get the service handle */
  (void) OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &svchp,
                   (ub4) OCI_HTYPE_SVCCTX,
                   52, (dvoid **) &tmp);

  /* set attribute server context in the service context */
  (void) OCIAttrSet( (dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX,
                    (dvoid *) srvhp, (ub4) 0,
                    (ub4) OCI_ATTR_SERVER, (OCIError *) errhp);
  /* get the user handle */
  (void) OCIHandleAlloc((dvoid *)envhp, (dvoid **)&usrhp,
                   (ub4)OCI_HTYPE_SESSION, 0, (dvoid **)0);
  /* set the user name attribute */
  (void) OCIAttrSet((dvoid *) usrhp, (ub4)OCI_HTYPE_SESSION,
                   (dvoid *)argv[1], (ub4)strlen(argv[1]),
                   (ub4)OCI_ATTR_USERNAME, errhp);
  /* set the password attribute */
  (void) OCIAttrSet((dvoid *) usrhp, (ub4)OCI_HTYPE_SESSION,
                   (dvoid *)argv[2], (ub4)strlen(argv[2]),
                   (ub4)OCI_ATTR_PASSWORD, errhp);
  /* Authenticate */
  checkerr(errhp, OCISessionBegin (svchp, errhp, usrhp,
                                             OCI_CRED_RDBMS, OCI_DEFAULT));
  /* set the user context attribute */
  (void) OCIAttrSet((dvoid *)svchp, (ub4)OCI_HTYPE_SVCCTX,
                   (dvoid *)usrhp, (ub4)0,
                   (ub4)OCI_ATTR_SESSION, errhp);
  /* loop through all the types */
  for (i = 3; i < argc; i++) {
    /* dump an adt with all the types */
    (void) printf("  %s\n", argv[i]);
    unit_test_type_access(envhp, errhp, svchp, argv[i]);
  }

  checkerr(errhp, OCISessionEnd (svchp, errhp, usrhp, OCI_DEFAULT));
  /* dettach the server */
  (void) OCIServerDetach( srvhp, errhp, (ub4) OCI_DEFAULT );
  checkerr(errhp, OCIHandleFree((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX));
  checkerr(errhp, OCIHandleFree((dvoid *) errhp, (ub4) OCI_HTYPE_ERROR));
  checkerr(errhp, OCIHandleFree((dvoid *) srvhp, (ub4) OCI_HTYPE_SERVER));

  return (0);
}


