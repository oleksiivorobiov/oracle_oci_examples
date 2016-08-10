#ifdef RCSID
static char *RCSid =
   "$Header: cdemodsc.c 14-jul-99.13:10:30 mjaeger Exp $ ";
#endif /* RCSID */

/* Copyright (c) 1997, 1999, Oracle Corporation.  All rights reserved.
*/

/*
   NAME
     cdemodsc.c
   DESCRIPTION
     Tests OCIDescribeAny on ADT.

     cdemodsc takes the user name and password and a type name
     (created in the database) as command line arguments and
     dumps all the information about the type --
     its attribute types, methods,
     method parameters, etc.

   NOTES
     see routines.

   MODIFIED   (MM/DD/YY)
   mjaeger     07/14/99 - bug 808870: OCCS: convert tabs, no long lines
   svedala     09/09/98 - lines longer than 79 chars reformatted - bug 722491
   echen       06/04/97 - fix the include files
   cchau       05/29/97 - creation
*/

#ifndef CDEMODSC_ORACLE
#include "cdemodsc.h"
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
    exit(1);
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

/*----------------------------------------------------------------------*/

static void chk_methodlst(envhp, errhp, svchp, parmp, count, comment)
OCIEnv *envhp;
OCIError *errhp;
OCISvcCtx *svchp;
dvoid   *parmp;
ub4      count;
const text    *comment;
{
  sword  retval;
  ub4    pos;
  dvoid *parmdp;

  for (pos = 1; pos <= count; pos++)
  {
    checkerr(errhp, OCIParamGet((dvoid *)parmp, (ub4) OCI_DTYPE_PARAM, errhp,
                       (dvoid *)&parmdp, (ub4) pos));
    chk_method(envhp, errhp, svchp, parmdp, comment);
  }
}

/*----------------------------------------------------------------------*/

static void chk_method(envhp, errhp, svchp, parmp, comment)
OCIEnv *envhp;
OCIError *errhp;
OCISvcCtx *svchp;
dvoid   *parmp;
const text    *comment;
{
  sword  retval;
  text   method[MAXNAME],
        *namep;
  ub4    size;
  ub4    num_arg;
  ub1    has_result,
         is_selfish,
         is_virtual,
         is_inline,
         is_constructor,
         is_destructor,
         is_constant,
         is_operator,
         is_map,
         is_order,
         is_rnds,
         is_rnps,
         is_wnds,
         is_wnps;
  OCITypeEncap encap;
  dvoid *list_arg;

  /* get name of the method */
  checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &namep, (ub4 *) &size,
                    (ub4) OCI_ATTR_NAME, (OCIError *) errhp));

  (void) strncpy((char *)method, (char *)namep, (size_t) size);
  method[size] = '\0';

  /* get the number of arguments */
  checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &num_arg, (ub4 *) 0,
                    (ub4) OCI_ATTR_NUM_ARGS, (OCIError *) errhp));

  /* encapsulation (public?) */
  checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &encap, (ub4 *) 0,
                    (ub4) OCI_ATTR_ENCAPSULATION, (OCIError *) errhp));

  /* has result */
  checkerr(errhp, OCIAttrGet((dvoid *) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid *)&has_result, (ub4 *)0,
                    (ub4)OCI_ATTR_HAS_RESULT, (OCIError *) errhp));

  /* map method */
  checkerr(errhp, OCIAttrGet((dvoid *) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid *)&is_map, (ub4 *)0,
                    (ub4)OCI_ATTR_IS_MAP, (OCIError *) errhp));

  /* order method */
  checkerr(errhp, OCIAttrGet((dvoid *) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid *)&is_order, (ub4 *)0,
                    (ub4)OCI_ATTR_IS_ORDER, (OCIError *) errhp));

  /* selfish method */
  checkerr(errhp, OCIAttrGet((dvoid *) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid *)&is_selfish, (ub4 *)0,
                    (ub4)OCI_ATTR_IS_SELFISH, (OCIError *) errhp));

  /* virtual method */
  checkerr(errhp, OCIAttrGet((dvoid *) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid *)&is_virtual, (ub4 *)0,
                    (ub4)OCI_ATTR_IS_VIRTUAL, (OCIError *) errhp));

  /* inline method */
  checkerr(errhp, OCIAttrGet((dvoid *) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid *)&is_inline, (ub4 *)0,
                    (ub4)OCI_ATTR_IS_INLINE, (OCIError *) errhp));

  /* constant method */
  checkerr(errhp, OCIAttrGet((dvoid *) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid *)&is_constant, (ub4 *)0,
                    (ub4)OCI_ATTR_IS_CONSTANT, (OCIError *) errhp));

  /* operator */
  checkerr(errhp, OCIAttrGet((dvoid *) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid *)&is_operator, (ub4 *)0,
                    (ub4)OCI_ATTR_IS_OPERATOR, (OCIError *) errhp));

  /* constructor method */
  checkerr(errhp, OCIAttrGet((dvoid *) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid *)&is_constructor, (ub4 *)0,
                    (ub4)OCI_ATTR_IS_CONSTRUCTOR, (OCIError *) errhp));

  /* destructor method */
  checkerr(errhp, OCIAttrGet((dvoid *) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid *)&is_destructor, (ub4 *)0,
                    (ub4)OCI_ATTR_IS_DESTRUCTOR, (OCIError *) errhp));

  checkerr(errhp, OCIAttrGet((dvoid *) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid *)&is_rnds, (ub4 *)0,
                    (ub4)OCI_ATTR_IS_RNDS, (OCIError *) errhp));
  checkerr(errhp, OCIAttrGet((dvoid *) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid *)&is_rnps, (ub4 *)0,
                    (ub4)OCI_ATTR_IS_RNPS, (OCIError *) errhp));
  checkerr(errhp, OCIAttrGet((dvoid *) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid *)&is_wnds, (ub4 *)0,
                    (ub4)OCI_ATTR_IS_WNDS, (OCIError *) errhp));
  checkerr(errhp, OCIAttrGet((dvoid *) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid *)&is_wnps, (ub4 *)0,
                    (ub4)OCI_ATTR_IS_WNPS, (OCIError *) errhp));

  /* get list of arguments */
  checkerr(errhp, OCIAttrGet((dvoid *) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid *)&list_arg, (ub4 *)0,
                    (ub4)OCI_ATTR_LIST_ARGUMENTS, (OCIError *) errhp));

  SPACING;
  printf ( "\n%s\n", comment);
  SPACING;
  printf ( "Name:              %s\n", method);
  SPACING;
  printf ( "Number of args:    %d\n", num_arg);
  SPACING;
  printf ( "Encapsulation:     %s\n",
           (encap==OCI_TYPEENCAP_PUBLIC) ? "public" : "private");
  SPACING;
  printf ( "Has result:        %d\n", has_result);
  SPACING;
  printf ( "Is selfish:        %d\n", is_selfish);
  SPACING;
  printf ( "Is virtual:        %d\n", is_virtual);
  SPACING;
  printf ( "Is inline:         %d\n", is_inline);
  SPACING;
  printf ( "Is constructor:    %d\n", is_constructor);
  SPACING;
  printf ( "Is desctructor:    %d\n", is_destructor);
  SPACING;
  printf ( "Is constant:       %d\n", is_constant);
  SPACING;
  printf ( "Is operator:       %d\n", is_operator);
  SPACING;
  printf ( "Is map:            %d\n", is_map);
  SPACING;
  printf ( "Is order:          %d\n", is_order);
  SPACING;
  printf ( "Is RNDS:           %d\n", is_rnds);
  SPACING;
  printf ( "Is RNPS:           %d\n", is_rnps);
  SPACING;
  printf ( "Is WNPS:           %d\n", is_wnps);
  printf("\n");

  if (has_result)
    chk_arg(envhp, errhp, svchp, list_arg, OCI_PTYPE_TYPE_RESULT, 0, 1);
  if (num_arg > 0)
    chk_arg(envhp, errhp, svchp, list_arg, OCI_PTYPE_TYPE_ARG, 1, num_arg + 1);
}

/*----------------------------------------------------------------------*/

static void chk_arglst(envhp, errhp, svchp, parmp)
OCIEnv *envhp;
OCIError *errhp;
OCISvcCtx *svchp;
dvoid   *parmp;
{
  dvoid *arglst;
  ub4   numargs;
  ub1   ptype;
  sword retval;

  /* get list of arguments */
  checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &arglst, (ub4 *) 0,
                    (ub4) OCI_ATTR_LIST_ARGUMENTS, (OCIError *) errhp));

  /* get number of parameters */
  checkerr(errhp, OCIAttrGet((dvoid*) arglst, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &numargs, (ub4 *) 0,
                    (ub4) OCI_ATTR_NUM_PARAMS, (OCIError *) errhp));

  checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &ptype, (ub4 *) 0,
                    (ub4) OCI_ATTR_PTYPE, (OCIError *) errhp));

  switch (ptype)
  {
  case OCI_PTYPE_FUNC:
    chk_arg (envhp, errhp, svchp, arglst, OCI_PTYPE_ARG, 0, numargs);
    break;
  case OCI_PTYPE_PROC:
    chk_arg (envhp, errhp, svchp, arglst, OCI_PTYPE_ARG, 1, numargs);
  }
}

/*----------------------------------------------------------------------*/

static void chk_arg (envhp, errhp, svchp, parmp, type, start, end)
OCIEnv *envhp;
OCIError *errhp;
OCISvcCtx *svchp;
dvoid *parmp;
ub1    type;
ub4    start;
ub4    end;
{
  text  argname[NPOS][30];
  text  *namep;
  ub4   sizep;
  ub2   collen[NPOS];
  ub2   coldesr[NPOS];
  dvoid *parmdp;
  ub4   i, pos;
  sword retval;
  ub2   level[NPOS];
  ub1   radix[NPOS], def[NPOS];
  ub4   iomode[NPOS];
  ub1   precision[NPOS], scale[NPOS], isnull[NPOS];


  for (pos = start; pos < end; pos++)
  {

    checkerr(errhp, OCIParamGet((dvoid *)parmp, (ub4) OCI_DTYPE_PARAM, errhp,
                       (dvoid *)&parmdp, (ub4) pos));

    /* get data type */
    checkerr(errhp, OCIAttrGet((dvoid*) parmdp, (ub4) OCI_DTYPE_PARAM,
                      (dvoid*) &coldesr[pos], (ub4 *) 0,
                      (ub4) OCI_ATTR_DATA_TYPE,
                      (OCIError *) errhp));

    /* method's result has no name */
    iomode[pos] = 0;
    def[pos] = 0;
    sizep = 0;
    if (type != OCI_PTYPE_TYPE_RESULT)
    {
      /* has default */
      checkerr(errhp, OCIAttrGet((dvoid *)parmdp, (ub4) OCI_DTYPE_PARAM,
                        (dvoid *)&def[pos], (ub4 *)0,
                        (ub4)OCI_ATTR_HAS_DEFAULT, (OCIError *) errhp));

      /* get iomode */
      checkerr(errhp, OCIAttrGet((dvoid*) parmdp, (ub4) OCI_DTYPE_PARAM,
                        (dvoid*) &iomode[pos], (ub4 *) 0,
                        (ub4) OCI_ATTR_IOMODE, (OCIError *) errhp));

      /* get argument name */
      checkerr(errhp, OCIAttrGet((dvoid*) parmdp, (ub4) OCI_DTYPE_PARAM,
                        (dvoid*) &namep, (ub4 *) &sizep,
                        (ub4) OCI_ATTR_NAME, (OCIError *) errhp));

      (void) strncpy((char *)argname[pos], (char *)namep, (size_t) sizep);
    }
    argname[pos][sizep] = '\0';

    /* the following are not for type arguments and results */
    precision[pos] = 0;
    scale[pos] = 0;
    collen[pos] = 0;
    level[pos] = 0;
    radix[pos] = 0;
    isnull[pos] = FALSE;
    if (type != OCI_PTYPE_TYPE_ARG && type != OCI_PTYPE_TYPE_RESULT)
    {
      /* get the data size */
      checkerr(errhp, OCIAttrGet((dvoid*) parmdp, (ub4) OCI_DTYPE_PARAM,
                        (dvoid*) &collen[pos], (ub4 *) 0,
                        (ub4) OCI_ATTR_DATA_SIZE, (OCIError *) errhp));

      /* get the precision of the attribute */
      checkerr(errhp, OCIAttrGet((dvoid*) parmdp, (ub4) OCI_DTYPE_PARAM,
                        (dvoid*) &precision, (ub4 *) 0,
                        (ub4) OCI_ATTR_PRECISION, (OCIError *) errhp));

      /* get the scale of the attribute */
      checkerr(errhp, OCIAttrGet((dvoid*) parmdp, (ub4) OCI_DTYPE_PARAM,
                        (dvoid*) &scale, (ub4 *) 0,
                        (ub4) OCI_ATTR_SCALE, (OCIError *) errhp));

      /* get the level of the attribute */
      checkerr(errhp, OCIAttrGet((dvoid*) parmdp, (ub4) OCI_DTYPE_PARAM,
                        (dvoid*) &level[pos], (ub4 *) 0,
                        (ub4) OCI_ATTR_LEVEL, (OCIError *) errhp));

      /* get the radix of the attribute */
      checkerr(errhp, OCIAttrGet((dvoid*) parmdp, (ub4) OCI_DTYPE_PARAM,
                        (dvoid*) &radix[pos], (ub4 *) 0,
                        (ub4) OCI_ATTR_RADIX, (OCIError *) errhp));

      /* is null */
      checkerr(errhp, OCIAttrGet((dvoid*) parmdp, (ub4) OCI_DTYPE_PARAM,
                        (dvoid*) &isnull, (ub4 *) 0,
                        (ub4) OCI_ATTR_IS_NULL, (OCIError *) errhp));

      /* should get error 24328 */
      if (OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                     (dvoid*) &isnull, (ub4 *) 0,
                     (ub4) OCI_ATTR_INDEX_ONLY, (OCIError *) errhp)
          != OCI_ERROR)
        printf("ERROR: should get error here\n");
    }
  }

  SPACING;
  (void) printf("Argument Name  Length  Datatype  Level Radix Default Iomode"
                " Prec Scl Null\n");
  SPACING;
  (void) printf("___________________________________________________"
                "______________________\n");
  for (i = start; i < end; i++)
  {
    SPACING;
    (void) printf( "%15s%6d%8d%6d%6d     %c%6d%9d%4d%4d\n",  argname[i],
                        collen[i], coldesr[i], level[i], radix[i],
                        (def[i])?'y':'n', iomode[i], precision[i], scale[i],
                        isnull[i]);
  }
  printf("\n");

}

static void chk_collection (envhp, errhp, svchp, parmp, is_array)
OCIEnv *envhp;
OCIError *errhp;
OCISvcCtx *svchp;
dvoid   *parmp;
sword    is_array;
{
  text         schema[MAXNAME],
               type[MAXNAME],
              *namep;
  ub4          size;
  ub2          len;
  ub4          num_elems;
  OCITypeCode  typecode;
  sword        retval;

  /* get the data size */
  checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &len, (ub4 *) 0,
                    (ub4) OCI_ATTR_DATA_SIZE, (OCIError *) errhp));

  /* get the name of the collection */
  checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &namep, (ub4 *) &size,
                    (ub4) OCI_ATTR_TYPE_NAME, (OCIError *) errhp));

  (void) strncpy((char *)type, (char *)namep, (size_t) size);
  type[size] = '\0';

  /* get the name of the schema */
  checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &namep, (ub4 *) &size,
                    (ub4) OCI_ATTR_SCHEMA_NAME, (OCIError *) errhp));

  (void) strncpy((char *)schema, (char *)namep, (size_t) size);
  schema[size] = '\0';

  /* get the data type */
  checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &typecode, (ub4 *) 0, (ub4) OCI_ATTR_DATA_TYPE,
                    (OCIError *) errhp));

  num_elems = 0;
  if (is_array)
    /* get the number of elements */
    checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                      (dvoid*) &num_elems, (ub4 *) 0,
                      (ub4) OCI_ATTR_NUM_ELEMS, (OCIError *) errhp));

  SPACING;
  (void)
    printf ( "Schema    Type            Length   Datatype Elements\n");
  SPACING;
  (void)
    printf ( "____________________________________________________\n");
  SPACING;
  (void) printf( "%10s%16s%6d%11d%9d\n",  schema, type, len, typecode,
                  num_elems);
  printf("\n");
}

/*----------------------------------------------------------------------*/

static void chk_column(envhp, errhp, svchp, parmp, parmcnt)
OCIEnv *envhp;
OCIError *errhp;
OCISvcCtx *svchp;
dvoid *parmp;
ub4 parmcnt;
{
  text  colname1[NPOS][30], colname2[NPOS][30], colname3[NPOS][30];
  text  *namep;
  ub4   sizep;
  ub2   collen[NPOS];
  ub2   coldesr[NPOS];
  dvoid *parmdp;
  ub4   i, pos;
  sword retval;

  /* loop through all the attributes in the type and get all information */
  for (pos = 1; pos <= parmcnt; pos++)
  {
    /* get the parameter list for each attribute */
    checkerr(errhp, OCIParamGet((dvoid *) parmp, (ub4) OCI_DTYPE_PARAM, errhp,
                       (dvoid *)&parmdp, (ub4) pos));

    /* size of the attribute (non ADT or REF) objects */
    checkerr(errhp, OCIAttrGet((dvoid*) parmdp, (ub4) OCI_DTYPE_PARAM,
                      (dvoid*) &collen[pos-1], (ub4 *) 0,
                      (ub4) OCI_ATTR_DATA_SIZE, (OCIError *) errhp));

    /* name of the attribute */
    checkerr(errhp, OCIAttrGet((dvoid*) parmdp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &namep, (ub4 *) &sizep,
                    (ub4) OCI_ATTR_NAME, (OCIError *) errhp));

    (void) strncpy((char *)colname1[pos-1], (char *)namep, (size_t) sizep);
    colname1[pos-1][sizep] = '\0';

    /* get the schema name */
    checkerr(errhp, OCIAttrGet((dvoid*) parmdp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &namep, (ub4 *) &sizep,
                    (ub4) OCI_ATTR_SCHEMA_NAME, (OCIError *) errhp));

    (void) strncpy((char *)colname2[pos-1], (char *)namep, (size_t) sizep);
    colname2[pos-1][sizep] = '\0';

    /* name of the attribute */
    checkerr(errhp, OCIAttrGet((dvoid*) parmdp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &namep, (ub4 *) &sizep,
                    (ub4) OCI_ATTR_TYPE_NAME, (OCIError *) errhp));

    (void) strncpy((char *)colname3[pos-1], (char *)namep, (size_t) sizep);
    colname3[pos-1][sizep] = '\0';

    /* get data type */
    checkerr(errhp, OCIAttrGet((dvoid*) parmdp, (ub4) OCI_DTYPE_PARAM,
                      (dvoid*) &coldesr[pos-1], (ub4 *) 0,
                      (ub4) OCI_ATTR_DATA_TYPE,
                      (OCIError *) errhp));

    if (coldesr[pos-1] == SQLT_NTY || coldesr[pos-1] == SQLT_REF)
    {
      /* call tst_desc_type here if the type is ADT or REF */
      tab += 5;
      SPACING;
      printf("!!!!ATTRIBUTE IS A TYPE OR REF!!!!\n");
      SPACING;
      printf("ATTRIBUTE NAME IS %s\n", colname3[pos-1]);
      SPACING;
      printf("ATTRIBUTE TYPE IS %d\n", coldesr[pos-1]);
      tst_desc_type(envhp, errhp, svchp, colname3[pos-1]);
      tab -= 5;
      printf("\n");
    }

  }

  SPACING;
  (void)
    printf ( "Column Name    Schema    Type            Length   Datatype\n");
  SPACING;
  (void)
    printf ( "__________________________________________________________\n");
  for (i = 1; i <= parmcnt; i++)
  {
    SPACING;
    (void) printf( "%15s%10s%16s%6d%8d\n",  colname1[i-1], colname2[i-1],
                    colname3[i-1], collen[i-1], coldesr[i-1] );
  }
  printf("\n");
}

/*----------------------------------------------------------------------*/

static void tst_desc_type(envhp, errhp, svchp, objname)
OCIEnv *envhp;
OCIError *errhp;
OCISvcCtx *svchp;
text *objname;
{
  OCIDescribe *dschp = (OCIDescribe *) 0;
  sword  retval;
  OCITypeCode typecode,
         collection_typecode;
  text   schema[MAXNAME],
         version[MAXNAME],
        *namep,
        *type_name;
  ub4    size,
         text_len;
  OCIRef *type_ref;
  ub2    num_attr,
         num_method;
  ub1    is_incomplete,
         is_system,
         is_predefined,
         is_transient,
         is_sysgen,
         has_table,
         has_lob,
         has_file;
  dvoid *list_attr,
        *list_method,
        *map_method,
        *order_method,
        *collection_dschp,
        *some_object;
  OCIParam *parmp;
  ub1 objtype;

  /* must allocate describe handle first for OCIDescribeAny */
  checkerr(errhp, OCIHandleAlloc((dvoid *) envhp, (dvoid **) &dschp,
                        (ub4) OCI_HTYPE_DESCRIBE,
                        (size_t) 0, (dvoid **) 0));

  /* call OCIDescribeAny and passing in the type name */
  checkerr(errhp, OCIDescribeAny(svchp, errhp, (text *)objname,
                  (ub4) strlen((char *)objname), OCI_OTYPE_NAME, (ub1)1,
                  (ub1) OCI_PTYPE_TYPE, dschp));

  /* get the parameter list for the requested type */
  checkerr(errhp, OCIAttrGet((dvoid *) dschp, (ub4) OCI_HTYPE_DESCRIBE,
                  (dvoid *)&parmp, (ub4 *)0, (ub4)OCI_ATTR_PARAM, errhp));

  /* get the schema name for the requested type */
  checkerr(errhp, OCIAttrGet((dvoid*) parmp,(ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &namep, (ub4 *) &size,
                    (ub4) OCI_ATTR_SCHEMA_NAME, (OCIError *) errhp));

  (void) strncpy((char *)schema, (char *)namep, (size_t) size);
  schema[size] = '\0';

  /* get the type code for the requested type */
  checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &typecode, (ub4 *) 0,
                    (ub4) OCI_ATTR_TYPECODE, (OCIError *) errhp));

  /* get other information for collection type */
  if (typecode == OCI_TYPECODE_NAMEDCOLLECTION)
  {
    checkerr(errhp, OCIAttrGet((dvoid *) parmp, (ub4) OCI_DTYPE_PARAM,
                      (dvoid *)&collection_typecode, (ub4 *)0,
                      (ub4)OCI_ATTR_COLLECTION_TYPECODE, (OCIError *)errhp));
    checkerr(errhp, OCIAttrGet((dvoid *) parmp, (ub4) OCI_DTYPE_PARAM,
                      (dvoid *)&collection_dschp, (ub4 *)0,
                      (ub4)OCI_ATTR_COLLECTION_ELEMENT, (OCIError *)errhp));
    checkerr(errhp, OCIAttrGet((dvoid *) parmp, (ub4) OCI_DTYPE_PARAM,
                      (dvoid *)&collection_dschp, (ub4 *)0,
                      (ub4)OCI_ATTR_COLLECTION_ELEMENT, (OCIError *)errhp));
  }

  /* get the ref to the type descriptor */
  checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &type_ref, (ub4 *) 0,
                    (ub4) OCI_ATTR_REF_TDO, (OCIError *) errhp));

  /* get the type version */
  checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &namep, (ub4 *) &size,
                    (ub4) OCI_ATTR_VERSION, (OCIError *) errhp));

  (void) strncpy((char *)version, (char *)namep, (size_t) size);
  version[size] = '\0';

  /* incomplete type */
  checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &is_incomplete, (ub4 *) 0,
                    (ub4) OCI_ATTR_IS_INCOMPLETE_TYPE, (OCIError *) errhp));

  /* system type */
  checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &is_system, (ub4 *) 0,
                    (ub4) OCI_ATTR_IS_SYSTEM_TYPE, (OCIError *) errhp));

  /* predefined type */
  checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &is_predefined, (ub4 *) 0,
                    (ub4) OCI_ATTR_IS_PREDEFINED_TYPE, (OCIError *) errhp));

  /* transient type */
  checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &is_transient, (ub4 *) 0,
                    (ub4) OCI_ATTR_IS_TRANSIENT_TYPE, (OCIError *) errhp));

  /* system generated type */
  checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                  (dvoid*) &is_sysgen, (ub4 *) 0,
                  (ub4) OCI_ATTR_IS_SYSTEM_GENERATED_TYPE, (OCIError*) errhp));

  /* has nested table */
  checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &has_table, (ub4 *) 0,
                    (ub4) OCI_ATTR_HAS_NESTED_TABLE, (OCIError *) errhp));

  /* has lob */
  checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &has_lob, (ub4 *) 0,
                    (ub4) OCI_ATTR_HAS_LOB, (OCIError *) errhp));

  /* has file */
  checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &has_file, (ub4 *) 0,
                    (ub4) OCI_ATTR_HAS_FILE, (OCIError *) errhp));

  /* get the list of attributes */
  checkerr(errhp, OCIAttrGet((dvoid *) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid *)&list_attr, (ub4 *)0,
                    (ub4)OCI_ATTR_LIST_TYPE_ATTRS, (OCIError *)errhp));

  /* number of attributes */
  checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &num_attr, (ub4 *) 0,
                    (ub4) OCI_ATTR_NUM_TYPE_ATTRS, (OCIError *) errhp));

  /* get method list */
  checkerr(errhp, OCIAttrGet((dvoid *) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid *)&list_method, (ub4 *)0,
                    (ub4)OCI_ATTR_LIST_TYPE_METHODS, (OCIError *)errhp));

  /* get number of methods */
  checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &num_method, (ub4 *) 0,
                    (ub4) OCI_ATTR_NUM_TYPE_METHODS, (OCIError *) errhp));

  /* get map method list */
  checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &map_method, (ub4 *) 0,
                    (ub4) OCI_ATTR_MAP_METHOD, (OCIError *) errhp));

  /* get order method list*/
  checkerr(errhp, OCIAttrGet((dvoid*) parmp, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &order_method, (ub4 *) 0,
                    (ub4) OCI_ATTR_ORDER_METHOD, (OCIError *) errhp));

  SPACING;
  printf ( "TYPE     : Attributes : \n");
  SPACING;
  printf ( "Schema:            %s\n", schema);
  SPACING;
  printf ( "Typecode:          %d\n", typecode);
  if (typecode == OCI_TYPECODE_NAMEDCOLLECTION)
  {
    SPACING;
    printf ( "Collection typecode: %d\n", collection_typecode);
  }
  SPACING;
  printf ( "Version:           %s\n", version);
  SPACING;
  printf ( "Number of attrs:   %d\n", num_attr);
  SPACING;
  printf ( "Number of methods: %d\n", num_method);
  SPACING;
  printf ( "Is incomplete:     %d\n", is_incomplete);
  SPACING;
  printf ( "Is system:         %d\n", is_system);
  SPACING;
  printf ( "Is predefined:     %d\n", is_predefined);
  SPACING;
  printf ( "Is sys-gen:        %d\n", is_sysgen);
  SPACING;
  printf ( "Is transient:      %d\n", is_transient);
  SPACING;
  printf ( "Has nested table:  %d\n", has_table);
  SPACING;
  printf ( "Has LOB:           %d\n", has_lob);
  SPACING;
  printf ( "Has file:          %d\n", has_file);
  printf("\n");

  if (num_attr > 0)
    chk_column(envhp, errhp, svchp, list_attr, num_attr);
  else if (typecode == OCI_TYPECODE_NAMEDCOLLECTION)
    chk_collection(envhp, errhp, svchp, collection_dschp,
                   collection_typecode == OCI_TYPECODE_VARRAY);
  if (map_method != (dvoid *)0)
    chk_method(envhp, errhp, svchp, map_method,
                                         "TYPE MAP METHOD\n---------------");
  if (order_method != (dvoid *)0)
    chk_method(envhp, errhp, svchp, order_method,
                                     "TYPE ORDER METHOD\n-----------------");
  if (num_method > 0)
    chk_methodlst(envhp, errhp, svchp, list_method, num_method,
                                                 "TYPE METHOD\n-----------");
}


/*****************************************************************************/
int main(int argc, char *argv[])
{
  OCIEnv *envhp = (OCIEnv *) 0;
  OCIServer *srvhp = (OCIServer *) 0;
  OCIError *errhp = (OCIError *) 0;
  OCISvcCtx *svchp = (OCISvcCtx *) 0;
  OCISession *usrhp = (OCISession *) 0;
  dvoid *tmp;
  int i;

  tab = 0;

  if (argc < 4)
  {
    (void) printf(
            "Usage -- cdemort <username> <password> <upper case typename>\n");
    return (0);
  }

  (void) OCIInitialize((ub4) OCI_THREADED | OCI_OBJECT,
                (dvoid *)0,  (dvoid * (*)()) 0,
                (dvoid * (*)()) 0,  (void (*)()) 0 );

  (void) OCIHandleAlloc( (dvoid *) NULL, (dvoid **) &envhp,
                   (ub4) OCI_HTYPE_ENV,
                   52, (dvoid **) &tmp);

  (void) OCIEnvInit( &envhp, (ub4) OCI_DEFAULT, 21, (dvoid **) &tmp  );

  (void) OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &errhp,
                   (ub4) OCI_HTYPE_ERROR,
                   52, (dvoid **) &tmp);

  (void) OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &srvhp,
                   (ub4) OCI_HTYPE_SERVER,
                   52, (dvoid **) &tmp);

  checkerr(errhp, OCIServerAttach( srvhp, errhp, (text *) "",
                 (sb4) strlen(""), (ub4) OCI_DEFAULT));

  checkerr(errhp, OCIHandleAlloc( (dvoid *) envhp, (dvoid **) &svchp,
                   (ub4) OCI_HTYPE_SVCCTX,
                   52, (dvoid **) &tmp));

  checkerr(errhp, OCIAttrSet( (dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX,
                    (dvoid *) srvhp, (ub4) 0,
                    (ub4) OCI_ATTR_SERVER, (OCIError *) errhp));

  checkerr(errhp, OCIHandleAlloc((dvoid *)envhp, (dvoid **)&usrhp,
                   (ub4)OCI_HTYPE_SESSION, 0, (dvoid **)0));

  checkerr(errhp, OCIAttrSet((dvoid *) usrhp, (ub4)OCI_HTYPE_SESSION,
                   (dvoid *)argv[1], (ub4)strlen(argv[1]),
                   (ub4)OCI_ATTR_USERNAME, errhp));

  checkerr(errhp, OCIAttrSet((dvoid *) usrhp, (ub4)OCI_HTYPE_SESSION,
                   (dvoid *)argv[2], (ub4)strlen(argv[2]),
                   (ub4)OCI_ATTR_PASSWORD, errhp));

  checkerr(errhp, OCISessionBegin (svchp, errhp, usrhp,
                                               OCI_CRED_RDBMS, OCI_DEFAULT));

  checkerr(errhp, OCIAttrSet((dvoid *)svchp, (ub4)OCI_HTYPE_SVCCTX,
                   (dvoid *)usrhp, (ub4)0,
                   (ub4)OCI_ATTR_SESSION, errhp));

  /* dump an adt with all the types */
  SPACING;
  (void) printf("%s\n", argv[3]);
  tst_desc_type(envhp, errhp, svchp, argv[3]);
  printf("\n");

  checkerr(errhp, OCISessionEnd (svchp, errhp, usrhp, OCI_DEFAULT));

  (void) OCIServerDetach( srvhp, errhp, (ub4) OCI_DEFAULT );

  checkerr(errhp, OCIHandleFree((dvoid *) svchp, (ub4) OCI_HTYPE_SVCCTX));
  checkerr(errhp, OCIHandleFree((dvoid *) errhp, (ub4) OCI_HTYPE_ERROR));
  checkerr(errhp, OCIHandleFree((dvoid *) srvhp, (ub4) OCI_HTYPE_SERVER));

  return (0);
}


/* end of file cdemodsc.c */

